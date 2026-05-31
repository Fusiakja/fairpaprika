#' Check whether a computed solution satisfies all PAPRIKA constraints
#'
#' Post-hoc validation of the LP solution (weights) against:
#' - Anchors (lowest level = 0)
#' - Monotonicity (adjacent levels increasing by >= epsilon_monotone)
#' - Preference constraints:
#'     pref == "A":  u(A1) - u(A2) >= eps_strict
#'     pref == "E": |u(A1) - u(A2)| <= tau_equal
#' - Normalization: sum(top-levels) == normalize_sum_top
#'
#' @param engine a paprika_engine
#' @param tol numeric tolerance used for equality checks (anchors, normalization)
#' @return a list with ok (TRUE/FALSE), plus diagnostics of violations
#' @export
engine_check_solution <- function(engine, tol = 1e-8) {
  engine <- validate_engine(engine)
  s <- engine$settings
  d <- engine$decisions

  if (is.null(engine$weights) || !nrow(engine$weights)) {
    return(list(ok = FALSE, reason = "no weights computed"))
  }

  # weights_df -> named vector
  w <- setNames(as.numeric(engine$weights$Nutzen), engine$weights$Merkmal)

  # --- CRITICAL: bring weights back to the solver's normalization scale ---
  crit <- names(engine$domains)
  top_keys <- vapply(crit, function(cn) paste0(cn, ":", tail(engine$domains[[cn]], 1)), character(1))
  sum_top <- sum(w[top_keys], na.rm = TRUE)

  if (!is.finite(sum_top) || sum_top <= 0) {
    return(list(ok = FALSE, reason = "sum(top-levels) <= 0 after scaling"))
  }

  # Renormalize so sum(top-levels) == normalize_sum_top (usually 1)
  w <- w * (s$normalize_sum_top / sum_top)

  # Build constraints exactly like the solver did
  con <- constraints_from_decisions(
    engine$domains, d,
    eps_strict = s$eps_strict,
    tau_equal = s$tau_equal,
    epsilon_monotone = s$epsilon_monotone,
    normalize_sum_top = s$normalize_sum_top,
    interactions = s$interactions %||% list()
  )

  # Evaluate LHS against constraints
  A <- con$A
  b <- con$b
  dir <- con$dir

  lhs <- as.numeric(A %*% w[con$var_names])

  # compute violations
  viol <- logical(length(b))
  margin <- numeric(length(b))

  for (i in seq_along(b)) {
    if (dir[i] == ">=") {
      margin[i] <- lhs[i] - b[i]
      viol[i] <- margin[i] < -tol
    } else if (dir[i] == "<=") {
      margin[i] <- b[i] - lhs[i]
      viol[i] <- margin[i] < -tol
    } else if (dir[i] == "=") {
      margin[i] <- -abs(lhs[i] - b[i])
      viol[i] <- abs(lhs[i] - b[i]) > tol
    } else {
      stop("Unknown dir in constraints.")
    }
  }

  list(
    ok = !any(viol),
    n_violations = sum(viol),
    worst = min(margin, na.rm = TRUE),
    violating_rows = which(viol),
    tol = tol
  )
}

#' Pretty-print an engine solution check
#'
#' Displays summary fields from [engine_check_solution()].
#'
#' @param x A result from [engine_check_solution()].
#' @param ... Ignored.
#' @export
print.engine_solution_check <- function(x, ...) {
  cat("<engine_solution_check>\n")
  cat(" ok:", x$ok, "\n")
  if (!is.null(x$tol)) cat(" tol:", x$tol, "\n")
  cat(" violations:", x$n_violations, "\n")
  if (isFALSE(x$ok) && length(x$violating_rows)) {
    cat(" violating rows:", paste(x$violating_rows, collapse = ", "), "\n")
    cat(" worst margin:", x$worst, "\n")
  }
  invisible(x)
}

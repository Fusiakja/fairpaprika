#' Build utility coefficient vector for an alternative
#' @keywords internal
.ror_alt_coef <- function(engine, alt_idx) {
  stopifnot(!is.null(engine$var_idx))
  alt <- engine$alternatives[alt_idx, , drop = FALSE]
  vars <- paste(names(alt), as.character(alt[1, ]), sep = ":")
  coef <- rep(0, length(engine$var_idx))
  names(coef) <- names(engine$var_idx)
  coef[vars] <- 1
  # include interaction terms if present
  inter_pairs <- engine$settings$interactions$pairs %||% list()
  if (length(inter_pairs)) {
    for (pair in inter_pairs) {
      if (length(pair) != 2) next
      ci <- pair[1]; cj <- pair[2]
      if (!(ci %in% names(alt) && cj %in% names(alt))) next
      li <- as.character(alt[[ci]]); lj <- as.character(alt[[cj]])
      nm <- paste(ci, li, cj, lj, sep = "::")
      if (!is.na(engine$var_idx[[nm]] %||% NA)) {
        coef[nm] <- 1
      } else {
        # also try swapped order in case var_idx uses the other permutation
        nm2 <- paste(cj, lj, ci, li, sep = "::")
        if (!is.na(engine$var_idx[[nm2]] %||% NA)) coef[nm2] <- 1
      }
    }
  }
  coef
}

#' Solve min/max utility difference under current constraints (ROR)
#' @keywords internal
.ror_delta <- function(engine, a_idx, b_idx, sense = c("min", "max")) {
  engine <- validate_engine(engine)
  sense <- match.arg(sense)

  con <- constraints_from_decisions(
    engine$domains, engine$decisions,
    eps_strict = engine$settings$eps_strict,
    tau_equal = engine$settings$tau_equal,
    epsilon_monotone = engine$settings$epsilon_monotone,
    normalize_sum_top = engine$settings$normalize_sum_top,
    interactions = engine$settings$interactions %||% list()
  )

  ca <- .ror_alt_coef(engine, a_idx)
  cb <- .ror_alt_coef(engine, b_idx)
  obj <- ca - cb

  dir <- if (sense == "min") "min" else "max"
  res <- lpSolve::lp(dir, obj, con$A, con$dir, con$b)
  list(ok = res$status == 0, value = res$objval, status = res$status)
}

#' Necessary / possible preference checks (ROR)
#' @param engine A `paprika_engine`.
#' @param a_idx Index of alternative A.
#' @param b_idx Index of alternative B.
#' @export
necessary_pref <- function(engine, a_idx, b_idx) {
  out <- .ror_delta(engine, a_idx, b_idx, sense = "min")
  isTRUE(out$ok) && out$value >= 0
}

#' Possible preference check
#' @param engine A `paprika_engine`.
#' @param a_idx Index of alternative A.
#' @param b_idx Index of alternative B.
#' @export
possible_pref <- function(engine, a_idx, b_idx) {
  out <- .ror_delta(engine, a_idx, b_idx, sense = "max")
  isTRUE(out$ok) && out$value >= 0
}

#' Necessary top-3 (pairwise sufficient check)
#' @param engine A `paprika_engine`.
#' @export
necessary_top3 <- function(engine) {
  engine <- validate_engine(engine)
  nA <- nrow(engine$alternatives)
  res <- rep(FALSE, nA)
  for (i in seq_len(nA)) {
    beaters <- 0L
    for (j in seq_len(nA)) {
      if (i == j) next
      out <- .ror_delta(engine, j, i, sense = "min") # j >= i?
      if (isTRUE(out$ok) && out$value >= 0) beaters <- beaters + 1L
      if (beaters > 2L) break
    }
    res[i] <- beaters <= 2L
  }
  res
}

#' Possible top-3 (pairwise necessary-domination proxy)
#' @param engine A `paprika_engine`.
#' @export
possible_top3 <- function(engine) {
  engine <- validate_engine(engine)
  nA <- nrow(engine$alternatives)
  res <- rep(TRUE, nA)
  for (i in seq_len(nA)) {
    beaters <- 0L
    for (j in seq_len(nA)) {
      if (i == j) next
      out <- .ror_delta(engine, j, i, sense = "min")
      if (isTRUE(out$ok) && out$value >= 0) beaters <- beaters + 1L
      if (beaters >= 3L) {
        res[i] <- FALSE
        break
      }
    }
  }
  res
}

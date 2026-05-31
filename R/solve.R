.lp_solve_quiet <- function(direction, objective.in, const.mat, const.dir, const.rhs) {
  withCallingHandlers(
    lpSolve::lp(direction, objective.in, const.mat, const.dir, const.rhs),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("number of columns of result is not a multiple of vector length", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

solve_partworths <- function(domains, decisions, settings) {
  con <- constraints_from_decisions(
    domains, decisions,
    eps_strict = settings$eps_strict,
    tau_equal = settings$tau_equal,
    epsilon_monotone = settings$epsilon_monotone,
    normalize_sum_top = settings$normalize_sum_top,
    interactions = settings$interactions %||% list()
  )

  # Optional: regularization variables (p_k/n_k) etc.
  # Strict PAPRIKA mode: disable all enhancements
  strict_mode <- isTRUE(settings$classic$strict_paprika) && settings$mode == "classic"

  if (strict_mode) {
    # Original PAPRIKA: strict feasibility, no regularization, error on infeasibility
    out <- solve_feasible(con)
    if (!isTRUE(out$ok)) {
      stop("Infeasible constraints in strict PAPRIKA mode. The answers are inconsistent.")
    }
    return(out)
  }

  # Classic mode with weak regularization for tie-breaking
  # This prevents degenerate solutions where LP solver picks arbitrary corners
  classic_with_reg <- identical(settings$mode, "classic") &&
    isTRUE(settings$classic$use_regularization %||% FALSE)

  if (classic_with_reg) {
    # Use very weak balanced regularization as tie-breaker
    strength <- settings$classic$regularization_strength %||% 0.01
    out <- solve_with_balanced_regularization(con, list(
      domains = domains,
      regularize_balanced = list(
        enabled = TRUE,
        strength = strength,
        target = "uniform"
      ),
      normalize_sum_top = settings$normalize_sum_top
    ))
    if (isTRUE(out$ok)) {
      return(out)
    }
    # If regularized solve fails, fall through to standard methods below
  }

  # Enhanced mode: try regularization if enabled, then fallback to slack
  if (isTRUE(settings$regularize)) {
    out <- solve_with_l1_regularization(con, settings)
    if (isTRUE(out$ok)) {
      return(out)
    }
  } else if (isTRUE(settings$regularize_balanced$enabled %||% FALSE)) {
    out <- solve_with_balanced_regularization(con, settings)
    if (isTRUE(out$ok)) {
      return(out)
    }
  }

  # Try strict feasibility
  out <- solve_feasible(con)
  if (isTRUE(out$ok)) {
    return(out)
  }

  # Fallback: slack recovery if enabled
  if (isTRUE(settings$slack$enabled %||% FALSE)) {
    out <- solve_with_slack(con, penalty = settings$slack$penalty %||% 1)
    if (isTRUE(out$ok)) {
      return(out)
    }
  }

  # No recovery available
  stop("Infeasible constraints and slack recovery is disabled.")
}

solve_feasible <- function(con) {
  # pure feasibility: objective = 0
  n <- length(con$var_names)
  f.obj <- rep(0, n)
  res <- .lp_solve_quiet("min", f.obj, con$A, con$dir, con$b)
  if (res$status != 0) {
    return(list(ok = FALSE, status = res$status))
  }
  w <- setNames(res$solution[seq_len(n)], con$var_names)
  list(ok = TRUE, weights = w, status = 0)
}

solve_with_l1_regularization <- function(con, settings) {
  # Minimize sum |top_k - 1/K| using slack variables (classic trick)
  var_names <- con$var_names
  n <- length(var_names)
  K <- length(con$top_idx)
  target <- settings$normalize_sum_top / K

  # augment variables: p_k, n_k >= 0 => 2K new vars
  n2 <- n + 2 * K
  A <- cbind(con$A, matrix(0, nrow(con$A), 2 * K))
  dir <- con$dir
  b <- con$b

  # constraints: top_k - p_k + n_k = target
  for (k in seq_len(K)) {
    row <- numeric(n2)
    row[con$top_idx[k]] <- 1
    row[n + (2 * k - 1)] <- -1
    row[n + (2 * k)] <- 1
    A <- rbind(A, row)
    dir <- c(dir, "=")
    b <- c(b, target)
  }

  f.obj <- c(rep(0, n), rep(1, 2 * K))
  res <- .lp_solve_quiet("min", f.obj, A, dir, b)
  if (res$status != 0) {
    return(list(ok = FALSE, status = res$status))
  }

  w <- setNames(res$solution[seq_len(n)], var_names)
  list(ok = TRUE, weights = w, status = 0)
}

solve_with_slack <- function(con, penalty = 1) {
  # Minimize total slack on constraints (L1) to restore feasibility
  A <- con$A
  dir <- con$dir
  b <- con$b
  m <- nrow(A)
  n <- ncol(A)
  if (m == 0) {
    return(solve_feasible(con))
  }

  # One slack per original constraint
  A_slack <- cbind(A, diag(m))
  dir_new <- dir
  b_new <- b

  for (i in seq_len(m)) {
    if (dir[i] == ">=") {
      A_slack[i, ] <- -A_slack[i, ]
      b_new[i] <- -b_new[i]
      dir_new[i] <- "<="
    } else if (dir[i] == "=") {
      # replace with two inequalities sharing the same slack variable
      row1 <- A_slack[i, ]
      row2 <- -A_slack[i, ]
      A_slack <- rbind(A_slack, row2)
      dir_new <- c(dir_new, "<=")
      b_new <- c(b_new, -b[i])
      dir_new[i] <- "<="
    }
  }

  f.obj <- c(rep(0, n), rep(penalty, m))
  # enforce slack >= 0 via extra constraints
  lb_rows <- cbind(matrix(0, nrow = m, ncol = n), diag(m))
  A_mat <- rbind(A_slack, lb_rows)
  dir_all <- c(dir_new, rep(">=", m))
  b_all <- c(b_new, rep(0, m))

  res <- .lp_solve_quiet("min", f.obj, A_mat, dir_all, b_all)
  if (res$status != 0) {
    return(list(ok = FALSE, status = res$status))
  }

  w <- setNames(res$solution[seq_len(n)], con$var_names)
  slack_vals <- res$solution[(n + 1):(n + m)]
  tol <- 1e-6
  violated <- which(slack_vals > tol)
  slack_info <- NULL
  if (length(violated)) {
    meta <- con$origin %||% vector("list", m)
    details <- lapply(violated, function(i) {
      o <- meta[[i]] %||% list()
      o$constraint <- i
      o$slack <- slack_vals[[i]]
      o
    })
    slack_info <- details
  }
  list(ok = TRUE, weights = w, status = res$status, slack = slack_vals, slack_info = slack_info)
}

# Balanced regularization: prefer balanced criterion importance
solve_with_balanced_regularization <- function(con, settings) {
  # Extract domains from variable names to group by criterion
  vars <- con$var_names
  # Minimize squared deviation from balanced criterion ranges
  # Objective: strength * sum |w_top_k - target_range|

  reg_cfg <- settings$regularize_balanced %||% list()
  base_strength <- reg_cfg$strength %||% 0.01
  target_type <- reg_cfg$target %||% "uniform"

  # CRITICAL FIX: Auto-scale strength based on constraint density
  # More constraints = solution more determined = less regularization needed
  num_constraints <- nrow(con$A)
  num_variables <- length(con$var_names)
  constraint_ratio <- num_constraints / num_variables

  # Scale down strength when system is well-constrained
  # sqrt() gives gentler scaling than linear
  strength <- base_strength / max(1.0, sqrt(constraint_ratio))

  var_names <- con$var_names
  n <- length(var_names)

  # Parse criterion from each variable (format: "Criterion:Level")
  sp <- strsplit(var_names, ":", fixed = TRUE)
  crit_of <- vapply(sp, `[[`, character(1), 1)
  criteria <- unique(crit_of)
  K <- length(criteria)

  # Target range for each criterion
  if (target_type == "uniform") {
    # All criteria should have equal range (importance)
    # With normalize_sum_top = 1, each criterion should have range ≈ 1/K
    target_range <- (settings$normalize_sum_top %||% 1) / K
  } else {
    target_range <- (settings$normalize_sum_top %||% 1) / K
  }

  # Find top-level variable index for each criterion
  # Assuming monotonicity constraints anchor min at 0, range ≈ top-level weight
  domains <- settings$domains %||% con$domains
  top_idx <- integer(K)
  for (k in seq_along(criteria)) {
    crit <- criteria[k]
    levels_k <- NULL
    if (!is.null(domains[[crit]]) && length(domains[[crit]]) > 0) {
      # Domain entries are stored as ordered level vectors, not named maps.
      levels_k <- as.character(unname(domains[[crit]]))
    }
    if (is.null(levels_k) || !length(levels_k)) {
      # Fall back to main-effect variables only; interaction variables use
      # "::" separators and would otherwise pollute the level ordering.
      mask <- crit_of == crit & !grepl("::", var_names, fixed = TRUE)
      levels_k <- unique(vapply(sp[mask], function(x) x[2], character(1)))
      levels_k <- levels_k[nzchar(levels_k)]
    }
    top_level <- levels_k[length(levels_k)]
    top_var <- paste(crit, top_level, sep = ":")
    top_idx[k] <- match(top_var, var_names)
    if (is.na(top_idx[k])) {
      warning("Could not find top-level variable for criterion: ", crit, ". Falling back to unregularized solve.")
      return(solve_feasible(con))
    }
  }

  # Quadratic programming approximated with L1 for LP compatibility:
  # Minimize: strength * sum |w_top_k - target_range|

  # Add auxiliary variables for absolute deviations: d+_k, d-_k >= 0
  # CHANGED: Use soft inequality constraints instead of hard equality
  n_aux <- 2 * K
  n_total <- n + n_aux

  # Objective: strength * sum(d+_k + d-_k)
  f.obj <- c(rep(0, n), rep(strength, n_aux))

  # Constraints: original constraints + deviation constraints
  A <- cbind(con$A, matrix(0, nrow(con$A), n_aux))
  dir <- con$dir
  b <- con$b

  # CRITICAL FIX: Use SOFT inequality constraints instead of hard equality
  # This allows deviations from target, penalized by objective
  # Instead of: w_top_k - d+ + d- = target (forces balance)
  # We use: w_top_k - target <= d+  AND  target - w_top_k <= d-
  # This lets LP deviate from target when preferences are strong

  for (k in seq_len(K)) {
    idx_top <- top_idx[k]
    idx_dplus <- n + (2 * k - 1)
    idx_dminus <- n + (2 * k)

    # Upper deviation: w_top_k - target <= d+_k
    row_upper <- numeric(n_total)
    row_upper[idx_top] <- 1
    row_upper[idx_dplus] <- -1
    A <- rbind(A, row_upper)
    dir <- c(dir, "<=")
    b <- c(b, target_range)

    # Lower deviation: target - w_top_k <= d-_k
    # Equivalent to: -w_top_k - d-_k <= -target
    row_lower <- numeric(n_total)
    row_lower[idx_top] <- -1
    row_lower[idx_dminus] <- -1
    A <- rbind(A, row_lower)
    dir <- c(dir, "<=")
    b <- c(b, -target_range)
  }

  # Solve LP
  res <- .lp_solve_quiet("min", f.obj, A, dir, b)

  if (res$status != 0) {
    return(list(ok = FALSE, status = res$status))
  }

  w <- setNames(res$solution[seq_len(n)], var_names)
  deviation_sum <- sum(res$solution[(n + 1):n_total])

  list(
    ok = TRUE,
    weights = w,
    status = 0,
    regularization = list(
      type = "balanced",
      deviation_sum = deviation_sum,
      strength = strength
    )
  )
}

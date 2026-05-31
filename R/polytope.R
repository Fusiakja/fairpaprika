## -----------------------------------------------------------------------------
## Polytope sampling (Hit-and-Run) for PAPRIKA part-worths
## -----------------------------------------------------------------------------

#' Sample feasible part-worths from the constraint polytope (Hit-and-Run)
#'
#' Draws samples from the feasible polytope implied by the observed decisions
#' and the structural constraints (anchors, monotonicity, and normalization).
#' Two Hit-and-Run variants are available: standard (random directions) and
#' coordinate (axis-aligned directions). Coordinate H&R often has better mixing
#' in high dimensions.
#'
#' @param engine A `paprika_engine` created by [engine_create()].
#' @param n Number of samples to return.
#' @param burnin Number of initial Hit-and-Run steps to discard.
#' @param thin Keep one sample every `thin` steps.
#' @param method Sampling method: `"standard"` (random directions) or
#'   `"coordinate"` (axis-aligned, often faster mixing in high dimensions).
#' @param chains Number of independent chains to run. If > 1, Gelman-Rubin
#'   convergence diagnostics will be computed.
#' @param parallel Logical. Use parallel processing for multiple chains?
#'   Requires `parallel` package (base R).
#' @param n_cores Number of cores for parallel sampling. Default uses
#'   `parallel::detectCores() - 1`.
#' @param progress Logical. Show progress bar for long-running sampling?
#'   Auto-enabled when total_steps > 1000.
#' @param seed Optional integer seed.
#' @param tol Numerical tolerance used for rank and feasibility checks.
#' @param jitter Small jitter added to the starting point (projected back to
#'   the equality subspace). Useful when the polytope is lower-dimensional.
#'
#' @return An object of class `paprika_polytope` with elements:
#'   * `weights` matrix of sampled part-worth vectors in the original normalization
#'     used by the constraints (alias: `w`).
#'   * `weights_scaled` matrix of the same samples rescaled so that the sum of
#'     criterion ranges equals 100 (alias: `w_scaled`).
#'   * `var_names` variable names (`criterion:level`).
#'   * `start` starting feasible point.
#'   * `chains` (if chains > 1) list of per-chain samples.
#'   * `diagnostics` (if chains > 1) convergence diagnostics (Gelman-Rubin R-hat).
#'
#' @details
#' **Method Selection:**
#' - `"standard"`: classic Hit-and-Run with random directions in nullspace.
#' - `"coordinate"`: cycles through coordinate directions (faster per-iteration,
#'   often better mixing for >10 variables).
#'
#' **Parallel Sampling:**
#' When `chains > 1` and `parallel = TRUE`, chains are run in parallel using
#' `parallel::mclapply()` (Unix/Mac) or `parallel::parLapply()` (Windows).
#' Seeds are set deterministically per chain for reproducibility.
#'
#' @examples
#' eng <- engine_create(list(Effekt = c("Niedrig", "Mittel", "Hoch"), Risiken = c("Viel", "Wenig")))
#' # After collecting some decisions:
#' # s <- engine_polytope_sample(eng, n = 200, method = "coordinate")
#' # Multi-chain with diagnostics:
#' # s <- engine_polytope_sample(eng, n = 200, chains = 4, parallel = TRUE)
#'
#' @export
engine_polytope_sample <- function(
    engine,
    n = 500,
    burnin = 200,
    thin = 1,
    method = c("standard", "coordinate"),
    chains = 1,
    parallel = FALSE,
    n_cores = NULL,
    progress = NULL,
    seed = NULL,
    tol = 1e-10,
    jitter = 1e-8) {
  stopifnot(inherits(engine, "paprika_engine"))
  stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
  stopifnot(is.numeric(burnin), length(burnin) == 1L, burnin >= 0)
  stopifnot(is.numeric(thin), length(thin) == 1L, thin >= 1)
  stopifnot(is.numeric(chains), length(chains) == 1L, chains >= 1)

  method <- match.arg(method)

  # Auto-enable progress for long runs
  if (is.null(progress)) {
    progress <- (burnin + n * thin) > 1000
  }

  # Multi-chain sampling
  if (chains > 1) {
    return(.polytope_multichain(
      engine = engine, n = n, burnin = burnin, thin = thin,
      method = method, chains = chains, parallel = parallel,
      n_cores = n_cores, progress = progress, seed = seed,
      tol = tol, jitter = jitter
    ))
  }

  if (!is.null(seed)) set.seed(seed)

  s <- engine$settings
  con <- constraints_from_decisions(
    engine$domains, engine$decisions,
    eps_strict = s$eps_strict,
    tau_equal = s$tau_equal,
    epsilon_monotone = s$epsilon_monotone,
    normalize_sum_top = s$normalize_sum_top,
    interactions = s$interactions %||% list()
  )
  std <- .polytope_standardize(con$A, con$dir, con$b, tol = tol)

  # starting point: any feasible solution via LP
  # If regularization is enabled, start at the regularized solution (center)
  # to avoid getting stuck in degenerate corners where Hit-and-Run mixes poorly.
  if (isTRUE(s$regularize_balanced$enabled)) {
    feas <- solve_with_balanced_regularization(con, engine$settings)
  } else {
    feas <- solve_feasible(con)
  }

  if (!isTRUE(feas$ok)) {
    # Fallback to basic feasible if regularized failed
    feas <- solve_feasible(con)
    if (!isTRUE(feas$ok)) {
      stop("No feasible solution for the current decisions (cannot sample polytope).")
    }
  }
  x <- as.numeric(feas$weights)
  names(x) <- con$var_names

  # optional jitter (projected to equality subspace)
  if (jitter > 0) {
    z <- stats::rnorm(length(x), sd = jitter)
    xj <- x + z
    x <- .polytope_project_to_equalities(xj, std$E, std$f, tol = tol)
    # If projection moved out of inequalities, fall back to original
    if (!.polytope_check_feasible(x, std$G, std$h, tol = tol)) x <- as.numeric(feas$weights)
  }

  # Nullspace basis of equality constraints (for directions)
  N <- .polytope_nullspace_basis(std$E, tol = tol)
  n_dim <- length(x)
  k_dim <- ncol(N)

  total_steps <- burnin + n * thin
  out <- matrix(NA_real_, nrow = n, ncol = n_dim, dimnames = list(NULL, con$var_names))
  kept <- 0L

  # Progress bar
  pb <- NULL
  if (progress) {
    pb <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
  }

  current_lp <- NULL

  # Pre-compute indices for fast Metropolis step
  crit_indices <- NULL
  if (isTRUE(s$regularize_balanced$enabled)) {
    crit_indices <- lapply(names(engine$domains), function(d) {
      levs <- engine$domains[[d]]
      vnames <- paste0(d, ":", levs)
      match(vnames, con$var_names)
    })
    # Define log-prob function relative to regularization settings
    calc_log_prob <- NULL
    if (isTRUE(s$regularize_balanced$enabled)) {
      reg_str <- s$regularize_balanced$strength %||% 0.1
      calc_log_prob <- function(w_vec) {
        n_crit <- length(crit_indices)
        ranges <- numeric(n_crit)
        for (i in seq_len(n_crit)) {
          vals <- w_vec[crit_indices[[i]]]
          ranges[i] <- max(vals) - min(vals)
        }

        total_range <- sum(ranges)
        if (total_range > 1e-9) ranges <- 100 * ranges / total_range

        target <- 100 / length(ranges)
        dist <- sum(abs(ranges - target))
        return(-reg_str * dist)
      }

      # Init LP for starting point
      current_lp <- calc_log_prob(x)
    }
  }

  for (step in seq_len(total_steps)) {
    if (!is.null(pb) && step %% 50 == 0) {
      utils::setTxtProgressBar(pb, step)
    }

    # direction in equality subspace
    if (method == "coordinate") {
      # Coordinate Hit-and-Run: cycle through basis directions
      if (k_dim == 0L) {
        d <- rep(0, n_dim)
      } else {
        coord_idx <- ((step - 1) %% k_dim) + 1
        d <- as.numeric(N[, coord_idx, drop = TRUE])
        nd <- sqrt(sum(d * d))
        if (!is.finite(nd) || nd <= 0) next
        d <- d / nd
      }
    } else {
      # Standard Hit-and-Run: random direction
      if (k_dim == 0L) {
        d <- rep(0, n_dim)
      } else {
        z <- stats::rnorm(k_dim)
        d <- as.numeric(N %*% z)
        nd <- sqrt(sum(d * d))
        if (!is.finite(nd) || nd <= 0) next
        d <- d / nd
      }
    }

    # if no degrees of freedom, stay at the unique point
    if (all(d == 0)) {
      # still record after burnin/thin
      if (step > burnin && ((step - burnin) %% thin == 0L)) {
        kept <- kept + 1L
        out[kept, ] <- x
      }
      next
    }

    # step interval from inequalities
    int <- .polytope_step_interval(x, d, std$G, std$h, tol = tol)

    # Store current point for Metropolis rejection (x_prev)
    x_prev <- x
    if (!is.finite(int$tmin) || !is.finite(int$tmax) || int$tmin > (int$tmax + tol)) {
      # direction numerically unusable; try again next iteration
      next
    }
    tmin <- int$tmin
    tmax <- int$tmax
    # If interval is nearly a point, collapse to the midpoint to allow movement
    if ((tmax - tmin) < tol) {
      tmin <- tmax <- (tmin + tmax) / 2
    }
    t <- stats::runif(1, min = tmin, max = tmax)
    x <- x + t * d

    # If using balanced regularization, weights are not uniform in phenotype
    accepted <- TRUE
    if (isTRUE(s$regularize_balanced$enabled) && step > burnin) {
      # Calculate LP of proposal 'x'
      prop_lp <- calc_log_prob(x)

      if (is.null(current_lp)) current_lp <- -Inf
      log_alpha <- prop_lp - current_lp

      if (log(stats::runif(1)) > log_alpha) {
        # REJECT: Revert to previous point
        x <- x_prev
        accepted <- FALSE
      } else {
        # ACCEPT: Update current LP
        current_lp <- prop_lp
        accepted <- TRUE
      }
    }

    if (step > burnin && ((step - burnin) %% thin == 0L)) {
      kept <- kept + 1L
      out[kept, ] <- x
      if (kept >= n) break
    }
  }

  if (!is.null(pb)) close(pb)

  if (kept < n) {
    out <- out[seq_len(kept), , drop = FALSE]
  }
  # Fallback: if no samples were kept (e.g., numerical issues), return the start
  # point as a single sample so downstream code has a non-empty matrix.
  if (nrow(out) == 0) {
    out <- matrix(x, nrow = 1, dimnames = list(NULL, con$var_names))
  }

  # scaled version (sum of criterion ranges = 100)
  w_scaled <- t(apply(out, 1, function(wi) {
    if (is.null(names(wi))) names(wi) <- con$var_names
    rescale_weights_range100(engine$domains, wi)
  }))

  ans <- list(
    weights = out,
    weights_scaled = w_scaled,
    var_names = con$var_names,
    start = setNames(as.numeric(feas$weights), con$var_names)
  )
  # Backward-compatibility aliases
  ans$w <- ans$weights
  ans$w_scaled <- ans$weights_scaled
  class(ans) <- "paprika_polytope"
  ans
}


#' Rank profiles using polytope samples
#'
#' Computes a ranking stability summary by scoring each profile under all
#' sampled part-worth vectors.
#'
#' @param engine A `paprika_engine`.
#' @param profiles A data frame of profiles with one column per criterion.
#' @param samples Result from [engine_polytope_sample()]. If `NULL`, the
#'   function will generate samples.
#' @param ... Passed to [engine_polytope_sample()] when `samples` is `NULL`.
#'
#' @return A data frame with one row per profile containing mean utility,
#'   win probability, and a "fit" score in percent.
#'
#' @export
polytope_rank_profiles <- function(engine, profiles, samples = NULL, ...) {
  stopifnot(inherits(engine, "paprika_engine"))
  stopifnot(is.data.frame(profiles))
  if (is.null(samples)) samples <- engine_polytope_sample(engine, ...)
  stopifnot(inherits(samples, "paprika_polytope"))

  crit <- engine$criteria
  missing_cols <- setdiff(crit, names(profiles))
  if (length(missing_cols)) {
    stop("profiles is missing criteria columns: ", paste(missing_cols, collapse = ", "))
  }

  # build profile -> variable index matrix
  var_names <- samples$var_names
  idx <- stats::setNames(seq_along(var_names), var_names)
  P <- nrow(profiles)
  K <- length(crit)
  prof_idx <- matrix(NA_integer_, nrow = P, ncol = K)
  colnames(prof_idx) <- crit
  for (j in seq_len(K)) {
    cn <- crit[j]
    keys <- paste(cn, as.character(profiles[[cn]]), sep = ":")
    prof_idx[, j] <- idx[keys]
    if (anyNA(prof_idx[, j])) {
      bad <- unique(keys[is.na(prof_idx[, j])])
      stop("profiles contain unknown levels for criterion '", cn, "': ", paste(bad, collapse = ", "))
    }
  }

  if (!is.null(samples$weights_scaled)) {
    W <- samples$weights_scaled
  } else if (!is.null(samples$w_scaled)) {
    W <- samples$w_scaled
  } else if (!is.null(samples$weights)) {
    W <- samples$weights
  } else {
    W <- samples$w
  }
  if (is.null(dim(W))) W <- matrix(W, nrow = 1)
  S <- nrow(W)

  # utility: sum of part-worths
  util <- matrix(0, nrow = P, ncol = S)
  for (p in seq_len(P)) {
    util[p, ] <- rowSums(W[, prof_idx[p, ], drop = FALSE])
  }

  mean_u <- rowMeans(util)
  sd_u <- apply(util, 1, stats::sd)
  # win probability: P(profile is best)
  winners <- max.col(t(util), ties.method = "first")
  win_p <- tabulate(winners, nbins = P) / S

  # fit%: map mean utility onto [0, 100] using per-sample best/worst
  best <- apply(util, 2, max)
  worst <- apply(util, 2, min)
  rng <- pmax(1e-12, best - worst)
  fit_mat <- sweep(util, 2, worst, "-")
  fit_mat <- sweep(fit_mat, 2, rng, "/")
  fit <- 100 * rowMeans(fit_mat)

  out <- data.frame(
    profile_id = seq_len(P),
    utility_mean = as.numeric(mean_u),
    utility_sd = as.numeric(sd_u),
    fit_percent = as.numeric(fit),
    win_prob = as.numeric(win_p),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$fit_percent, -out$win_prob), , drop = FALSE]
  rownames(out) <- NULL
  out
}


## -----------------------------------------------------------------------------
## Internal helpers
## -----------------------------------------------------------------------------

.polytope_standardize <- function(A, dir, b, tol = 1e-10) {
  stopifnot(is.matrix(A))
  stopifnot(length(dir) == nrow(A), length(b) == nrow(A))

  # split
  eq <- dir == "="
  le <- dir == "<="
  ge <- dir == ">="

  E <- if (any(eq)) A[eq, , drop = FALSE] else matrix(0, 0, ncol(A))
  f <- if (any(eq)) b[eq] else numeric(0)

  G <- matrix(0, 0, ncol(A))
  h <- numeric(0)
  if (any(le)) {
    G <- rbind(G, A[le, , drop = FALSE])
    h <- c(h, b[le])
  }
  if (any(ge)) {
    G <- rbind(G, -A[ge, , drop = FALSE])
    h <- c(h, -b[ge])
  }

  # remove numerically redundant equalities (rank reduction)
  if (nrow(E) > 0) {
    q <- qr(E, tol = tol)
    rk <- q$rank
    if (rk == 0) {
      E <- matrix(0, 0, ncol(A))
      f <- numeric(0)
    } else if (rk < nrow(E)) {
      keep <- sort(q$pivot[seq_len(rk)])
      E <- E[keep, , drop = FALSE]
      f <- f[keep]
    }
  }

  list(E = E, f = f, G = G, h = h)
}

.polytope_nullspace_basis <- function(E, tol = 1e-10) {
  n <- ncol(E)
  if (is.null(n) || n == 0) {
    return(matrix(0, 0, 0))
  }
  if (nrow(E) == 0) {
    return(diag(n))
  }

  s <- svd(E, nu = 0, nv = n)
  d <- s$d
  if (!length(d)) {
    return(diag(n))
  }
  thr <- tol * max(d)
  r <- sum(d > thr)
  if (r < n) {
    s$v[, seq.int(r + 1L, n), drop = FALSE]
  } else {
    # full column rank => unique point (no degrees of freedom)
    matrix(0, nrow = n, ncol = 0)
  }
}

.polytope_project_to_equalities <- function(x, E, f, tol = 1e-10) {
  if (nrow(E) == 0) {
    return(as.numeric(x))
  }
  x <- as.numeric(x)
  # Solve least squares: E * x = f
  # Use QR for stability
  q <- qr(E, tol = tol)
  dx <- qr.coef(q, f - as.numeric(E %*% x))
  if (anyNA(dx)) {
    return(x)
  }
  x + as.numeric(dx)
}

.polytope_check_feasible <- function(x, G, h, tol = 1e-10) {
  if (nrow(G) == 0) {
    return(TRUE)
  }
  all(as.numeric(G %*% x) <= (h + tol))
}

.polytope_step_interval <- function(x, d, G, h, tol = 1e-10) {
  if (nrow(G) == 0) {
    return(list(tmin = -1, tmax = 1))
  }
  gx <- as.numeric(G %*% x)
  gd <- as.numeric(G %*% d)
  rhs <- h - gx

  tmin <- -Inf
  tmax <- Inf
  for (i in seq_along(rhs)) {
    a <- gd[i]
    b <- rhs[i]
    if (abs(a) <= tol) {
      # direction parallel to constraint
      if (b < -tol) {
        return(list(tmin = Inf, tmax = -Inf))
      }
      next
    }
    t <- b / a
    if (a > 0) {
      # t <= b/a
      tmax <- min(tmax, t)
    } else {
      # t >= b/a
      tmin <- max(tmin, t)
    }
  }
  list(tmin = tmin, tmax = tmax)
}

## -----------------------------------------------------------------------------
## Multi-chain sampling with convergence diagnostics
## -----------------------------------------------------------------------------

.polytope_multichain <- function(engine, n, burnin, thin, method, chains,
                                 parallel, n_cores, progress, seed, tol, jitter) {
  # Set up chain-specific seeds for reproducibility
  if (is.null(seed)) seed <- as.integer(Sys.time())
  chain_seeds <- seed + seq_len(chains)

  # Function to run a single chain
  run_single_chain <- function(chain_id) {
    engine_polytope_sample(
      engine = engine,
      n = n,
      burnin = burnin,
      thin = thin,
      method = method,
      chains = 1, # single chain
      parallel = FALSE,
      progress = progress && chain_id == 1, # only show progress for first chain
      seed = chain_seeds[chain_id],
      tol = tol,
      jitter = jitter
    )
  }

  # Run chains in parallel or sequentially
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    n_cores <- min(n_cores, chains)

    # Platform-specific parallel execution
    if (.Platform$OS.type == "windows") {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      # Export necessary objects
      parallel::clusterExport(cl, c("engine_polytope_sample", "engine"),
        envir = environment()
      )
      chain_results <- parallel::parLapply(cl, seq_len(chains), run_single_chain)
    } else {
      chain_results <- parallel::mclapply(seq_len(chains), run_single_chain,
        mc.cores = n_cores
      )
    }
  } else {
    chain_results <- lapply(seq_len(chains), run_single_chain)
  }

  # Combine chains
  weights_list <- lapply(chain_results, `[[`, "weights")

  # Check if all chains have the same number of samples
  chain_lengths <- vapply(weights_list, nrow, integer(1))
  min_length <- min(chain_lengths)

  if (length(unique(chain_lengths)) > 1) {
    warning(
      "Chains have different lengths (", paste(chain_lengths, collapse = ", "),
      "). Truncating all chains to ", min_length, " samples for diagnostics."
    )
    # Truncate all chains to minimum length
    weights_list <- lapply(weights_list, function(w) w[seq_len(min_length), , drop = FALSE])

    # Also update the stored chain results so polytope_diagnostics works correctly
    for (i in seq_along(chain_results)) {
      chain_results[[i]]$weights <- chain_results[[i]]$weights[seq_len(min_length), , drop = FALSE]
      chain_results[[i]]$weights_scaled <- chain_results[[i]]$weights_scaled[seq_len(min_length), , drop = FALSE]
      chain_results[[i]]$w <- chain_results[[i]]$weights
      chain_results[[i]]$w_scaled <- chain_results[[i]]$weights_scaled
    }
  }

  weights_combined <- do.call(rbind, weights_list)

  weights_scaled_list <- lapply(chain_results, `[[`, "weights_scaled")

  # Truncate scaled weights too
  if (length(unique(chain_lengths)) > 1) {
    weights_scaled_list <- lapply(weights_scaled_list, function(w) w[seq_len(min_length), , drop = FALSE])
  }

  weights_scaled_combined <- do.call(rbind, weights_scaled_list)

  var_names <- chain_results[[1]]$var_names

  # Compute Gelman-Rubin diagnostic (R-hat)
  rhat <- .compute_gelman_rubin(weights_list)

  # Effective sample size
  ess <- .compute_ess(weights_list)

  ans <- list(
    weights = weights_combined,
    weights_scaled = weights_scaled_combined,
    var_names = var_names,
    start = chain_results[[1]]$start,
    chains = chain_results,
    n_chains = chains,
    diagnostics = list(
      rhat = rhat,
      ess = ess,
      max_rhat = max(rhat, na.rm = TRUE),
      min_ess = min(ess, na.rm = TRUE),
      converged = all(rhat < 1.1, na.rm = TRUE)
    )
  )

  # Backward-compatibility aliases
  ans$w <- ans$weights
  ans$w_scaled <- ans$weights_scaled

  class(ans) <- "paprika_polytope"
  ans
}

## Gelman-Rubin diagnostic (R-hat)
.compute_gelman_rubin <- function(chains_list) {
  m <- length(chains_list) # number of chains
  n <- nrow(chains_list[[1]]) # number of samples per chain
  p <- ncol(chains_list[[1]]) # number of variables

  if (m < 2) {
    return(rep(NA_real_, p))
  }

  rhat <- numeric(p)

  for (j in seq_len(p)) {
    # Extract variable j from all chains - ensure matrix format
    chains_j <- vapply(chains_list, function(ch) ch[, j], numeric(n))
    # chains_j is now n x m matrix (rows = samples, cols = chains)

    # Chain means
    chain_means <- colMeans(chains_j)
    # Overall mean
    overall_mean <- mean(chain_means)

    # Between-chain variance
    B <- n * var(chain_means)

    # Within-chain variance
    W <- mean(apply(chains_j, 2, var))

    # Pooled variance estimate
    var_plus <- ((n - 1) / n) * W + (1 / n) * B

    # R-hat
    rhat[j] <- sqrt(var_plus / W)
  }

  setNames(rhat, colnames(chains_list[[1]]))
}

## Effective sample size (simple autocorrelation-based estimate)
.compute_ess <- function(chains_list) {
  m <- length(chains_list)
  n <- nrow(chains_list[[1]])
  p <- ncol(chains_list[[1]])

  ess <- numeric(p)

  for (j in seq_len(p)) {
    # Extract variable j from all chains - ensure matrix format
    chains_j <- vapply(chains_list, function(ch) ch[, j], numeric(n))
    all_samples <- as.vector(chains_j)

    # Simple ESS estimate based on lag-1 autocorrelation
    if (length(all_samples) > 10) {
      acf_result <- tryCatch(
        {
          stats::acf(all_samples, lag.max = 1, plot = FALSE)$acf[2]
        },
        error = function(e) 0
      )

      rho <- max(0, min(0.99, acf_result)) # clip to reasonable range
      ess[j] <- (m * n) * (1 - rho) / (1 + rho)
    } else {
      ess[j] <- m * n
    }
  }

  setNames(ess, colnames(chains_list[[1]]))
}

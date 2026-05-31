# Bootstrap utilities for fairpaprika (PAPRIKA engine)
# Keep ASCII only in code (CRAN portability)

#' Bootstrap uncertainty for PAPRIKA weights
#'
#' Resamples the observed decisions with replacement (nonparametric bootstrap),
#' re-solves the PAPRIKA linear program for each replicate, and returns a
#' distribution of part-worths and criterion importances.
#'
#' @param engine A `paprika_engine` with `decisions`.
#' @param B Integer. Number of bootstrap replicates.
#' @param seed Optional integer for reproducibility.
#' @param min_ok Minimum number of successful replicates required for `ok=TRUE`.
#'        Default: max(10, floor(B * 0.5)).
#' @param eps_strict Bootstrap strict margin (override). Default 0 (more tolerant).
#' @param tau_equal Bootstrap equality band (override). Default uses `engine$settings$tau_equal`.
#' @param parallel Logical. Use parallel processing? Requires `parallel` package (base R).
#' @param n_cores Number of cores for parallel processing. Default uses `parallel::detectCores() - 1`.
#' @param verbose Logical. Print progress every ~50 reps (ignored if parallel = TRUE).
#' @param progress Logical. Show progress bar? Auto-enabled for B > 100 (disabled for parallel).
#'
#' @return An S3 object of class `paprika_bootstrap`.
#' @export
engine_bootstrap <- function(engine,
                             B = 200L,
                             seed = NULL,
                             min_ok = NULL,
                             eps_strict = 0,
                             tau_equal = NULL,
                             parallel = FALSE,
                             n_cores = NULL,
                             verbose = FALSE,
                             progress = NULL) {
  engine <- validate_engine(engine)
  d <- engine$decisions
  if (is.null(d) || !nrow(d)) stop("engine_bootstrap: engine has no decisions.")
  if (!all(c("A1", "A2", "pref") %in% names(d))) stop("engine_bootstrap: decisions must have columns A1, A2, pref.")

  B <- as.integer(B)
  if (B < 1L) stop("engine_bootstrap: B must be >= 1.")

  if (!is.null(seed)) set.seed(seed)

  if (is.null(min_ok)) min_ok <- max(10L, as.integer(floor(B * 0.5)))

  # Auto-enable progress for larger B
  if (is.null(progress)) {
    progress <- B > 100 && !parallel
  }

  # bootstrap settings: copy engine settings, but allow tolerant margins
  settings_boot <- engine$settings
  settings_boot$eps_strict <- eps_strict
  if (!is.null(tau_equal)) settings_boot$tau_equal <- tau_equal

  crit <- engine$criteria
  dom <- engine$domains

  # var_names in the same order as the solver uses
  var_names <- unlist(lapply(crit, function(k) paste(k, dom[[k]], sep = ":")))
  n_var <- length(var_names)

  # pre-allocate (we will subset to successful columns later)
  W <- matrix(NA_real_, nrow = n_var, ncol = B, dimnames = list(var_names, NULL))
  I <- matrix(NA_real_, nrow = length(crit), ncol = B, dimnames = list(crit, NULL))

  ok <- logical(B)
  status <- integer(B)

  n_dec <- nrow(d)

  # Parallel execution
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    n_cores <- min(n_cores, B)

    # Function to run single bootstrap replicate
    boot_replicate <- function(b) {
      idxb <- sample.int(n_dec, size = n_dec, replace = TRUE)
      db <- d[idxb, , drop = FALSE]
      sol <- solve_partworths(dom, db, settings_boot)

      if (!isTRUE(sol$ok)) {
        return(list(
          ok = FALSE,
          status = if (!is.null(sol$status)) as.integer(sol$status) else NA_integer_,
          W = NA,
          I = NA
        ))
      }

      w_scaled <- rescale_weights_range100(dom, sol$weights)
      W_b <- as.numeric(w_scaled[var_names])

      w_df <- data.frame(Merkmal = var_names, Nutzen = W_b, stringsAsFactors = FALSE)
      imp <- importance_from_weights(w_df)
      I_b <- as.numeric(imp[crit])

      list(ok = TRUE, status = 0L, W = W_b, I = I_b)
    }

    # Platform-specific parallel execution
    if (.Platform$OS.type == "windows") {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(cl, c(
        "solve_partworths", "rescale_weights_range100",
        "importance_from_weights", "dom", "d", "settings_boot",
        "var_names", "crit", "n_dec"
      ),
      envir = environment()
      )
      results <- parallel::parLapply(cl, seq_len(B), boot_replicate)
    } else {
      results <- parallel::mclapply(seq_len(B), boot_replicate, mc.cores = n_cores)
    }

    # Collect results
    for (b in seq_len(B)) {
      ok[b] <- results[[b]]$ok
      status[b] <- results[[b]]$status
      if (ok[b]) {
        W[, b] <- results[[b]]$W
        I[, b] <- results[[b]]$I
      }
    }
  } else {
    # Sequential execution with progress bar
    pb <- NULL
    if (progress) {
      pb <- utils::txtProgressBar(min = 0, max = B, style = 3)
    }

    for (b in seq_len(B)) {
      if (!is.null(pb)) {
        utils::setTxtProgressBar(pb, b)
      }

      idxb <- sample.int(n_dec, size = n_dec, replace = TRUE)
      db <- d[idxb, , drop = FALSE]

      sol <- solve_partworths(dom, db, settings_boot)

      if (!isTRUE(sol$ok)) {
        ok[b] <- FALSE
        status[b] <- if (!is.null(sol$status)) as.integer(sol$status) else NA_integer_
        next
      }

      # scale to sum(range_k)=100 (consistent with your engine_compute)
      w_scaled <- rescale_weights_range100(dom, sol$weights)

      # store in consistent order
      W[, b] <- as.numeric(w_scaled[var_names])

      # criterion importances (range shares)
      w_df <- data.frame(Merkmal = var_names, Nutzen = W[, b], stringsAsFactors = FALSE)
      imp <- importance_from_weights(w_df)
      I[, b] <- as.numeric(imp[crit])

      ok[b] <- TRUE
      status[b] <- 0L

      if (isTRUE(verbose) && (b %% 50L == 0L)) {
        message(sprintf("bootstrap %d/%d (ok=%d)", b, B, sum(ok)))
      }
    }

    if (!is.null(pb)) close(pb)
  }

  keep <- which(ok)

  out <- list(
    ok = (length(keep) >= min_ok),
    B = B,
    ok_count = length(keep),
    failed = B - length(keep),
    min_ok = as.integer(min_ok),
    seed = seed,
    domains = dom,
    criteria = crit,
    var_names = var_names,
    weights_samples = W[, keep, drop = FALSE],
    importance_samples = I[, keep, drop = FALSE],
    status = status,
    settings_boot = settings_boot
  )
  class(out) <- "paprika_bootstrap"
  out
}

#' @export
print.paprika_bootstrap <- function(x, ...) {
  cat("<paprika_bootstrap>\n")
  cat(" B:", x$B, "\n")
  cat(" ok_count:", x$ok_count, "\n")
  cat(" failed:", x$failed, "\n")
  cat(" ok:", if (isTRUE(x$ok)) "TRUE" else "FALSE", "\n")
  invisible(x)
}

# internal helper: CI bounds
.bootstrap_ci_bounds <- function(level = 0.95) {
  level <- as.numeric(level)
  if (!is.finite(level) || level <= 0 || level >= 1) stop("level must be in (0,1).")
  alpha <- (1 - level) / 2
  c(lo = alpha, hi = 1 - alpha)
}

#' Summarize bootstrap distributions
#'
#' @param boot A `paprika_bootstrap` object.
#' @param what One of "importance" or "partworths".
#' @param level CI level (e.g., 0.95).
#'
#' @return data.frame with mean, sd, median, lo, hi.
#' @export
bootstrap_summary <- function(boot,
                              what = c("importance", "partworths"),
                              level = 0.95) {
  stopifnot(inherits(boot, "paprika_bootstrap"))
  what <- match.arg(what)

  probs <- .bootstrap_ci_bounds(level)

  M <- switch(what,
    importance = boot$importance_samples,
    partworths = boot$weights_samples
  )

  if (is.null(M) || !ncol(M)) stop("bootstrap_summary: no samples available.")

  nm <- rownames(M)
  meanv <- rowMeans(M, na.rm = TRUE)
  sdv <- apply(M, 1, stats::sd, na.rm = TRUE)
  medv <- apply(M, 1, stats::median, na.rm = TRUE)
  lov <- apply(M, 1, stats::quantile, probs = probs["lo"], na.rm = TRUE, names = FALSE, type = 7)
  hiv <- apply(M, 1, stats::quantile, probs = probs["hi"], na.rm = TRUE, names = FALSE, type = 7)

  data.frame(
    Name = nm,
    Mean = as.numeric(meanv),
    SD = as.numeric(sdv),
    Median = as.numeric(medv),
    Lo = as.numeric(lov),
    Hi = as.numeric(hiv),
    stringsAsFactors = FALSE
  )
}

#' Bootstrap ranking + Fit percentages for arbitrary profiles
#'
#' Given a set of profiles (rows) with criterion levels, compute for each profile:
#' - Fit% distribution (0-100 normalized within the model)
#' - CI for Fit%
#' - Probability to be rank 1 and rank <= top_k across bootstrap samples
#' - Optionally pairwise P(i > j)
#'
#' @param boot A `paprika_bootstrap` object.
#' @param profiles data.frame with at least the engine criteria columns.
#' @param id_col Optional column name in `profiles` used as identifier.
#'        If NULL, rownames(profiles) or row index is used.
#' @param top_k Integer (e.g., 3 for Top-3 probability).
#' @param level CI level for Fit% (e.g., 0.95).
#' @param return_pairwise Logical. If TRUE, returns pairwise P(i > j).
#'
#' @return A list with `table` (data.frame) and optionally `pairwise` (matrix).
#' @export
bootstrap_rank_profiles <- function(boot,
                                    profiles,
                                    id_col = NULL,
                                    top_k = 3L,
                                    level = 0.95,
                                    return_pairwise = FALSE) {
  stopifnot(inherits(boot, "paprika_bootstrap"))
  if (!is.data.frame(profiles) || !nrow(profiles)) stop("profiles must be a non-empty data.frame.")

  top_k <- as.integer(top_k)
  if (top_k < 1L) stop("top_k must be >= 1.")

  crit <- boot$criteria
  dom <- boot$domains
  var_names <- boot$var_names

  missing_cols <- setdiff(crit, names(profiles))
  if (length(missing_cols)) {
    stop("profiles is missing criteria columns: ", paste(missing_cols, collapse = ", "))
  }

  ids <- if (!is.null(id_col)) {
    if (!id_col %in% names(profiles)) stop("id_col not found in profiles.")
    as.character(profiles[[id_col]])
  } else if (!is.null(rownames(profiles)) && all(nzchar(rownames(profiles)))) {
    rownames(profiles)
  } else {
    as.character(seq_len(nrow(profiles)))
  }

  W <- boot$weights_samples
  if (is.null(W) || !ncol(W)) stop("bootstrap_rank_profiles: boot has no weight samples.")

  # map each profile/criterion to a row-index in W
  idx_mat <- matrix(NA_integer_, nrow = nrow(profiles), ncol = length(crit))
  colnames(idx_mat) <- crit

  for (j in seq_along(crit)) {
    cn <- crit[j]
    keys <- paste0(cn, ":", as.character(profiles[[cn]]))
    idx <- match(keys, var_names)
    if (anyNA(idx)) {
      bad <- unique(keys[is.na(idx)])
      stop("profiles contain unknown levels for ", cn, ": ", paste(bad, collapse = ", "))
    }
    idx_mat[, j] <- idx
  }

  # utilities: n_profiles x n_samples
  n_prof <- nrow(profiles)
  n_samp <- ncol(W)
  U <- matrix(0, nrow = n_prof, ncol = n_samp)

  for (j in seq_along(crit)) {
    U <- U + W[idx_mat[, j], , drop = FALSE]
  }

  # normalize to Fit% per sample: 0..100 based on model worst/best
  worst_idx <- vapply(crit, function(cn) {
    match(paste0(cn, ":", dom[[cn]][1]), var_names)
  }, integer(1))

  best_idx <- vapply(crit, function(cn) {
    match(paste0(cn, ":", utils::tail(dom[[cn]], 1)), var_names)
  }, integer(1))

  u_min <- colSums(W[worst_idx, , drop = FALSE])
  u_max <- colSums(W[best_idx, , drop = FALSE])
  denom <- u_max - u_min
  denom[abs(denom) < 1e-12] <- NA_real_

  Fit <- 100 * sweep(U, 2, u_min, FUN = "-")
  Fit <- sweep(Fit, 2, denom, FUN = "/")

  # rank probabilities (ties handled via ties.method="min")
  rmin <- apply(-U, 2, rank, ties.method = "min")

  p_top1 <- rowMeans(rmin <= 1L, na.rm = TRUE)
  p_topk <- rowMeans(rmin <= top_k, na.rm = TRUE)

  probs <- .bootstrap_ci_bounds(level)

  fit_med <- apply(Fit, 1, stats::median, na.rm = TRUE)
  fit_lo <- apply(Fit, 1, stats::quantile, probs = probs["lo"], na.rm = TRUE, names = FALSE, type = 7)
  fit_hi <- apply(Fit, 1, stats::quantile, probs = probs["hi"], na.rm = TRUE, names = FALSE, type = 7)

  rank_med <- apply(rmin, 1, stats::median, na.rm = TRUE)

  tab <- data.frame(
    id = ids,
    Fit_median = as.numeric(fit_med),
    Fit_lo = as.numeric(fit_lo),
    Fit_hi = as.numeric(fit_hi),
    P_top1 = as.numeric(p_top1),
    P_topk = as.numeric(p_topk),
    Rank_median = as.numeric(rank_med),
    stringsAsFactors = FALSE
  )

  # sort by median utility (equivalently Fit_median)
  tab <- tab[order(-tab$Fit_median, -tab$P_top1, tab$Rank_median), , drop = FALSE]
  rownames(tab) <- NULL

  out <- list(table = tab)

  if (isTRUE(return_pairwise)) {
    P <- matrix(NA_real_,
      nrow = n_prof, ncol = n_prof,
      dimnames = list(ids, ids)
    )
    for (i in seq_len(n_prof)) {
      for (j in seq_len(n_prof)) {
        if (i == j) {
          P[i, j] <- 0.5
        } else {
          P[i, j] <- mean(U[i, ] > U[j, ], na.rm = TRUE)
        }
      }
    }
    out$pairwise <- P
  }

  out
}

#' Simple instability flag based on bootstrap SD of criterion importance
#'
#' @param boot A `paprika_bootstrap`.
#' @param thr_sd Threshold in percentage points (e.g. 5).
#'
#' @return TRUE if any criterion importance SD exceeds thr_sd.
#' @export
bootstrap_is_unstable <- function(boot, thr_sd = 5) {
  stopifnot(inherits(boot, "paprika_bootstrap"))
  I <- boot$importance_samples
  if (is.null(I) || !ncol(I)) {
    return(TRUE)
  }
  sds <- apply(I, 1, stats::sd, na.rm = TRUE)
  any(sds > thr_sd)
}

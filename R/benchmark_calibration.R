#' Fit simulation-based uncertainty calibration on benchmark outputs
#'
#' Learns a held-out calibration layer for the uncertainty-aware outputs of a
#' benchmarked engine configuration. Winner probabilities are calibrated with a
#' symmetric set-wise beta transform followed by renormalization; top-3
#' probabilities are calibrated with a shared beta calibration on profile-level
#' membership events.
#'
#' @param study A `paprika_benchmark_study` object or compatible benchmark
#'   details data frame containing stored probability vectors.
#' @param config Configuration name to calibrate, e.g. `"full"`.
#' @param calibration_frac Fraction of runs used for calibration fitting. The
#'   remainder is reserved for held-out evaluation.
#' @param seed Integer seed used for the split.
#' @param stratify_by_scenario Logical. Split within each `scenario_key`?
#' @param eps Small positive value used for probability clipping.
#' @param maxit Maximum iterations for the internal optimizers.
#'
#' @return An object of class `paprika_uncertainty_calibration`.
#' @export
benchmark_fit_uncertainty_calibration <- function(study,
                                                  config = "full",
                                                  calibration_frac = 0.5,
                                                  seed = 1L,
                                                  stratify_by_scenario = TRUE,
                                                  eps = 1e-6,
                                                  maxit = 500L) {
  calibration_frac <- as.numeric(calibration_frac)
  if (!is.finite(calibration_frac) || calibration_frac <= 0 || calibration_frac >= 1) {
    stop("calibration_frac must be strictly between 0 and 1.")
  }

  data <- .benchmark_prepare_uncertainty_calibration_data(study, config = config)
  split <- .benchmark_uncertainty_split(
    data,
    calibration_frac = calibration_frac,
    seed = seed,
    stratify_by_scenario = stratify_by_scenario
  )

  idx_cal <- split$calibration
  winner_fit <- .benchmark_fit_setwise_beta(
    prob_list = data$winner_probabilities[idx_cal],
    true_index = data$true_top1[idx_cal],
    eps = eps,
    maxit = maxit
  )
  top3_fit <- .benchmark_fit_binary_beta(
    prob = unlist(data$top3_probabilities[idx_cal], use.names = FALSE),
    event = unlist(data$true_top3_mask[idx_cal], use.names = FALSE),
    eps = eps,
    maxit = maxit
  )

  out <- list(
    config = config,
    calibration_frac = calibration_frac,
    seed = as.integer(seed),
    eps = eps,
    winner = winner_fit,
    top3 = top3_fit,
    split = split,
    data = data
  )
  class(out) <- "paprika_uncertainty_calibration"
  out$summary <- benchmark_evaluate_uncertainty_calibration(out)
  out
}

#' Evaluate a fitted uncertainty calibration
#'
#' @param calibration A `paprika_uncertainty_calibration` object.
#' @param split Which split to report. Any subset of `"calibration"`, `"test"`,
#'   or `"all"`.
#'
#' @return A data frame summarizing raw versus calibrated uncertainty metrics.
#' @export
benchmark_evaluate_uncertainty_calibration <- function(calibration,
                                                       split = c("calibration", "test", "all")) {
  stopifnot(inherits(calibration, "paprika_uncertainty_calibration"))
  split <- unique(as.character(split))
  bad <- setdiff(split, c("calibration", "test", "all"))
  if (length(bad)) stop("Unsupported split values: ", paste(bad, collapse = ", "))

  data <- calibration$data
  idx_map <- list(
    calibration = calibration$split$calibration,
    test = calibration$split$test,
    all = seq_len(nrow(data))
  )

  rows <- list()
  for (sp in split) {
    idx <- idx_map[[sp]]
    if (!length(idx)) next
    winner_raw <- .benchmark_uncertainty_metrics_winner(
      data$winner_probabilities[idx],
      data$true_top1[idx]
    )
    winner_cal <- .benchmark_uncertainty_metrics_winner(
      .benchmark_apply_setwise_beta(data$winner_probabilities[idx], calibration$winner),
      data$true_top1[idx]
    )
    top3_raw <- .benchmark_uncertainty_metrics_top3(
      data$top3_probabilities[idx],
      data$true_top3_mask[idx]
    )
    top3_cal <- .benchmark_uncertainty_metrics_top3(
      .benchmark_apply_binary_beta(data$top3_probabilities[idx], calibration$top3),
      data$true_top3_mask[idx]
    )

    rows[[length(rows) + 1L]] <- .benchmark_uncertainty_metrics_row(sp, "winner", "raw", winner_raw)
    rows[[length(rows) + 1L]] <- .benchmark_uncertainty_metrics_row(sp, "winner", "calibrated", winner_cal)
    rows[[length(rows) + 1L]] <- .benchmark_uncertainty_metrics_row(sp, "top3", "raw", top3_raw)
    rows[[length(rows) + 1L]] <- .benchmark_uncertainty_metrics_row(sp, "top3", "calibrated", top3_cal)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Apply fitted uncertainty calibration to an engine
#'
#' @param engine A PAPRIKA engine with computed uncertainty diagnostics.
#' @param calibration A `paprika_uncertainty_calibration` object.
#' @param replace Logical. If `TRUE`, overwrite the active probabilities and
#'   keep the raw values under `*_raw`. Otherwise add calibrated values under
#'   `*_calibrated`.
#'
#' @return The updated engine.
#' @export
engine_apply_uncertainty_calibration <- function(engine,
                                                 calibration,
                                                 replace = FALSE) {
  stopifnot(inherits(calibration, "paprika_uncertainty_calibration"))
  if (is.null(engine$diagnostics)) engine$diagnostics <- list()

  win_prob <- engine$diagnostics$winner_probabilities
  if (!is.null(win_prob) && length(win_prob)) {
    cal_win <- .benchmark_apply_setwise_beta(list(as.numeric(win_prob)), calibration$winner)[[1L]]
    if (isTRUE(replace)) {
      engine$diagnostics$winner_probabilities_raw <- as.numeric(win_prob)
      engine$diagnostics$winner_probabilities <- cal_win
    } else {
      engine$diagnostics$winner_probabilities_calibrated <- cal_win
    }
  }

  top3_prob <- engine$diagnostics$profile_top3_prob
  if (!is.null(top3_prob) && length(top3_prob)) {
    cal_top3 <- .benchmark_apply_binary_beta(list(as.numeric(top3_prob)), calibration$top3)[[1L]]
    if (isTRUE(replace)) {
      engine$diagnostics$profile_top3_prob_raw <- as.numeric(top3_prob)
      engine$diagnostics$profile_top3_prob <- cal_top3
    } else {
      engine$diagnostics$profile_top3_prob_calibrated <- cal_top3
    }
  }

  engine$diagnostics$uncertainty_calibration <- list(
    applied = TRUE,
    replace = isTRUE(replace),
    config = calibration$config,
    calibration_frac = calibration$calibration_frac,
    seed = calibration$seed
  )
  engine
}

#' @export
print.paprika_uncertainty_calibration <- function(x, ...) {
  cat("paprika_uncertainty_calibration\n")
  cat(sprintf("- config: %s\n", x$config))
  cat(sprintf("- calibration fraction: %.2f\n", x$calibration_frac))
  cat(sprintf("- runs: %d (calibration=%d, test=%d)\n",
    nrow(x$data),
    length(x$split$calibration),
    length(x$split$test)
  ))
  if (!is.null(x$summary) && nrow(x$summary)) {
    test_rows <- x$summary[x$summary$split == "test", , drop = FALSE]
    if (nrow(test_rows)) {
      cat("- test summary:\n")
      print(test_rows, row.names = FALSE)
    }
  }
  invisible(x)
}

#' @keywords internal
.benchmark_prepare_uncertainty_calibration_data <- function(study, config) {
  details <- if (is.list(study) && !is.null(study$details)) study$details else study
  stopifnot(is.data.frame(details))

  required <- c("config", "output_mode", "feasible", "scenario_key", "patient_id",
    "winner_probabilities", "top3_probabilities", "true_top1", "true_top3_mask")
  missing_cols <- setdiff(required, names(details))
  if (length(missing_cols)) {
    stop(
      "Benchmark details are missing probability-vector columns: ",
      paste(missing_cols, collapse = ", "),
      ". Rerun the benchmark with the current benchmark pipeline."
    )
  }

  df <- details[details$config == config & details$output_mode == "uncertainty" & details$feasible %in% TRUE, , drop = FALSE]
  if (!nrow(df)) {
    stop("No feasible uncertainty-mode runs found for config '", config, "'.")
  }

  winner_probabilities <- lapply(df$winner_probabilities, .benchmark_decode_numeric)
  top3_probabilities <- lapply(df$top3_probabilities, .benchmark_decode_numeric)
  true_top3_mask <- lapply(df$true_top3_mask, .benchmark_decode_numeric)
  ok <- vapply(seq_len(nrow(df)), function(i) {
    length(winner_probabilities[[i]]) > 0L &&
      length(top3_probabilities[[i]]) == length(winner_probabilities[[i]]) &&
      length(true_top3_mask[[i]]) == length(winner_probabilities[[i]]) &&
      is.finite(df$true_top1[i])
  }, logical(1))
  df <- df[ok, , drop = FALSE]
  winner_probabilities <- winner_probabilities[ok]
  top3_probabilities <- top3_probabilities[ok]
  true_top3_mask <- true_top3_mask[ok]

  out <- data.frame(
    run_id = seq_len(nrow(df)),
    scenario_key = df$scenario_key,
    patient_id = df$patient_id,
    true_top1 = as.integer(df$true_top1),
    stringsAsFactors = FALSE
  )
  out$winner_probabilities <- I(winner_probabilities)
  out$top3_probabilities <- I(top3_probabilities)
  out$true_top3_mask <- I(true_top3_mask)
  out
}

#' @keywords internal
.benchmark_uncertainty_split <- function(data,
                                         calibration_frac = 0.5,
                                         seed = 1L,
                                         stratify_by_scenario = TRUE) {
  n <- nrow(data)
  if (n < 2L) stop("At least two runs are required for held-out calibration.")
  set.seed(seed)

  strata <- if (isTRUE(stratify_by_scenario)) split(seq_len(n), data$scenario_key) else list(all = seq_len(n))
  calibration_idx <- integer()
  for (idx in strata) {
    if (!length(idx)) next
    if (length(idx) == 1L) next
    n_cal <- round(length(idx) * calibration_frac)
    n_cal <- max(1L, min(length(idx) - 1L, n_cal))
    calibration_idx <- c(calibration_idx, sample(idx, size = n_cal))
  }
  calibration_idx <- sort(unique(calibration_idx))
  if (!length(calibration_idx)) {
    calibration_idx <- sample.int(n, size = max(1L, min(n - 1L, round(n * calibration_frac))))
  }
  test_idx <- setdiff(seq_len(n), calibration_idx)
  if (!length(test_idx)) {
    move <- tail(calibration_idx, 1L)
    calibration_idx <- setdiff(calibration_idx, move)
    test_idx <- move
  }
  list(calibration = calibration_idx, test = sort(test_idx))
}

#' @keywords internal
.benchmark_fit_setwise_beta <- function(prob_list, true_index, eps = 1e-6, maxit = 500L) {
  prob_list <- lapply(prob_list, .benchmark_clip_prob, eps = eps)
  true_index <- as.integer(true_index)

  loss <- function(par) {
    a <- par[1]
    b <- par[2]
    vals <- vapply(seq_along(prob_list), function(i) {
      p <- prob_list[[i]]
      q <- .benchmark_softmax(a * log(p) + b * log1p(-p))
      -log(q[true_index[i]])
    }, numeric(1))
    mean(vals)
  }

  init <- c(a = 1, b = 0)
  fit <- tryCatch(
    stats::optim(init, loss, method = "BFGS", control = list(maxit = as.integer(maxit))),
    error = function(e) NULL
  )
  params <- if (is.null(fit) || !is.finite(fit$value)) init else fit$par
  list(
    method = "setwise_beta",
    params = stats::setNames(as.numeric(params), c("a", "b")),
    eps = eps,
    converged = !is.null(fit) && isTRUE(fit$convergence == 0),
    objective = if (is.null(fit)) NA_real_ else fit$value
  )
}

#' @keywords internal
.benchmark_fit_binary_beta <- function(prob, event, eps = 1e-6, maxit = 500L) {
  ok <- is.finite(prob) & is.finite(event)
  prob <- .benchmark_clip_prob(prob[ok], eps = eps)
  event <- as.numeric(event[ok])

  loss <- function(par) {
    eta <- par[1] * log(prob) + par[2] * log1p(-prob) + par[3]
    q <- stats::plogis(eta)
    -mean(event * log(q) + (1 - event) * log1p(-q))
  }

  init <- c(a = 1, b = -1, c = 0)
  fit <- tryCatch(
    stats::optim(init, loss, method = "BFGS", control = list(maxit = as.integer(maxit))),
    error = function(e) NULL
  )
  params <- if (is.null(fit) || !is.finite(fit$value)) init else fit$par
  list(
    method = "binary_beta",
    params = stats::setNames(as.numeric(params), c("a", "b", "c")),
    eps = eps,
    converged = !is.null(fit) && isTRUE(fit$convergence == 0),
    objective = if (is.null(fit)) NA_real_ else fit$value
  )
}

#' @keywords internal
.benchmark_apply_setwise_beta <- function(prob_list, model) {
  lapply(prob_list, function(p) {
    p <- .benchmark_clip_prob(p, eps = model$eps)
    .benchmark_softmax(model$params[["a"]] * log(p) + model$params[["b"]] * log1p(-p))
  })
}

#' @keywords internal
.benchmark_apply_binary_beta <- function(prob_list, model) {
  lapply(prob_list, function(p) {
    p <- .benchmark_clip_prob(p, eps = model$eps)
    stats::plogis(
      model$params[["a"]] * log(p) +
        model$params[["b"]] * log1p(-p) +
        model$params[["c"]]
    )
  })
}

#' @keywords internal
.benchmark_uncertainty_metrics_winner <- function(prob_list, true_index) {
  prob_list <- lapply(prob_list, .benchmark_clip_prob, eps = 1e-12)
  pred_top1 <- vapply(prob_list, which.max, integer(1))
  top1_conf <- vapply(prob_list, max, numeric(1))
  true_prob <- vapply(seq_along(prob_list), function(i) prob_list[[i]][true_index[i]], numeric(1))
  brier <- mean(vapply(seq_along(prob_list), function(i) {
    y <- rep(0, length(prob_list[[i]]))
    y[true_index[i]] <- 1
    mean((prob_list[[i]] - y)^2)
  }, numeric(1)))
  log_loss <- -mean(log(true_prob))
  data.frame(
    n_runs = length(prob_list),
    n_obs = length(prob_list),
    top1_acc = mean(pred_top1 == true_index),
    mean_confidence = mean(top1_conf),
    mean_true_prob = mean(true_prob),
    brier = brier,
    log_loss = log_loss,
    ece = .benchmark_ece(top1_conf, as.numeric(pred_top1 == true_index), bins = 10L),
    sum_prob_mean = mean(vapply(prob_list, sum, numeric(1))),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.benchmark_uncertainty_metrics_top3 <- function(prob_list, true_mask_list) {
  prob <- unlist(prob_list, use.names = FALSE)
  event <- unlist(true_mask_list, use.names = FALSE)
  ok <- is.finite(prob) & is.finite(event)
  prob <- .benchmark_clip_prob(prob[ok], eps = 1e-12)
  event <- as.numeric(event[ok])
  log_loss <- -mean(event * log(prob) + (1 - event) * log1p(-prob))
  data.frame(
    n_runs = length(prob_list),
    n_obs = length(prob),
    top1_acc = NA_real_,
    mean_confidence = mean(prob),
    mean_true_prob = mean(prob[event > 0.5]),
    brier = mean((prob - event)^2),
    log_loss = log_loss,
    ece = .benchmark_ece(prob, event, bins = 10L),
    sum_prob_mean = mean(vapply(prob_list, sum, numeric(1))),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.benchmark_uncertainty_metrics_row <- function(split, target, stage, metrics) {
  data.frame(
    split = split,
    target = target,
    stage = stage,
    n_runs = metrics$n_runs,
    n_obs = metrics$n_obs,
    top1_acc = metrics$top1_acc,
    mean_confidence = metrics$mean_confidence,
    mean_true_prob = metrics$mean_true_prob,
    brier = metrics$brier,
    log_loss = metrics$log_loss,
    ece = metrics$ece,
    sum_prob_mean = metrics$sum_prob_mean,
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.benchmark_softmax <- function(x) {
  x <- as.numeric(x)
  x <- x - max(x)
  ex <- exp(x)
  ex / sum(ex)
}

#' @keywords internal
.benchmark_clip_prob <- function(x, eps = 1e-6) {
  pmin(pmax(as.numeric(x), eps), 1 - eps)
}

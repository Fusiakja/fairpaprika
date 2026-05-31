# Diagnostic functions for fairpaprika
# Provides MCMC convergence diagnostics, sensitivity analysis, and model comparison

#' Polytope sampling convergence diagnostics
#'
#' Computes diagnostics for assessing MCMC convergence of polytope sampling.
#' When multiple chains are provided, computes Gelman-Rubin R-hat statistic.
#' Always computes effective sample size (ESS) estimates.
#'
#' @param samples Result from [engine_polytope_sample()]. If chains > 1,
#'   diagnostics include multi-chain R-hat.
#' @param chains_list Optional list of separate chain results (alternative to
#'   passing multi-chain samples object).
#'
#' @return List with elements:
#'   * `rhat` Gelman-Rubin statistic per variable (< 1.1 suggests convergence)
#'   * `ess` Effective sample size per variable
#'   * `summary` Data frame with variable-wise diagnostics
#'   * `converged` Logical, TRUE if all R-hat < 1.1
#'
#' @export
polytope_diagnostics <- function(samples, chains_list = NULL) {
    stopifnot(inherits(samples, "paprika_polytope"))

    # Extract from multi-chain result or use provided chains
    if (!is.null(samples$chains) && is.null(chains_list)) {
        chains_list <- lapply(samples$chains, `[[`, "weights")
    } else if (!is.null(chains_list)) {
        # Use provided chains
    } else {
        # Single chain: compute ESS only
        chains_list <- list(samples$weights)
    }

    n_chains <- length(chains_list)

    # Compute diagnostics
    if (n_chains > 1) {
        rhat <- .compute_gelman_rubin(chains_list)
    } else {
        rhat <- rep(NA_real_, ncol(chains_list[[1]]))
        names(rhat) <- colnames(chains_list[[1]])
    }

    ess <- .compute_ess(chains_list)

    # Summary table
    summary_df <- data.frame(
        variable = names(rhat),
        rhat = as.numeric(rhat),
        ess = as.numeric(ess),
        ess_per_chain = as.numeric(ess) / n_chains,
        converged = rhat < 1.1,
        stringsAsFactors = FALSE
    )

    list(
        rhat = rhat,
        ess = ess,
        summary = summary_df,
        n_chains = n_chains,
        n_samples_per_chain = nrow(chains_list[[1]]),
        converged = all(rhat < 1.1, na.rm = TRUE)
    )
}

#' Sensitivity analysis for tolerance parameters
#'
#' Tests how model solutions change across a range of tolerance parameter values.
#' Useful for assessing robustness of preference inference to methodological choices.
#'
#' @param engine A `paprika_engine`.
#' @param param Parameter to vary: `\"eps_strict\"`, `\"tau_equal\"`, or `\"epsilon_monotone\"`.
#' @param range Multiplier range for parameter (e.g., c(0.5, 2.0) tests 50%-200% of current value).
#' @param steps Number of parameter values to test.
#' @param compute_importance Logical. Compute criterion importance at each step?
#'
#' @return List with:
#'   * `param_values` Vector of tested parameter values
#'   * `feasible` Logical vector indicating feasibility at each value
#'   * `weights` Matrix of weights (if feasible) at each parameter value
#'   * `importance` Matrix of criterion importance (if compute_importance = TRUE)
#'   * `summary` Data frame summarizing stability
#'
#' @export
sensitivity_analysis <- function(engine,
                                 param = c("eps_strict", "tau_equal", "epsilon_monotone"),
                                 range = c(0.5, 2.0),
                                 steps = 5,
                                 compute_importance = TRUE) {
    engine <- validate_engine(engine)
    param <- match.arg(param)

    # Get current parameter value
    current_value <- engine$settings[[param]]
    if (is.null(current_value)) {
        stop("Parameter ", param, " not found in engine$settings")
    }

    # Generate parameter values to test
    param_values <- seq(current_value * range[1], current_value * range[2], length.out = steps)

    # Storage
    feasible <- logical(steps)
    weights_list <- vector("list", steps)
    importance_list <- if (compute_importance) vector("list", steps) else NULL

    for (i in seq_along(param_values)) {
        # Modify settings temporarily
        settings_temp <- engine$settings
        settings_temp[[param]] <- param_values[i]

        # Try to solve
        con <- constraints_from_decisions(
            engine$domains, engine$decisions,
            eps_strict = settings_temp$eps_strict,
            tau_equal = settings_temp$tau_equal,
            epsilon_monotone = settings_temp$epsilon_monotone,
            normalize_sum_top = settings_temp$normalize_sum_top,
            interactions = settings_temp$interactions %||% list()
        )

        sol <- solve_feasible(con)

        if (isTRUE(sol$ok)) {
            feasible[i] <- TRUE
            w_scaled <- rescale_weights_range100(engine$domains, sol$weights)
            weights_list[[i]] <- w_scaled

            if (compute_importance) {
                var_names <- names(w_scaled)
                w_df <- data.frame(
                    Merkmal = var_names, Nutzen = as.numeric(w_scaled),
                    stringsAsFactors = FALSE
                )
                imp <- importance_from_weights(w_df)
                importance_list[[i]] <- imp
            }
        } else {
            feasible[i] <- FALSE
        }
    }

    # Combine weights into matrix (rows = param values, cols = variables)
    if (any(feasible)) {
        feas_idx <- which(feasible)
        var_names <- names(weights_list[[feas_idx[1]]])
        n_vars <- length(var_names)

        weights_mat <- matrix(NA_real_, nrow = steps, ncol = n_vars)
        colnames(weights_mat) <- var_names

        for (i in feas_idx) {
            weights_mat[i, ] <- as.numeric(weights_list[[i]])
        }

        if (compute_importance) {
            crit_names <- names(importance_list[[feas_idx[1]]])
            n_crit <- length(crit_names)
            importance_mat <- matrix(NA_real_, nrow = steps, ncol = n_crit)
            colnames(importance_mat) <- crit_names

            for (i in feas_idx) {
                importance_mat[i, ] <- as.numeric(importance_list[[i]])
            }
        } else {
            importance_mat <- NULL
        }
    } else {
        weights_mat <- NULL
        importance_mat <- NULL
    }

    # Summary: compute coefficient of variation for each variable
    if (!is.null(weights_mat) && sum(feasible) > 1) {
        feas_weights <- weights_mat[feasible, , drop = FALSE]
        cv <- apply(feas_weights, 2, function(x) {
            m <- mean(x, na.rm = TRUE)
            s <- stats::sd(x, na.rm = TRUE)
            if (m == 0) NA_real_ else s / m
        })

        summary_df <- data.frame(
            variable = names(cv),
            mean = colMeans(feas_weights, na.rm = TRUE),
            sd = apply(feas_weights, 2, stats::sd, na.rm = TRUE),
            cv = cv,
            stable = cv < 0.1, # arbitrary threshold
            stringsAsFactors = FALSE
        )
    } else {
        summary_df <- NULL
    }

    list(
        param = param,
        param_values = param_values,
        current_value = current_value,
        feasible = feasible,
        weights = weights_mat,
        importance = importance_mat,
        summary = summary_df,
        n_feasible = sum(feasible)
    )
}

#' Check bootstrap convergence
#'
#' Assesses whether bootstrap has converged by examining running statistics.
#' Useful for determining if more replicates (B) are needed.
#'
#' @param boot A `paprika_bootstrap` object.
#' @param window_size Window for computing running mean/SD.
#'
#' @return List with:
#'   * `converged` Logical, TRUE if appears converged
#'   * `running_mean` Matrix of running means
#'   * `running_sd` Matrix of running SDs
#'   * `suggested_B` Suggested minimum B based on stability
#'
#' @export
bootstrap_convergence <- function(boot, window_size = 50) {
    stopifnot(inherits(boot, "paprika_bootstrap"))

    I <- boot$importance_samples
    if (is.null(I) || ncol(I) < window_size * 2) {
        warning("Not enough bootstrap samples for convergence check")
        return(list(converged = NA, suggested_B = max(100, ncol(I) * 2)))
    }

    n_samp <- ncol(I)
    n_crit <- nrow(I)
    crit_names <- rownames(I)

    # Compute running mean and SD
    n_windows <- floor(n_samp / window_size)
    running_mean <- matrix(NA_real_, nrow = n_crit, ncol = n_windows)
    running_sd <- matrix(NA_real_, nrow = n_crit, ncol = n_windows)
    rownames(running_mean) <- rownames(running_sd) <- crit_names

    for (w in seq_len(n_windows)) {
        idx <- seq_len(w * window_size)
        running_mean[, w] <- rowMeans(I[, idx, drop = FALSE], na.rm = TRUE)
        running_sd[, w] <- apply(I[, idx, drop = FALSE], 1, stats::sd, na.rm = TRUE)
    }

    # Check stability of last windows
    if (n_windows >= 3) {
        last_windows <- max(1, n_windows - 2):n_windows
        mean_change <- apply(running_mean[, last_windows, drop = FALSE], 1, function(x) {
            max(abs(diff(x))) / mean(x, na.rm = TRUE)
        })

        converged_flags <- mean_change < 0.05 # 5% change threshold
        converged <- all(converged_flags)

        if (!converged) {
            suggested_B <- ceiling(n_samp * 1.5)
        } else {
            suggested_B <- n_samp
        }
    } else {
        converged <- FALSE
        suggested_B <- window_size * 5
    }

    list(
        converged = converged,
        running_mean = running_mean,
        running_sd = running_sd,
        window_size = window_size,
        n_windows = n_windows,
        suggested_B = suggested_B
    )
}

#' Compare models with and without interactions
#'
#' Compares preference models with and without interaction terms.
#' Helps assess whether interactions meaningfully improve model fit.
#'
#' @param engine_base Engine without interactions.
#' @param engine_interaction Engine with interactions enabled.
#' @param n_samples Number of polytope samples for comparison (default 500).
#'
#' @return List with:
#'   * `feasible` Named logical (base, interaction) indicating feasibility
#'   * `weights_base` Base model weights
#'   * `weights_interaction` Interaction model weights
#'   * `polytope_variance` Variance in polytope (proxy for model flexibility)
#'   * `recommendation` Character string with interpretation
#'
#' @export
model_comparison <- function(engine_base, engine_interaction, n_samples = 500) {
    stopifnot(inherits(engine_base, "paprika_engine"))
    stopifnot(inherits(engine_interaction, "paprika_engine"))

    # Solve both models
    engine_base <- engine_compute(engine_base)
    engine_interaction <- engine_compute(engine_interaction)

    feasible <- c(
        base = !is.null(engine_base$weights),
        interaction = !is.null(engine_interaction$weights)
    )

    if (!all(feasible)) {
        warning("One or both models are infeasible")
        return(list(
            feasible = feasible,
            recommendation = "Cannot compare: infeasible model(s)"
        ))
    }

    # Sample polytopes to assess uncertainty/flexibility
    samples_base <- tryCatch(
        {
            engine_polytope_sample(engine_base, n = n_samples, progress = FALSE)
        },
        error = function(e) NULL
    )

    samples_int <- tryCatch(
        {
            engine_polytope_sample(engine_interaction, n = n_samples, progress = FALSE)
        },
        error = function(e) NULL
    )

    # Compute variance in samples (flexibility proxy)
    if (!is.null(samples_base)) {
        var_base <- mean(apply(samples_base$weights_scaled, 2, stats::var, na.rm = TRUE))
    } else {
        var_base <- NA_real_
    }

    if (!is.null(samples_int)) {
        var_int <- mean(apply(samples_int$weights_scaled, 2, stats::var, na.rm = TRUE))
    } else {
        var_int <- NA_real_
    }

    # Simple recommendation based on variance
    if (!is.na(var_base) && !is.na(var_int)) {
        var_ratio <- var_int / var_base
        if (var_ratio > 2) {
            recommendation <- "Interaction model shows much higher uncertainty. Consider using base model for stability."
        } else if (var_ratio > 1.2) {
            recommendation <- "Interaction model slightly more uncertain. Check if interactions are meaningful."
        } else {
            recommendation <- "Interaction model comparable to base. Use if interactions are interpretable."
        }
    } else {
        recommendation <- "Unable to assess: polytope sampling failed."
    }

    list(
        feasible = feasible,
        weights_base = engine_base$weights,
        weights_interaction = engine_interaction$weights,
        polytope_variance = c(base = var_base, interaction = var_int),
        variance_ratio = if (!is.na(var_base) && !is.na(var_int)) var_int / var_base else NA_real_,
        recommendation = recommendation
    )
}

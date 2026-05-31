# Visualization functions for fairpaprika
# Base R graphics for maximum compatibility

#' Plot decision quality metrics
#'
#' Creates a visual summary of SDM decision quality across all dimensions.
#'
#' @param quality_result Output from `sdm_decision_quality()`.
#' @param main Plot title.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_decision_quality <- function(quality_result, main = "SDM Decision Quality") {
  components <- quality_result$quality_components

  # Create bar plot
  par(mar = c(5, 10, 4, 2))
  barplot(components,
    horiz = TRUE, xlim = c(0, 1),
    main = main, xlab = "Score (0-1)",
    col = ifelse(components > 0.7, "darkgreen",
      ifelse(components > 0.5, "orange", "red")
    ),
    las = 1
  )
  abline(v = c(0.5, 0.7), lty = 2, col = "gray")

  # Add overall quality
  mtext(sprintf("Overall Quality: %.2f", quality_result$overall_quality),
    side = 3, line = 0.5, cex = 0.9
  )

  par(mar = c(5, 4, 4, 2)) # Reset margins
  invisible(NULL)
}

#' Plot patient journey
#'
#' Visualizes the patient journey through preference elicitation.
#'
#'
#' @param journey_result Output from `sdm_journey_report()`.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_patient_journey <- function(journey_result, title = "Patient Journey", subtitle = "") {
  qdata <- journey_result$question_data

  if (is.null(qdata) || nrow(qdata) == 0) {
    plot.new()
    text(0.5, 0.5, "No journey data available", cex = 1.5)
    return(invisible(NULL))
  }

  n_q <- nrow(qdata)

  # Set up 2x2 panel
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  # 1. Question complexity over time
  plot(qdata$question_id, qdata$complexity,
    type = "b",
    main = "Cognitive Load", xlab = "Question #", ylab = "Complexity",
    col = "steelblue", pch = 19
  )
  if (n_q > 5) {
    lines(lowess(qdata$question_id, qdata$complexity, f = 0.3), col = "red", lwd = 2)
  }

  # 2. Response patterns
  pref_colors <- c(A = "green", B = "blue", E = "orange")
  pref_counts <- table(factor(qdata$pref, levels = c("A", "B", "E")))
  barplot(pref_counts,
    col = pref_colors,
    main = "Response Distribution", ylab = "Count",
    names.arg = c("Prefer A", "Prefer B", "Equal")
  )

  # 3. Undo/revision patterns
  undos <- which(qdata$after_undo)
  plot(qdata$question_id, rep(1, n_q),
    type = "n", ylim = c(0, 2),
    main = "Revision Patterns", xlab = "Question #", ylab = ""
  )
  abline(v = undos, col = "red", lwd = 2)
  text(n_q / 2, 1, sprintf(
    "%d undos (%.1f%%)",
    length(undos), 100 * length(undos) / n_q
  ),
  cex = 1.2
  )

  # 4. Fatigue indicators
  if (!is.null(journey_result$fatigue_indicators)) {
    fat <- journey_result$fatigue_indicators
    plot.new()
    text(0.5, 0.7, "Fatigue Assessment", cex = 1.3, font = 2)
    text(0.5, 0.5, sprintf("Questions: %d", fat$total_questions), cex = 1.1)
    if (!is.na(fat$fatigue_suspected)) {
      text(0.5, 0.3,
        ifelse(fat$fatigue_suspected, "[!] Fatigue Suspected", "[OK] No Fatigue"),
        col = ifelse(fat$fatigue_suspected, "red", "darkgreen"),
        cex = 1.2, font = 2
      )
    }
  }

  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2)) # Reset
  invisible(NULL)
}

#' Plot treatment comparison
#'
#' Patient-friendly visual comparison of treatment options.
#'
#' @param engine A computed `paprika_engine`.
#' @param profiles Data frame of treatment profiles to compare.
#' @param top_n Number of top options to highlight (default 3).
#' @param main Plot title.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_treatment_comparison <- function(engine, profiles, top_n = 3,
                                      main = "Treatment Comparison") {
  engine <- validate_engine(engine)

  if (is.null(engine$diagnostics$winner_probabilities)) {
    stop("Engine must be computed with winner probabilities")
  }

  win_probs <- engine$diagnostics$winner_probabilities
  n_profiles <- length(win_probs)

  # Sort by probability
  ord <- order(win_probs, decreasing = TRUE)
  top_idx <- ord[seq_len(min(top_n, n_profiles))]

  # Create bar plot
  par(mar = c(5, 8, 4, 2))
  bp <- barplot(win_probs[ord],
    horiz = TRUE, xlim = c(0, 1),
    main = main, xlab = "Match Score",
    col = ifelse(seq_along(win_probs) <= top_n, "steelblue", "gray80"),
    las = 1, names.arg = paste("Option", ord)
  )

  # Highlight top choices
  if (top_n > 0) {
    rect(0, bp[1] - 0.5, win_probs[ord[1]], bp[1] + 0.5,
      border = "darkgreen", lwd = 3
    )
  }

  par(mar = c(5, 4, 4, 2)) # Reset
  invisible(NULL)
}

#' Plot diagnostic summary
#'
#' Multi-panel plot showing diagnostic information.
#'
#' @param diag_result Output from `polytope_diagnostics()` or sensitivity analysis.
#' @param type Type of diagnostic: "polytope" or "sensitivity".
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_diagnostics <- function(diag_result, type = c("polytope", "sensitivity")) {
  type <- match.arg(type)

  if (type == "polytope") {
    # Polytope diagnostics
    if (is.null(diag_result$summary)) {
      plot.new()
      text(0.5, 0.5, "No diagnostic data", cex = 1.5)
      return(invisible(NULL))
    }

    summ <- diag_result$summary

    par(mfrow = c(1, 2), mar = c(5, 10, 4, 2))

    # R-hat values
    if ("rhat" %in% names(summ)) {
      rhat_vals <- summ$rhat[!is.na(summ$rhat)]
      if (length(rhat_vals) > 0) {
        barplot(rhat_vals,
          horiz = TRUE, xlim = c(0.9, max(1.2, max(rhat_vals))),
          main = "Gelman-Rubin R-hat",
          xlab = "R-hat (< 1.1 = converged)",
          col = ifelse(rhat_vals < 1.1, "darkgreen", "red"),
          las = 1, cex.names = 0.7
        )
        abline(v = 1.1, lty = 2, col = "black", lwd = 2)
      }
    }

    # ESS values
    if ("ess" %in% names(summ)) {
      ess_vals <- summ$ess[!is.na(summ$ess)]
      if (length(ess_vals) > 0) {
        barplot(ess_vals,
          horiz = TRUE,
          main = "Effective Sample Size",
          xlab = "ESS",
          col = "steelblue",
          las = 1, cex.names = 0.7
        )
      }
    }

    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  } else if (type == "sensitivity") {
    # Sensitivity analysis
    if (is.null(diag_result$param_values)) {
      plot.new()
      text(0.5, 0.5, "No sensitivity data", cex = 1.5)
      return(invisible(NULL))
    }

    par(mfrow = c(1, 2))

    # Feasibility across parameter range
    plot(diag_result$param_values, diag_result$feasible,
      type = "b", pch = 19, col = "steelblue",
      main = "Feasibility",
      xlab = diag_result$param,
      ylab = "Feasible (0/1)", ylim = c(-0.1, 1.1)
    )

    # Stability (if available)
    if (!is.null(diag_result$summary)) {
      plot(1,
        type = "n", xlim = c(0, 1), ylim = c(0, 1),
        main = "Stability", xlab = "", ylab = "", axes = FALSE
      )
      text(0.5, 0.5, sprintf(
        "%d/%d feasible\n%.0f%% stable variables",
        sum(diag_result$feasible),
        length(diag_result$feasible),
        100 * mean(diag_result$summary$stable, na.rm = TRUE)
      ),
      cex = 1.3
      )
      box()
    }

    par(mfrow = c(1, 1))
  }

  invisible(NULL)
}

#' Plot importance with uncertainty
#'
#' Shows criterion importance with confidence intervals.
#'
#' @param bootstrap_result Output from `engine_bootstrap()`.
#' @param conf_level Confidence level for intervals (default 0.95).
#' @param main Plot title.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_importance_ci <- function(bootstrap_result, conf_level = 0.95,
                               main = "Criterion Importance") {
  if (!inherits(bootstrap_result, "paprika_bootstrap")) {
    stop("Input must be a paprika_bootstrap object")
  }

  I <- bootstrap_result$importance_samples
  crit <- rownames(I)

  # Compute means and CIs
  means <- rowMeans(I, na.rm = TRUE)
  alpha <- 1 - conf_level
  lower <- apply(I, 1, quantile, probs = alpha / 2, na.rm = TRUE)
  upper <- apply(I, 1, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

  # Plot
  par(mar = c(5, 10, 4, 2))

  y_pos <- seq_along(crit)
  plot(means, y_pos,
    xlim = c(0, max(upper) * 1.1), ylim = c(0.5, length(crit) + 0.5),
    pch = 19, col = "steelblue", cex = 1.5,
    xlab = "Importance (%)", ylab = "",
    main = main, yaxt = "n"
  )
  axis(2, at = y_pos, labels = crit, las = 1, cex.axis = 0.9)

  # Add error bars
  segments(lower, y_pos, upper, y_pos, col = "steelblue", lwd = 2)
  segments(lower, y_pos - 0.1, lower, y_pos + 0.1, col = "steelblue", lwd = 2)
  segments(upper, y_pos - 0.1, upper, y_pos + 0.1, col = "steelblue", lwd = 2)

  par(mar = c(5, 4, 4, 2))
  invisible(NULL)
}

#' Plot bootstrap weight distributions
#' Plot preference weights
#'
#' Visualizes preference weights from an engine (point estimates) or bootstrap result (distributions).
#'
#' @param x A `paprika_engine` or `paprika_bootstrap` object.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param criteria Optional character vector of criteria to plot (NULL = all).
#' @param type Plot type for bootstrap input: "density" or "boxplot".
#'
#' @return Invisibly returns NULL.
#' @export
plot_weights <- function(x, title = NULL, subtitle = NULL, criteria = NULL, type = c("density", "boxplot")) {
  # Handle Engine (Point Estimates)
  if (inherits(x, "paprika_engine")) {
    w <- x$results$weights
    if (is.null(w)) {
      plot.new()
      text(0.5, 0.5, "No weights computed yet", cex = 1.5)
      return(invisible(NULL))
    }

    # Extract weights vector
    if (is.data.frame(w)) {
      num_cols <- vapply(w, is.numeric, logical(1))
      w_vec <- w[[which(num_cols)[1]]]
      names(w_vec) <- rownames(w)
    } else {
      w_vec <- w
    }

    # Sort and filter
    if (!is.null(criteria)) {
      w_vec <- w_vec[grep(paste(criteria, collapse = "|"), names(w_vec))]
    }
    w_vec <- sort(w_vec, decreasing = FALSE) # Ascending for horiz barplot

    par(mar = c(5, 12, 4, 2))
    barplot(w_vec,
      horiz = TRUE, las = 1,
      main = title %||% "Preference Weights",
      sub = subtitle,
      xlab = "Weight", col = "steelblue", border = NA
    )
    return(invisible(NULL))
  }

  # Handle Bootstrap (Distributions)
  if (inherits(x, "paprika_bootstrap")) {
    type <- match.arg(type)
    W <- x$weights_samples
    var_names <- rownames(W)

    if (!is.null(criteria)) {
      keep_idx <- grepl(paste(criteria, collapse = "|"), var_names)
      W <- W[keep_idx, , drop = FALSE]
    }

    if (nrow(W) == 0) {
      plot.new()
      text(0.5, 0.5, "No data to plot", cex = 1.5)
      return(invisible(NULL))
    }

    n_vars <- min(12, nrow(W))
    W_plot <- W[seq_len(n_vars), , drop = FALSE]

    if (type == "density") {
      n_rows <- ceiling(sqrt(n_vars))
      n_cols <- ceiling(n_vars / n_rows)
      par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 1))

      for (i in seq_len(n_vars)) {
        vals <- W_plot[i, ]
        dens <- stats::density(vals[!is.na(vals)])
        plot(dens, main = rownames(W_plot)[i], col = "steelblue", lwd = 2, xlab = "", ylab = "")
        abline(v = mean(vals, na.rm = TRUE), lty = 2, col = "red")
      }
      mtext(title %||% "Weight Distributions", outer = TRUE, line = -2)
      par(mfrow = c(1, 1))
    } else {
      par(mar = c(5, 12, 4, 2))
      boxplot(t(W_plot),
        horizontal = TRUE, las = 1, col = "lightblue",
        main = title %||% "Weight Uncertainty", sub = subtitle, xlab = "Weight"
      )
    }
    return(invisible(NULL))
  }

  stop("Input must be a paprika_engine or paprika_bootstrap object")
}


#' Plot pairwise comparison matrix
#'
#' Heatmap showing P(option i > option j) for all pairs.
#'
#' @param pairwise_matrix Square matrix with P(i > j) values.
#' @param threshold Threshold for highlighting strong preferences (default 0.6).
#' @param main Plot title.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_pairwise <- function(pairwise_matrix, threshold = 0.6,
                          main = "Pairwise Win Probabilities") {
  if (!is.matrix(pairwise_matrix)) {
    stop("pairwise_matrix must be a matrix")
  }

  n <- nrow(pairwise_matrix)

  # Create color palette
  colors <- colorRampPalette(c("white", "lightblue", "steelblue", "darkblue"))(100)

  # Plot heatmap
  par(mar = c(5, 5, 4, 2))

  image(1:n, 1:n, t(pairwise_matrix[n:1, ]),
    col = colors,
    xlab = "Option i", ylab = "Option j",
    main = main, axes = FALSE
  )

  axis(1, at = 1:n, labels = colnames(pairwise_matrix) %||% 1:n)
  axis(2, at = 1:n, labels = rev(rownames(pairwise_matrix) %||% 1:n), las = 1)

  # Add grid
  abline(h = seq(0.5, n + 0.5, 1), col = "gray", lty = 1)
  abline(v = seq(0.5, n + 0.5, 1), col = "gray", lty = 1)

  # Add text values
  for (i in 1:n) {
    for (j in 1:n) {
      val <- pairwise_matrix[i, j]
      text(i, n - j + 1, sprintf("%.2f", val),
        col = if (val > threshold) "white" else "black",
        cex = 0.8
      )
    }
  }

  # Add legend
  legend("topright",
    legend = sprintf("> %.2f", threshold),
    fill = "darkblue", border = "black", bg = "white"
  )

  par(mar = c(5, 4, 4, 2))
  invisible(NULL)
}

#' Plot rank probabilities for treatment profiles
#'
#' Heatmap showing the probability that each option achieves a specific rank
#' (1st, 2nd, etc.) based on bootstrap samples.
#'
#' @param profiles Data frame of treatment profiles.
#' @param boot Bootstrap result from \code{engine_bootstrap()}.
#' @param main Plot title.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_rank_probabilities <- function(profiles, boot, main = "Rank Probabilities") {
  if (!inherits(boot, "paprika_bootstrap")) {
    stop("boot must be a paprika_bootstrap object")
  }

  B <- boot$ok_count
  if (B < 10) {
    plot.new()
    text(0.5, 0.5, "Not enough bootstrap samples\nfor rank probabilities", cex = 1.2)
    return(invisible(NULL))
  }

  n_opts <- nrow(profiles)
  n_boot <- ncol(boot$weights_samples)

  # Initialize rank counts: row=Option, col=Rank
  rank_counts <- matrix(0, nrow = n_opts, ncol = n_opts)
  rownames(rank_counts) <- rownames(profiles) %||% paste("Opt", 1:n_opts)
  colnames(rank_counts) <- paste("Rank", 1:n_opts)

  # Calculate ranks for each bootstrap sample
  var_names <- boot$var_names
  domains <- boot$domains

  # Pre-compute indices for each profile's levels in the weights vector
  # profiles_idx: list of length n_opts, each containing indices into W
  profiles_idx <- vector("list", n_opts)
  for (i in 1:n_opts) {
    idx_vec <- integer(ncol(profiles))
    for (j in 1:ncol(profiles)) {
      crit <- names(profiles)[j]
      lvl <- as.character(profiles[i, j])
      key <- paste0(crit, ":", lvl)
      # Find index in var_names
      match_idx <- match(key, var_names)
      # Note: Reference levels (typically lowest) might not be in var_names
      # depending on coding. If missing, assumes 0 utility (standard).
      if (!is.na(match_idx)) {
        idx_vec[j] <- match_idx
      } else {
        idx_vec[j] <- 0L # 0 sentinel
      }
    }
    profiles_idx[[i]] <- idx_vec
  }

  W <- boot$weights_samples

  for (b in 1:n_boot) {
    w_vec <- W[, b]
    scores <- numeric(n_opts)

    for (i in 1:n_opts) {
      idx <- profiles_idx[[i]]
      valid_idx <- idx[idx > 0]
      if (length(valid_idx) > 0) {
        scores[i] <- sum(w_vec[valid_idx])
      }
    }

    # Rank: 1 = data with highest score
    # ties.method = "random" to break ties fairly
    rks <- rank(-scores, ties.method = "random")

    for (i in 1:n_opts) {
      r <- rks[i]
      rank_counts[i, r] <- rank_counts[i, r] + 1
    }
  }

  # Convert to probabilities
  rank_probs <- rank_counts / n_boot

  # Helper to truncate long names
  trunc_names <- rownames(rank_probs)
  if (any(nchar(trunc_names) > 15)) {
    trunc_names <- substr(trunc_names, 1, 15)
  }

  # Plot heatmap
  par(mar = c(5, 8, 4, 2))

  # Reverse row order for plotting (top option at top)
  plot_data <- rank_probs[n_opts:1, , drop = FALSE]

  colors <- colorRampPalette(c("white", "lightblue", "darkblue"))(100)

  image(1:n_opts, 1:n_opts, t(plot_data),
    col = colors,
    xlab = "Rank (1 = Best)", ylab = "",
    main = main, axes = FALSE
  )

  axis(1, at = 1:n_opts, labels = 1:n_opts)
  axis(2, at = 1:n_opts, labels = rev(trunc_names), las = 1)

  # Grid
  abline(h = seq(0.5, n_opts + 0.5, 1), col = "gray", lty = 1)
  abline(v = seq(0.5, n_opts + 0.5, 1), col = "gray", lty = 1)

  # Text
  for (i in 1:n_opts) { # Row (Option)
    for (j in 1:n_opts) { # Col (Rank)
      # Map to plot coordinates
      # plot_data is reversed rows.
      # row i in plot_data corresponds to option (n_opts - i + 1)
      # col j corresponds to rank j
      val <- plot_data[i, j]

      text(j, i,
        labels = if (val >= 0.01) sprintf("%.0f%%", val * 100) else "",
        col = if (val > 0.6) "white" else "black",
        cex = 0.8
      )
    }
  }

  par(mar = c(5, 4, 4, 2))
  invisible(NULL)
}

#' Plot procedural justice dashboard
#'
#' Multi-panel dashboard showing justice metrics.
#'
#' @param engine A \code{paprika_engine}.
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_justice_dashboard <- function(engine) {
  engine <- validate_engine(engine)

  # Get comprehensive justice metrics
  pj <- procedural_justice_full(engine)
  justice_basic <- engine_procedural_justice(engine)

  # 4-panel dashboard
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  # Panel 1: Overall justice scores
  scores <- c(
    Voice = pj$voice$voice_score,
    Neutrality = pj$neutrality$neutrality_score,
    Respect = pj$respect$respect_score,
    Trust = pj$trustworthiness$trust_score
  )

  barplot(scores,
    col = ifelse(scores > 0.7, "darkgreen",
      ifelse(scores > 0.5, "orange", "red")
    ),
    ylim = c(0, 1), main = "Justice Dimensions",
    ylab = "Score (0-1)", las = 1
  )
  abline(h = c(0.5, 0.7), lty = 2, col = "gray")

  # Panel 2: Criterion exposure
  exposure_counts <- vapply(justice_basic$criteria, function(x) x$exposure, integer(1))

  if (length(exposure_counts) > 0 && any(!is.na(exposure_counts)) && sum(exposure_counts, na.rm = TRUE) > 0) {
    barplot(exposure_counts,
      col = "steelblue",
      main = "Criterion Exposure", ylab = "Questions",
      las = 2, cex.names = 0.8
    )
    abline(h = mean(exposure_counts, na.rm = TRUE), lty = 2, col = "red")
  } else {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Criterion Exposure")
    text(1, 1, "No data")
  }

  # Panel 3: Coverage & balance metrics
  metrics <- c(
    Coverage = justice_basic$session$coverage,
    Balance = 1 - justice_basic$session$exposure_gini,
    Composite = pj$composite_score
  )

  barplot(metrics,
    col = c("lightblue", "lightgreen", "gold"),
    ylim = c(0, 1), main = "Summary Metrics",
    ylab = "Score (0-1)", las = 1
  )

  # Panel 4: Session summary text
  plot.new()
  text(0.5, 0.8, "Session Summary", cex = 1.3, font = 2)
  text(0.5, 0.6, sprintf("Questions: %d", nrow(engine$decisions)), cex = 1.1)
  text(0.5, 0.5, sprintf("Coverage: %.0f%%", justice_basic$session$coverage * 100), cex = 1.1)
  text(0.5, 0.4, sprintf("Gini: %.3f", justice_basic$session$exposure_gini), cex = 1.1)
  text(0.5, 0.3, sprintf("Composite: %.2f", pj$composite_score), cex = 1.1)

  overall_assessment <- if (pj$composite_score > 0.7) {
    "[OK] High Justice"
  } else if (pj$composite_score > 0.5) {
    "[-] Moderate Justice"
  } else {
    "[!] Low Justice"
  }

  text(0.5, 0.15, overall_assessment,
    col = if (pj$composite_score > 0.7) "darkgreen" else if (pj$composite_score > 0.5) "orange" else "red",
    cex = 1.3, font = 2
  )

  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  invisible(NULL)
}

#' Plot treatment profile radar charts
#'
#' Spider/radar chart for treatment options.
#'
#' @param profiles Data frame of treatment profiles.
#' @param engine A computed engine (for importance weights).
#' @param patient_weights Logical. Overlay patient importance weights?
#' @param options_to_plot Indices of options to plot (NULL = top 3).
#'
#' @return Invisibly returns NULL (plots to current device).
#' @export
plot_treatment_profiles <- function(profiles, engine,
                                    patient_weights = TRUE,
                                    options_to_plot = NULL) {
  engine <- validate_engine(engine)

  # Select options to plot
  if (is.null(options_to_plot)) {
    if (!is.null(engine$diagnostics$winner_probabilities)) {
      win_probs <- engine$diagnostics$winner_probabilities
      options_to_plot <- order(win_probs, decreasing = TRUE)[1:min(3, length(win_probs))]
    } else {
      options_to_plot <- 1:min(3, nrow(profiles))
    }
  }

  # Simplified radar chart using base R
  # (Full radar charts would require additional package or custom polygon drawing)

  n_criteria <- ncol(profiles)
  n_opts <- length(options_to_plot)

  # Convert profiles to numeric scores (0-1 scale)
  profile_scores <- matrix(0, nrow = n_opts, ncol = n_criteria)

  for (j in 1:n_criteria) {
    crit_name <- names(profiles)[j]
    levels <- engine$domains[[crit_name]]

    for (i in 1:n_opts) {
      opt_idx <- options_to_plot[i]
      level_val <- as.character(profiles[opt_idx, j])
      level_idx <- match(level_val, levels)
      profile_scores[i, j] <- (level_idx - 1) / (length(levels) - 1)
    }
  }

  # Plot as grouped bars (simpler than radar for base R)
  par(mar = c(5, 5, 4, 2))

  barplot(t(profile_scores),
    beside = TRUE,
    col = rainbow(n_criteria, alpha = 0.7),
    names.arg = paste("Opt", options_to_plot),
    legend.text = names(profiles),
    args.legend = list(x = "topright", cex = 0.8),
    main = "Treatment Profile Comparison",
    ylab = "Normalized Score (0-1)",
    ylim = c(0, 1.2)
  )

  par(mar = c(5, 4, 4, 2))
  invisible(NULL)
}

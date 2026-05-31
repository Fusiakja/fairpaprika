#' Run the default algorithm-paper benchmark
#'
#' Convenience wrapper around the paper benchmark generators and simulation
#' runner. It builds standard configs and scenarios, optionally filters them,
#' executes the study, and can directly export manuscript-ready tables and
#' figures.
#'
#' @param domains A named list of ordered criterion levels.
#' @param n_patients Number of simulated patients per scenario cell.
#' @param n_profiles Number of profiles per simulated run.
#' @param max_q Default question budget.
#' @param include Configuration groups to include when creating defaults. Any
#'   subset of `"reference"`, `"cumulative"`, `"ablation"`, `"tuning"`.
#' @param config_names Optional character vector of config names to retain.
#' @param scenario_ids Optional character vector of scenario IDs/names to retain,
#'   e.g. `c("S0", "S2", "S6")`.
#' @param interaction_pairs Optional list of interaction pairs passed to the
#'   benchmark config/scenario builders.
#' @param n_samples Number of polytope samples for full-mode configs.
#' @param burnin Burn-in for polytope sampling.
#' @param thin Thinning for polytope sampling.
#' @param top_k Top-k randomization parameter for question selection.
#' @param seed Integer seed for reproducibility.
#' @param progress Logical. Print progress while simulating?
#' @param output_dir Optional directory for automatic export via
#'   [benchmark_export_bundle()]. If `NULL`, nothing is written to disk.
#' @param prefix File prefix used when `output_dir` is provided.
#' @param devices Graphics devices for figure export. Any subset of `"pdf"` and
#'   `"png"`.
#'
#' @return A `paprika_benchmark_study` object. If `output_dir` is given, the
#'   returned object also contains an `exports` element with written paths.
#' @export
run_algorithm_paper_benchmark <- function(domains,
                                          n_patients = 100L,
                                          n_profiles = 7L,
                                          max_q = 16L,
                                          include = c("reference", "cumulative", "ablation"),
                                          config_names = NULL,
                                          scenario_ids = NULL,
                                          interaction_pairs = NULL,
                                          n_samples = 200L,
                                          burnin = 50L,
                                          thin = 1L,
                                          top_k = 1L,
                                          seed = 1L,
                                          progress = FALSE,
                                          output_dir = NULL,
                                          prefix = "algorithm_paper_benchmark",
                                          devices = c("pdf", "png")) {
  domains <- validate_domains(domains)

  configs <- benchmark_paper_configs(
    domains,
    max_q = max_q,
    interaction_pairs = interaction_pairs,
    n_samples = n_samples,
    burnin = burnin,
    thin = thin,
    top_k = top_k,
    include = include
  )
  if (!is.null(config_names)) {
    keep <- intersect(config_names, names(configs))
    if (length(keep) < length(unique(config_names)) && !"tuning" %in% include) {
      configs <- benchmark_paper_configs(
        domains,
        max_q = max_q,
        interaction_pairs = interaction_pairs,
        n_samples = n_samples,
        burnin = burnin,
        thin = thin,
        top_k = top_k,
        include = c(include, "tuning")
      )
      keep <- intersect(config_names, names(configs))
    }
    if (length(keep) < 1L) stop("No requested config_names matched the generated configs.")
    missing_cfg <- setdiff(config_names, keep)
    if (length(missing_cfg)) {
      stop("Requested config_names were not generated: ", paste(missing_cfg, collapse = ", "))
    }
    configs <- configs[keep]
  }

  scenarios <- benchmark_paper_scenarios(
    domains,
    n_profiles = n_profiles,
    max_q = max_q,
    interaction_pairs = interaction_pairs
  )
  if (!is.null(scenario_ids)) {
    keep <- intersect(scenario_ids, names(scenarios))
    if (!length(keep)) stop("No requested scenario_ids matched the generated scenarios.")
    scenarios <- scenarios[keep]
  }

  study <- benchmark_simulation_study(
    domains,
    configs = configs,
    scenarios = scenarios,
    n_patients = n_patients,
    seed = seed,
    progress = progress
  )

  if (!is.null(output_dir)) {
    study$exports <- benchmark_export_bundle(
      study,
      dir = output_dir,
      prefix = prefix,
      devices = devices
    )
  }

  invisible(study)
}

#' Export benchmark tables for manuscripts
#'
#' Writes the main benchmark summary tables to disk as CSV plus an RDS bundle for
#' downstream Quarto or R workflows.
#'
#' @param study A `paprika_benchmark_study` object or compatible details data
#'   frame.
#' @param dir Output directory.
#' @param prefix File prefix for exported tables.
#'
#' @return Invisibly returns a named list of written paths.
#' @export
benchmark_export_tables <- function(study,
                                    dir,
                                    prefix = "algorithm_paper_benchmark") {
  dir <- .benchmark_ensure_dir(dir)
  summary <- .benchmark_summary_ordered(study)
  details <- if (is.list(study) && !is.null(study$details)) study$details else NULL

  cumulative <- summary[summary$config_group %in% c("reference", "cumulative", "full"), , drop = FALSE]
  ablation <- summary[summary$config_group %in% c("full", "ablation"), , drop = FALSE]
  tuning <- summary[summary$config_group %in% c("full", "tuning"), , drop = FALSE]
  baseline_full <- .benchmark_baseline_full_table(summary)
  baseline_full_ci <- benchmark_compare_configs_ci(
    study,
    config_a = "reference_paprika",
    config_b = "full",
    metrics = c("top1_acc", "n_questions_mean", "regret_mean", "pair_coverage", "feasible_rate", "brier_winner", "calibration_ece"),
    by = "scenario_key",
    B = 500L,
    seed = 1L
  )

  path_summary <- file.path(dir, paste0(prefix, "_summary.csv"))
  path_cumulative <- file.path(dir, paste0(prefix, "_cumulative.csv"))
  path_ablation <- file.path(dir, paste0(prefix, "_ablation.csv"))
  path_tuning <- file.path(dir, paste0(prefix, "_tuning.csv"))
  path_baseline_full <- file.path(dir, paste0(prefix, "_baseline_vs_full.csv"))
  path_baseline_full_ci <- file.path(dir, paste0(prefix, "_baseline_vs_full_ci.csv"))
  path_tables_rds <- file.path(dir, paste0(prefix, "_tables.rds"))

  paths <- list(
    summary_csv = path_summary,
    tables_rds = path_tables_rds
  )
  if (!is.null(details)) {
    paths$details_csv <- file.path(dir, paste0(prefix, "_details.csv"))
    utils::write.csv(details, paths$details_csv, row.names = FALSE)
  }

  utils::write.csv(summary, path_summary, row.names = FALSE)

  .benchmark_write_optional_csv(
    cumulative,
    path_cumulative,
    write = any(summary$config_group %in% c("reference", "cumulative"))
  )
  if (file.exists(path_cumulative)) paths$cumulative_csv <- path_cumulative

  .benchmark_write_optional_csv(
    ablation,
    path_ablation,
    write = any(summary$config_group == "ablation")
  )
  if (file.exists(path_ablation)) paths$ablation_csv <- path_ablation

  .benchmark_write_optional_csv(
    tuning,
    path_tuning,
    write = any(summary$config_group == "tuning")
  )
  if (file.exists(path_tuning)) paths$tuning_csv <- path_tuning

  .benchmark_write_optional_csv(
    baseline_full,
    path_baseline_full,
    write = nrow(baseline_full) > 0L
  )
  if (file.exists(path_baseline_full)) paths$baseline_full_csv <- path_baseline_full

  .benchmark_write_optional_csv(
    baseline_full_ci,
    path_baseline_full_ci,
    write = nrow(baseline_full_ci) > 0L
  )
  if (file.exists(path_baseline_full_ci)) paths$baseline_full_ci_csv <- path_baseline_full_ci

  saveRDS(
    list(
      summary = summary,
      details = details,
      cumulative = cumulative,
      ablation = ablation,
      tuning = tuning,
      baseline_full = baseline_full,
      baseline_full_ci = baseline_full_ci
    ),
    path_tables_rds
  )

  invisible(paths)
}

#' Plot a benchmark metric across scenarios and configurations
#'
#' Creates a grouped `ggplot2` bar plot from the study summary for a single
#' metric.
#'
#' @param study A `paprika_benchmark_study` object or compatible summary/details
#'   data frame.
#' @param metric Metric column to plot, e.g. `"top1_acc"` or
#'   `"n_questions_mean"`.
#' @param config_groups Optional character vector restricting which config
#'   groups to show.
#' @param scenario_keys Optional character vector restricting which scenario
#'   keys to show.
#' @param main Optional plot title.
#' @param ylab Optional y-axis label. Defaults to a readable label derived from
#'   `metric`.
#' @param show_legend Logical. Draw a legend?
#'
#' @return Invisibly returns the `ggplot2` object.
#' @export
plot_benchmark_metric <- function(study,
                                  metric = "top1_acc",
                                  config_groups = NULL,
                                  scenario_keys = NULL,
                                  main = NULL,
                                  ylab = NULL,
                                  show_legend = TRUE) {
  summary <- .benchmark_summary_ordered(study)
  df <- summary
  if (!is.null(config_groups)) {
    df <- df[df$config_group %in% config_groups, , drop = FALSE]
  }
  if (!is.null(scenario_keys)) {
    df <- df[df$scenario_key %in% scenario_keys, , drop = FALSE]
  }
  if (!nrow(df)) {
    p <- .benchmark_empty_plot("No benchmark data available for this selection")
    print(p)
    return(invisible(p))
  }
  if (!metric %in% names(df)) {
    stop(sprintf("Metric '%s' not found in benchmark summary.", metric))
  }

  plot_df <- data.frame(
    scenario_key = as.character(df$scenario_key),
    config = as.character(df$config),
    value = as.numeric(df[[metric]]),
    stringsAsFactors = FALSE
  )

  scenario_levels <- unique(plot_df$scenario_key)
  config_levels <- unique(plot_df$config)
  plot_df$scenario_key <- factor(plot_df$scenario_key, levels = scenario_levels)
  plot_df$config <- factor(plot_df$config, levels = config_levels)

  if (is.null(ylab)) ylab <- .benchmark_metric_label(metric)
  if (is.null(main)) main <- ylab

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = scenario_key, y = value, fill = config)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.78),
      width = 0.72,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = .benchmark_config_palette(config_levels),
      breaks = config_levels,
      labels = .benchmark_config_label(config_levels),
      name = "Configuration"
    ) +
    ggplot2::labs(
      title = main,
      x = NULL,
      y = ylab
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  if (.benchmark_metric_unit_interval(metric)) {
    ymax <- if (any(is.finite(plot_df$value))) max(plot_df$value, na.rm = TRUE) else 0
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(0, max(1, ymax)),
        breaks = seq(0, 1, by = 0.25),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      )
  } else {
    p <- p +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05))
      )
  }

  print(p)
  invisible(p)
}

#' Plot a four-panel overview of benchmark metrics
#'
#' @param study A `paprika_benchmark_study` object or compatible summary/details
#'   data frame.
#' @param metrics Character vector of metrics to show. Defaults to a balanced
#'   set of accuracy, effort, fairness, and calibration.
#' @param config_groups Optional character vector restricting which config
#'   groups to show.
#' @param scenario_keys Optional character vector restricting which scenario
#'   keys to show.
#' @param main Optional overall title.
#'
#' @return Invisibly returns the `ggplot2` object.
#' @export
plot_benchmark_overview <- function(study,
                                    metrics = c("top1_acc", "n_questions_mean", "pair_coverage", "brier_winner"),
                                    config_groups = NULL,
                                    scenario_keys = NULL,
                                    main = "Algorithm Benchmark Overview") {
  summary <- .benchmark_summary_ordered(study)
  df <- summary
  if (!is.null(config_groups)) {
    df <- df[df$config_group %in% config_groups, , drop = FALSE]
  }
  if (!is.null(scenario_keys)) {
    df <- df[df$scenario_key %in% scenario_keys, , drop = FALSE]
  }

  metrics <- unique(as.character(metrics))
  metrics <- metrics[metrics %in% names(df)]
  if (!length(metrics) || !nrow(df)) {
    p <- .benchmark_empty_plot("No benchmark data available for this selection")
    print(p)
    return(invisible(p))
  }

  long_df <- do.call(rbind, lapply(metrics, function(metric) {
    data.frame(
      scenario_key = as.character(df$scenario_key),
      config = as.character(df$config),
      metric = metric,
      metric_label = .benchmark_metric_label(metric),
      value = as.numeric(df[[metric]]),
      stringsAsFactors = FALSE
    )
  }))

  scenario_levels <- unique(df$scenario_key)
  config_levels <- unique(df$config)
  metric_levels <- .benchmark_metric_label(metrics)
  nc <- ceiling(length(metrics) / ceiling(sqrt(length(metrics))))

  long_df$scenario_key <- factor(long_df$scenario_key, levels = scenario_levels)
  long_df$config <- factor(long_df$config, levels = config_levels)
  long_df$metric_label <- factor(long_df$metric_label, levels = metric_levels)

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = scenario_key, y = value, fill = config)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.78),
      width = 0.72,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~metric_label, scales = "free_y", ncol = nc) +
    ggplot2::scale_fill_manual(
      values = .benchmark_config_palette(config_levels),
      breaks = config_levels,
      labels = .benchmark_config_label(config_levels),
      name = "Configuration"
    ) +
    ggplot2::labs(
      title = main,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

  print(p)
  invisible(p)
}

#' Export benchmark figures for manuscripts
#'
#' Writes a small, reproducible set of manuscript-ready figures from a benchmark
#' study using `ggplot2`.
#'
#' @param study A `paprika_benchmark_study` object or compatible summary/details
#'   data frame.
#' @param dir Output directory.
#' @param prefix File prefix for exported figures.
#' @param devices Graphics devices to use. Any subset of `"pdf"` and `"png"`.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#'
#' @return Invisibly returns a named list of written paths.
#' @export
benchmark_export_plots <- function(study,
                                   dir,
                                   prefix = "algorithm_paper_benchmark",
                                   devices = c("pdf", "png"),
                                   width = 11,
                                   height = 8) {
  dir <- .benchmark_ensure_dir(dir)
  devices <- unique(as.character(devices))
  bad <- setdiff(devices, c("pdf", "png"))
  if (length(bad)) stop("Unsupported devices: ", paste(bad, collapse = ", "))

  summary <- .benchmark_summary_ordered(study)
  has_reference_or_cumulative <- any(summary$config_group %in% c("reference", "cumulative"))
  has_ablation <- any(summary$config_group == "ablation")
  has_tuning <- any(summary$config_group == "tuning")

  figures <- list(
    overview = function() {
      plot_benchmark_overview(study, main = "Benchmark Overview")
    },
    uncertainty_quality = function() {
      plot_benchmark_overview(
        study,
        metrics = c("brier_winner", "calibration_ece", "winner_prob_true", "top1_confidence"),
        config_groups = c("reference", "cumulative", "full", "tuning", "ablation"),
        main = "Uncertainty and Calibration"
      )
    }
  )
  if (has_reference_or_cumulative) {
    figures$cumulative_accuracy <- function() {
      plot_benchmark_metric(
        study,
        metric = "top1_acc",
        config_groups = c("reference", "cumulative", "full"),
        main = "Top-1 Accuracy: Baseline and Cumulative Extensions"
      )
    }
    figures$cumulative_efficiency <- function() {
      plot_benchmark_metric(
        study,
        metric = "n_questions_mean",
        config_groups = c("reference", "cumulative", "full"),
        main = "Question Burden: Baseline and Cumulative Extensions"
      )
    }
    figures$fairness_profile <- function() {
      plot_benchmark_overview(
        study,
        metrics = c("pair_coverage", "exposure_gap", "exposure_gini", "interaction_share"),
        config_groups = c("reference", "cumulative", "full"),
        main = "Fairness and Interaction Diagnostics"
      )
    }
  }
  if (has_ablation) {
    figures$ablation_accuracy <- function() {
      plot_benchmark_metric(
        study,
        metric = "top1_acc",
        config_groups = c("full", "ablation"),
        main = "Top-1 Accuracy: Full Model and Ablations"
      )
    }
  }
  if (has_tuning) {
    figures$tuning_accuracy <- function() {
      plot_benchmark_metric(
        study,
        metric = "top1_acc",
        config_groups = c("full", "tuning"),
        main = "Top-1 Accuracy: Full Model and Tuning Variants"
      )
    }
  }

  paths <- list()
  for (fig_name in names(figures)) {
    for (dev in devices) {
      path <- file.path(dir, paste0(prefix, "_", fig_name, ".", dev))
      .benchmark_with_device(
        path = path,
        device = dev,
        width = width,
        height = height,
        code = figures[[fig_name]]
      )
      paths[[paste(fig_name, dev, sep = "_")]] <- path
    }
  }

  omitted <- setdiff(
    c("cumulative_accuracy", "cumulative_efficiency", "fairness_profile", "ablation_accuracy", "tuning_accuracy"),
    names(figures)
  )
  for (fig_name in omitted) {
    for (dev in devices) {
      path <- file.path(dir, paste0(prefix, "_", fig_name, ".", dev))
      if (file.exists(path)) unlink(path)
    }
  }

  invisible(paths)
}

#' Export a complete manuscript bundle for a benchmark study
#'
#' Writes tables, figures, and the full study object to disk.
#'
#' @param study A `paprika_benchmark_study` object.
#' @param dir Output directory.
#' @param prefix File prefix.
#' @param devices Graphics devices to use for figure export.
#'
#' @return Invisibly returns a named list of written paths.
#' @export
benchmark_export_bundle <- function(study,
                                    dir,
                                    prefix = "algorithm_paper_benchmark",
                                    devices = c("pdf", "png")) {
  dir <- .benchmark_ensure_dir(dir)
  exports <- list(
    tables = benchmark_export_tables(study, dir = dir, prefix = prefix),
    plots = benchmark_export_plots(study, dir = dir, prefix = prefix, devices = devices)
  )
  exports$study_rds <- file.path(dir, paste0(prefix, "_study.rds"))
  saveRDS(study, exports$study_rds)
  invisible(exports)
}

#' Compare two benchmark configurations with paired bootstrap confidence intervals
#'
#' Computes paired differences between two configurations on matched
#' scenario/patient runs and reports bootstrap confidence intervals for the
#' selected metrics.
#'
#' @param study A `paprika_benchmark_study` object or compatible details data
#'   frame.
#' @param config_a Reference configuration name.
#' @param config_b Comparison configuration name.
#' @param metrics Character vector of metrics to compare.
#' @param by Either `"scenario_key"` or `"overall"`.
#' @param level Confidence level for percentile intervals.
#' @param B Number of bootstrap replicates.
#' @param seed Integer seed for reproducibility.
#' @param calibration_bins Number of bins for ECE calculations.
#'
#' @return A data frame with observed means, paired deltas, and confidence
#'   intervals.
#' @export
benchmark_compare_configs_ci <- function(study,
                                         config_a = "reference_paprika",
                                         config_b = "full",
                                         metrics = c(
                                           "top1_acc", "top3_recovery", "n_questions_mean",
                                           "regret_mean", "pair_coverage", "feasible_rate",
                                           "brier_winner", "calibration_ece"
                                         ),
                                         by = c("scenario_key", "overall"),
                                         level = 0.95,
                                         B = 1000L,
                                         seed = 1L,
                                         calibration_bins = 10L) {
  by <- match.arg(by)
  level <- as.numeric(level)
  if (!is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be strictly between 0 and 1.")
  }
  B <- max(1L, as.integer(B))
  calibration_bins <- max(2L, as.integer(calibration_bins))

  df <- .benchmark_prepare_paired_details(study, config_a = config_a, config_b = config_b)
  if (!nrow(df)) {
    return(data.frame())
  }
  metrics <- unique(as.character(metrics))

  groups <- if (identical(by, "overall")) {
    list(overall = df)
  } else {
    split(df, df$scenario_key, drop = TRUE)
  }

  alpha <- (1 - level) / 2
  probs <- c(alpha, 1 - alpha)
  set.seed(seed)
  rows <- list()

  for (grp_name in names(groups)) {
    gdf <- groups[[grp_name]]
    if (!nrow(gdf)) next

    for (metric in metrics) {
      obs <- .benchmark_pair_metric_stats(gdf, metric = metric, calibration_bins = calibration_bins)
      if (is.null(obs)) next

      boot_delta <- numeric(B)
      strata <- split(seq_len(nrow(gdf)), gdf$scenario_key, drop = TRUE)
      for (b in seq_len(B)) {
        idx <- if (identical(by, "overall")) {
          unlist(lapply(strata, function(ii) sample(ii, length(ii), replace = TRUE)), use.names = FALSE)
        } else {
          sample.int(nrow(gdf), nrow(gdf), replace = TRUE)
        }
        boot_delta[b] <- .benchmark_pair_metric_stats(gdf[idx, , drop = FALSE], metric = metric, calibration_bins = calibration_bins)$delta
      }

      ci <- stats::quantile(boot_delta, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
      rows[[length(rows) + 1L]] <- data.frame(
        group = grp_name,
        scenario_id = gdf$scenario_id[1],
        scenario_key = if (identical(by, "overall")) "overall" else gdf$scenario_key[1],
        scenario_label = if (identical(by, "overall")) "Overall" else gdf$scenario_label[1],
        metric = metric,
        config_a = config_a,
        config_b = config_b,
        n_pairs = nrow(gdf),
        mean_a = obs$mean_a,
        mean_b = obs$mean_b,
        delta = obs$delta,
        ci_lo = ci[1],
        ci_hi = ci[2],
        level = level,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Build a paper-ready calibration report
#'
#' Summarizes held-out calibration performance and reliability-curve data for a
#' fitted `paprika_uncertainty_calibration` object.
#'
#' @param calibration A `paprika_uncertainty_calibration` object.
#' @param split Which split to report. Defaults to `"test"`.
#' @param bins Number of reliability bins.
#'
#' @return A list with `summary` and `reliability` data frames.
#' @export
benchmark_calibration_report <- function(calibration,
                                         split = "test",
                                         bins = 10L) {
  stopifnot(inherits(calibration, "paprika_uncertainty_calibration"))
  bins <- max(2L, as.integer(bins))
  split <- as.character(split)[1]
  if (!split %in% c("calibration", "test", "all")) {
    stop("split must be one of: calibration, test, all")
  }

  summary <- benchmark_evaluate_uncertainty_calibration(calibration, split = split)
  reliability <- rbind(
    .benchmark_calibration_reliability(calibration, target = "winner", split = split, bins = bins),
    .benchmark_calibration_reliability(calibration, target = "top3", split = split, bins = bins)
  )

  list(summary = summary, reliability = reliability)
}

#' Plot raw versus calibrated reliability curves
#'
#' @param calibration A `paprika_uncertainty_calibration` object.
#' @param target Either `"winner"` or `"top3"`.
#' @param split Which split to plot.
#' @param bins Number of reliability bins.
#' @param main Optional plot title.
#'
#' @return Invisibly returns the plotted `ggplot2` object.
#' @export
plot_benchmark_calibration <- function(calibration,
                                       target = c("winner", "top3"),
                                       split = "test",
                                       bins = 10L,
                                       main = NULL) {
  target <- match.arg(target)
  rep <- benchmark_calibration_report(calibration, split = split, bins = bins)
  df <- rep$reliability
  df <- df[df$target == target & is.finite(df$mean_confidence) & is.finite(df$empirical_rate), , drop = FALSE]

  if (!nrow(df)) {
    p <- .benchmark_empty_plot("No calibration data available")
    print(p)
    return(invisible(p))
  }

  if (is.null(main)) {
    main <- sprintf("%s reliability (%s split)", .benchmark_or(c(winner = "Winner", top3 = "Top-3")[[target]], target), split)
  }

  df$stage <- factor(df$stage, levels = c("raw", "calibrated"))
  stage_labels <- c(raw = "Uncalibrated", calibrated = "Beta-calibrated")

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = mean_confidence,
      y = empirical_rate,
      colour = stage,
      shape = stage,
      group = stage
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "gray70", linetype = 2) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c(raw = "#b22222", calibrated = "#1f4e79"),
      labels = stage_labels,
      name = "Support score"
    ) +
    ggplot2::scale_shape_manual(
      values = c(raw = 16, calibrated = 17),
      labels = stage_labels,
      name = "Support score"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(
      title = main,
      x = "Predicted probability",
      y = "Empirical frequency"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )

  print(p)
  invisible(p)
}

#' Export calibration tables and reliability plots
#'
#' @param calibration A `paprika_uncertainty_calibration` object.
#' @param dir Output directory.
#' @param prefix File prefix.
#' @param devices Graphics devices to use. Any subset of `"pdf"` and `"png"`.
#' @param bins Number of reliability bins.
#'
#' @return Invisibly returns written paths.
#' @export
benchmark_export_calibration_report <- function(calibration,
                                                dir,
                                                prefix = "algorithm_calibration",
                                                devices = c("pdf", "png"),
                                                bins = 10L) {
  dir <- .benchmark_ensure_dir(dir)
  report <- benchmark_calibration_report(calibration, split = "test", bins = bins)

  path_summary <- file.path(dir, paste0(prefix, "_summary.csv"))
  path_reliability <- file.path(dir, paste0(prefix, "_reliability.csv"))
  utils::write.csv(report$summary, path_summary, row.names = FALSE)
  utils::write.csv(report$reliability, path_reliability, row.names = FALSE)

  paths <- list(summary_csv = path_summary, reliability_csv = path_reliability)
  for (target in c("winner", "top3")) {
    for (dev in unique(as.character(devices))) {
      path <- file.path(dir, paste0(prefix, "_", target, "_reliability.", dev))
      .benchmark_with_device(
        path = path,
        device = dev,
        width = 7,
        height = 6,
        code = function() {
          plot_benchmark_calibration(calibration, target = target, split = "test", bins = bins)
        }
      )
      paths[[paste(target, dev, sep = "_")]] <- path
    }
  }

  invisible(paths)
}

#' @keywords internal
.benchmark_ensure_dir <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
.benchmark_prepare_paired_details <- function(study, config_a, config_b) {
  details <- .benchmark_extract_details(study)
  a <- details[details$config == config_a, , drop = FALSE]
  b <- details[details$config == config_b, , drop = FALSE]
  if (!nrow(a) || !nrow(b)) return(data.frame())

  a <- a[order(a$scenario_key, a$patient_id), , drop = FALSE]
  b <- b[order(b$scenario_key, b$patient_id), , drop = FALSE]
  a$pair_index <- ave(a$patient_id, a$scenario_key, FUN = seq_along)
  b$pair_index <- ave(b$patient_id, b$scenario_key, FUN = seq_along)

  keep <- c(
    "scenario_id", "scenario_key", "scenario_label", "pair_index", "patient_id",
    "n_questions", "top1_correct", "top3_recovery", "regret", "regret_normalized",
    "feasible", "pair_coverage", "exposure_gap", "exposure_gini",
    "brier_winner", "top1_confidence", "winner_prob_true", "elapsed_sec"
  )
  a <- a[, keep, drop = FALSE]
  b <- b[, keep, drop = FALSE]
  names(a)[!(names(a) %in% c("scenario_id", "scenario_key", "scenario_label", "pair_index"))] <-
    paste0(names(a)[!(names(a) %in% c("scenario_id", "scenario_key", "scenario_label", "pair_index"))], "_a")
  names(b)[!(names(b) %in% c("scenario_id", "scenario_key", "scenario_label", "pair_index"))] <-
    paste0(names(b)[!(names(b) %in% c("scenario_id", "scenario_key", "scenario_label", "pair_index"))], "_b")
  merge(a, b, by = c("scenario_id", "scenario_key", "scenario_label", "pair_index"), all = FALSE)
}

#' @keywords internal
.benchmark_extract_details <- function(study) {
  if (is.list(study) && !is.null(study$details)) return(study$details)
  if (is.data.frame(study)) return(study)
  stop("study must be a paprika_benchmark_study object or a details data frame.")
}

#' @keywords internal
.benchmark_pair_metric_stats <- function(df, metric, calibration_bins = 10L) {
  map <- c(
    top1_acc = "top1_correct",
    top3_recovery = "top3_recovery",
    n_questions_mean = "n_questions",
    regret_mean = "regret",
    regret_norm_mean = "regret_normalized",
    feasible_rate = "feasible",
    pair_coverage = "pair_coverage",
    exposure_gap = "exposure_gap",
    exposure_gini = "exposure_gini",
    brier_winner = "brier_winner",
    top1_confidence = "top1_confidence",
    winner_prob_true = "winner_prob_true",
    elapsed_sec = "elapsed_sec"
  )

  if (metric %in% names(map)) {
    nm <- map[[metric]]
    a <- mean(df[[paste0(nm, "_a")]], na.rm = TRUE)
    b <- mean(df[[paste0(nm, "_b")]], na.rm = TRUE)
    return(list(mean_a = a, mean_b = b, delta = b - a))
  }
  if (identical(metric, "calibration_ece")) {
    a <- .benchmark_ece(df$top1_confidence_a, df$top1_correct_a, bins = calibration_bins)
    b <- .benchmark_ece(df$top1_confidence_b, df$top1_correct_b, bins = calibration_bins)
    return(list(mean_a = a, mean_b = b, delta = b - a))
  }
  NULL
}

#' @keywords internal
.benchmark_calibration_reliability <- function(calibration, target = c("winner", "top3"), split = "test", bins = 10L) {
  target <- match.arg(target)
  data <- calibration$data
  idx <- switch(split,
    calibration = calibration$split$calibration,
    test = calibration$split$test,
    all = seq_len(nrow(data))
  )
  if (!length(idx)) return(data.frame())

  if (identical(target, "winner")) {
    raw_prob <- lapply(data$winner_probabilities[idx], function(p) as.numeric(p))
    cal_prob <- .benchmark_apply_setwise_beta(raw_prob, calibration$winner)
    raw_conf <- vapply(raw_prob, max, numeric(1), na.rm = TRUE)
    cal_conf <- vapply(cal_prob, max, numeric(1), na.rm = TRUE)
    raw_pred <- vapply(raw_prob, which.max, integer(1))
    cal_pred <- vapply(cal_prob, which.max, integer(1))
    truth <- data$true_top1[idx]
    raw_event <- as.numeric(raw_pred == truth)
    cal_event <- as.numeric(cal_pred == truth)
  } else {
    raw_prob <- unlist(data$top3_probabilities[idx], use.names = FALSE)
    cal_prob <- unlist(.benchmark_apply_binary_beta(data$top3_probabilities[idx], calibration$top3), use.names = FALSE)
    truth <- unlist(data$true_top3_mask[idx], use.names = FALSE)
    raw_conf <- raw_prob
    cal_conf <- cal_prob
    raw_event <- truth
    cal_event <- truth
  }

  rbind(
    .benchmark_reliability_bins(raw_conf, raw_event, bins = bins, target = target, split = split, stage = "raw"),
    .benchmark_reliability_bins(cal_conf, cal_event, bins = bins, target = target, split = split, stage = "calibrated")
  )
}

#' @keywords internal
.benchmark_reliability_bins <- function(conf, event, bins, target, split, stage) {
  ok <- is.finite(conf) & is.finite(event)
  conf <- pmin(pmax(as.numeric(conf[ok]), 0), 1)
  event <- as.numeric(event[ok])
  if (!length(conf)) return(data.frame())

  breaks <- seq(0, 1, length.out = bins + 1L)
  bin <- cut(conf, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  out <- lapply(seq_len(bins), function(i) {
    idx <- which(bin == i)
    if (!length(idx)) return(NULL)
    data.frame(
      target = target,
      split = split,
      stage = stage,
      bin = i,
      n = length(idx),
      mean_confidence = mean(conf[idx], na.rm = TRUE),
      empirical_rate = mean(event[idx], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' @keywords internal
.benchmark_summary_ordered <- function(study) {
  if (is.list(study) && !is.null(study$summary)) {
    summary <- study$summary
    configs <- study$configs
  } else if (is.data.frame(study)) {
    if (all(c("scenario_key", "config", "top1_acc") %in% names(study))) {
      summary <- study
    } else {
      summary <- benchmark_simulation_summary(study)
    }
    configs <- NULL
  } else {
    stop("study must be a paprika_benchmark_study object or a data frame.")
  }

  group_rank <- c(reference = 1, cumulative = 2, full = 3, tuning = 4, ablation = 5)
  config_rank <- rep(Inf, nrow(summary))
  if (!is.null(configs)) {
    cfg_names <- names(configs)
    cfg_order <- vapply(configs, function(x) .benchmark_or(x$order, Inf), numeric(1))
    idx <- match(summary$config, cfg_names)
    has <- !is.na(idx)
    config_rank[has] <- cfg_order[idx[has]]
  }

  scen_num <- suppressWarnings(as.integer(sub("^S", "", summary$scenario_id)))
  grp_num <- group_rank[summary$config_group]
  grp_num[is.na(grp_num)] <- Inf

  ord <- order(scen_num, summary$scenario_key, grp_num, config_rank, summary$config)
  out <- summary[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @keywords internal
.benchmark_baseline_full_table <- function(summary) {
  baseline <- summary[summary$config == "reference_paprika", , drop = FALSE]
  full <- summary[summary$config == "full", , drop = FALSE]
  if (!nrow(full)) {
    full <- summary[summary$config == "c7_interactions", , drop = FALSE]
  }
  if (!nrow(baseline) || !nrow(full)) {
    return(data.frame())
  }

  keep <- c(
    "scenario_id", "scenario_key", "scenario_label",
    "n_questions_mean", "top1_acc", "top3_recovery", "regret_mean",
    "pair_coverage", "exposure_gap", "exposure_gini",
    "brier_winner", "calibration_ece"
  )
  baseline <- baseline[, keep, drop = FALSE]
  full <- full[, keep, drop = FALSE]

  names(baseline)[4:ncol(baseline)] <- paste0("baseline_", names(baseline)[4:ncol(baseline)])
  names(full)[4:ncol(full)] <- paste0("full_", names(full)[4:ncol(full)])
  out <- merge(baseline, full, by = c("scenario_id", "scenario_key", "scenario_label"), all = FALSE)

  delta_pairs <- c(
    "n_questions_mean", "top1_acc", "top3_recovery", "regret_mean",
    "pair_coverage", "exposure_gap", "exposure_gini", "brier_winner", "calibration_ece"
  )
  for (nm in delta_pairs) {
    out[[paste0("delta_", nm)]] <- out[[paste0("full_", nm)]] - out[[paste0("baseline_", nm)]]
  }

  out[order(out$scenario_id, out$scenario_key), , drop = FALSE]
}

#' @keywords internal
.benchmark_write_optional_csv <- function(df, path, write = TRUE) {
  if (isTRUE(write) && !is.null(df) && nrow(df) > 0L) {
    utils::write.csv(df, path, row.names = FALSE)
  } else if (file.exists(path)) {
    unlink(path)
  }
  invisible(path)
}

#' @keywords internal
.benchmark_empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = 5) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void()
}

#' @keywords internal
.benchmark_config_label <- function(config) {
  config <- as.character(config)
  map <- c(
    reference_paprika = "PAPRIKA reconstruction",
    c1_polytope = " + feasible-set sampling",
    c2_eig = " + information gain",
    c3_fairness = " + coverage-aware selection",
    c4_adaptive_stop = " + adaptive stopping",
    c5_uncertainty_outputs = " + uncertainty outputs",
    c6_robust = " + contradiction recovery",
    c7_interactions = " + interactions",
    full = "fairpaprika",
    full_minus_polytope = "Without feasible-set sampling",
    full_minus_eig = "Without information gain",
    full_minus_fairness = "Without coverage-aware selection",
    full_minus_adaptive_stop = "Without adaptive stopping",
    full_minus_uncertainty_outputs = "Without uncertainty outputs",
    full_minus_robust = "Without contradiction recovery",
    full_minus_interactions = "Without interactions",
    t1_conservative_stop = "Conservative stopping",
    t2_top3 = "Top-3 targeting",
    t3_annealed_fairness = "Annealed coverage term",
    d1_stop_probe = "Stop-rule diagnostic"
  )
  out <- unname(map[config])
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- config[missing]
  out
}

#' @keywords internal
.benchmark_metric_label <- function(metric) {
  metric <- as.character(metric)
  labels <- c(
    top1_acc = "Top-1 Accuracy",
    top3_recovery = "Top-3 Recovery",
    n_questions_mean = "Mean Questions",
    regret_mean = "Mean Regret",
    regret_norm_mean = "Normalized Regret",
    pair_coverage = "Pair Coverage",
    exposure_gap = "Exposure Gap",
    exposure_gini = "Exposure Gini",
    interaction_share = "Interaction Share",
    brier_winner = "Winner Brier Score",
    calibration_ece = "Calibration ECE",
    winner_prob_true = "True Winner Probability",
    top1_confidence = "Top-1 Confidence",
    feasible_rate = "Feasibility Rate",
    slack_rate = "Slack Rate",
    elapsed_sec = "Elapsed Seconds"
  )
  out <- unname(labels[metric])
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- metric[missing]
  out
}

#' @keywords internal
.benchmark_metric_unit_interval <- function(metric) {
  metric %in% c(
    "top1_acc", "top3_recovery", "regret_norm_mean", "pair_coverage",
    "exposure_gap", "exposure_gini", "interaction_share", "brier_winner",
    "calibration_ece", "winner_prob_true", "top1_confidence", "feasible_rate",
    "slack_rate"
  )
}

#' @keywords internal
.benchmark_palette <- function(n) {
  base_cols <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
    "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
    "#bcbd22", "#17becf"
  )
  if (n <= length(base_cols)) return(base_cols[seq_len(n)])
  rep_len(base_cols, n)
}

#' @keywords internal
.benchmark_config_palette <- function(config) {
  config <- as.character(config)
  map <- c(
    reference_paprika = "#4E79A7",
    c1_polytope = "#9C755F",
    c2_eig = "#59A14F",
    c3_fairness = "#E15759",
    c4_adaptive_stop = "#B07AA1",
    c5_uncertainty_outputs = "#76B7B2",
    c6_robust = "#F28E2B",
    c7_interactions = "#EDC948",
    full = "#F28E2B",
    full_minus_polytope = "#9C755F",
    full_minus_eig = "#59A14F",
    full_minus_fairness = "#E15759",
    full_minus_adaptive_stop = "#B07AA1",
    full_minus_uncertainty_outputs = "#76B7B2",
    full_minus_robust = "#D62728",
    full_minus_interactions = "#8C564B",
    t1_conservative_stop = "#499894",
    t2_top3 = "#86BCB6",
    t3_annealed_fairness = "#E15759",
    d1_stop_probe = "#7F7F7F"
  )
  out <- unname(map[config])
  missing <- is.na(out) | !nzchar(out)
  if (any(missing)) {
    out[missing] <- .benchmark_palette(sum(missing))
  }
  stats::setNames(out, config)
}

#' @keywords internal
.benchmark_with_device <- function(path, device = c("pdf", "png"), width = 11, height = 8, code) {
  device <- match.arg(device)
  if (device == "pdf") {
    grDevices::pdf(path, width = width, height = height, onefile = FALSE)
  } else {
    grDevices::png(path, width = width, height = height, units = "in", res = 160)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  code()
  invisible(path)
}

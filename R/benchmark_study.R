#' Paper-ready benchmark configurations for algorithm studies
#'
#' Builds a reference PAPRIKA baseline plus cumulative and ablation-style
#' configurations for the seven-engine-extension comparison used in simulation
#' studies.
#'
#' @param domains A named list of ordered criterion levels.
#' @param max_q Integer question budget used as default for generated configs.
#' @param interaction_pairs Optional list of criterion pairs. If `NULL`, one
#'   default pair is chosen from `domains`.
#' @param n_samples Number of polytope samples for full-mode configs.
#' @param burnin Burn-in for polytope sampling.
#' @param thin Thinning for polytope sampling.
#' @param top_k Top-k randomization parameter for question selection.
#' @param include Character vector of config groups to include. Any subset of
#'   `"reference"`, `"cumulative"`, `"ablation"`, `"tuning"`,
#'   `"diagnostic"`.
#'
#' @return A named list of configuration descriptors.
#' @export
benchmark_paper_configs <- function(domains,
                                    max_q = 16L,
                                    interaction_pairs = NULL,
                                    n_samples = 200L,
                                    burnin = 50L,
                                    thin = 1L,
                                    top_k = 1L,
                                    include = c("reference", "cumulative", "ablation")) {
  domains <- validate_domains(domains)
  include <- unique(include)
  pairs_default <- .benchmark_default_interaction_pairs(domains, n_pairs = 2L)
  if (is.null(interaction_pairs)) {
    interaction_pairs <- pairs_default[seq_len(min(1L, length(pairs_default)))]
  }
  n_pairs <- length(domains) * (length(domains) - 1L) / 2L

  mk <- function(name, group, extension, order_idx,
                 polytope = TRUE, eig = TRUE, fairness = TRUE,
                 adaptive_stop = TRUE, uncertainty_outputs = TRUE,
                 robust = TRUE, interactions = TRUE,
                 settings_override = NULL) {
    desc <- list(
      name = name,
      group = group,
      extension = extension,
      order = as.integer(order_idx),
      polytope = isTRUE(polytope),
      eig = isTRUE(eig),
      fairness = isTRUE(fairness),
      adaptive_stop = isTRUE(adaptive_stop),
      uncertainty_outputs = isTRUE(uncertainty_outputs),
      robust = isTRUE(robust),
      interactions = isTRUE(interactions),
      interaction_pairs = if (isTRUE(interactions)) interaction_pairs else list(),
      n_samples = as.integer(n_samples),
      burnin = as.integer(burnin),
      thin = as.integer(thin),
      top_k = as.integer(top_k),
      settings_override = settings_override %||% list()
    )
    desc$settings <- .benchmark_descriptor_settings(desc, max_q = max_q)
    desc
  }

  out <- list()

  if ("reference" %in% include) {
    out$reference_paprika <- mk(
      name = "reference_paprika",
      group = "reference",
      extension = "baseline",
      order_idx = 0L,
      polytope = FALSE,
      eig = FALSE,
      fairness = FALSE,
      adaptive_stop = FALSE,
      uncertainty_outputs = FALSE,
      robust = FALSE,
      interactions = FALSE
    )
  }

  if ("cumulative" %in% include) {
    out$c1_polytope <- mk("c1_polytope", "cumulative", "polytope", 1L,
      polytope = TRUE, eig = FALSE, fairness = FALSE,
      adaptive_stop = FALSE, uncertainty_outputs = FALSE,
      robust = FALSE, interactions = FALSE
    )
    out$c2_eig <- mk("c2_eig", "cumulative", "eig", 2L,
      polytope = TRUE, eig = TRUE, fairness = FALSE,
      adaptive_stop = FALSE, uncertainty_outputs = FALSE,
      robust = FALSE, interactions = FALSE
    )
    out$c3_fairness <- mk("c3_fairness", "cumulative", "fairness", 3L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = FALSE, uncertainty_outputs = FALSE,
      robust = FALSE, interactions = FALSE
    )
    out$c4_adaptive_stop <- mk("c4_adaptive_stop", "cumulative", "adaptive_stop", 4L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = FALSE,
      robust = FALSE, interactions = FALSE
    )
    out$c5_uncertainty_outputs <- mk("c5_uncertainty_outputs", "cumulative", "uncertainty_outputs", 5L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = FALSE, interactions = FALSE
    )
    out$c6_robust <- mk("c6_robust", "cumulative", "robust", 6L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = FALSE
    )
    out$c7_interactions <- mk("c7_interactions", "cumulative", "interactions", 7L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE
    )
    out$full <- out$c7_interactions
    out$full$name <- "full"
    out$full$group <- "full"
  }

  if ("ablation" %in% include) {
    out$full_minus_polytope <- mk("full_minus_polytope", "ablation", "polytope", 1L,
      polytope = FALSE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE
    )
    out$full_minus_eig <- mk("full_minus_eig", "ablation", "eig", 2L,
      polytope = TRUE, eig = FALSE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE
    )
    out$full_minus_fairness <- mk("full_minus_fairness", "ablation", "fairness", 3L,
      polytope = TRUE, eig = TRUE, fairness = FALSE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE
    )
    out$full_minus_adaptive_stop <- mk("full_minus_adaptive_stop", "ablation", "adaptive_stop", 4L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = FALSE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE
    )
    out$full_minus_uncertainty_outputs <- mk("full_minus_uncertainty_outputs", "ablation", "uncertainty_outputs", 5L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = FALSE,
      robust = TRUE, interactions = TRUE
    )
    out$full_minus_robust <- mk("full_minus_robust", "ablation", "robust", 6L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = FALSE, interactions = TRUE
    )
    out$full_minus_interactions <- mk("full_minus_interactions", "ablation", "interactions", 7L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = FALSE
    )
  }

  if ("tuning" %in% include) {
    conservative_min_q <- min(as.integer(max_q), max(6L, floor(as.integer(max_q) / 2L)))

    out$t1_conservative_stop <- mk("t1_conservative_stop", "tuning", "conservative_stop", 1L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE,
      settings_override = list(
        stop = list(
          min_q = conservative_min_q,
          win_prob = 0.97,
          win_conf_prob = 0.97,
          win_conf_streak = 3L,
          topk_prob = 0.9
        )
      )
    )
    out$t2_top3 <- mk("t2_top3", "tuning", "top3", 2L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE,
      settings_override = list(
        selector = list(
          eig_target = "top3",
          utility_top_k = 3L
        )
      )
    )
    out$t3_annealed_fairness <- mk("t3_annealed_fairness", "tuning", "annealed_fairness", 3L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = TRUE,
      settings_override = list(
        fair = list(
          anneal_after_coverage = TRUE,
          anneal_beta_scale = 0.25,
          anneal_disable_balance = TRUE
        )
      )
    )
  }

  if ("diagnostic" %in% include) {
    stop_probe_min_q <- min(as.integer(max_q), max(4L, as.integer(n_pairs)))
    out$d1_stop_probe <- mk("d1_stop_probe", "diagnostic", "stop_probe", 1L,
      polytope = TRUE, eig = TRUE, fairness = TRUE,
      adaptive_stop = TRUE, uncertainty_outputs = TRUE,
      robust = TRUE, interactions = FALSE,
      settings_override = list(
        full = list(
          bank_move = "all",
          bank_baseline = "mid"
        ),
        selector = list(
          pair_cap = 999L,
          candidate_pool = 9999L,
          top_k = 50L
        ),
        stop = list(
          min_q = stop_probe_min_q,
          margin_q05 = 0.35,
          topk_prob = 0.9
        )
      )
    )
  }

  out
}

#' Paper-ready simulation scenarios for the algorithm study
#'
#' Creates the default scenario collection `S0`-`S7` discussed in the companion
#' design notes: sanity check, under-determination, adaptive selection,
#' fairness stress, adaptive stopping, uncertainty outputs, inconsistency
#' robustness, and interaction scenarios.
#'
#' @param domains A named list of ordered criterion levels.
#' @param n_profiles Number of real-world profiles sampled per run.
#' @param max_q Default question budget.
#' @param interaction_pairs Optional list of criterion pairs. If `NULL`, default
#'   pairs are derived from `domains`.
#'
#' @return A named list of scenario specifications.
#' @export
benchmark_paper_scenarios <- function(domains,
                                      n_profiles = 7L,
                                      max_q = 16L,
                                      interaction_pairs = NULL) {
  domains <- validate_domains(domains)
  pairs_default <- .benchmark_default_interaction_pairs(domains, n_pairs = 2L)
  if (is.null(interaction_pairs)) interaction_pairs <- pairs_default
  q_small <- unique(pmax(2L, pmin(c(4L, 8L, 12L), as.integer(max_q))))
  q_mid <- unique(pmax(2L, pmin(c(8L, 12L, as.integer(max_q)), as.integer(max_q))))

  mk <- function(id, label, description, ...) {
    spec <- list(
      id = id,
      label = label,
      description = description,
      utility_model = "additive",
      n_profiles = as.integer(n_profiles),
      max_q_values = as.integer(max_q),
      noise_sd_values = 0,
      choice_tau_values = 0,
      flip_prob_values = 0,
      delta_equal_values = 0,
      n_interactions_values = 0L,
      interaction_strength_values = 0,
      interaction_pairs = list(),
      profile_difficulty_values = "random",
      winner_gap_easy_frac = 0.20,
      winner_gap_hard_frac = 0.05
    )
    overrides <- list(...)
    if (length(overrides)) {
      spec[names(overrides)] <- overrides
    }
    spec
  }

  list(
    S0 = mk(
      "S0",
      "Sanity Check",
      "Additive, noise-free baseline scenario.",
      max_q_values = as.integer(max_q)
    ),
    S1 = mk(
      "S1",
      "Under-determination",
      "Low-budget additive scenarios for assessing the polytope benefit.",
      max_q_values = q_small
    ),
    S2 = mk(
      "S2",
      "Adaptive Selection Efficiency",
      "Budgeted additive scenarios with mild response noise.",
      max_q_values = q_mid,
      choice_tau_values = c(0, 0.15)
    ),
    S3 = mk(
      "S3",
      "Fairness Stress Test",
      "Random additive scenarios emphasizing pair coverage and exposure balance.",
      max_q_values = as.integer(max_q)
    ),
    S4 = mk(
      "S4",
      "Adaptive Stopping",
      "Contrasts easy and hard profile sets for stopping behaviour.",
      max_q_values = as.integer(max_q),
      profile_difficulty_values = c("easy", "hard"),
      winner_gap_easy_frac = 0.25,
      winner_gap_hard_frac = 0.05
    ),
    S5 = mk(
      "S5",
      "Uncertainty Outputs",
      "Moderately noisy scenarios to assess calibration of output probabilities.",
      max_q_values = q_mid,
      choice_tau_values = 0.15,
      flip_prob_values = 0.05
    ),
    S6 = mk(
      "S6",
      "Inconsistency Robustness",
      "Scenarios with explicit answer noise and reversals.",
      max_q_values = as.integer(max_q),
      choice_tau_values = 0.15,
      flip_prob_values = c(0.05, 0.15),
      delta_equal_values = c(0, 0.02)
    ),
    S7 = mk(
      "S7",
      "Interactions",
      "Non-additive preference scenarios with active criterion interactions.",
      utility_model = "interaction",
      max_q_values = as.integer(max_q),
      n_interactions_values = c(1L, 2L),
      interaction_strength_values = c(0.15, 0.3),
      interaction_pairs = interaction_pairs,
      profile_difficulty_values = "random"
    )
  )
}

#' Run the paper simulation study
#'
#' Simulates synthetic patients with known ground truth, replays the same
#' profile set across multiple engine configurations, and computes per-run and
#' summary metrics suitable for the algorithm paper.
#'
#' @param domains A named list of ordered criterion levels.
#' @param configs Configuration descriptors as returned by
#'   [benchmark_paper_configs()].
#' @param scenarios Scenario specifications as returned by
#'   [benchmark_paper_scenarios()].
#' @param n_patients Number of simulated patients per scenario cell.
#' @param seed Integer seed for reproducibility.
#' @param progress Logical. Print coarse progress messages?
#'
#' @return A list with `summary`, `details`, `configs`, and `scenarios`.
#' @export
benchmark_simulation_study <- function(domains,
                                       configs = benchmark_paper_configs(domains),
                                       scenarios = benchmark_paper_scenarios(domains),
                                       n_patients = 100L,
                                       seed = 1L,
                                       progress = FALSE) {
  domains <- validate_domains(domains)
  stopifnot(is.list(configs), length(configs) > 0)
  stopifnot(is.list(scenarios), length(scenarios) > 0)

  n_patients <- as.integer(n_patients)
  if (is.na(n_patients) || n_patients < 1L) stop("n_patients must be >= 1.")

  expanded <- .benchmark_expand_scenarios(scenarios)
  details <- vector("list", length(expanded) * length(configs) * n_patients)
  idx <- 0L

  for (s_idx in seq_along(expanded)) {
    sc <- expanded[[s_idx]]
    if (isTRUE(progress)) {
      message(sprintf("Scenario %d/%d: %s", s_idx, length(expanded), sc$scenario_key))
    }
    for (p in seq_len(n_patients)) {
      run_seed <- seed + s_idx * 100000L + p * 100L
      truth <- .benchmark_sample_true_model(domains, sc, seed = run_seed + 1L)
      profiles <- .benchmark_prepare_profiles(
        domains,
        n_profiles = sc$n_profiles,
        seed = run_seed,
        difficulty = sc$profile_difficulty,
        truth = truth,
        scenario = sc
      )
      for (cfg_idx in seq_along(configs)) {
        cfg <- configs[[cfg_idx]]
        idx <- idx + 1L
        details[[idx]] <- .benchmark_run_single(
          domains = domains,
          config = cfg,
          scenario = sc,
          profiles = profiles,
          truth = truth,
          seed = run_seed + cfg_idx
        )
      }
    }
  }

  details <- do.call(rbind, lapply(details[seq_len(idx)], as.data.frame, stringsAsFactors = FALSE))
  rownames(details) <- NULL
  summary <- benchmark_simulation_summary(details)

  out <- list(
    summary = summary,
    details = details,
    configs = configs,
    scenarios = expanded,
    seed = seed
  )
  class(out) <- "paprika_benchmark_study"
  out
}

#' Summarize simulation-study results
#'
#' @param study Either a benchmark-study object returned by
#'   [benchmark_simulation_study()] or a per-run data frame.
#' @param calibration_bins Number of bins for expected calibration error.
#'
#' @return A data frame with one row per scenario/configuration combination.
#' @export
benchmark_simulation_summary <- function(study, calibration_bins = 10L) {
  details <- if (is.data.frame(study)) study else study$details
  stopifnot(is.data.frame(details), nrow(details) > 0)

  calibration_bins <- max(2L, as.integer(calibration_bins))
  keys <- interaction(details$scenario_key, details$config, drop = TRUE, lex.order = TRUE)
  splits <- split(details, keys)

  rows <- lapply(splits, function(df) {
    data.frame(
      scenario_id = df$scenario_id[1],
      scenario_key = df$scenario_key[1],
      scenario_label = df$scenario_label[1],
      config = df$config[1],
      config_group = df$config_group[1],
      extension = df$extension[1],
      output_mode = df$output_mode[1],
      n_runs = nrow(df),
      n_questions_mean = mean(df$n_questions, na.rm = TRUE),
      top1_acc = mean(df$top1_correct, na.rm = TRUE),
      top3_recovery = mean(df$top3_recovery, na.rm = TRUE),
      rank_spearman = mean(df$rank_spearman, na.rm = TRUE),
      regret_mean = mean(df$regret, na.rm = TRUE),
      regret_norm_mean = mean(df$regret_normalized, na.rm = TRUE),
      feasible_rate = mean(df$feasible, na.rm = TRUE),
      slack_rate = mean(df$slack_flag, na.rm = TRUE),
      pair_coverage = mean(df$pair_coverage, na.rm = TRUE),
      exposure_gap = mean(df$exposure_gap, na.rm = TRUE),
      exposure_gini = mean(df$exposure_gini, na.rm = TRUE),
      interaction_share = mean(df$interaction_share, na.rm = TRUE),
      stopped_early_rate = mean(df$stopped_early, na.rm = TRUE),
      true_winner_margin_mean = mean(df$true_winner_margin, na.rm = TRUE),
      top1_confidence = mean(df$top1_confidence, na.rm = TRUE),
      winner_prob_true = mean(df$winner_prob_true, na.rm = TRUE),
      brier_winner = mean(df$brier_winner, na.rm = TRUE),
      stop_reason_mode = .benchmark_mode_chr(df$stop_reason),
      stop_probe_reason_mode = .benchmark_mode_chr(df$stop_probe_reason),
      stop_probe_best_prob_mean = mean(df$stop_probe_best_prob, na.rm = TRUE),
      stop_probe_margin_q05_mean = mean(df$stop_probe_margin_q05, na.rm = TRUE),
      stop_probe_top3_min_prob_mean = mean(df$stop_probe_top3_min_prob, na.rm = TRUE),
      stop_probe_can_pick_rate = mean(df$stop_probe_can_pick, na.rm = TRUE),
      calibration_ece = .benchmark_ece(df$top1_confidence, df$top1_correct, bins = calibration_bins),
      elapsed_sec = mean(df$elapsed_sec, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @keywords internal
.benchmark_or <- function(x, default) {
  if (is.null(x)) default else x
}

#' @keywords internal
.benchmark_mode_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

#' @keywords internal
.benchmark_descriptor_settings <- function(desc, max_q) {
  max_q <- as.integer(max_q)
  interaction_pairs <- desc$interaction_pairs
  zero_scores <- list(alpha = 0, beta = 0, gamma = 0, delta = 0, kappa = 0, zeta = 0, eta = 0)

  if (!isTRUE(desc$polytope)) {
    return(list(
      mode = "classic",
      min_q = 1L,
      max_q = max_q,
      eps_strict = 1e-3,
      tau_equal = 1e-3,
      fair = list(
        enabled = FALSE,
        pair_coverage = FALSE,
        exposure_balance = FALSE,
        interaction_coverage = FALSE
      ),
      interactions = list(
        enabled = FALSE,
        pairs = list(),
        max_pairs = 0L,
        min_questions = 0L
      ),
      slack = list(enabled = FALSE),
      regularize_balanced = list(enabled = FALSE),
      classic = list(
        strict_paprika = TRUE,
        use_anchor_phase = FALSE,
        use_regularization = FALSE
      )
    ))
  }

  score_cfg <- zero_scores
  if (isTRUE(desc$eig)) {
    score_cfg$alpha <- 1
    score_cfg$gamma <- 2.5
    score_cfg$delta <- 0.25
    score_cfg$kappa <- 0.75
  }
  if (isTRUE(desc$fairness)) {
    score_cfg$beta <- 0.25
  }
  if (isTRUE(desc$interactions)) {
    score_cfg$zeta <- 1
    score_cfg$eta <- 0.5
  }

  settings <- list(
    mode = "full",
    min_q = if (isTRUE(desc$adaptive_stop)) 1L else max_q,
    max_q = max_q,
    eps_strict = 1e-3,
    tau_equal = 1e-3,
    selector = list(
      method = "polytope_eig",
      n_samples = desc$n_samples,
      burnin = desc$burnin,
      thin = desc$thin,
      top_k = desc$top_k,
      score = score_cfg
    ),
    fair = list(
      enabled = isTRUE(desc$fairness),
      pair_coverage = isTRUE(desc$fairness),
      exposure_balance = isTRUE(desc$fairness),
      interaction_coverage = isTRUE(desc$fairness) && isTRUE(desc$interactions)
    ),
    stop = list(
      min_q = if (isTRUE(desc$adaptive_stop)) 1L else max_q
    ),
    interactions = list(
      enabled = isTRUE(desc$interactions),
      pairs = if (isTRUE(desc$interactions)) interaction_pairs else list(),
      max_pairs = if (isTRUE(desc$interactions)) length(interaction_pairs) else 0L,
      min_questions = if (isTRUE(desc$interactions) && length(interaction_pairs)) 1L else 0L,
      activate = list(slack = isTRUE(desc$robust))
    ),
    slack = list(enabled = isTRUE(desc$robust)),
    regularize_balanced = list(enabled = isTRUE(desc$robust))
  )

  override <- desc$settings_override %||% list()
  if (length(override)) {
    settings <- utils::modifyList(settings, override)
  }

  settings
}

#' @keywords internal
.benchmark_default_interaction_pairs <- function(domains, n_pairs = 1L) {
  crit <- names(domains)
  if (length(crit) < 2L) return(list())
  comb <- utils::combn(crit, 2, simplify = FALSE)
  comb[seq_len(min(as.integer(n_pairs), length(comb)))]
}

#' @keywords internal
.benchmark_expand_scenarios <- function(scenarios) {
  out <- list()
  idx <- 0L
  varying_fields <- c(
    "max_q_values", "noise_sd_values", "choice_tau_values",
    "flip_prob_values", "delta_equal_values", "n_interactions_values",
    "interaction_strength_values", "profile_difficulty_values"
  )

  for (nm in names(scenarios)) {
    sc <- scenarios[[nm]]
    grid <- expand.grid(
      max_q = sc$max_q_values,
      noise_sd = sc$noise_sd_values,
      choice_tau = sc$choice_tau_values,
      flip_prob = sc$flip_prob_values,
      delta_equal = sc$delta_equal_values,
      n_interactions = sc$n_interactions_values,
      interaction_strength = sc$interaction_strength_values,
      profile_difficulty = sc$profile_difficulty_values,
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(grid))) {
      idx <- idx + 1L
      row <- grid[i, , drop = FALSE]
      item <- list(
        scenario_id = sc$id,
        scenario_label = sc$label,
        scenario_description = sc$description,
        utility_model = sc$utility_model,
        n_profiles = as.integer(sc$n_profiles),
        max_q = as.integer(row$max_q),
        noise_sd = as.numeric(row$noise_sd),
        choice_tau = as.numeric(row$choice_tau),
        flip_prob = as.numeric(row$flip_prob),
        delta_equal = as.numeric(row$delta_equal),
        n_interactions = as.integer(row$n_interactions),
        interaction_strength = as.numeric(row$interaction_strength),
        interaction_pairs = sc$interaction_pairs,
        profile_difficulty = as.character(row$profile_difficulty),
        winner_gap_easy_frac = as.numeric(sc$winner_gap_easy_frac %||% 0.20),
        winner_gap_hard_frac = as.numeric(sc$winner_gap_hard_frac %||% 0.05)
      )
      item$scenario_key <- .benchmark_scenario_key(item, varying_fields)
      out[[idx]] <- item
    }
  }
  out
}

#' @keywords internal
.benchmark_scenario_key <- function(item, varying_fields) {
  parts <- c(item$scenario_id)
  parts <- c(parts, paste0("q", item$max_q))
  if (isTRUE(item$noise_sd > 0)) parts <- c(parts, paste0("ns", format(item$noise_sd, trim = TRUE)))
  if (isTRUE(item$choice_tau > 0)) parts <- c(parts, paste0("tau", format(item$choice_tau, trim = TRUE)))
  if (isTRUE(item$flip_prob > 0)) parts <- c(parts, paste0("flip", format(item$flip_prob, trim = TRUE)))
  if (isTRUE(item$delta_equal > 0)) parts <- c(parts, paste0("eq", format(item$delta_equal, trim = TRUE)))
  if (isTRUE(item$n_interactions > 0)) parts <- c(parts, paste0("int", item$n_interactions))
  if (isTRUE(item$interaction_strength > 0)) parts <- c(parts, paste0("str", format(item$interaction_strength, trim = TRUE)))
  if (!identical(item$profile_difficulty, "random")) parts <- c(parts, item$profile_difficulty)
  paste(parts, collapse = "_")
}

#' @keywords internal
.benchmark_prepare_profiles <- function(domains,
                                        n_profiles = 7L,
                                        seed = NULL,
                                        difficulty = c("random", "easy", "hard"),
                                        truth = NULL,
                                        scenario = NULL) {
  difficulty <- match.arg(difficulty)
  if (!is.null(seed)) set.seed(seed)
  prof <- expand.grid(domains, stringsAsFactors = FALSE)
  n_profiles <- min(as.integer(n_profiles), nrow(prof))
  if (nrow(prof) <= n_profiles) {
    rownames(prof) <- paste0("Profile ", seq_len(nrow(prof)))
    return(prof)
  }

  if (!is.null(truth) && difficulty %in% c("easy", "hard")) {
    util_all <- apply(prof, 1, .benchmark_profile_utility, truth = truth)
    ord <- order(util_all, decreasing = TRUE)
    top_idx <- ord[1]
    util_span <- max(util_all) - min(util_all)
    util_span <- max(util_span, 1e-8)
    easy_gap <- (scenario$winner_gap_easy_frac %||% 0.20) * util_span
    hard_gap <- (scenario$winner_gap_hard_frac %||% 0.05) * util_span

    pick <- switch(difficulty,
      easy = {
        rest <- setdiff(seq_len(nrow(prof)), top_idx)
        thr <- easy_gap
        eligible <- rest[util_all[rest] <= (util_all[top_idx] - thr)]
        while (length(eligible) < (n_profiles - 1L) && thr > 0) {
          thr <- thr * 0.8
          eligible <- rest[util_all[rest] <= (util_all[top_idx] - thr)]
        }
        if (length(eligible) < (n_profiles - 1L)) {
          eligible <- tail(ord, max(n_profiles - 1L, length(rest)))
          eligible <- setdiff(eligible, top_idx)
        }
        c(top_idx, sample(eligible, n_profiles - 1L))
      },
      hard = {
        thr <- hard_gap
        close_idx <- ord[util_all[ord] >= (util_all[top_idx] - thr)]
        while (length(close_idx) < 2L && thr < util_span) {
          thr <- min(util_span, max(thr * 1.5, thr + 1e-8))
          close_idx <- ord[util_all[ord] >= (util_all[top_idx] - thr)]
        }
        close_idx <- unique(c(top_idx, setdiff(close_idx, top_idx)))
        second_pool <- setdiff(close_idx, top_idx)
        if (!length(second_pool)) {
          second_pool <- setdiff(ord[seq_len(min(length(ord), 2L))], top_idx)
        }
        second_idx <- sample(second_pool, 1L)
        remaining <- setdiff(seq_len(nrow(prof)), c(top_idx, second_idx))
        fill_n <- max(0L, n_profiles - 2L)
        fill_idx <- if (fill_n > 0L) sample(remaining, fill_n) else integer()
        c(top_idx, second_idx, fill_idx)
      }
    )
  } else {
    pick <- switch(difficulty,
      random = sample.int(nrow(prof), n_profiles),
      easy = {
        best <- vapply(domains, function(x) tail(x, 1), character(1))
        best_idx <- which(apply(prof, 1, function(x) all(as.character(x) == best)))
        rest <- setdiff(seq_len(nrow(prof)), best_idx)
        c(best_idx[1], sample(rest, n_profiles - 1L))
      },
      hard = {
        near_top <- lapply(domains, function(x) tail(x, min(2L, length(x))))
        hard_prof <- expand.grid(near_top, stringsAsFactors = FALSE)
        if (nrow(hard_prof) >= n_profiles) {
          idx_hard <- sample.int(nrow(hard_prof), n_profiles)
          out <- hard_prof[idx_hard, , drop = FALSE]
          rownames(out) <- paste0("Profile ", seq_len(nrow(out)))
          return(out)
        }
        sample.int(nrow(prof), n_profiles)
      }
    )
  }

  out <- prof[pick, , drop = FALSE]
  rownames(out) <- paste0("Profile ", seq_len(nrow(out)))
  out
}

#' @keywords internal
.benchmark_sample_true_model <- function(domains, scenario, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  crit <- names(domains)
  crit_weight_raw <- stats::rgamma(length(crit), shape = 1.5, rate = 1)
  crit_weights <- crit_weight_raw / sum(crit_weight_raw)
  names(crit_weights) <- crit

  additive <- numeric()
  for (cn in crit) {
    lv <- domains[[cn]]
    if (length(lv) == 1L) {
      vals <- crit_weights[[cn]]
    } else {
      steps <- stats::rexp(length(lv) - 1L)
      vals <- c(0, cumsum(steps))
      vals <- vals / max(vals + 1e-12) * crit_weights[[cn]]
    }
    names(vals) <- paste(cn, lv, sep = ":")
    additive <- c(additive, vals)
  }

  interactions <- list()
  if (identical(scenario$utility_model, "interaction") &&
    scenario$n_interactions > 0L &&
    length(scenario$interaction_pairs)) {
    pairs <- scenario$interaction_pairs[seq_len(min(length(scenario$interaction_pairs), scenario$n_interactions))]
    for (pair in pairs) {
      ci <- pair[1]
      cj <- pair[2]
      pos_i <- seq(0, 1, length.out = length(domains[[ci]]))
      pos_j <- seq(0, 1, length.out = length(domains[[cj]]))
      sign <- sample(c(-1, 1), 1)
      scale <- scenario$interaction_strength * mean(crit_weights[c(ci, cj)])
      mat <- sign * scale * outer(pos_i, pos_j)
      dimnames(mat) <- list(domains[[ci]], domains[[cj]])
      interactions[[paste(ci, cj, sep = "::")]] <- list(pair = c(ci, cj), values = mat)
    }
  }

  list(
    additive = additive,
    criterion_weights = crit_weights,
    interactions = interactions
  )
}

#' @keywords internal
.benchmark_profile_utility <- function(profile_row, truth) {
  if (is.data.frame(profile_row)) profile_row <- profile_row[1, , drop = TRUE]
  profile_row <- as.list(profile_row)
  keys <- paste(names(profile_row), as.character(unlist(profile_row)), sep = ":")
  util <- sum(truth$additive[keys])
  if (length(truth$interactions)) {
    for (itm in truth$interactions) {
      ci <- itm$pair[1]
      cj <- itm$pair[2]
      li <- as.character(profile_row[[ci]])
      lj <- as.character(profile_row[[cj]])
      util <- util + itm$values[li, lj]
    }
  }
  util
}

#' @keywords internal
.benchmark_choose_response <- function(ua, ub, scenario) {
  diff <- ua - ub
  if (isTRUE(scenario$noise_sd > 0)) {
    diff <- diff + stats::rnorm(1, sd = scenario$noise_sd)
  }
  thr <- .benchmark_or(scenario$delta_equal, 0)
  if (abs(diff) <= thr) {
    pref <- "E"
  } else if (isTRUE(scenario$choice_tau > 0)) {
    p_a <- 1 / (1 + exp(-diff / scenario$choice_tau))
    pref <- if (stats::runif(1) < p_a) "A" else "B"
  } else {
    pref <- if (diff > 0) "A" else "B"
  }
  if (pref != "E" && isTRUE(scenario$flip_prob > 0) && stats::runif(1) < scenario$flip_prob) {
    pref <- if (pref == "A") "B" else "A"
  }
  pref
}

#' @keywords internal
.benchmark_run_single <- function(domains, config, scenario, profiles, truth, seed) {
  settings <- .benchmark_descriptor_settings(config, max_q = scenario$max_q)
  settings$tau_equal <- max(1e-6, scenario$delta_equal)
  engine <- engine_create(domains, settings = settings, seed = seed)
  engine <- engine_set_profiles(engine, profiles)
  runtime_warnings <- character()

  elapsed <- system.time(
    withCallingHandlers(
      {
        repeat {
          nxt <- engine_next_question(engine)
          engine <- nxt$engine
          q <- nxt$question
          if (is.null(q)) {
            reason <- if (isTRUE(engine_has_more_questions(engine))) {
              "selector_no_pick"
            } else {
              "question_bank_exhausted"
            }
            if (is.na(engine$stop_state$stop_reason %||% NA_character_)) {
              .engine_set_stop_state(engine$stop_state, reason = reason, n = nrow(engine$decisions))
            }
            break
          }
          ua <- .benchmark_profile_utility(q$a, truth)
          ub <- .benchmark_profile_utility(q$b, truth)
          pref <- .benchmark_choose_response(ua, ub, scenario)
          engine <- engine_add_decision(engine, pref)
          if (engine_done(engine)) {
            if (is.na(engine$stop_state$stop_reason %||% NA_character_)) {
              reason <- if (identical(engine$phase, "done")) {
                "phase_done"
              } else if (isTRUE(engine_has_more_questions(engine))) {
                "selector_no_pick"
              } else {
                "question_bank_exhausted"
              }
              .engine_set_stop_state(engine$stop_state, reason = reason, n = nrow(engine$decisions))
            }
            break
          }
        }
      },
      warning = function(w) {
        runtime_warnings <<- c(runtime_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )[["elapsed"]]

  compute_error <- NULL
  compute_warning <- NULL
  engine <- tryCatch(
    withCallingHandlers(
      engine_compute(engine),
      warning = function(w) {
        compute_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      compute_error <<- conditionMessage(e)
      engine
    }
  )

  metrics <- .benchmark_collect_run_metrics(
    engine = engine,
    truth = truth,
    profiles = profiles,
    output_mode = if (isTRUE(config$uncertainty_outputs)) "uncertainty" else "deterministic",
    max_q = scenario$max_q
  )

  data.frame(
    scenario_id = scenario$scenario_id,
    scenario_key = scenario$scenario_key,
    scenario_label = scenario$scenario_label,
    config = config$name,
    config_group = config$group,
    extension = config$extension,
    output_mode = if (isTRUE(config$uncertainty_outputs)) "uncertainty" else "deterministic",
    patient_id = as.integer(seed),
    n_questions = metrics$n_questions,
    top1_correct = metrics$top1_correct,
    top3_recovery = metrics$top3_recovery,
    rank_spearman = metrics$rank_spearman,
    regret = metrics$regret,
    regret_normalized = metrics$regret_normalized,
    feasible = metrics$feasible,
    slack_flag = metrics$slack_flag,
    pair_coverage = metrics$pair_coverage,
    exposure_gap = metrics$exposure_gap,
    exposure_gini = metrics$exposure_gini,
    interaction_share = metrics$interaction_share,
    stopped_early = metrics$stopped_early,
    true_winner_margin = metrics$true_winner_margin,
    top1_confidence = metrics$top1_confidence,
    winner_prob_true = metrics$winner_prob_true,
    brier_winner = metrics$brier_winner,
    stop_reason = metrics$stop_reason,
    stop_best_prob = metrics$stop_best_prob,
    stop_margin_q05 = metrics$stop_margin_q05,
    stop_top3_min_prob = metrics$stop_top3_min_prob,
    stop_next_ig = metrics$stop_next_ig,
    stop_probe_reason = metrics$stop_probe_reason,
    stop_probe_best_prob = metrics$stop_probe_best_prob,
    stop_probe_margin_q05 = metrics$stop_probe_margin_q05,
    stop_probe_top3_min_prob = metrics$stop_probe_top3_min_prob,
    stop_probe_next_ig = metrics$stop_probe_next_ig,
    stop_probe_can_pick = metrics$stop_probe_can_pick,
    true_top1 = metrics$true_top1,
    estimated_top1 = metrics$estimated_top1,
    winner_probabilities = .benchmark_encode_numeric(metrics$winner_probabilities),
    top3_probabilities = .benchmark_encode_numeric(metrics$top3_probabilities),
    true_top3_mask = .benchmark_encode_numeric(metrics$true_top3_mask),
    elapsed_sec = elapsed,
    runtime_warning = if (length(runtime_warnings)) paste(unique(runtime_warnings), collapse = " | ") else NA_character_,
    compute_warning = .benchmark_or(compute_warning, NA_character_),
    compute_error = .benchmark_or(compute_error, NA_character_),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
.benchmark_collect_run_metrics <- function(engine, truth, profiles, output_mode = c("uncertainty", "deterministic"), max_q) {
  output_mode <- match.arg(output_mode)
  true_util <- apply(profiles, 1, .benchmark_profile_utility, truth = truth)
  true_rank <- rank(-true_util, ties.method = "average")
  true_ord <- order(true_util, decreasing = TRUE)
  true_top1 <- true_ord[1]
  true_top3 <- true_ord[seq_len(min(3L, length(true_ord)))]
  true_margin <- if (length(true_ord) >= 2L) true_util[true_ord[1]] - true_util[true_ord[2]] else NA_real_
  feasible <- !is.null(engine$weights)
  stop_info <- .benchmark_stop_info(engine)
  stop_probe <- .benchmark_stop_probe(engine)

  justice <- tryCatch(engine_procedural_justice(engine), error = function(e) NULL)
  pair_coverage <- .benchmark_or(justice$session$coverage, NA_real_)
  exposure_gap <- .benchmark_or(justice$session$exposure_gap, NA_real_)
  exposure_gini <- .benchmark_or(justice$session$exposure_gini, NA_real_)
  interaction_share <- .benchmark_or(justice$session$burden$interaction_share, NA_real_)

  if (!feasible) {
    return(list(
      n_questions = nrow(engine$decisions),
      top1_correct = 0,
      top3_recovery = 0,
      rank_spearman = NA_real_,
      regret = NA_real_,
      regret_normalized = NA_real_,
      feasible = FALSE,
      slack_flag = isTRUE(engine$diagnostics$slack_flag),
      pair_coverage = pair_coverage,
      exposure_gap = exposure_gap,
      exposure_gini = exposure_gini,
      interaction_share = interaction_share,
      stopped_early = nrow(engine$decisions) < max_q,
      true_winner_margin = true_margin,
      top1_confidence = NA_real_,
      winner_prob_true = NA_real_,
      brier_winner = NA_real_,
      stop_reason = stop_info$reason,
      stop_best_prob = stop_info$best_prob,
      stop_margin_q05 = stop_info$margin_q05,
      stop_top3_min_prob = stop_info$top3_min_prob,
      stop_next_ig = stop_info$next_ig,
      stop_probe_reason = stop_probe$reason,
      stop_probe_best_prob = stop_probe$best_prob,
      stop_probe_margin_q05 = stop_probe$margin_q05,
      stop_probe_top3_min_prob = stop_probe$top3_min_prob,
      stop_probe_next_ig = stop_probe$next_ig,
      stop_probe_can_pick = stop_probe$can_pick,
      true_top1 = true_top1,
      estimated_top1 = NA_integer_,
      winner_probabilities = rep(NA_real_, length(true_util)),
      top3_probabilities = rep(NA_real_, length(true_util)),
      true_top3_mask = as.numeric(seq_along(true_util) %in% true_top3)
    ))
  }

  est_score <- .benchmark_profile_score(engine, output_mode = output_mode)
  est_rank <- rank(-est_score, ties.method = "average")
  est_ord <- order(est_score, decreasing = TRUE)
  est_top1 <- est_ord[1]
  est_top3 <- est_ord[seq_len(min(3L, length(est_ord)))]

  win_prob <- .benchmark_profile_prob(engine, output_mode = output_mode)
  top3_prob <- .benchmark_profile_top3_prob(engine, output_mode = output_mode)
  top1_conf <- max(win_prob, na.rm = TRUE)
  winner_prob_true <- win_prob[true_top1]
  truth_onehot <- rep(0, length(win_prob))
  truth_onehot[true_top1] <- 1
  brier <- mean((win_prob - truth_onehot)^2, na.rm = TRUE)

  regret <- max(true_util) - true_util[est_top1]
  regret_norm <- regret / max(1e-12, max(true_util) - min(true_util))

  list(
    n_questions = nrow(engine$decisions),
    top1_correct = as.numeric(est_top1 == true_top1),
    top3_recovery = length(intersect(est_top3, true_top3)) / length(true_top3),
    rank_spearman = suppressWarnings(stats::cor(true_rank, est_rank, method = "spearman")),
    regret = regret,
    regret_normalized = regret_norm,
    feasible = TRUE,
    slack_flag = isTRUE(engine$diagnostics$slack_flag),
    pair_coverage = pair_coverage,
    exposure_gap = exposure_gap,
    exposure_gini = exposure_gini,
    interaction_share = interaction_share,
    stopped_early = nrow(engine$decisions) < max_q,
    true_winner_margin = true_margin,
    top1_confidence = top1_conf,
    winner_prob_true = winner_prob_true,
    brier_winner = brier,
    stop_reason = stop_info$reason,
    stop_best_prob = stop_info$best_prob,
    stop_margin_q05 = stop_info$margin_q05,
    stop_top3_min_prob = stop_info$top3_min_prob,
    stop_next_ig = stop_info$next_ig,
    stop_probe_reason = stop_probe$reason,
    stop_probe_best_prob = stop_probe$best_prob,
    stop_probe_margin_q05 = stop_probe$margin_q05,
    stop_probe_top3_min_prob = stop_probe$top3_min_prob,
    stop_probe_next_ig = stop_probe$next_ig,
    stop_probe_can_pick = stop_probe$can_pick,
    true_top1 = true_top1,
    estimated_top1 = est_top1,
    winner_probabilities = as.numeric(win_prob),
    top3_probabilities = as.numeric(top3_prob),
    true_top3_mask = as.numeric(seq_along(true_util) %in% true_top3)
  )
}

#' @keywords internal
.benchmark_profile_score <- function(engine, output_mode = c("uncertainty", "deterministic")) {
  output_mode <- match.arg(output_mode)
  if (output_mode == "uncertainty") {
    probs <- engine$diagnostics$winner_probabilities
    if (!is.null(probs)) return(as.numeric(probs))
  }

  if (!is.null(engine$profiles_idx_full) || !is.null(engine$profiles_idx)) {
    w <- engine$weights$Nutzen
    scores <- numeric(nrow(engine$profiles))
    if (!is.null(engine$profiles_idx_full)) {
      for (i in seq_along(engine$profiles_idx_full)) {
        scores[i] <- sum(w[engine$profiles_idx_full[[i]]])
      }
    } else {
      for (i in seq_len(nrow(engine$profiles_idx))) {
        scores[i] <- sum(w[engine$profiles_idx[i, ]])
      }
    }
    return(scores)
  }

  .benchmark_or(engine$diagnostics$winner_probabilities, numeric())
}

#' @keywords internal
.benchmark_profile_prob <- function(engine, output_mode = c("uncertainty", "deterministic")) {
  output_mode <- match.arg(output_mode)
  probs <- engine$diagnostics$winner_probabilities
  P <- nrow(engine$profiles)
  if (output_mode == "uncertainty" && !is.null(probs) && length(probs) == P) {
    return(as.numeric(probs))
  }

  score <- .benchmark_profile_score(engine, output_mode = "deterministic")
  if (!length(score)) return(rep(NA_real_, P))
  out <- rep(0, length(score))
  out[which.max(score)] <- 1
  out
}

#' @keywords internal
.benchmark_profile_top3_prob <- function(engine, output_mode = c("uncertainty", "deterministic")) {
  output_mode <- match.arg(output_mode)
  probs <- engine$diagnostics$profile_top3_prob
  P <- nrow(engine$profiles)
  if (output_mode == "uncertainty" && !is.null(probs) && length(probs) == P) {
    return(as.numeric(probs))
  }

  score <- .benchmark_profile_score(engine, output_mode = "deterministic")
  if (!length(score)) return(rep(NA_real_, P))
  out <- rep(0, length(score))
  top3 <- order(score, decreasing = TRUE)[seq_len(min(3L, length(score)))]
  out[top3] <- 1
  out
}

#' @keywords internal
.benchmark_ece <- function(prob, event, bins = 10L) {
  ok <- is.finite(prob) & is.finite(event)
  prob <- prob[ok]
  event <- event[ok]
  if (!length(prob)) return(NA_real_)

  breaks <- seq(0, 1, length.out = bins + 1L)
  grp <- cut(prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  ece <- 0
  for (g in sort(unique(grp))) {
    idx <- grp == g
    if (!any(idx)) next
    conf <- mean(prob[idx], na.rm = TRUE)
    acc <- mean(event[idx], na.rm = TRUE)
    ece <- ece + abs(conf - acc) * mean(idx)
  }
  ece
}

#' @keywords internal
.benchmark_stop_info <- function(engine) {
  st <- engine$stop_state %||% NULL
  reason <- NA_character_
  best_prob <- NA_real_
  margin_q05 <- NA_real_
  top3_min_prob <- NA_real_
  next_ig <- NA_real_

  if (is.null(st)) {
    st <- new.env(parent = emptyenv())
  }
  reason <- st$stop_reason %||% NA_character_
  best_prob <- st$stop_best_prob %||% NA_real_
  margin_q05 <- st$stop_margin_q05 %||% NA_real_
  top3_min_prob <- st$stop_top3_min_prob %||% NA_real_
  next_ig <- st$stop_next_ig %||% NA_real_

  if (is.na(reason) || !nzchar(reason)) {
    s <- engine$settings
    n <- nrow(engine$decisions)
    if (identical(engine$phase, "done")) {
      reason <- "phase_done"
    } else if (isTRUE(n >= (s$max_q %||% Inf))) {
      reason <- "max_q"
    } else if (identical(s$mode, "full") && !engine_has_more_questions(engine)) {
      reason <- "question_bank_exhausted"
    } else if (identical(s$mode, "classic")) {
      strict_mode <- isTRUE(s$classic$strict_paprika)
      if (strict_mode && !is.null(engine$profiles) && nrow(engine$profiles) > 0 &&
        isTRUE(paprika_all_profiles_resolved(engine))) {
        reason <- "strict_profiles_resolved"
      } else if (!engine_has_more_questions(engine)) {
        reason <- "classic_bank_exhausted"
      }
    }
  }

  list(
    reason = reason,
    best_prob = best_prob,
    margin_q05 = margin_q05,
    top3_min_prob = top3_min_prob,
    next_ig = next_ig
  )
}

#' @keywords internal
.benchmark_stop_probe <- function(engine) {
  s <- engine$settings
  out <- list(
    reason = NA_character_,
    best_prob = NA_real_,
    margin_q05 = NA_real_,
    top3_min_prob = NA_real_,
    next_ig = NA_real_,
    can_pick = NA_real_
  )

  if (!identical(s$mode, "full") || is.null(engine$profiles_idx)) {
    return(out)
  }

  n <- nrow(engine$decisions)
  min_q_threshold <- s$stop$min_q %||% s$min_q
  if (n < min_q_threshold) {
    out$reason <- "min_q_gate"
    return(out)
  }

  if (isTRUE(s$fair$enabled) && isTRUE(s$fair$pair_coverage)) {
    h <- engine_health(engine)
    if (!isTRUE(h$pairs_covered)) {
      out$reason <- "pair_coverage_gate"
      return(out)
    }
  }

  samples <- NULL
  if (!is.null(engine$cache$samples) && identical(engine$cache$n_decisions, n)) {
    samples <- engine$cache$samples
  } else {
    sel <- s$selector
    samples <- tryCatch(
      engine_polytope_sample(
        engine,
        n = sel$n_samples,
        burnin = sel$burnin,
        thin = sel$thin,
        seed = engine$seed
      ),
      error = function(e) NULL
    )
  }
  if (is.null(samples)) {
    out$reason <- "sampling_failed"
    return(out)
  }

  W <- samples$weights
  P <- nrow(engine$profiles_idx)
  winners <- .fp_samples_winners(W, engine$profiles_idx, engine$profiles_idx_full)
  win_prob <- tabulate(winners, nbins = P) / length(winners)
  best <- which.max(win_prob)
  best_prob <- win_prob[best]
  out$best_prob <- best_prob

  util <- matrix(0, nrow = nrow(W), ncol = P)
  for (p in seq_len(P)) util[, p] <- rowSums(W[, engine$profiles_idx[p, ], drop = FALSE])
  ord <- order(win_prob, decreasing = TRUE)
  second <- ord[min(2L, length(ord))]
  delta_bs <- util[, best] - util[, second]
  q05 <- stats::quantile(delta_bs, probs = 0.05, names = FALSE)
  out$margin_q05 <- q05

  topk_prob <- .fp_samples_topk(W, engine$profiles_idx, k = 3, profiles_idx_full = engine$profiles_idx_full)
  topk_prob <- rowMeans(topk_prob)
  top_ids <- ord[seq_len(min(3L, length(ord)))]
  out$top3_min_prob <- min(topk_prob[top_ids], na.rm = TRUE)

  pick <- engine_pick_tradeoff_polytope(engine, samples = samples)
  out$can_pick <- as.numeric(!is.null(pick))
  if (!is.null(pick) && !is.null(pick$meta$ig)) {
    out$next_ig <- pick$meta$ig
  }

  win_conf_prob <- s$stop$win_conf_prob %||% s$stop$win_prob %||% 0.95
  win_conf_streak_need <- as.integer(s$stop$win_conf_streak %||% 2L)
  cur_streak <- engine$stop_state$win_conf_streak %||% 0L
  cur_streak <- if (best_prob >= win_conf_prob) cur_streak + 1L else 0L
  if (win_conf_streak_need > 0 && cur_streak >= win_conf_streak_need) {
    out$reason <- "win_conf_streak"
    return(out)
  }

  if (best_prob >= (s$stop$win_prob %||% 0.95) &&
    q05 > (s$stop$margin_q05 %||% 0.0)) {
    out$reason <- "winner_margin"
    return(out)
  }

  topk_thr <- s$stop$topk_prob %||% NA_real_
  if (is.finite(topk_thr) && all(topk_prob[top_ids] >= topk_thr)) {
    out$reason <- "top3_stability"
    return(out)
  }

  weight_eps <- s$stop$weight_span_eps %||% NA_real_
  if (is.finite(weight_eps)) {
    vn <- colnames(W) %||% samples$var_names
    crit <- engine$criteria
    crit_ranges <- matrix(NA_real_, nrow = nrow(W), ncol = length(crit))
    colnames(crit_ranges) <- crit
    for (k in seq_along(crit)) {
      cn <- crit[k]
      lv <- engine$domains[[cn]]
      idxs <- match(paste(cn, lv, sep = ":"), vn)
      if (any(is.na(idxs))) next
      wsub <- W[, idxs, drop = FALSE]
      crit_ranges[, k] <- apply(wsub, 1, function(x) max(x) - min(x))
    }
    keep_k <- s$stop$weight_span_criteria %||% crit
    keep_k <- intersect(crit, keep_k)
    if (length(keep_k)) {
      q <- apply(crit_ranges[, keep_k, drop = FALSE], 2, function(x) {
        stats::quantile(x, probs = c(0.05, 0.95), names = FALSE)
      })
      span_width <- if (!is.null(dim(q))) q[2, ] - q[1, ] else q[2] - q[1]
      if (all(span_width <= weight_eps, na.rm = TRUE)) {
        out$reason <- "weight_span"
        return(out)
      }
    }
  }

  eig_thr <- compute_adaptive_eig_threshold(n, s)
  if (eig_thr > 0 && is.finite(out$next_ig)) {
    if (out$next_ig < eig_thr) {
      out$reason <- "eig_threshold"
      return(out)
    }
    np_thr <- s$stop$no_progress_ig %||% eig_thr
    np_need <- as.integer(s$stop$no_progress_streak %||% 0L)
    streak <- engine$stop_state$no_progress_streak %||% 0L
    streak <- if (out$next_ig < np_thr) streak + 1L else 0L
    if (np_thr > 0 && np_need > 0 && streak >= np_need) {
      out$reason <- "no_progress_eig"
      return(out)
    }
  }

  if (isFALSE(as.logical(out$can_pick))) {
    out$reason <- "question_bank_exhausted"
  }
  out
}

#' @keywords internal
.benchmark_encode_numeric <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x <- as.numeric(x)
  if (!length(x) || all(!is.finite(x))) return(NA_character_)
  paste(format(signif(x, 12), scientific = FALSE, trim = TRUE), collapse = ";")
}

#' @keywords internal
.benchmark_decode_numeric <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)) return(numeric())
  as.numeric(strsplit(x, ";", fixed = TRUE)[[1]])
}

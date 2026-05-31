test_that("benchmark_paper_configs exposes baseline, ablation, and tuning configs", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  cfg <- benchmark_paper_configs(
    D,
    max_q = 4L,
    n_samples = 20L,
    burnin = 5L,
    include = c("reference", "cumulative", "ablation", "tuning", "diagnostic")
  )

  expect_true("reference_paprika" %in% names(cfg))
  expect_true("c7_interactions" %in% names(cfg))
  expect_true("full_minus_fairness" %in% names(cfg))
  expect_true("t1_conservative_stop" %in% names(cfg))
  expect_true("t2_top3" %in% names(cfg))
  expect_true("t3_annealed_fairness" %in% names(cfg))
  expect_true("d1_stop_probe" %in% names(cfg))
  expect_false(cfg$reference_paprika$polytope)
  expect_true(cfg$c7_interactions$interactions)
  expect_equal(cfg$t1_conservative_stop$settings$stop$win_conf_streak, 3L)
  expect_equal(cfg$t2_top3$settings$selector$eig_target, "top3")
  expect_true(isTRUE(cfg$t3_annealed_fairness$settings$fair$anneal_after_coverage))
  expect_equal(cfg$d1_stop_probe$settings$full$bank_move, "all")
  expect_equal(cfg$d1_stop_probe$settings$stop$margin_q05, 0.35)
})

test_that("benchmark_paper_scenarios returns S0-S7 with interaction scenario", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  sc <- benchmark_paper_scenarios(D, n_profiles = 4L, max_q = 4L)

  expect_true(all(paste0("S", 0:7) %in% names(sc)))
  expect_equal(sc$S7$utility_model, "interaction")
  expect_true(length(sc$S7$interaction_pairs) >= 1)
})

test_that("S4 easy and hard scenarios induce different winner margins", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "mid", "high"),
    c3 = c("low", "high")
  )

  s4 <- benchmark_paper_scenarios(D, n_profiles = 6L, max_q = 8L)[["S4"]]
  expanded <- fairpaprika:::.benchmark_expand_scenarios(list(S4 = s4))
  easy_sc <- expanded[[which(vapply(expanded, function(x) identical(x$profile_difficulty, "easy"), logical(1)))[1]]]
  hard_sc <- expanded[[which(vapply(expanded, function(x) identical(x$profile_difficulty, "hard"), logical(1)))[1]]]
  truth <- fairpaprika:::.benchmark_sample_true_model(D, easy_sc, seed = 101L)

  easy_profiles <- fairpaprika:::.benchmark_prepare_profiles(
    D,
    n_profiles = 6L,
    seed = 7L,
    difficulty = "easy",
    truth = truth,
    scenario = easy_sc
  )
  hard_profiles <- fairpaprika:::.benchmark_prepare_profiles(
    D,
    n_profiles = 6L,
    seed = 7L,
    difficulty = "hard",
    truth = truth,
    scenario = hard_sc
  )

  easy_util <- apply(easy_profiles, 1, fairpaprika:::.benchmark_profile_utility, truth = truth)
  hard_util <- apply(hard_profiles, 1, fairpaprika:::.benchmark_profile_utility, truth = truth)
  easy_ord <- order(easy_util, decreasing = TRUE)
  hard_ord <- order(hard_util, decreasing = TRUE)
  easy_margin <- easy_util[easy_ord[1]] - easy_util[easy_ord[2]]
  hard_margin <- hard_util[hard_ord[1]] - hard_util[hard_ord[2]]

  expect_gt(easy_margin, hard_margin)
})

test_that("benchmark_simulation_study runs a small end-to-end study", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  cfg <- benchmark_paper_configs(D, max_q = 4L, n_samples = 20L, burnin = 5L, include = c("reference", "cumulative"))
  cfg <- cfg[c("reference_paprika", "c2_eig")]

  sc <- benchmark_paper_scenarios(D, n_profiles = 4L, max_q = 4L)
  sc <- sc["S0"]

  res <- benchmark_simulation_study(D, configs = cfg, scenarios = sc, n_patients = 2L, seed = 11L)

  expect_true(is.list(res))
  expect_s3_class(res$summary, "data.frame")
  expect_s3_class(res$details, "data.frame")
  expect_true(nrow(res$summary) >= 2)
  expect_true(all(c("config", "scenario_key", "top1_acc", "brier_winner") %in% names(res$summary)))
  expect_true(all(c(
    "config", "n_questions", "top1_correct", "regret",
    "winner_probabilities", "top3_probabilities", "true_top1", "true_top3_mask"
  ) %in% names(res$details)))
})

test_that("benchmark_simulation_study records stop diagnostics and S4 margins", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "mid", "high"),
    c3 = c("low", "high")
  )

  cfg <- benchmark_paper_configs(
    D,
    max_q = 8L,
    n_samples = 20L,
    burnin = 5L,
    include = c("reference", "cumulative")
  )
  cfg <- cfg[c("reference_paprika", "full")]
  sc <- benchmark_paper_scenarios(D, n_profiles = 6L, max_q = 8L)
  sc <- sc["S4"]

  res <- benchmark_simulation_study(D, configs = cfg, scenarios = sc, n_patients = 4L, seed = 23L)

  expect_true(all(c(
    "true_winner_margin", "stop_reason", "stop_best_prob",
    "stop_margin_q05", "stop_top3_min_prob", "stop_next_ig",
    "stop_probe_reason", "stop_probe_best_prob", "stop_probe_margin_q05",
    "stop_probe_top3_min_prob", "stop_probe_next_ig", "stop_probe_can_pick"
  ) %in% names(res$details)))
  expect_true(all(c(
    "true_winner_margin_mean", "stop_reason_mode", "stop_probe_reason_mode",
    "stop_probe_best_prob_mean", "stop_probe_margin_q05_mean",
    "stop_probe_top3_min_prob_mean", "stop_probe_can_pick_rate"
  ) %in% names(res$summary)))
  expect_true(all(!is.na(res$details$stop_reason)))
  expect_true(any(!is.na(res$details$stop_probe_reason)))

  full_s4 <- subset(res$summary, config == "full")
  easy_margin <- mean(full_s4$true_winner_margin_mean[grepl("_easy$", full_s4$scenario_key)])
  hard_margin <- mean(full_s4$true_winner_margin_mean[grepl("_hard$", full_s4$scenario_key)])
  expect_true(is.finite(easy_margin))
  expect_true(is.finite(hard_margin))
  expect_gt(easy_margin, hard_margin)
})

test_that("run_algorithm_paper_benchmark exports manuscript bundle", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  out_dir <- tempfile("benchmark-export-")
  res <- run_algorithm_paper_benchmark(
    D,
    n_patients = 2L,
    n_profiles = 4L,
    max_q = 4L,
    config_names = c("reference_paprika", "full"),
    scenario_ids = "S0",
    n_samples = 20L,
    burnin = 5L,
    seed = 19L,
    progress = FALSE,
    output_dir = out_dir,
    devices = "pdf"
  )

  expect_true(is.list(res$exports))
  expect_true(file.exists(res$exports$tables$summary_csv))
  expect_true(file.exists(res$exports$tables$baseline_full_csv))
  expect_true(file.exists(res$exports$plots$overview_pdf))
  expect_true(file.exists(res$exports$study_rds))
})

test_that("run_algorithm_paper_benchmark can request tuning configs by name", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  res <- run_algorithm_paper_benchmark(
    D,
    n_patients = 2L,
    n_profiles = 4L,
    max_q = 4L,
    config_names = c("reference_paprika", "t2_top3"),
    scenario_ids = "S0",
    n_samples = 20L,
    burnin = 5L,
    seed = 29L,
    progress = FALSE
  )

  expect_true(all(c("reference_paprika", "t2_top3") %in% unique(res$summary$config)))
})

test_that("balanced regularization resolves top-level weights with interactions enabled", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  cfg <- benchmark_paper_configs(
    D,
    max_q = 4L,
    n_samples = 20L,
    burnin = 5L,
    include = "cumulative"
  )
  settings <- cfg$c7_interactions$settings
  eng <- engine_create(D, settings = settings, seed = 1L)

  con <- constraints_from_decisions(
    D,
    eng$decisions,
    eps_strict = settings$eps_strict,
    tau_equal = settings$tau_equal,
    epsilon_monotone = settings$epsilon_monotone,
    normalize_sum_top = settings$normalize_sum_top,
    interactions = settings$interactions
  )

  expect_no_warning({
    out <- fairpaprika:::solve_with_balanced_regularization(con, settings)
  })
  expect_true(isTRUE(out$ok))
})

test_that("benchmark_fit_uncertainty_calibration fits and evaluates held-out calibration", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  res <- run_algorithm_paper_benchmark(
    D,
    n_patients = 8L,
    n_profiles = 4L,
    max_q = 6L,
    config_names = c("full"),
    scenario_ids = c("S5"),
    n_samples = 20L,
    burnin = 5L,
    seed = 41L,
    progress = FALSE
  )

  cal <- benchmark_fit_uncertainty_calibration(
    res,
    config = "full",
    calibration_frac = 0.5,
    seed = 7L
  )

  expect_s3_class(cal, "paprika_uncertainty_calibration")
  expect_true(all(c("winner", "top3", "summary") %in% names(cal)))
  expect_true(all(c("winner", "top3") %in% unique(cal$summary$target)))
  expect_true(all(c("raw", "calibrated") %in% unique(cal$summary$stage)))
  expect_true(all(c("calibration", "test", "all") %in% unique(cal$summary$split)))
  expect_true(all(is.finite(cal$summary$brier)))
  expect_true(all(is.finite(cal$summary$ece)))
})

test_that("engine_apply_uncertainty_calibration attaches calibrated probabilities", {
  cal <- structure(
    list(
      config = "full",
      calibration_frac = 0.5,
      seed = 1L,
      winner = list(method = "setwise_beta", params = c(a = 1, b = 0), eps = 1e-6),
      top3 = list(method = "binary_beta", params = c(a = 1, b = -1, c = 0), eps = 1e-6)
    ),
    class = "paprika_uncertainty_calibration"
  )
  eng <- list(
    diagnostics = list(
      winner_probabilities = c(0.2, 0.3, 0.5),
      profile_top3_prob = c(0.8, 0.6, 0.4)
    )
  )

  out <- engine_apply_uncertainty_calibration(eng, cal, replace = FALSE)

  expect_true(!is.null(out$diagnostics$winner_probabilities_calibrated))
  expect_true(!is.null(out$diagnostics$profile_top3_prob_calibrated))
  expect_equal(sum(out$diagnostics$winner_probabilities_calibrated), 1, tolerance = 1e-8)
  expect_true(all(out$diagnostics$profile_top3_prob_calibrated >= 0))
  expect_true(all(out$diagnostics$profile_top3_prob_calibrated <= 1))
})

test_that("benchmark_compare_configs_ci returns paired deltas and confidence intervals", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  res <- run_algorithm_paper_benchmark(
    D,
    n_patients = 6L,
    n_profiles = 4L,
    max_q = 6L,
    config_names = c("reference_paprika", "full"),
    scenario_ids = c("S0", "S6"),
    n_samples = 20L,
    burnin = 5L,
    seed = 51L,
    progress = FALSE
  )

  cmp <- benchmark_compare_configs_ci(
    res,
    config_a = "reference_paprika",
    config_b = "full",
    metrics = c("top1_acc", "pair_coverage", "calibration_ece"),
    by = "scenario_key",
    B = 50L,
    seed = 3L
  )

  expect_s3_class(cmp, "data.frame")
  expect_true(all(c(
    "scenario_key", "metric", "mean_a", "mean_b",
    "delta", "ci_lo", "ci_hi", "n_pairs"
  ) %in% names(cmp)))
  expect_true(all(c("S0_q6", "S6_q6_tau0.15_flip0.05", "S6_q6_tau0.15_flip0.15", "S6_q6_tau0.15_flip0.05_eq0.02", "S6_q6_tau0.15_flip0.15_eq0.02") %in% unique(cmp$scenario_key)))
  expect_true(all(is.finite(cmp$delta)))
})

test_that("benchmark_calibration_report and export produce paper-ready artifacts", {
  D <- list(
    c1 = c("low", "mid", "high"),
    c2 = c("low", "high"),
    c3 = c("low", "high")
  )

  res <- run_algorithm_paper_benchmark(
    D,
    n_patients = 8L,
    n_profiles = 4L,
    max_q = 6L,
    config_names = c("full"),
    scenario_ids = c("S5"),
    n_samples = 20L,
    burnin = 5L,
    seed = 61L,
    progress = FALSE
  )

  cal <- benchmark_fit_uncertainty_calibration(
    res,
    config = "full",
    calibration_frac = 0.5,
    seed = 5L
  )

  rep <- benchmark_calibration_report(cal, split = "test", bins = 5L)
  expect_true(all(c("summary", "reliability") %in% names(rep)))
  expect_true(all(c("winner", "top3") %in% unique(rep$reliability$target)))
  expect_true(all(c("raw", "calibrated") %in% unique(rep$reliability$stage)))

  out_dir <- tempfile("calibration-export-")
  paths <- benchmark_export_calibration_report(cal, dir = out_dir, prefix = "cal", devices = "pdf", bins = 5L)
  expect_true(file.exists(paths$summary_csv))
  expect_true(file.exists(paths$reliability_csv))
  expect_true(file.exists(paths$winner_pdf))
  expect_true(file.exists(paths$top3_pdf))
})

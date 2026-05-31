test_that("benchmark_ablation returns a data.frame", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  cfgs <- list(list(name = "base", settings = list(max_q = 3L)))
  res <- benchmark_ablation(D, cfgs, n_patients = 2, max_profiles = 4)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("config", "n_questions_mean", "top1_acc", "top3_overlap", "coverage") %in% names(res)))
})

test_that("seed_stability_report returns variability metrics", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  rep <- seed_stability_report(D, settings = list(max_q = 2L), seeds = 1:3, max_profiles = 4)
  expect_true(rep$top1_unique >= 1)
  expect_true(is.na(rep$top3_jaccard) || rep$top3_jaccard <= 1)
  expect_equal(length(rep$runs), 3)
  expect_true(length(rep$top3_freq) == nrow(expand.grid(D)))
})

test_that("permutation_stress_test runs and reports stability", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  decisions <- data.frame(
    A1 = c("c1:high,c2:high", "c1:low,c2:low"),
    A2 = c("c1:low,c2:low", "c1:high,c2:high"),
    pref = c("A", "B"),
    stringsAsFactors = FALSE
  )
  res <- permutation_stress_test(D, decisions, n_perm = 3)
  expect_true(res$top1_unique >= 1)
  expect_true(is.na(res$top3_jaccard) || res$top3_jaccard <= 1)
  expect_equal(length(res$runs), 3)
})

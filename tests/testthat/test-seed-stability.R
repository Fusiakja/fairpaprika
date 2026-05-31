test_that("seed stability report runs without errors", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  settings <- list(
    interactions = list(enabled = FALSE),
    selector = list(top_k = 1L, n_samples = 50L, burnin = 20L, thin = 1L),
    fair = list(pair_coverage = FALSE, exposure_balance = FALSE),
    max_q = 10L
  )
  rep <- seed_stability_report(D, settings = settings, seeds = 1:4, max_profiles = 4)
  expect_true(is.list(rep))
  expect_equal(length(rep$seeds), 4)
  expect_true(is.numeric(rep$top3_jaccard))
})


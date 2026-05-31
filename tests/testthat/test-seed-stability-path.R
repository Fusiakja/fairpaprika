test_that("seed stability varies paths but report is consistent", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  settings <- list(
    interactions = list(enabled = FALSE),
    selector = list(top_k = 1L, n_samples = 40L, burnin = 15L, thin = 1L),
    fair = list(pair_coverage = FALSE, exposure_balance = FALSE),
    max_q = 12L
  )
  rep <- seed_stability_report(D, settings = settings, seeds = 1:3, max_profiles = 4)
  expect_true(rep$top1_unique >= 1)
  expect_true(rep$top3_jaccard >= 0 && rep$top3_jaccard <= 1)
})


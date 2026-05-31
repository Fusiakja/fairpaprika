test_that("bayes posterior sampling runs with interactions (approx backend)", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")), max_abs = 0.5)), seed = 70)
  eng$decisions <- data.frame(
    A1 = "c1:high,c2:low",
    A2 = "c1:low,c2:high",
    pref = "A",
    stringsAsFactors = FALSE
  )
  eng <- engine_fit_posterior(engine = eng, backend = "approx")
  expect_true(!is.null(eng$posterior_samples))
  expect_true(is.matrix(eng$posterior_samples$weights))
})

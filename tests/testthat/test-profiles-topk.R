test_that("profile ranking with interactions returns top3 probabilities", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  prof <- data.frame(
    c1 = c("low", "high", "high"),
    c2 = c("low", "high", "low"),
    stringsAsFactors = FALSE
  )
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))), seed = 21)
  eng <- engine_set_profiles(eng, prof)
  # simple decisions to make region feasible
  eng$decisions <- data.frame(
    A1 = "c1:high,c2:low",
    A2 = "c1:low,c2:high",
    pref = "A",
    stringsAsFactors = FALSE
  )
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$diagnostics$profile_top3_prob))
  expect_equal(length(eng$diagnostics$profile_top3_prob), nrow(prof))
})


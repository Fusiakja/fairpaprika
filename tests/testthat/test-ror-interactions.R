test_that("ROR runs with interactions and bounds", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")), max_abs = 0.5)), seed = 1)
  # add a simple decision to orient the preference
  eng$decisions <- data.frame(
    A1 = "c1:high,c2:low",
    A2 = "c1:low,c2:high",
    pref = "A",
    stringsAsFactors = FALSE
  )
  res <- necessary_pref(eng, 1, 2)
  expect_true(is.logical(res))
  pos <- possible_pref(eng, 1, 2)
  expect_true(is.logical(pos))
})


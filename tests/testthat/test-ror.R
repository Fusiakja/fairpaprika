test_that("ROR necessary/possible pref work on simple domain", {
  dom <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(dom, seed = 1)
  # preference: C1 high, C2 low preferred to opposite
  eng$decisions <- data.frame(
    A1 = "C1:high,C2:low",
    A2 = "C1:low,C2:high",
    pref = "A",
    stringsAsFactors = FALSE
  )
  expect_true(necessary_pref(eng, 1, 2))
  expect_true(possible_pref(eng, 1, 2))
})

test_that("necessary_top3 proxy returns logical vector", {
  dom <- list(C1 = c("low", "high"), C2 = c("low", "high"), C3 = c("low", "high"))
  eng <- engine_create(dom, seed = 2)
  res <- necessary_top3(eng)
  expect_length(res, nrow(eng$alternatives))
  expect_true(all(is.logical(res)))
})

test_that("eligibility excludes alternatives by max level", {
  dom <- list(C1 = c("low", "mid", "high"), C2 = c("low", "high"))
  eng <- engine_create(dom, seed = 1)
  eng <- engine_set_eligibility(eng, list(max_level = c(C1 = "mid")))
  expect_true(length(eng$eligibility$excluded_alts) > 0)
  nxt <- engine_next_question(eng)
  expect_false(is.null(nxt$question))
})

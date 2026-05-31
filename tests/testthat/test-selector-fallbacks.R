test_that("selector relax reason is set when balance relaxed", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(fair = list(pair_coverage = TRUE, exposure_balance = TRUE, exposure_gap_limit = 0)), seed = 6)
  # run a few questions to trigger balance relax
  for (k in 1:4) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    eng <- engine_add_decision(eng, "A")
  }
  relax_reasons <- vapply(eng$audit, function(x) x$fairness_relaxed_reason %||% NA_character_, character(1))
  expect_true(any(!is.na(relax_reasons)))
})

test_that("selector rarely relaxes when gap limit generous", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(fair = list(pair_coverage = TRUE, exposure_balance = TRUE, exposure_gap_limit = 3)), seed = 7)
  for (k in 1:4) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    eng <- engine_add_decision(eng, "A")
  }
  relax_flags <- vapply(eng$audit, function(x) isTRUE(x$balance_relaxed), logical(1))
  expect_lt(sum(relax_flags, na.rm = TRUE), length(relax_flags))
})

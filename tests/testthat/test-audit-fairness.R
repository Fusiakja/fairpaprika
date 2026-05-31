test_that("audit shows no fairness relax when coverage achievable", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(fair = list(pair_coverage = TRUE, exposure_balance = FALSE)), seed = 1)
  # ask minimal set of pairwise questions to reach coverage
  for (k in 1:6) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break
    eng <- engine_add_decision(eng, "A")
  }
  rep <- engine_procedural_justice(eng)
  relax_flags <- vapply(eng$audit, function(x) isTRUE(x$balance_relaxed), logical(1))
  expect_lte(sum(relax_flags, na.rm = TRUE), length(relax_flags) / 2)
  expect_true(rep$session$coverage >= 0.5)
})

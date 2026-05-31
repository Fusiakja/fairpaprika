test_that("classic mode exhausts pairwise queue and marks used pairs", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "classic", fair = list(pair_coverage = TRUE)), seed = 12)
  asked <- character()
  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break
    asked <- c(asked, q$key)
    eng <- engine_add_decision(eng, "A")
  }

  # Verify all asked questions are marked as used
  expect_equal(length(unique(asked)), length(eng$used_pairs))
})

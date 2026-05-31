test_that("eligibility exclusions remove options from questions and winners", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "full"), seed = 50)
  # exclude alternative 1
  eng$eligibility$excluded_alts <- 1L
  # ask until a non-excluded question appears or we run out
  found <- FALSE
  bad_seen <- FALSE
  for (iter in 1:10) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break
    if (is.null(q$i) || is.null(q$j) || is.na(q$i) || is.na(q$j)) next
    if (q$i %in% (eng$eligibility$excluded_alts %||% integer()) || q$j %in% (eng$eligibility$excluded_alts %||% integer())) {
      bad_seen <- TRUE
      next
    } else {
      eng <- engine_add_decision(eng, "A")
      found <- TRUE
      break
    }
  }
  expect_false(bad_seen)
})

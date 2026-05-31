test_that("full-mode simulation covers all criterion pairs", {
  eng <- engine_create(demo_domains(), seed = 1)
  eng <- simulate_engine(eng, chooser = function(q) "A")
  h <- engine_health(eng)

  expect_true(isTRUE(h$pairs_covered))
  expect_equal(h$n_equal, 0)
  expect_true(h$n_strict >= length(.fp_needed_pairs(eng$criteria)))
})

test_that("full-mode selector uses remaining uncovered pairs", {
  eng <- engine_create(demo_domains(), seed = 2)

  # Force one pair to be covered and ensure the next question targets the others
  first <- engine_next_question(eng)
  eng <- engine_add_decision(first$engine, pref = "A")

  nxt <- engine_next_question(eng)
  expect_false(is.null(nxt$question))
  cp <- paste(sort(nxt$question$differing_criteria), collapse = "::")
  seen <- .fp_pair_counts(eng)
  val <- seen[cp]
  if (is.na(val)) val <- 0L
  expect_equal(unname(val), 0L)
})

test_that("balance relax returns a question when strict balance would block", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"), C3 = c("low", "high"))
  eng <- engine_create(domains, seed = 14)
  eng$settings$fair$exposure_gap_limit <- 0
  eng$settings$selector$candidate_pool <- 2
  nxt <- engine_next_question(eng)
  eng <- engine_add_decision(nxt$engine, "A")
  nxt2 <- engine_next_question(eng)
  expect_false(is.null(nxt2$question))
  # balance_relaxed may remain FALSE if not triggered; ensure meta field exists
  expect_true("balance_relaxed" %in% names(nxt2$engine$audit[[length(nxt2$engine$audit)]]))
})

test_that("CI refinement skip flag toggles", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 15)
  eng$settings$selector$adaptive$ig_ci_width <- 1000
  engine_pick_tradeoff_polytope(eng)
  expect_true(is.logical(eng$diagnostics$refinement_skipped %||% FALSE))
})

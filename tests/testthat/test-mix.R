test_that("interaction share limit falls back to base questions", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  settings <- list(
    interactions = list(enabled = TRUE, pairs = list(c("c1", "c2")),
                        mix = list(window = 3L, max_share = 0.2),
                        activate = list(min_questions = 0, winner_entropy = 0, slack = FALSE)),
    fair = list(pair_coverage = FALSE, exposure_balance = FALSE, interaction_coverage = FALSE),
    selector = list(top_k = 1L, fairness_lambda = 0)
  )
  eng <- engine_create(D, settings = settings, seed = 1)
  eng$interactions_active <- TRUE
  eng$interactions_pairs_active <- eng$settings$interactions$pairs
  # pre-fill audit with interaction questions to push share over max_share
  eng$audit <- list(list(interaction_pair = TRUE), list(interaction_pair = TRUE), list(interaction_pair = TRUE))
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_false(is.null(pick))
  expect_equal(pick$meta$type, "tradeoff")
})

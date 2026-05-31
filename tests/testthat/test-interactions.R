test_that("interactions pairs survive engine_create settings", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))))
  expect_equal(length(eng$settings$interactions$pairs), 1)
})

test_that("interaction fallback does not return NULL when coverage strict", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  settings <- list(
    interactions = list(enabled = TRUE, pairs = list(c("c1", "c2")),
                        activate = list(min_questions = 0, winner_entropy = 0, slack = FALSE),
                        max_pairs = 1, max_fraction = 1),
    selector = list(top_k = 1L, n_samples = 20L, burnin = 5L, thin = 1L, fairness_lambda = 0),
    fair = list(pair_coverage = TRUE, exposure_balance = TRUE, interaction_coverage = TRUE)
  )
  eng <- engine_create(D, settings = settings, seed = 1)
  eng$interactions_active <- TRUE
  eng$interactions_pairs_active <- eng$settings$interactions$pairs
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_false(is.null(pick))
})

test_that("interaction questions are available in candidates", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(enabled = TRUE, pairs = list(c("c1", "c2")))), seed = 1)
  types <- vapply(eng$candidates, function(c) c$type, character(1))
  expect_true("interaction_conditional" %in% types)
})

test_that("interaction mode falls back to base questions when no interactions scored", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(fair = list(pair_coverage = FALSE, exposure_balance = FALSE)), seed = 2)
  eng$interactions_active <- TRUE
  eng$interactions_pairs_active <- list() # no interaction candidates
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_false(is.null(pick))
  expect_equal(pick$meta$type, "tradeoff")
})

test_that("justice report handles numeric seeds in audit", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, seed = 1)
  nxt <- engine_next_question(eng); eng <- nxt$engine
  eng <- engine_add_decision(eng, "A")
  rep <- engine_procedural_justice(eng)
  expect_true(length(rep$session$seeds) > 0)
})

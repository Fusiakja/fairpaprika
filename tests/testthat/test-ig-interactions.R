test_that("IG/utility use interaction weights", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  # engine with interactions; use a simple chooser matching the domains
  chooser <- function(q) {
    ua <- (q$a$c1 == "high") - (q$a$c2 == "high")
    ub <- (q$b$c1 == "high") - (q$b$c2 == "high")
    if (ua == ub) "E" else if (ua > ub) "A" else "B"
  }
  eng_int <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))), seed = 10)
  eng_int <- simulate_engine(eng_int, chooser = chooser, n = 2)
  samples <- engine_polytope_sample(eng_int, n = 30, burnin = 10, thin = 1, seed = 11)

  # pick a tradeoff candidate and compute delta with interaction idx
  ci <- eng_int$candidates[[1]]
  a <- eng_int$alternatives[ci$i, , drop = FALSE]
  b <- eng_int$alternatives[ci$j, , drop = FALSE]
  a_idx <- eng_int$alt_var_idx_full[[ci$i]]
  b_idx <- eng_int$alt_var_idx_full[[ci$j]]
  delta_int <- rowSums(samples$weights[, a_idx, drop = FALSE]) -
               rowSums(samples$weights[, b_idx, drop = FALSE])
  # recompute with additive-only idx for comparison
  delta_add <- rowSums(samples$weights[, eng_int$alt_var_idx[ci$i, ], drop = FALSE]) -
               rowSums(samples$weights[, eng_int$alt_var_idx[ci$j, ], drop = FALSE])
  expect_true(any(delta_int != delta_add))
})

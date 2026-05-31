test_that("engine_polytope_sample returns samples and ranking works", {
  eng <- demo_engine_simple()

  # Create a consistent set of decisions so the feasible region is non-empty
  simulate_engine(eng, chooser = demo_monotone_chooser, n = 18)

  s <- engine_polytope_sample(eng, n = 150, burnin = 60, thin = 1, seed = 1)
  expect_type(s, "list")
  expect_true(is.matrix(s$weights))
  expect_equal(nrow(s$weights), 150)
  expect_equal(ncol(s$weights), length(s$var_names))
  expect_true(all(is.finite(s$weights)))

  # Quick smoke test: ranking works and returns one row per profile
  prof <- data.frame(
    Effekt = c("Niedrig", "Hoch"),
    Nebenwirkungen = c("Viel", "Wenig"),
    stringsAsFactors = FALSE
  )
  r <- polytope_rank_profiles(eng, prof, s)
  expect_true(is.data.frame(r))
  expect_equal(nrow(r), nrow(prof))
  expect_true(all(c("utility_mean", "utility_sd", "fit_percent") %in% names(r)))
})

test_that("engine_polytope_sample returns named weights and start point", {
  eng <- demo_engine_simple()
  simulate_engine(eng, chooser = demo_monotone_chooser, n = 6)

  s <- engine_polytope_sample(eng, n = 10, burnin = 10, thin = 1, seed = 123)
  expect_true(!is.null(colnames(s$weights)))
  expect_equal(colnames(s$weights), s$var_names)
  expect_equal(names(s$start), s$var_names)
  expect_equal(colnames(s$weights_scaled), s$var_names)
})

test_that("engine_fit_posterior populates posterior_samples when cmdstanr present", {
  dom <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(dom, seed = 99)
  eng$decisions <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
  eng <- engine_fit_posterior(eng, iter = 10, chains = 1, seed = 123, backend = "approx")
  expect_true(!is.null(eng$posterior_samples))
  expect_true(!is.null(eng$posterior_samples$weights_scaled))
})

test_that("selector reuses posterior samples for IG", {
  # Mock posterior samples to avoid cmdstanr dependency
  dom <- list(C1 = c("low", "high"), C2 = c("low", "high"), C3 = c("low", "high"))
  eng <- engine_create(dom, seed = 100)
  # create fake samples
  var_names <- unlist(lapply(names(dom), function(cn) paste(cn, dom[[cn]], sep=":")))
  W <- matrix(runif(50 * length(var_names)), nrow = 50, ncol = length(var_names))
  colnames(W) <- var_names
  eng$posterior_samples <- list(weights = W, weights_scaled = W, var_names = var_names)
  eng$cache$samples <- eng$posterior_samples
  eng$cache$n_decisions <- 0L

  res <- engine_pick_tradeoff_polytope(eng)
  expect_false(is.null(res))
})

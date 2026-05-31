test_that("engine_compute sets slack stats and message", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 11)
  eng$decisions <- data.frame(
    A1 = c("C1:high,C2:low", "C1:low,C2:high"),
    A2 = c("C1:low,C2:high", "C1:high,C2:low"),
    pref = c("A", "A"),
    stringsAsFactors = FALSE
  )
  eng <- engine_compute(eng)
  expect_true(isTRUE(eng$diagnostics$slack_flag))
  expect_true(!is.null(eng$diagnostics$slack_stats$top5))
  expect_true(!is.null(eng$diagnostics$slack_message))
})

test_that("engine_reset_caches clears cache and last_pick", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 12)
  eng$cache$samples <- matrix(1)
  eng$cache$outcomes <- list(foo = 1)
  eng$cache$implied <- list(bar = 2)
  eng$last_pick <- list(meta = list(), n_decisions = 0)
  eng <- engine_reset_caches(eng)
  expect_null(eng$cache$samples)
  expect_null(eng$cache$outcomes)
  expect_null(eng$cache$implied)
  expect_null(eng$last_pick)
})

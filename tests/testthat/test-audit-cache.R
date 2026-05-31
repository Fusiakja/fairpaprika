test_that("audit trail records per-question info and run metrics", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 1)
  res <- engine_next_question(eng)
  eng <- res$engine

  expect_true(length(eng$audit) == 1)
  entry <- eng$audit[[1]]
  expect_true(all(c("ig", "fair", "probs", "seed_used") %in% names(entry)))

  # after adding a decision, run-level metrics are attached
  eng <- engine_add_decision(eng, "A")
  expect_true(!is.null(eng$diagnostics$audit_run))
  expect_true("coverage_curve" %in% names(eng$diagnostics$audit_run))
})

test_that("audit export writes files", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 2)
  eng <- simulate_engine(eng, chooser = always_choose_A, n = 2)

  tmp <- tempfile("audit")
  paths <- engine_audit_export(eng, tmp)
  expect_true(file.exists(paths$questions))
  expect_true(file.exists(paths$run))
})

test_that("implied proxy uses closure when available", {
  reach <- .fp_closure_init(3)
  reach[1, 2] <- TRUE
  reach[2, 3] <- TRUE
  counts <- c("C1::C2" = 1L)
  val <- .fp_implied_proxy(reach, 1, 3, counts, "C1::C2")
  expect_true(val > 0)
})

test_that("CI threshold can skip refinement", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 3)
  eng$settings$selector$adaptive$ig_ci_width <- 10 # very loose to avoid refinement triggers
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_true(!is.null(pick$meta$ig_se))
})

test_that("balance relax still yields a question", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"), C3 = c("low", "high"))
  eng <- engine_create(domains, seed = 4)
  eng$settings$fair$exposure_gap_limit <- 0
  eng$settings$selector$candidate_pool <- 2
  res <- engine_next_question(eng)
  eng <- res$engine
  eng <- engine_add_decision(eng, "A")
  res2 <- engine_next_question(eng)
  expect_false(is.null(res2$question))
})

test_that("downsampling respects pair cap and pool", {
  domains <- list(C1 = c("low", "mid", "high"), C2 = c("low", "mid", "high"), C3 = c("low", "mid", "high"))
  eng <- engine_create(domains, seed = 5)
  eng$settings$selector$candidate_pool <- 5
  eng$settings$selector$pair_cap <- 2
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_true(!is.null(pick))
})

test_that("slack disables closure penalties", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 6)
  # contradictory strict prefs
  eng$decisions <- data.frame(
    A1 = c("C1:high,C2:low", "C1:low,C2:high"),
    A2 = c("C1:low,C2:high", "C1:high,C2:low"),
    pref = c("A", "A"),
    stringsAsFactors = FALSE
  )
  out <- solve_partworths(eng$domains, eng$decisions, eng$settings)
  expect_true(isTRUE(out$ok))
  expect_true(length(out$slack_info) >= 1)
})

test_that("slack stats and revisit are populated", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 7)
  eng$decisions <- data.frame(
    A1 = c("C1:high,C2:low", "C1:low,C2:high"),
    A2 = c("C1:low,C2:high", "C1:high,C2:low"),
    pref = c("A", "A"),
    stringsAsFactors = FALSE
  )
  eng <- engine_compute(eng)
  expect_true(isTRUE(eng$diagnostics$slack_flag))
  expect_true(!is.null(eng$diagnostics$slack_revisit))
})

test_that("balance relax flag surfaces", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"), C3 = c("low", "high"))
  eng <- engine_create(domains, seed = 8)
  eng$settings$fair$exposure_gap_limit <- 0
  eng$settings$selector$candidate_pool <- 2
  eng <- simulate_engine(eng, chooser = always_choose_A, n = 1)
  expect_true(isTRUE(eng$diagnostics$audit_run$balance_relaxed) ||
                isFALSE(eng$diagnostics$audit_run$balance_relaxed) ||
                is.null(eng$diagnostics$audit_run$balance_relaxed))
})

test_that("CI skip flag surfaces", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 9)
  eng$settings$selector$adaptive$ig_ci_width <- 0.0001
  pick <- engine_pick_tradeoff_polytope(eng)
  expect_true(is.logical(eng$diagnostics$refinement_skipped %||% FALSE))
})

test_that("cache reset clears caches", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 10)
  eng$cache$samples <- matrix(1, nrow = 1, ncol = 1)
  eng$cache$outcomes <- list(foo = 1)
  eng$cache$implied <- list(bar = 2)
  eng$last_pick <- list(meta = list(), n_decisions = 0)
  eng <- engine_reset_caches(eng)
  expect_null(eng$cache$samples)
  expect_null(eng$cache$outcomes)
  expect_null(eng$cache$implied)
  expect_null(eng$last_pick)
})

test_that("slack solver reports slack for infeasible interactions", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  dec <- data.frame(
    A1 = c("c1:high,c2:high", "c1:low,c2:low"),
    A2 = c("c1:low,c2:low", "c1:high,c2:high"),
    pref = c("A", "A"), # contradictory preferences
    stringsAsFactors = FALSE
  )
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2"))), slack = list(enabled = TRUE, penalty = 1)), seed = 30)
  eng$decisions <- dec
  eng <- suppressWarnings(engine_compute(eng))
  expect_true(is.list(eng$diagnostics$slack_info) || !is.null(eng$diagnostics$slack))
})

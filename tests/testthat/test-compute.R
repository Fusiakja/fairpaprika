test_that("engine_compute returns weights for feasible decisions", {
  dom <- demo_domains()
  eng <- engine_create(dom, seed = 1)

  model <- make_random_utility_model(dom, seed = 123)
  chooser <- function(q) choose_by_model(q, model, tau = 1e-9)

  eng <- simulate_engine(eng, chooser = chooser)
  eng <- engine_compute(eng)

  expect_false(is.null(eng$weights))
  expect_true(is.data.frame(eng$weights))
  expect_true(all(c("Merkmal","Nutzen") %in% names(eng$weights)))

  # Solver-Status ok
  expect_true(isTRUE(eng$diagnostics$solve_status == 0))

  # Importance vorhanden und normalisiert
  imp <- eng$diagnostics$importance
  expect_true(all(is.finite(imp)))
  expect_equal(sum(imp), 100, tolerance = 1e-6)
})

test_that("engine_compute handles infeasible decisions gracefully", {
  dom <- demo_domains()
  eng <- engine_create(dom, seed = 1)
  crit <- eng$criteria

  base <- vapply(crit, function(cn) dom[[cn]][1], character(1))
  alt1 <- base; alt1["Effekt"] <- "Hoch";    alt1["Nebenwirkungen"] <- "Viel"
  alt2 <- base; alt2["Effekt"] <- "Niedrig"; alt2["Nebenwirkungen"] <- "Wenig"

  A1 <- make_alt_string(alt1, crit)
  A2 <- make_alt_string(alt2, crit)

  eng$decisions <- data.frame(
    A1 = c(A1, A2),
    A2 = c(A2, A1),
    pref = c("A", "A"),
    stringsAsFactors = FALSE
  )

  eng <- engine_compute(eng)
  expect_false(is.null(eng$weights))
  expect_true(isTRUE(eng$diagnostics$slack_flag))
  expect_true(!is.null(eng$diagnostics$slack_stats$sum))
})

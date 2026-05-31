test_that("Scenario: Effekt dominiert -> Effekt-Range deutlich größer", {
  dom <- demo_domains()
  eng <- engine_create(dom, seed = 1)

  chooser <- function(q) {
    if ("Effekt" %in% q$differing_criteria) {
      a_eff <- q$a$Effekt
      b_eff <- q$b$Effekt
      if (match(a_eff, dom$Effekt) > match(b_eff, dom$Effekt)) return("A") else return("B")
    }
    "E"
  }

  eng <- simulate_engine(eng, chooser = chooser)
  eng <- engine_compute(eng)

  expect_false(is.null(eng$weights))
  imp <- eng$diagnostics$importance

  expect_equal(names(imp)[which.max(imp)], "Effekt")
  expect_true(all(imp[names(imp) != "Effekt"] < imp[["Effekt"]]))
})

test_that("Scenario: konsistent zufällig -> keine Degeneration, alles > 0", {
  dom <- demo_domains()
  eng <- engine_create(dom, seed = 2)

  model <- make_random_utility_model(dom, seed = 99)
  chooser <- function(q) choose_by_model(q, model, tau = 1e-9)

  eng <- simulate_engine(eng, chooser = chooser)
  eng <- engine_compute(eng)

  imp <- eng$diagnostics$importance
  expect_true(all(imp > 0))
  expect_equal(sum(imp), 100, tolerance = 1e-6)
})

test_that("Scenario: viele Egal -> tau greift, Ergebnis bleibt berechenbar", {
  dom <- demo_domains()
  eng <- engine_create(dom, seed = 3)

  model <- make_random_utility_model(dom, seed = 123)
  chooser <- function(q) choose_by_model(q, model, tau = 0.25)

  eng <- simulate_engine(eng, chooser = chooser)
  eng <- engine_compute(eng)

  h <- engine_health(eng)
  expect_gt(h$n_equal, 0)
  expect_false(is.null(eng$weights))
})

test_that("Scenario: kontradiktorisch/infeasible -> sauberer Diagnose-Status", {
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
})

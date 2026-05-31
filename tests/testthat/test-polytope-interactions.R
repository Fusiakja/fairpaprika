test_that("polytope sampling respects interaction bounds", {
  # build compatible model/chooser for these domains
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  chooser <- function(q) {
    # prefer high c1 and low c2
    ua <- (q$a$c1 == "high") - (q$a$c2 == "high")
    ub <- (q$b$c1 == "high") - (q$b$c2 == "high")
    if (ua == ub) "E" else if (ua > ub) "A" else "B"
  }
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")), max_abs = 0.2)), seed = 2)
  eng <- simulate_engine(eng, chooser = chooser, n = 4)
  s <- engine_polytope_sample(eng, n = 50, burnin = 10, thin = 1, seed = 3)
  inter_vars <- grep("::", s$var_names, value = TRUE, fixed = TRUE)
  if (length(inter_vars)) {
    max_abs <- max(abs(s$weights[, inter_vars, drop = FALSE]))
    expect_lte(max_abs, 0.21) # small numerical wiggle
  } else {
    succeed("no interaction vars present")
  }
})

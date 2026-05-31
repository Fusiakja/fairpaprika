# Tests for diagnostic tools

test_that("polytope_diagnostics works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Single chain
    samples <- engine_polytope_sample(eng, n = 50, progress = FALSE)
    diag <- polytope_diagnostics(samples)

    expect_type(diag, "list")
    expect_true("ess" %in% names(diag))
    expect_true("summary" %in% names(diag))
    expect_equal(diag$n_chains, 1)

    # Multi-chain
    samples_mc <- engine_polytope_sample(eng, n = 30, chains = 2, progress = FALSE)
    diag_mc <- polytope_diagnostics(samples_mc)

    expect_true("rhat" %in% names(diag_mc))
    expect_true("converged" %in% names(diag_mc))
    expect_equal(diag_mc$n_chains, 2)
})

test_that("sensitivity_analysis works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "Medium", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 8, seed = 42)
    eng <- engine_compute(eng)

    # Test eps_strict sensitivity
    sens <- sensitivity_analysis(eng, param = "eps_strict", range = c(0.5, 2.0), steps = 5)

    expect_type(sens, "list")
    expect_equal(length(sens$param_values), 5)
    expect_true("feasible" %in% names(sens))
    expect_true("weights" %in% names(sens))
    expect_true(sum(sens$feasible) > 0) # At least some should be feasible

    # Test tau_equal sensitivity
    sens_tau <- sensitivity_analysis(eng, param = "tau_equal", range = c(0.5, 2.0), steps = 3)
    expect_equal(length(sens_tau$param_values), 3)
})

test_that("bootstrap_convergence works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 5, seed = 42)
    eng <- engine_compute(eng)

    boot <- engine_bootstrap(eng, B = 100, progress = FALSE)
    conv <- bootstrap_convergence(boot, window_size = 25)

    expect_type(conv, "list")
    expect_true("converged" %in% names(conv))
    expect_true("suggested_B" %in% names(conv))
    expect_true("running_mean" %in% names(conv))
})

test_that("model_comparison works", {
    skip_if_not_installed("fairpaprika")
    skip_on_cran() # May take longer

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    # Base model (no interactions)
    eng_base <- engine_create(domains, seed = 42)
    eng_base <- add_test_decisions(eng_base, n = 5, seed = 42)

    # Interaction model
    eng_int <- engine_create(domains,
        settings = list(interactions = list(enabled = TRUE)),
        seed = 42
    )
    eng_int <- add_test_decisions(eng_int, n = 5, seed = 42)

    comp <- model_comparison(eng_base, eng_int, n_samples = 50)

    expect_type(comp, "list")
    expect_true("feasible" %in% names(comp))
    expect_true("recommendation" %in% names(comp))
    expect_equal(length(comp$feasible), 2)
})

test_that("sensitivity analysis handles infeasibility", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Test with extreme range that might cause infeasibility
    sens <- sensitivity_analysis(eng, param = "eps_strict", range = c(0.001, 10.0), steps = 5)

    expect_type(sens$feasible, "logical")
    expect_equal(length(sens$feasible), 5)
    # At least some should be feasible
    expect_true(sum(sens$feasible) > 0)
})

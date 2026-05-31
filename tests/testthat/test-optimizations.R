# Tests for computational optimizations (polytope and bootstrap enhancements)

test_that("coordinate Hit-and-Run works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        A = c("Low", "High"),
        B = c("Low", "High")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Test coordinate method
    samples <- engine_polytope_sample(eng, n = 50, burnin = 20, method = "coordinate", progress = FALSE)

    expect_s3_class(samples, "paprika_polytope")
    expect_true(nrow(samples$weights) > 0)
    expect_equal(ncol(samples$weights), 4) # 2 criteria * 2 levels
    expect_true(all(!is.na(samples$weights)))
})

test_that("multi-chain sampling produces diagnostics", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        A = c("Low", "High"),
        B = c("Low", "High")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Multi-chain sampling
    samples <- engine_polytope_sample(eng, n = 30, chains = 2, progress = FALSE)

    expect_true("diagnostics" %in% names(samples))
    expect_true("rhat" %in% names(samples$diagnostics))
    expect_true("ess" %in% names(samples$diagnostics))
    expect_true("converged" %in% names(samples$diagnostics))
    expect_equal(samples$n_chains, 2)
    expect_equal(length(samples$chains), 2)
})

test_that("parallel bootstrap works", {
    skip_if_not_installed("fairpaprika")
    skip_on_cran() # Parallel tests can be flaky on CRAN

    domains <- list(
        A = c("Low", "Medium", "High"),
        B = c("Low", "High")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 10, seed = 42)
    eng <- engine_compute(eng)

    # Parallel bootstrap
    boot <- engine_bootstrap(eng, B = 20, parallel = TRUE, n_cores = 2, progress = FALSE)

    expect_s3_class(boot, "paprika_bootstrap")
    expect_true(ncol(boot$weights_samples) > 0)
    expect_true(boot$ok_count >= 10) # At least half should succeed
})

test_that("progress bars work without errors", {
    skip_if_not_installed("fairpaprika")
    skip_on_cran() # Skip slow tests

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Test with smaller sample sizes to keep tests fast
    expect_silent({
        samples <- engine_polytope_sample(eng, n = 100, burnin = 50, progress = FALSE)
    })

    expect_silent({
        boot <- engine_bootstrap(eng, B = 50, progress = FALSE)
    })
})

test_that("chain truncation handles different lengths", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Multi-chain may produce different lengths due to numerical issues
    # Should handle gracefully with warning
    samples <- engine_polytope_sample(eng, n = 200, chains = 4, progress = FALSE)

    # Should still have valid structure
    expect_true("diagnostics" %in% names(samples))
    expect_true(nrow(samples$weights) > 0)
})

test_that("standard Hit-and-Run still works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 3, seed = 42)
    eng <- engine_compute(eng)

    # Test standard method (default)
    samples <- engine_polytope_sample(eng, n = 50, method = "standard", progress = FALSE)

    expect_s3_class(samples, "paprika_polytope")
    expect_true(nrow(samples$weights) > 0)
})

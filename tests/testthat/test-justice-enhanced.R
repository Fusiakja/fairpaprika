# Tests for enhanced procedural justice features

test_that("procedural_justice_full works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 10, seed = 42)
    eng <- engine_compute(eng)

    pj <- procedural_justice_full(eng)

    expect_type(pj, "list")
    expect_true("composite_score" %in% names(pj))
    expect_true("voice" %in% names(pj))
    expect_true("neutrality" %in% names(pj))
    expect_true("respect" %in% names(pj))
    expect_true("trustworthiness" %in% names(pj))

    # Scores should be 0-1
    expect_true(pj$composite_score >= 0 && pj$composite_score <= 1)
    expect_true(pj$voice$voice_score >= 0 && pj$voice$voice_score <= 1)
})

test_that("justice_benchmark compares multiple sessions", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    # Create 3 engines with different seeds
    eng1 <- engine_create(domains, seed = 1)
    eng1 <- add_test_decisions(eng1, n = 8, seed = 1)
    eng1 <- engine_compute(eng1)

    eng2 <- engine_create(domains, seed = 2)
    eng2 <- add_test_decisions(eng2, n = 8, seed = 2)
    eng2 <- engine_compute(eng2)

    eng3 <- engine_create(domains, seed = 3)
    eng3 <- add_test_decisions(eng3, n = 8, seed = 3)
    eng3 <- engine_compute(eng3)

    benchmark <- justice_benchmark(list(eng1, eng2, eng3))

    expect_type(benchmark, "list")
    expect_equal(benchmark$n_sessions, 3)
    expect_s3_class(benchmark$summary, "data.frame")
    expect_equal(nrow(benchmark$summary), 5) # 5 dimensions
})

test_that("justice_transparency_report generates audit", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 5, seed = 42)
    eng <- engine_compute(eng)

    report <- justice_transparency_report(eng)

    expect_type(report, "character")
    expect_true(length(report) > 10) # Should have substantive content
    expect_true(any(grepl("AUDIT REPORT", report)))
})

test_that("justice metrics handle edge cases", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    # Minimal session
    eng_min <- engine_create(domains, seed = 42)
    eng_min <- add_test_decisions(eng_min, n = 1, seed = 42)
    eng_min <- engine_compute(eng_min)

    pj_min <- procedural_justice_full(eng_min)

    # Should still compute
    expect_true(!is.null(pj_min$composite_score))
    expect_true(is.finite(pj_min$composite_score))
})

# Tests for SDM functions

test_that("sdm_decision_quality works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Effect = c("Low", "Medium", "High"),
        SideEffects = c("High", "Medium", "Low")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 10, seed = 42)
    eng <- engine_compute(eng)

    quality <- sdm_decision_quality(eng)

    expect_type(quality, "list")
    expect_true("overall_quality" %in% names(quality))
    expect_true("preference_clarity" %in% names(quality))
    expect_true("confidence" %in% names(quality))
    expect_true("value_congruence" %in% names(quality))
    expect_true("knowledge_quality" %in% names(quality))
    expect_true("deliberation_quality" %in% names(quality))

    # Overall quality should be 0-1
    expect_true(quality$overall_quality >= 0 && quality$overall_quality <= 1)

    # Should have 5 quality components
    expect_equal(length(quality$quality_components), 5)
})

test_that("sdm_journey_report works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 10, seed = 42)
    eng <- engine_compute(eng)

    journey <- sdm_journey_report(eng)

    expect_type(journey, "list")
    expect_true("cognitive_load" %in% names(journey))
    expect_true("revision_patterns" %in% names(journey))
    expect_true("fatigue_indicators" %in% names(journey))
    expect_true("question_data" %in% names(journey))

    # Check question_data structure
    expect_s3_class(journey$question_data, "data.frame")
    expect_true(nrow(journey$question_data) > 0)
})

test_that("sdm_journey_report handles empty audit", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 1, seed = 42)
    eng <- engine_compute(eng)

    # Remove audit trail
    eng$audit <- NULL

    expect_warning(
        {
            journey <- sdm_journey_report(eng)
        },
        "No audit trail"
    )

    expect_null(journey$cognitive_load)
    expect_null(journey$fatigue_indicators)
})

test_that("sdm_decision_burden works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        A = c("Low", "Medium", "High"),
        B = c("Low", "High")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 15, seed = 42)
    eng <- engine_compute(eng)

    burden <- sdm_decision_burden(eng)

    expect_type(burden, "list")
    expect_true("cognitive_complexity" %in% names(burden))
    expect_true("emotional_burden" %in% names(burden))
    expect_true("information_overload" %in% names(burden))
    expect_true("fatigue_score" %in% names(burden))
    expect_true("recommendations" %in% names(burden))

    # Fatigue score should be 0-1
    expect_true(burden$fatigue_score >= 0 && burden$fatigue_score <= 1)

    # Recommendations should be character
    expect_type(burden$recommendations, "character")
})

test_that("validate_treatment_profiles works with general context", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Effect = c("Low", "Medium", "High"),
        SideEffects = c("High", "Medium", "Low"),
        Convenience = c("Low", "Medium", "High")
    )

    # Valid profiles
    profiles <- data.frame(
        Effect = c("High", "Medium"),
        SideEffects = c("Low", "Medium"),
        Convenience = c("High", "Medium")
    )

    validation <- validate_treatment_profiles(profiles, domains, context = "general")

    expect_true(validation$valid)
    expect_equal(length(validation$missing_criteria), 0)
    expect_equal(length(validation$invalid_levels), 0)
})

test_that("validate_treatment_profiles detects missing criteria", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Effect = c("Low", "High"),
        SideEffects = c("High", "Low"),
        Convenience = c("Low", "High")
    )

    # Missing Convenience
    profiles <- data.frame(
        Effect = c("High", "Low"),
        SideEffects = c("Low", "High")
    )

    validation <- validate_treatment_profiles(profiles, domains, context = "general")

    expect_false(validation$valid)
    expect_true("Convenience" %in% validation$missing_criteria)
    expect_true(length(validation$warnings) > 0)
})

test_that("validate_treatment_profiles detects invalid levels", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Effect = c("Low", "High"),
        SideEffects = c("High", "Low")
    )

    # Invalid level "Extreme"
    profiles <- data.frame(
        Effect = c("High", "Extreme"),
        SideEffects = c("Low", "High")
    )

    validation <- validate_treatment_profiles(profiles, domains, context = "general")

    expect_false(validation$valid)
    expect_true(length(validation$invalid_levels) > 0)
})

test_that("validate_treatment_profiles works with MS context", {
    skip_if_not_installed("fairpaprika")

    # Use case-insensitive matching
    domains <- list(
        Efficacy = c("Low", "High"),
        Side_effects = c("High", "Low"),
        Monitoring = c("High", "Low"),
        Convenience = c("Low", "High")
    )

    profiles <- data.frame(
        Efficacy = c("High", "Low"),
        Side_effects = c("Low", "High"),
        Monitoring = c("Low", "High"),
        Convenience = c("High", "Low")
    )

    validation <- validate_treatment_profiles(profiles, domains, context = "ms")

    # Should validate successfully (case-insensitive match)
    expect_true(validation$valid)
    expect_equal(validation$context, "ms")
})

test_that("sdm_decision_burden handles empty audit", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("Low", "High"), B = c("Low", "High"))

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 1, seed = 42)
    eng <- engine_compute(eng)

    # Remove audit
    eng$audit <- NULL

    burden <- sdm_decision_burden(eng)

    expect_true(is.na(burden$cognitive_complexity))
    expect_true(is.na(burden$fatigue_score))
})

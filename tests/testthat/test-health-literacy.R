# Tests for health literacy tools

test_that("question_readability works", {
    skip_if_not_installed("fairpaprika")

    # Simple text
    simple <- "Would you prefer option A or option B?"
    result_simple <- question_readability(simple)

    expect_type(result_simple, "list")
    expect_true("flesch_kincaid_grade" %in% names(result_simple))
    expect_true("readability_level" %in% names(result_simple))
    expect_true(is.numeric(result_simple$n_words))

    # Complex medical text
    complex <- "Would you prefer high therapeutic efficacy with moderate adverse effects requiring intensive pharmacological monitoring, or lower efficacy with minimal toxicity?"
    result_complex <- question_readability(complex)

    expect_true(result_complex$flesch_kincaid_grade > result_simple$flesch_kincaid_grade)
    expect_true(result_complex$jargon_count > 0)
})

test_that("plain_language_explain works for MS context", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Efficacy = c("Low", "High"),
        SideEffects = c("High", "Low")
    )

    eng <- engine_create(domains)

    # MS-specific translation
    explanation <- plain_language_explain(eng, "Efficacy", "High", context = "ms")

    expect_type(explanation, "character")
    expect_true(nchar(explanation) > 0)
    expect_true(grepl("effective", explanation, ignore.case = TRUE))
})

test_that("create_option_comparison_table works", {
    skip_if_not_installed("fairpaprika")

    domains <- list(
        Effect = c("Low", "High"),
        SideEffects = c("High", "Low")
    )

    eng <- engine_create(domains, seed = 42)
    eng <- add_test_decisions(eng, n = 5, seed = 42)
    eng <- engine_compute(eng)

    # Create test profiles
    profiles <- data.frame(
        Effect = c("High", "Low", "High"),
        SideEffects = c("Low", "High", "High")
    )
    rownames(profiles) <- c("Option A", "Option B", "Option C")

    comparison <- create_option_comparison_table(profiles, eng, top_n = 2)

    expect_s3_class(comparison, "data.frame")
    expect_equal(nrow(comparison), 2) # top_n = 2
    expect_true("Rank" %in% names(comparison))
    expect_true("Match" %in% names(comparison))
})

test_that("create_visual_decision_aid works", {
    skip_if_not_installed("fairpaprika")

    # Create fake comparison table
    comparison <- data.frame(
        Rank = 1:2,
        Option = c("Treatment A", "Treatment B"),
        Match = c("85%", "60%"),
        Effect = c("High benefit", "Moderate benefit"),
        stringsAsFactors = FALSE
    )

    aid <- create_visual_decision_aid(comparison)

    expect_type(aid, "character")
    expect_true(length(aid) > 5)
    expect_true(any(grepl("OPTION", aid, ignore.case = TRUE)))
})

test_that("readability handles edge cases", {
    skip_if_not_installed("fairpaprika")

    # Very short text
    short <- "A or B?"
    result_short <- question_readability(short)
    expect_true(is.finite(result_short$flesch_kincaid_grade))

    # No jargon
    plain <- "Which do you like better, the red one or the blue one?"
    result_plain <- question_readability(plain)
    expect_equal(result_plain$jargon_count, 0)
})

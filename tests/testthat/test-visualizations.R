# Tests for visualization functions

# Mock plot function to capture output without graphical device
mock_plot <- function(...) invisible(NULL)

test_that("plot_decision_quality runs without error", {
    skip_if_not_installed("fairpaprika")

    # Mock data
    quality <- list(
        overall_quality = 0.8,
        quality_components = c(
            Clarity = 0.7,
            Confidence = 0.8,
            Congruence = 0.9,
            Knowledge = 0.6,
            Deliberation = 0.7
        )
    )

    # Should run without error
    pdf(NULL) # redirect graphics
    on.exit(dev.off())
    expect_silent(plot_decision_quality(quality))
})

test_that("plot_patient_journey runs without error", {
    skip_if_not_installed("fairpaprika")

    # Mock journey data
    journey <- list(
        question_data = data.frame(
            question_id = 1:5,
            response_time = c(5, 4, 3, 2, 2),
            time_gap = c(1, 1, 1, 1, 1),
            complexity = c(1.0, 1.2, 1.0, 0.8, 1.0),
            pref = c("A", "B", "A", "E", "A"),
            after_undo = c(FALSE, FALSE, TRUE, FALSE, FALSE)
        ),
        revision_patterns = list(
            undo_rate = 0.1,
            total_undos = 1
        ),
        fatigue_indicators = list(
            fatigue_suspected = FALSE,
            total_questions = 5
        )
    )

    pdf(NULL)
    on.exit(dev.off())
    # Allow warnings (e.g. lowess) but no errors
    expect_no_error(plot_patient_journey(journey))
})

test_that("plot_rank_probabilities runs without error", {
    skip_if_not_installed("fairpaprika")

    # Mock bootstrap object
    B <- 20
    n_vars <- 4
    weights <- matrix(runif(B * n_vars), nrow = n_vars, ncol = B)

    boot <- list(
        ok_count = B,
        weights_samples = weights,
        var_names = c("A:1", "A:2", "B:1", "B:2"),
        domains = list(A = c("1", "2"), B = c("1", "2"))
    )
    class(boot) <- "paprika_bootstrap"

    profiles <- data.frame(
        A = c("1", "2"),
        B = c("2", "1")
    )
    rownames(profiles) <- c("Opt1", "Opt2")

    pdf(NULL)
    on.exit(dev.off())
    expect_no_error(plot_rank_probabilities(profiles, boot))
})

test_that("plot_pairwise runs without error", {
    skip_if_not_installed("fairpaprika")

    # Mock matrix
    mat <- matrix(runif(16), nrow = 4)
    rownames(mat) <- colnames(mat) <- LETTERS[1:4]

    pdf(NULL)
    on.exit(dev.off())
    expect_no_error(plot_pairwise(mat))
})

test_that("plot_justice_dashboard runs without error", {
    skip_if_not_installed("fairpaprika")

    # Create a computed engine with sufficient decisions
    domains <- list(A = c("L", "H"), B = c("L", "H"))
    eng <- engine_create(domains, seed = 42)
    # Ensure enough decisions to have non-zero exposure for criteria
    eng <- add_test_decisions(eng, n = 10, seed = 42)
    eng <- engine_compute(eng)

    pdf(NULL)
    on.exit(dev.off())
    expect_no_error(plot_justice_dashboard(eng))
})

test_that("plot_treatment_profiles runs without error", {
    skip_if_not_installed("fairpaprika")

    domains <- list(A = c("L", "H"), B = c("L", "H"))
    eng <- engine_create(domains, seed = 42)
    # eng <- engine_compute(eng) # Don't need validation for this plot if engine is structure

    profiles <- data.frame(A = c("H", "L"), B = c("L", "H"))

    pdf(NULL)
    on.exit(dev.off())
    expect_silent(plot_treatment_profiles(profiles, eng))
})

test_that("plot_diagnostics runs without error", {
    skip_if_not_installed("fairpaprika")

    # Mock sensitivity object
    sens <- list(
        summary = data.frame(
            Var = c("A", "B"),
            Stable = c(TRUE, TRUE),
            CV = c(0.05, 0.06),
            Mean = c(0.5, 0.5)
        ),
        parameter = "eps_strict",
        range = c(0.1, 1.0)
    )
    class(sens) <- "paprika_sensitivity"

    pdf(NULL)
    on.exit(dev.off())
    expect_silent(plot_diagnostics(sens, type = "sensitivity"))
})

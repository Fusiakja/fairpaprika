# Enhanced procedural justice metrics for fairpaprika
# Extends existing justice.R with research-grade fairness assessment

#' Comprehensive procedural justice assessment
#'
#' Extends \code{engine_procedural_justice()} with four additional dimensions
#' based on Tyler's procedural justice framework: voice, neutrality, respect,
#' and trustworthiness.
#'
#' @param engine A computed \code{paprika_engine}.
#' @param include_basic Logical. Include basic metrics from \code{engine_procedural_justice()}?
#'
#' @return List with comprehensive justice metrics across all dimensions.
#' @export
procedural_justice_full <- function(engine, include_basic = TRUE) {
    engine <- validate_engine(engine)

    # Get basic justice metrics
    basic <- if (include_basic) engine_procedural_justice(engine) else NULL

    # 1. VOICE: Did patient have opportunity to express preferences?
    voice <- .assess_voice(engine)

    # 2. NEUTRALITY: Was question selection balanced and unbiased?
    neutrality <- .assess_neutrality(engine)

    # 3. RESPECT: Were patient preferences honored?
    respect <- .assess_respect(engine)

    # 4. TRUSTWORTHINESS: Is the process transparent and consistent?
    trust <- .assess_trustworthiness(engine)

    # Composite justice score (0-1)
    composite_score <- mean(c(
        voice$voice_score,
        neutrality$neutrality_score,
        respect$respect_score,
        trust$trust_score
    ), na.rm = TRUE)

    list(
        composite_score = composite_score,
        voice = voice,
        neutrality = neutrality,
        respect = respect,
        trustworthiness = trust,
        basic_metrics = basic
    )
}

#' @keywords internal
.assess_voice <- function(engine) {
    # Voice = opportunity to express preferences on all relevant criterion pairs

    n_criteria <- length(engine$criteria)
    n_possible_pairs <- n_criteria * (n_criteria - 1) / 2

    # How many pairs were actually covered?
    covered_pairs <- length(unique(engine$used_pairs))

    # How many questions did patient answer?
    n_questions <- nrow(engine$decisions)

    # Audit trail for participation indicators
    audit <- engine$audit %||% list()

    # Did patient have chances to express uncertainty?
    n_equal <- sum(engine$decisions$pref == "E", na.rm = TRUE)
    equal_rate <- n_equal / nrow(engine$decisions)

    # Voice score (0-1)
    # High score = comprehensive coverage + patient had agency to express preferences
    coverage_component <- covered_pairs / n_possible_pairs
    agency_component <- min(1, n_questions / n_possible_pairs) # Had enough questions
    expression_component <- 1 - abs(equal_rate - 0.15) # ~15% indifference is healthy

    voice_score <- mean(c(
        coverage_component, agency_component,
        min(1, expression_component)
    ), na.rm = TRUE)

    list(
        voice_score = voice_score,
        pair_coverage = covered_pairs / n_possible_pairs,
        question_adequacy = agency_component,
        expression_opportunities = n_questions,
        indifference_rate = equal_rate,
        recommendation = if (voice_score > 0.7) {
            "Good voice - patient had adequate opportunity to express preferences"
        } else if (voice_score > 0.5) {
            "Moderate voice - consider more comprehensive elicitation"
        } else {
            "Low voice - insufficient opportunities for preference expression"
        }
    )
}

#' @keywords internal
.assess_neutrality <- function(engine) {
    # Neutrality = balanced, unbiased question selection

    # Check exposure balance (via Gini coefficient)
    justice_basic <- engine_procedural_justice(engine)
    exposure_gini <- justice_basic$session$exposure_gini

    # Check for systematic bias in question types
    audit <- engine$audit %||% list()

    if (length(audit) > 0) {
        # Were interaction questions evenly distributed or clustered?
        int_flags <- vapply(audit, function(x) isTRUE(x$interaction_pair), logical(1))

        if (sum(int_flags) > 0) {
            # Check if interactions appeared early, late, or throughout
            int_positions <- which(int_flags) / length(audit)
            int_clustering <- stats::sd(int_positions) / mean(int_positions) # High = well distributed
        } else {
            int_clustering <- NA_real_
        }

        # Were criteria exposed early and late, or biased to beginning?
        criteria_exposure <- table(vapply(audit, function(x) {
            paste(sort(x$criteria %||% character()), collapse = "-")
        }, character(1)))

        exposure_cv <- stats::sd(as.numeric(criteria_exposure)) / mean(as.numeric(criteria_exposure))
    } else {
        int_clustering <- NA_real_
        exposure_cv <- NA_real_
    }

    # Neutrality score (0-1)
    # High score = balanced exposure + no systematic bias
    balance_component <- 1 - exposure_gini # Lower Gini = better

    neutrality_score <- balance_component

    list(
        neutrality_score = neutrality_score,
        exposure_gini = exposure_gini,
        exposure_cv = exposure_cv,
        interaction_clustering = int_clustering,
        recommendation = if (neutrality_score > 0.8) {
            "Excellent neutrality - balanced, unbiased elicitation"
        } else if (neutrality_score > 0.6) {
            "Good neutrality - minor imbalances detected"
        } else {
            "Concerns about neutrality - significant exposure imbalances"
        }
    )
}

#' @keywords internal
.assess_respect <- function(engine) {
    # Respect = honoring patient preferences including uncertainty and reversals

    audit <- engine$audit %||% list()
    n_decisions <- nrow(engine$decisions)

    # Were undos/reversals allowed?
    undo_count <- sum(vapply(audit, function(x) isTRUE(x$after_undo), logical(1)))
    undo_rate <- if (length(audit) > 0) undo_count / length(audit) else NA_real_

    # Were "equal" responses accepted?
    n_equal <- sum(engine$decisions$pref == "E", na.rm = TRUE)
    equal_acceptance <- n_equal > 0 # At least some equals allowed

    # Was there slack for contradictions (allowing imperfect consistency)?
    slack_enabled <- isTRUE(engine$settings$slack$enabled)

    # Respect score (0-1)
    # High score = patient agency was respected (undos ok, equals ok, slack for contradictions)
    undo_component <- if (is.na(undo_rate)) 0.5 else min(1, undo_rate * 10) # Some undos is good
    equal_component <- if (equal_acceptance) 1 else 0
    slack_component <- if (slack_enabled) 1 else 0.5

    respect_score <- mean(c(undo_component, equal_component, slack_component), na.rm = TRUE)

    list(
        respect_score = respect_score,
        undos_allowed = undo_count > 0,
        undo_rate = undo_rate,
        equals_accepted = equal_acceptance,
        slack_enabled = slack_enabled,
        recommendation = if (respect_score > 0.7) {
            "High respect - patient agency was honored"
        } else if (respect_score > 0.5) {
            "Moderate respect - some restrictions on patient expression"
        } else {
            "Low respect - patient agency may have been constrained"
        }
    )
}

#' @keywords internal
.assess_trustworthiness <- function(engine) {
    # Trustworthiness = transparency, consistency, auditability

    # Is there a complete audit trail?
    has_audit <- !is.null(engine$audit) && length(engine$audit) > 0

    # Was seed set (reproducibility)?
    has_seed <- !is.null(engine$seed)

    # Are diagnostics available?
    has_diagnostics <- !is.null(engine$diagnostics) && length(engine$diagnostics) > 0

    # Check for consistency (no contradictions without slack)?
    slack_used <- engine$diagnostics$slack_used %||% 0
    has_contradictions <- slack_used > 0

    # Trustworthiness score (0-1)
    audit_component <- if (has_audit) 1 else 0
    reproducibility_component <- if (has_seed) 1 else 0.5
    transparency_component <- if (has_diagnostics) 1 else 0.5
    consistency_component <- if (!has_contradictions) 1 else 0.7 # Some slack is ok

    trust_score <- mean(c(
        audit_component, reproducibility_component,
        transparency_component, consistency_component
    ))

    list(
        trust_score = trust_score,
        has_audit_trail = has_audit,
        is_reproducible = has_seed,
        has_diagnostics = has_diagnostics,
        contradictions_detected = has_contradictions,
        recommendation = if (trust_score > 0.8) {
            "High trustworthiness - transparent and auditable process"
        } else if (trust_score > 0.6) {
            "Moderate trustworthiness - some transparency limitations"
        } else {
            "Low trustworthiness - limited auditability"
        }
    )
}

#' Compare procedural justice across multiple sessions
#'
#' Research tool for comparing fairness metrics across patient sessions,
#' useful for identifying systematic biases or disparities.
#'
#' @param engines_list Named list of \code{paprika_engine} objects.
#' @param group_labels Optional character vector of group labels (e.g., demographic groups).
#'
#' @return List with comparative justice metrics and disparity analysis.
#' @export
justice_benchmark <- function(engines_list, group_labels = NULL) {
    if (!is.list(engines_list) || length(engines_list) < 2) {
        stop("engines_list must be a list of at least 2 engines")
    }

    # Compute full justice for each engine
    justice_results <- lapply(engines_list, function(eng) {
        tryCatch(procedural_justice_full(eng), error = function(e) NULL)
    })

    # Remove any that failed
    justice_results <- justice_results[!vapply(justice_results, is.null, logical(1))]

    if (length(justice_results) == 0) {
        stop("No valid justice assessments computed")
    }

    # Extract scores
    composite_scores <- vapply(justice_results, function(jr) jr$composite_score, numeric(1))
    voice_scores <- vapply(justice_results, function(jr) jr$voice$voice_score, numeric(1))
    neutrality_scores <- vapply(justice_results, function(jr) jr$neutrality$neutrality_score, numeric(1))
    respect_scores <- vapply(justice_results, function(jr) jr$respect$respect_score, numeric(1))
    trust_scores <- vapply(justice_results, function(jr) jr$trustworthiness$trust_score, numeric(1))

    # Summary statistics
    summary_stats <- data.frame(
        Dimension = c("Composite", "Voice", "Neutrality", "Respect", "Trust"),
        Mean = c(
            mean(composite_scores), mean(voice_scores), mean(neutrality_scores),
            mean(respect_scores), mean(trust_scores)
        ),
        SD = c(
            stats::sd(composite_scores), stats::sd(voice_scores), stats::sd(neutrality_scores),
            stats::sd(respect_scores), stats::sd(trust_scores)
        ),
        Min = c(
            min(composite_scores), min(voice_scores), min(neutrality_scores),
            min(respect_scores), min(trust_scores)
        ),
        Max = c(
            max(composite_scores), max(voice_scores), max(neutrality_scores),
            max(respect_scores), max(trust_scores)
        )
    )

    # Group comparisons if labels provided
    group_comparison <- NULL
    if (!is.null(group_labels) && length(group_labels) == length(engines_list)) {
        # Compare groups using t-tests or Wilcoxon
        unique_groups <- unique(group_labels)

        if (length(unique_groups) == 2) {
            group1_idx <- group_labels == unique_groups[1]
            group2_idx <- group_labels == unique_groups[2]

            # Test for disparity in composite scores
            test_result <- stats::wilcox.test(
                composite_scores[group1_idx],
                composite_scores[group2_idx]
            )

            group_comparison <- list(
                groups = unique_groups,
                group1_mean = mean(composite_scores[group1_idx]),
                group2_mean = mean(composite_scores[group2_idx]),
                difference = mean(composite_scores[group1_idx]) - mean(composite_scores[group2_idx]),
                p_value = test_result$p.value,
                significant = test_result$p.value < 0.05
            )
        }
    }

    list(
        n_sessions = length(justice_results),
        summary = summary_stats,
        group_comparison = group_comparison,
        individual_results = justice_results,
        recommendation = if (stats::sd(composite_scores) > 0.15) {
            "High variability in justice scores - investigate systematic differences"
        } else {
            "Consistent justice across sessions"
        }
    )
}

#' Generate transparency audit report
#'
#' Creates detailed audit trail for procedural transparency.
#' Documents complete question sequence with selection rationale.
#'
#' @param engine A \code{paprika_engine} with audit trail.
#' @param output_path Optional path to write report (if NULL, returns text).
#'
#' @return Character vector with audit report (invisibly if written to file).
#' @export
justice_transparency_report <- function(engine, output_path = NULL) {
    engine <- validate_engine(engine)

    audit <- engine$audit %||% list()

    if (length(audit) == 0) {
        stop("No audit trail available. Enable audit in engine settings.")
    }

    report <- character()

    # Header
    report <- c(report, "===== PROCEDURAL TRANSPARENCY AUDIT REPORT =====", "")
    report <- c(report, sprintf("Generated: %s", Sys.time()))
    report <- c(report, sprintf("Engine seed: %s", engine$seed %||% "NOT SET"))
    report <- c(report, sprintf("Total questions: %d", length(audit)))
    report <- c(report, "")

    # Question sequence
    report <- c(report, "QUESTION SEQUENCE:", "")

    for (i in seq_along(audit)) {
        entry <- audit[[i]]

        report <- c(report, sprintf("Question %d:", i))
        report <- c(report, sprintf("  Type: %s", entry$type %||% "unknown"))
        report <- c(report, sprintf(
            "  Criteria: %s",
            paste(entry$criteria %||% "unknown", collapse = " vs ")
        ))
        report <- c(report, sprintf(
            "  Interaction: %s",
            if (isTRUE(entry$interaction_pair)) "YES" else "NO"
        ))
        report <- c(report, sprintf(
            "  Decision: %s",
            engine$decisions$pref[entry$decision_idx %||% NA]
        ))
        report <- c(report, sprintf(
            "  After undo: %s",
            if (isTRUE(entry$after_undo)) "YES" else "NO"
        ))

        if (!is.null(entry$response_time)) {
            report <- c(report, sprintf("  Response time: %.1f sec", entry$response_time))
        }

        if (!is.null(entry$cost)) {
            report <- c(report, sprintf("  Complexity: %.2f", entry$cost))
        }

        report <- c(report, "")
    }

    # Exposure summary
    report <- c(report, "CRITERION EXPOSURE SUMMARY:", "")

    justice <- engine_procedural_justice(engine)

    for (crit in engine$criteria) {
        exposure <- justice$criteria[[crit]]$exposure
        report <- c(report, sprintf("  %s: %d questions", crit, exposure))
    }

    report <- c(report, "")
    report <- c(report, sprintf("Exposure Gini: %.3f", justice$session$exposure_gini))
    report <- c(report, sprintf("Coverage: %.1f%%", justice$session$coverage * 100))

    # Procedural justice assessment
    report <- c(report, "", "PROCEDURAL JUSTICE ASSESSMENT:", "")

    pj_full <- procedural_justice_full(engine)

    report <- c(report, sprintf("  Composite Score: %.2f/1.0", pj_full$composite_score))
    report <- c(report, sprintf("  Voice: %.2f", pj_full$voice$voice_score))
    report <- c(report, sprintf("  Neutrality: %.2f", pj_full$neutrality$neutrality_score))
    report <- c(report, sprintf("  Respect: %.2f", pj_full$respect$respect_score))
    report <- c(report, sprintf("  Trustworthiness: %.2f", pj_full$trustworthiness$trust_score))

    report <- c(report, "", "=== END OF AUDIT REPORT ===")

    # Write to file if requested
    if (!is.null(output_path)) {
        writeLines(report, output_path)
        message("Audit report written to: ", output_path)
    }

    invisible(report)
}

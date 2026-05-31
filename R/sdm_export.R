# Report generation for fairpaprika SDM
# Patient-facing and clinician-facing decision summaries

#' Generate patient-facing decision summary
#'
#' Creates an accessible text summary of the SDM session for patients.
#' Uses plain language and focuses on actionable insights.
#'
#' @param engine A computed `paprika_engine`.
#' @param include_quality Logical. Include quality metrics?
#' @param include_journey Logical. Include journey summary?
#'
#' @return Character vector with formatted report lines.
#' @export
sdm_patient_summary <- function(engine, include_quality = TRUE, include_journey = TRUE) {
    engine <- validate_engine(engine)

    if (is.null(engine$weights)) {
        engine <- engine_compute(engine)
    }

    report <- character()

    # Header
    report <- c(report, "===== Your Treatment Decision Summary =====", "")

    # Top options
    if (!is.null(engine$diagnostics$winner_probabilities)) {
        win_probs <- engine$diagnostics$winner_probabilities
        ord <- order(win_probs, decreasing = TRUE)

        report <- c(report, "YOUR TOP OPTIONS:", "")
        for (i in seq_len(min(3, length(win_probs)))) {
            idx <- ord[i]
            prob <- win_probs[idx]
            report <- c(report, sprintf(
                "  %d. Option %d (%.0f%% match with your priorities)",
                i, idx, prob * 100
            ))
        }
        report <- c(report, "")
    }

    # What matters most
    if (!is.null(engine$importance)) {
        imp <- engine$importance
        top_crit <- names(sort(imp, decreasing = TRUE))[1:min(3, length(imp))]

        report <- c(report, "WHAT MATTERS MOST TO YOU:", "")
        for (i in seq_along(top_crit)) {
            crit <- top_crit[i]
            report <- c(report, sprintf(
                "  %d. %s (%.0f%% importance)",
                i, crit, imp[crit]
            ))
        }
        report <- c(report, "")
    }

    # Decision quality
    if (include_quality) {
        quality <- sdm_decision_quality(engine)

        report <- c(report, "YOUR DECISION QUALITY:", "")
        report <- c(report, sprintf("  Overall Quality: %.0f/100", quality$overall_quality * 100))

        if (!is.na(quality$preference_clarity)) {
            clarity_msg <- if (quality$preference_clarity > 0.3) {
                "You have a clear top choice"
            } else if (quality$preference_clarity > 0.1) {
                "Your top choices are fairly close"
            } else {
                "Multiple options are very similar for you"
            }
            report <- c(report, sprintf("  Clarity: %s", clarity_msg))
        }

        if (!is.null(quality$confidence$confidence_score) && !is.na(quality$confidence$confidence_score)) {
            conf <- quality$confidence$confidence_score
            conf_msg <- if (conf > 0.8) {
                "High confidence in results"
            } else if (conf > 0.6) {
                "Moderate confidence - some uncertainty"
            } else {
                "Lower confidence - consider discussing with clinician"
            }
            report <- c(report, sprintf("  Confidence: %s", conf_msg))
        }
        report <- c(report, "")
    }

    # Journey summary
    if (include_journey) {
        journey <- sdm_journey_report(engine)

        if (!is.null(journey$question_data)) {
            report <- c(report, "YOUR DECISION PROCESS:", "")
            report <- c(report, sprintf("  Questions answered: %d", nrow(journey$question_data)))

            if (!is.null(journey$revision_patterns$undo_rate)) {
                undo_rate <- journey$revision_patterns$undo_rate
                if (undo_rate > 0) {
                    report <- c(report, sprintf(
                        "  Revisions made: %d (%.0f%%) - taking time to reflect is good!",
                        journey$revision_patterns$total_undos,
                        undo_rate * 100
                    ))
                }
            }

            if (!is.null(journey$fatigue_indicators$fatigue_suspected)) {
                if (!journey$fatigue_indicators$fatigue_suspected) {
                    report <- c(report, "  Engagement: Strong throughout session")
                }
            }
            report <- c(report, "")
        }
    }

    # Footer
    report <- c(
        report, "NEXT STEPS:",
        "  - Review these results with your healthcare provider",
        "  - Discuss any questions or concerns",
        "  - Consider how these priorities align with your life goals",
        ""
    )

    report <- c(
        report, "This summary reflects YOUR priorities and values.",
        "Your healthcare team can help interpret these results."
    )

    report
}

#' Generate clinician-facing SDM summary
#'
#' Creates a clinical decision support summary for healthcare providers.
#'
#' @param engine A computed `paprika_engine`.
#' @param include_audit Logical. Include audit trail summary?
#' @param include_red_flags Logical. Include quality red flags?
#'
#' @return Character vector with formatted report lines.
#' @export
sdm_clinician_summary <- function(engine, include_audit = TRUE, include_red_flags = TRUE) {
    engine <- validate_engine(engine)

    if (is.null(engine$weights)) {
        engine <- engine_compute(engine)
    }

    report <- character()

    # Header
    report <- c(report, "===== Clinical SDM Summary =====", "")
    report <- c(report, sprintf("Session Date: %s", Sys.Date()))
    report <- c(report, sprintf("Criteria Evaluated: %d", length(engine$criteria)))
    report <- c(report, sprintf("Decisions Collected: %d", nrow(engine$decisions)))
    report <- c(report, "")

    # Patient preference profile
    report <- c(report, "PATIENT PREFERENCE PROFILE:", "")

    if (!is.null(engine$importance)) {
        imp <- engine$importance
        imp_sorted <- sort(imp, decreasing = TRUE)

        for (i in seq_along(imp_sorted)) {
            crit <- names(imp_sorted)[i]
            report <- c(report, sprintf("  %d. %s: %.1f%%", i, crit, imp_sorted[i]))
        }
        report <- c(report, "")
    }

    # Recommended options
    if (!is.null(engine$diagnostics$winner_probabilities)) {
        win_probs <- engine$diagnostics$winner_probabilities
        ord <- order(win_probs, decreasing = TRUE)

        report <- c(report, "RECOMMENDED OPTIONS (by patient values):", "")
        for (i in seq_len(min(5, length(win_probs)))) {
            idx <- ord[i]
            prob <- win_probs[idx]
            report <- c(report, sprintf("  %d. Option %d: %.1f%% match", i, idx, prob * 100))
        }
        report <- c(report, "")
    }

    # Session quality metrics
    quality <- sdm_decision_quality(engine)

    report <- c(report, "SESSION QUALITY METRICS:", "")
    report <- c(report, sprintf("  Overall Quality: %.2f/1.0", quality$overall_quality))
    report <- c(report, sprintf("  Preference Clarity: %.2f", quality$preference_clarity %||% NA))
    report <- c(report, sprintf("  Confidence: %.2f", quality$confidence$confidence_score %||% NA))
    report <- c(report, sprintf("  Value Congruence: %.2f", quality$value_congruence$congruence_score))
    report <- c(report, sprintf("  Deliberation Quality: %.2f", quality$deliberation_quality$engagement_score %||% NA))
    report <- c(report, "")

    # Procedural justice
    justice <- engine_procedural_justice(engine)

    report <- c(report, "PROCEDURAL FAIRNESS:", "")
    report <- c(report, sprintf("  Criterion Coverage: %.1f%%", justice$session$coverage * 100))
    report <- c(report, sprintf("  Exposure Balance (Gini): %.3f", justice$session$exposure_gini))
    report <- c(report, "")

    # Red flags
    if (include_red_flags) {
        red_flags <- character()

        # Check for low quality
        if (quality$overall_quality < 0.5) {
            red_flags <- c(red_flags, "  ⚠ Low overall decision quality - consider additional elicitation")
        }

        # Check for low confidence
        if (!is.null(quality$confidence$confidence_score)) {
            if (quality$confidence$confidence_score < 0.5) {
                red_flags <- c(red_flags, "  ⚠ High uncertainty - patient may benefit from more information")
            }
        }

        # Check for fatigue
        burden <- sdm_decision_burden(engine)
        if (burden$fatigue_score > 0.7) {
            red_flags <- c(red_flags, "  ⚠ High decision burden detected - session may have been overwhelming")
        }

        # Check for contradictions
        strict_rate <- quality$value_congruence$congruence_score
        if (strict_rate < 0.3) {
            red_flags <- c(red_flags, "  ⚠ High indifference rate - patient may be uncertain or fatigued")
        }

        if (length(red_flags) > 0) {
            report <- c(report, "QUALITY RED FLAGS:", "", red_flags, "")
        } else {
            report <- c(report, "QUALITY CHECK: ✓ No significant concerns", "")
        }
    }

    # Audit summary
    if (include_audit) {
        audit <- engine$audit %||% list()

        if (length(audit) > 0) {
            report <- c(report, "SESSION AUDIT:", "")
            report <- c(report, sprintf("  Total Questions: %d", length(audit)))
            report <- c(report, sprintf(
                "  Undos/Revisions: %d",
                sum(vapply(audit, function(x) isTRUE(x$after_undo), logical(1)))
            ))
            report <- c(report, sprintf(
                "  Interaction Questions: %d",
                sum(vapply(audit, function(x) isTRUE(x$interaction_pair), logical(1)))
            ))
            report <- c(report, "")
        }
    }

    # Footer
    report <- c(
        report, "CLINICAL RECOMMENDATIONS:",
        "  - Use this preference profile to guide shared decision-making discussion",
        "  - Address any red flags or quality concerns",
        "  - Explore patient''s reasoning for top-ranked options",
        "  - Consider if results align with clinical contraindications",
        ""
    )

    report
}

#' Print patient summary to console
#'
#' @param engine A computed engine.
#' @export
print_patient_summary <- function(engine) {
    summary <- sdm_patient_summary(engine)
    cat(summary, sep = "\n")
    invisible(NULL)
}

#' Print clinician summary to console
#'
#' @param engine A computed engine.
#' @export
print_clinician_summary <- function(engine) {
    summary <- sdm_clinician_summary(engine)
    cat(summary, sep = "\n")
    invisible(NULL)
}

#' Export patient report to file
#'
#' Generates and saves a patient-facing decision summary.
#'
#' @param engine A computed `paprika_engine`.
#' @param file Path to output file.
#' @param ... Additional arguments passed to `sdm_patient_summary`.
#'
#' @return Invisible TRUE on success.
#' @export
sdm_export_patient_report <- function(engine, file, ...) {
    report <- sdm_patient_summary(engine, ...)
    writeLines(report, file)
    invisible(TRUE)
}

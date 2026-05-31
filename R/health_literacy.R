# Health literacy tools for fairpaprika
# Patient accessibility features for SDM

#' Assess question readability
#'
#' Estimates reading level and complexity of question text.
#' Uses Flesch-Kincaid grade level and jargon detection.
#'
#' @param question_text Character string with question text.
#'
#' @return List with readability metrics and suggestions.
#' @export
question_readability <- function(question_text) {
    if (!is.character(question_text) || length(question_text) == 0) {
        stop("question_text must be a non-empty character string")
    }

    # Clean text
    text <- tolower(question_text)

    # Count sentences (simple heuristic)
    n_sentences <- max(1, length(gregexpr("[.!?]", text)[[1]]))

    # Count words
    words <- strsplit(text, "\\s+")[[1]]
    words <- words[nchar(words) > 0]
    n_words <- length(words)

    # Count syllables (simplified approximation)
    n_syllables <- sum(vapply(words, .count_syllables, integer(1)))

    # Flesch-Kincaid Grade Level
    if (n_words > 0 && n_sentences > 0) {
        fk_grade <- 0.39 * (n_words / n_sentences) + 11.8 * (n_syllables / n_words) - 15.59
        fk_grade <- max(1, fk_grade) # Clamp to minimum grade 1
    } else {
        fk_grade <- NA_real_
    }

    # Detect medical/technical jargon
    medical_terms <- c(
        "efficacy", "adverse", "toxicity", "monitoring", "contraindication",
        "therapeutic", "pharmacological", "comorbidity", "prognosis"
    )

    jargon_detected <- tolower(words[words %in% medical_terms])
    jargon_count <- length(jargon_detected)

    # Sentence complexity (word count)
    avg_words_per_sentence <- n_words / n_sentences

    # Readability classification
    readability_level <- if (is.na(fk_grade)) {
        "Unable to assess"
    } else if (fk_grade <= 6) {
        "Easy (elementary school)"
    } else if (fk_grade <= 9) {
        "Moderate (middle school)"
    } else if (fk_grade <= 12) {
        "Difficult (high school)"
    } else {
        "Very difficult (college+)"
    }

    # Suggestions
    suggestions <- character()

    if (!is.na(fk_grade) && fk_grade > 8) {
        suggestions <- c(suggestions, "Consider shorter sentences")
    }

    if (avg_words_per_sentence > 20) {
        suggestions <- c(suggestions, "Break long sentences into multiple shorter ones")
    }

    if (jargon_count > 0) {
        suggestions <- c(suggestions, sprintf(
            "Replace medical jargon: %s",
            paste(unique(jargon_detected), collapse = ", ")
        ))
    }

    if (length(suggestions) == 0) {
        suggestions <- "Text is accessible"
    }

    list(
        flesch_kincaid_grade = fk_grade,
        readability_level = readability_level,
        n_words = n_words,
        n_sentences = n_sentences,
        avg_words_per_sentence = avg_words_per_sentence,
        jargon_count = jargon_count,
        jargon_detected = unique(jargon_detected),
        suggestions = suggestions
    )
}

#' @keywords internal
.count_syllables <- function(word) {
    # Simplified syllable counter (approximation)
    word <- tolower(word)
    word <- gsub("[^a-z]", "", word) # Remove non-letters

    if (nchar(word) == 0) {
        return(0L)
    }

    # Count vowel groups
    vowels <- gregexpr("[aeiouy]+", word)[[1]]

    if (vowels[1] == -1) {
        return(1L)
    } # No vowels, count as 1

    n_syllables <- length(vowels)

    # Adjust for silent 'e'
    if (grepl("e$", word) && n_syllables > 1) {
        n_syllables <- n_syllables - 1L
    }

    max(1L, n_syllables)
}

#' Generate plain language explanation
#'
#' Converts technical criterion/level names to patient-friendly language.
#'
#' @param engine A \code{paprika_engine}.
#' @param criterion Character. Criterion name.
#' @param level Character. Level name.
#' @param context Character. Healthcare context for domain-specific translations.
#'
#' @return Character string with plain language explanation.
#' @export
plain_language_explain <- function(engine, criterion, level,
                                   context = c("general", "ms", "oncology", "cardiology")) {
    context <- match.arg(context)

    # Context-specific translations
    translations <- list(
        ms = list(
            Efficacy = list(
                Low = "less effective at preventing relapses",
                Medium = "moderately effective at controlling disease",
                High = "highly effective at reducing relapses"
            ),
            SideEffects = list(
                High = "higher risk of side effects",
                Medium = "some side effects possible",
                Low = "fewer side effects"
            ),
            Monitoring = list(
                High = "frequent medical check-ups required",
                Medium = "regular monitoring needed",
                Low = "minimal monitoring required"
            ),
            Convenience = list(
                Low = "more difficult to take (e.g., frequent injections)",
                Medium = "moderate ease of use",
                High = "easy to take (e.g., oral pill)"
            )
        ),
        oncology = list(
            Survival = list(
                Low = "shorter expected survival time",
                Medium = "moderate survival benefit",
                High = "best chance for longer survival"
            ),
            QualityOfLife = list(
                Low = "more impact on daily activities",
                Medium = "some lifestyle restrictions",
                High = "better quality of life maintained"
            ),
            Toxicity = list(
                High = "more severe treatment side effects",
                Medium = "manageable side effects",
                Low = "milder side effects"
            )
        ),
        cardiology = list(
            MortalityReduction = list(
                Low = "smaller benefit for survival",
                Medium = "moderate survival benefit",
                High = "greatest reduction in death risk"
            ),
            SymptomControl = list(
                Low = "less relief of symptoms like chest pain",
                Medium = "moderate symptom improvement",
                High = "excellent symptom relief"
            )
        ),
        general = list()
    )

    # Try context-specific translation first
    if (context %in% names(translations)) {
        context_dict <- translations[[context]]

        if (criterion %in% names(context_dict)) {
            crit_dict <- context_dict[[criterion]]

            if (level %in% names(crit_dict)) {
                return(crit_dict[[level]])
            }
        }
    }

    # Fallback to generic pattern-based translation
    criterion_clean <- gsub("([a-z])([A-Z])", "\\1 \\2", criterion)
    criterion_clean <- tolower(criterion_clean)

    level_clean <- tolower(level)

    sprintf("%s %s", level_clean, criterion_clean)
}

#' Create patient-friendly option comparison table
#'
#' Generates formatted comparison table with visual indicators.
#'
#' @param profiles Data frame of treatment profiles.
#' @param engine A computed \code{paprika_engine}.
#' @param top_n Number of top options to include.
#' @param patient_friendly Logical. Use plain language?
#' @param context Healthcare context for translations.
#'
#' @return Data frame with formatted comparison.
#' @export
create_option_comparison_table <- function(profiles, engine, top_n = 3,
                                           patient_friendly = TRUE,
                                           context = c("general", "ms", "oncology", "cardiology")) {
    context <- match.arg(context)
    engine <- validate_engine(engine)

    if (is.null(engine$diagnostics$winner_probabilities)) {
        stop("Engine must have winner probabilities. Run engine_compute() first.")
    }

    # Get top options
    win_probs <- engine$diagnostics$winner_probabilities
    top_idx <- order(win_probs, decreasing = TRUE)[seq_len(min(top_n, length(win_probs)))]

    # Build comparison table
    comparison <- data.frame(
        Rank = seq_along(top_idx),
        Option = if (!is.null(rownames(profiles))) {
            rownames(profiles)[top_idx]
        } else {
            paste("Option", top_idx)
        },
        Match = sprintf("%.0f%%", win_probs[top_idx] * 100)
    )

    # Add criterion values with visual indicators
    for (crit in names(profiles)) {
        values <- profiles[[crit]][top_idx]

        if (patient_friendly) {
            # Translate to plain language
            values_translated <- vapply(seq_along(values), function(i) {
                plain_language_explain(engine, crit, as.character(values[i]), context)
            }, character(1))

            # Add visual indicators
            # Use ★ for high-importance criteria
            if (!is.null(engine$importance)) {
                crit_importance <- engine$importance[crit] %||% 0

                if (crit_importance > 30) {
                    values_translated <- paste("★", values_translated)
                }
            }

            comparison[[crit]] <- values_translated
        } else {
            comparison[[crit]] <- as.character(values)
        }
    }

    comparison
}

#' Generate visual decision aid
#'
#' Creates simple text-based visual aid for treatment comparison.
#'
#' @param comparison_table Output from \code{create_option_comparison_table()}.
#'
#' @return Character vector with formatted visual aid.
#' @export
create_visual_decision_aid <- function(comparison_table) {
    if (!is.data.frame(comparison_table)) {
        stop("comparison_table must be a data frame")
    }

    aid <- character()

    aid <- c(aid, "╔════════════════════════════════════════════════════════╗")
    aid <- c(aid, "║          YOUR PERSONALIZED TREATMENT OPTIONS          ║")
    aid <- c(aid, "╚════════════════════════════════════════════════════════╝")
    aid <- c(aid, "")

    for (i in seq_len(nrow(comparison_table))) {
        row <- comparison_table[i, ]

        # Header for each option
        rank_symbol <- switch(as.character(i),
            "1" = "🥇",
            "2" = "🥈",
            "3" = "🥉",
            sprintf("%d.", i)
        )

        aid <- c(aid, sprintf(
            "%s %s - %s match to your priorities",
            rank_symbol, row$Option, row$Match
        ))
        aid <- c(aid, "")

        # List attributes
        for (col in names(row)) {
            if (col %in% c("Rank", "Option", "Match")) next

            value <- row[[col]]
            aid <- c(aid, sprintf("   • %s: %s", col, value))
        }

        aid <- c(aid, "")

        if (i < nrow(comparison_table)) {
            aid <- c(aid, "   ────────────────────────────────────────────────", "")
        }
    }

    aid <- c(aid, "★ = High priority for you")
    aid <- c(aid, "")
    aid <- c(aid, "Discuss these options with your healthcare provider.")

    aid
}

#' Print visual decision aid to console
#'
#' @param profiles Data frame of treatment profiles.
#' @param engine A computed engine.
#' @param top_n Number of top options.
#' @param context Healthcare context.
#' @export
print_decision_aid <- function(profiles, engine, top_n = 3,
                               context = c("general", "ms", "oncology", "cardiology")) {
    context <- match.arg(context)

    comparison <- create_option_comparison_table(profiles, engine,
        top_n = top_n,
        patient_friendly = TRUE,
        context = context
    )

    aid <- create_visual_decision_aid(comparison)

    cat(aid, sep = "\n")
    invisible(NULL)
}

# Enhanced Diagnostics for PAPRIKA
# Tracks efficiency, closure effectiveness, and detects inconsistencies

#' Calculate PAPRIKA efficiency metrics
#'
#' Measures how efficiently the question selection process works
#'
#' @param engine Paprika engine with questions and decisions
#' @return List with efficiency metrics
#' @export
paprika_efficiency_metrics <- function(engine) {
    n_decisions <- nrow(engine$decisions)

    # Total possible questions = all undominated pairs
    if (!is.null(engine$paprika) && !is.null(engine$paprika$total_pairs)) {
        total_possible <- engine$paprika$total_pairs
    } else if (!is.null(engine$candidates)) {
        total_possible <- length(engine$candidates)
    } else {
        # Estimate: for K criteria with average L levels, roughly K*(K-1)/2 * L^2 pairs
        n_crit <- length(engine$criteria)
        avg_levels <- mean(vapply(engine$domains, length, integer(1)))
        total_possible <- n_crit * (n_crit - 1) / 2 * avg_levels^2
    }

    # Questions asked
    questions_asked <- n_decisions

    # Question efficiency
    efficiency <- if (total_possible > 0) {
        questions_asked / total_possible
    } else {
        NA_real_
    }

    # Closure effectiveness
    closure_result <- NULL
    implied_count <- 0
    total_preferences <- 0

    if (n_decisions > 0) {
        closure_result <- paprika_build_closure(engine)

        # Count directly asked preferences
        asked_preferences <- n_decisions

        # Count total inferred preferences (from closure matrix)
        if (!is.null(closure_result$matrix)) {
            # Count non-zero entries in upper triangle (each represents a preference)
            pref_matrix <- closure_result$matrix
            n_alts <- nrow(pref_matrix)
            total_preferences <- sum(pref_matrix != 0) / 2 # Divide by 2 because matrix is symmetric
            implied_count <- total_preferences - asked_preferences
        }
    }

    closure_effectiveness <- if (total_preferences > 0) {
        implied_count / total_preferences
    } else {
        NA_real_
    }

    list(
        questions_asked = questions_asked,
        total_possible_pairs = total_possible,
        question_efficiency = efficiency,
        implied_preferences = implied_count,
        total_preferences = total_preferences,
        closure_effectiveness = closure_effectiveness,
        reduction_factor = if (total_possible > 0) 1 - efficiency else NA_real_
    )
}

#' Detect inconsistencies in preference responses
#'
#' Checks if any decisions contradict the transitive closure
#'
#' @param engine Paprika engine with decisions
#' @return List with inconsistency information
#' @export
paprika_detect_inconsistencies <- function(engine) {
    n_decisions <- nrow(engine$decisions)

    if (n_decisions == 0) {
        return(list(
            has_inconsistencies = FALSE,
            inconsistent_decisions = integer(0),
            inconsistency_details = data.frame(
                decision_idx = integer(0),
                stated = character(0),
                implied = character(0),
                conflicting_chain = character(0),
                stringsAsFactors = FALSE
            )
        ))
    }

    # Build closure incrementally and check for contradictions
    inconsistencies <- data.frame(
        decision_idx = integer(0),
        stated = character(0),
        implied = character(0),
        conflicting_chain = character(0),
        stringsAsFactors = FALSE
    )

    # Build key-to-index mapping
    alt_keys <- engine$alt_keys
    n_alts <- nrow(engine$alternatives)
    if (is.null(alt_keys)) {
        alt_keys <- as.character(1:n_alts)
    }
    key_to_idx <- setNames(1:n_alts, alt_keys)

    # Check each decision against the closure built from all previous decisions
    for (d in seq_len(n_decisions)) {
        # Build closure from decisions 1 to d-1
        if (d > 1) {
            prev_decisions <- engine$decisions[1:(d - 1), , drop = FALSE]
            temp_engine <- engine
            temp_engine$decisions <- prev_decisions
            closure <- paprika_build_closure(temp_engine)

            # Check if decision d contradicts the closure
            a_key <- as.character(engine$decisions$A1[d])
            b_key <- as.character(engine$decisions$A2[d])
            stated_pref <- as.character(engine$decisions$pref[d])

            i <- key_to_idx[[a_key]]
            j <- key_to_idx[[b_key]]

            if (!is.null(i) && !is.null(j)) {
                # Check what closure says
                pref_val <- closure$matrix[i, j]

                implied_pref <- NULL
                if (pref_val == 1) {
                    implied_pref <- "A"
                } else if (pref_val == -1) {
                    implied_pref <- "B"
                } else if (pref_val == 0) implied_pref <- NULL # Unknown or equal

                # Check for contradiction
                if (!is.null(implied_pref) && implied_pref != stated_pref) {
                    # Handle B preference (swap)
                    stated_normalized <- if (stated_pref == "B") "B" else stated_pref

                    if (implied_pref != stated_normalized) {
                        inconsistencies <- rbind(inconsistencies, data.frame(
                            decision_idx = d,
                            stated = stated_pref,
                            implied = implied_pref,
                            conflicting_chain = sprintf(
                                "%s vs %s (from previous %d decisions)",
                                a_key, b_key, d - 1
                            ),
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }
        }
    }

    list(
        has_inconsistencies = nrow(inconsistencies) > 0,
        inconsistent_count = nrow(inconsistencies),
        inconsistent_decisions = inconsistencies$decision_idx,
        inconsistency_details = inconsistencies
    )
}

#' Generate comprehensive PAPRIKA diagnostic report
#'
#' Combines efficiency metrics, closure effectiveness, and inconsistency detection
#'
#' @param engine Paprika engine
#' @return List with complete diagnostic information
#' @export
paprika_diagnostics <- function(engine) {
    # Get efficiency metrics
    efficiency <- paprika_efficiency_metrics(engine)

    # Detect inconsistencies
    inconsistencies <- paprika_detect_inconsistencies(engine)

    # Combine
    list(
        efficiency = efficiency,
        inconsistencies = inconsistencies,
        summary = sprintf(
            "PAPRIKA Diagnostics:\n  Questions: %d / %d possible (%.1f%% efficiency)\n  Implied: %d / %d preferences (%.1f%% from closure)\n  Inconsistencies: %s",
            efficiency$questions_asked,
            efficiency$total_possible_pairs,
            efficiency$question_efficiency * 100,
            efficiency$implied_preferences,
            efficiency$total_preferences,
            efficiency$closure_effectiveness * 100,
            if (inconsistencies$has_inconsistencies) {
                sprintf("%d found", inconsistencies$inconsistent_count)
            } else {
                "None"
            }
        )
    )
}

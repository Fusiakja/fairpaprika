# Healthcare SDM (Shared Decision Making) functions for fairpaprika
# General-purpose healthcare decision support, applicable to any condition

#' Compute SDM decision quality metrics
#'
#' Assesses the quality of a shared decision-making session across multiple
#' dimensions relevant to healthcare contexts. All metrics are disease-agnostic.
#'
#' @param engine A `paprika_engine` with completed preference elicitation.
#'
#' @return List with:
#'   * `preference_clarity` Strength of top choice (0-1, higher = clearer preference)
#'   * `confidence` Uncertainty quantification metrics
#'   * `value_congruence` Alignment with stated preferences
#'   * `knowledge_quality` Coverage of relevant trade-offs
#'   * `deliberation_quality` Process quality indicators
#'   * `procedural_justice` Fairness score (exposure balance, coverage)
#'
#' @export
sdm_decision_quality <- function(engine) {
  engine <- validate_engine(engine)

  # Ensure engine is computed
  if (is.null(engine$weights)) {
    engine <- engine_compute(engine)
  }

  # 1. Preference clarity: how distinct is the top choice?
  if (!is.null(engine$diagnostics$winner_probabilities)) {
    win_probs <- engine$diagnostics$winner_probabilities
    top_prob <- max(win_probs, na.rm = TRUE)
    second_prob <- sort(win_probs, decreasing = TRUE)[2]
    preference_clarity <- top_prob - second_prob # gap between 1st and 2nd
  } else {
    preference_clarity <- NA_real_
  }

  # 2. Confidence: sample polytope to assess uncertainty
  confidence <- tryCatch(
    {
      samples <- engine_polytope_sample(engine, n = 200, progress = FALSE)

      # Compute variance in criterion importance
      crit_names <- engine$criteria
      importance_samples <- matrix(NA_real_,
        nrow = nrow(samples$weights_scaled),
        ncol = length(crit_names)
      )
      colnames(importance_samples) <- crit_names

      for (i in seq_len(nrow(samples$weights_scaled))) {
        w_df <- data.frame(
          Merkmal = samples$var_names,
          Nutzen = samples$weights_scaled[i, ],
          stringsAsFactors = FALSE
        )
        imp <- importance_from_weights(w_df)
        importance_samples[i, ] <- as.numeric(imp[crit_names])
      }

      # Average coefficient of variation
      cv_importance <- apply(importance_samples, 2, function(x) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        if (m == 0) NA_real_ else s / m
      })

      mean_cv <- mean(cv_importance, na.rm = TRUE)

      list(
        uncertainty = mean_cv,
        confidence_score = 1 / (1 + mean_cv), # 0-1 scale, higher = more confident
        importance_cv = cv_importance
      )
    },
    error = function(e) {
      list(uncertainty = NA_real_, confidence_score = NA_real_, importance_cv = NULL)
    }
  )

  # 3. Value congruence: consistency of decisions
  n_decisions <- nrow(engine$decisions)
  n_equal <- sum(engine$decisions$pref == "E", na.rm = TRUE)
  n_strict <- n_decisions - n_equal

  value_congruence <- list(
    decision_count = n_decisions,
    strict_count = n_strict,
    equal_count = n_equal,
    equal_rate = n_equal / n_decisions,
    congruence_score = n_strict / n_decisions # Higher = more consistent preferences
  )

  # 4. Knowledge quality: criterion pair coverage
  justice <- engine_procedural_justice(engine)
  knowledge_quality <- list(
    pair_coverage = justice$session$coverage,
    exposure_balance = 1 - justice$session$exposure_gini, # 1 = perfect balance
    information_completeness = justice$session$coverage
  )

  # 5. Deliberation quality
  audit <- engine$audit %||% list()

  if (length(audit) > 0) {
    # Response time trends (if available)
    response_times <- vapply(audit, function(x) x$response_time %||% NA_real_, numeric(1))

    # Undo/reversal rate
    undo_count <- sum(vapply(audit, function(x) isTRUE(x$after_undo), logical(1)))

    deliberation_quality <- list(
      n_questions = length(audit),
      undo_count = undo_count,
      undo_rate = undo_count / length(audit),
      avg_response_time = mean(response_times, na.rm = TRUE),
      engagement_score = 1 - (undo_count / length(audit)) # Higher = fewer reversals
    )
  } else {
    deliberation_quality <- list(
      n_questions = n_decisions,
      undo_count = NA,
      undo_rate = NA,
      avg_response_time = NA,
      engagement_score = NA
    )
  }

  # 6. Overall quality score (composite)
  quality_components <- c(
    preference_clarity = preference_clarity %||% 0.5,
    confidence = confidence$confidence_score %||% 0.5,
    congruence = value_congruence$congruence_score %||% 0.5,
    knowledge = knowledge_quality$pair_coverage %||% 0.5,
    engagement = deliberation_quality$engagement_score %||% 0.5
  )

  overall_quality <- mean(quality_components, na.rm = TRUE)

  list(
    overall_quality = overall_quality,
    preference_clarity = preference_clarity,
    confidence = confidence,
    value_congruence = value_congruence,
    knowledge_quality = knowledge_quality,
    deliberation_quality = deliberation_quality,
    procedural_justice_score = knowledge_quality$exposure_balance,
    quality_components = quality_components
  )
}

#' Track patient journey through elicitation
#'
#' Documents the evolution of preferences throughout the SDM session.
#' Useful for understanding patient learning, fatigue, and decision trajectory.
#'
#' @param engine A `paprika_engine`.
#' @param include_timestamps Logical. Include time information if available?
#'
#' @return List with:
#'   * `trajectory` Preference evolution over time
#'   * `cognitive_load` Question complexity indicators
#'   * `learning_curve` Response time trends
#'   * `revision_patterns` Undo/reversal analysis
#'   * `fatigue_indicators` Signs of decision fatigue
#'
#' @export
sdm_journey_report <- function(engine, include_timestamps = TRUE) {
  engine <- validate_engine(engine)

  audit <- engine$audit %||% list()
  n_q <- length(audit)

  if (n_q == 0) {
    warning("No audit trail available. Journey report will be limited.")
    return(list(
      trajectory = NULL,
      cognitive_load = NULL,
      learning_curve = NULL,
      revision_patterns = NULL,
      fatigue_indicators = NULL
    ))
  }

  # Extract question-level data
  question_data <- data.frame(
    question_id = seq_len(n_q),
    type = vapply(audit, function(x) x$type %||% "tradeoff", character(1)),
    pref = vapply(audit, function(x) {
      idx <- x$decision_idx %||% NA_integer_
      if (is.na(idx) || idx > nrow(engine$decisions)) {
        NA_character_
      } else {
        as.character(engine$decisions$pref[idx])
      }
    }, character(1)),
    complexity = vapply(audit, function(x) x$cost %||% NA_real_, numeric(1)),
    response_time = vapply(audit, function(x) x$response_time %||% NA_real_, numeric(1)),
    after_undo = vapply(audit, function(x) isTRUE(x$after_undo), logical(1)),
    interaction = vapply(audit, function(x) isTRUE(x$interaction_pair), logical(1)),
    stringsAsFactors = FALSE
  )

  # Cognitive load trends
  cognitive_load <- list(
    mean_complexity = mean(question_data$complexity, na.rm = TRUE),
    interaction_rate = sum(question_data$interaction, na.rm = TRUE) / n_q
  )

  # Revision patterns
  undo_positions <- which(question_data$after_undo)
  revision_patterns <- list(
    total_undos = length(undo_positions),
    undo_rate = length(undo_positions) / n_q,
    undo_positions = undo_positions
  )

  # Fatigue indicators
  if (n_q >= 10) {
    q1_end <- floor(n_q / 4)
    q4_start <- floor(3 * n_q / 4)

    equal_rate_q1 <- sum(question_data$pref[seq_len(q1_end)] == "E", na.rm = TRUE) / q1_end
    equal_rate_q4 <- sum(question_data$pref[q4_start:n_q] == "E", na.rm = TRUE) / (n_q - q4_start + 1)

    fatigue_indicators <- list(
      total_questions = n_q,
      indifference_increase = equal_rate_q4 - equal_rate_q1,
      late_stage_indifference = equal_rate_q4,
      fatigue_suspected = equal_rate_q4 > 0.3
    )
  } else {
    fatigue_indicators <- list(
      total_questions = n_q,
      fatigue_suspected = NA
    )
  }

  list(
    cognitive_load = cognitive_load,
    revision_patterns = revision_patterns,
    fatigue_indicators = fatigue_indicators,
    question_data = question_data
  )
}

#' Assess cognitive and emotional burden of decision process
#'
#' Quantifies the burden placed on patients during preference elicitation.
#' Useful for optimizing question selection strategies.
#'
#' @param engine A `paprika_engine`.
#'
#' @return List with burden metrics (general healthcare SDM):
#'   * `cognitive_complexity` Question difficulty score
#'   * `emotional_burden` Reversal/hesitation indicators
#'   * `information_overload` Flags for excessive complexity
#'   * `fatigue_score` Estimated decision fatigue (0-1)
#'
#' @export
sdm_decision_burden <- function(engine) {
  engine <- validate_engine(engine)

  audit <- engine$audit %||% list()
  n_q <- length(audit)

  if (n_q == 0) {
    return(list(
      cognitive_complexity = NA_real_,
      emotional_burden = NA_real_,
      information_overload = FALSE,
      fatigue_score = NA_real_
    ))
  }

  # Cognitive complexity
  costs <- vapply(audit, function(x) x$cost %||% 1, numeric(1))
  interaction_flags <- vapply(audit, function(x) isTRUE(x$interaction_pair), logical(1))

  cognitive_complexity <- mean(costs) + 0.5 * mean(interaction_flags)

  # Emotional burden
  undo_count <- sum(vapply(audit, function(x) isTRUE(x$after_undo), logical(1)))
  equal_count <- sum(engine$decisions$pref == "E", na.rm = TRUE)

  emotional_burden <- (undo_count / n_q) + (equal_count / nrow(engine$decisions))

  # Information overload
  information_overload <- n_q > 30 || mean(interaction_flags) > 0.3

  # Fatigue score
  fatigue_components <- c(
    length_factor = min(1, n_q / 50),
    complexity_factor = min(1, cognitive_complexity / 3),
    reversal_factor = min(1, emotional_burden)
  )

  fatigue_score <- mean(fatigue_components)

  list(
    cognitive_complexity = cognitive_complexity,
    emotional_burden = emotional_burden,
    information_overload = information_overload,
    fatigue_score = fatigue_score,
    burden_components = fatigue_components,
    recommendations = if (is.na(fatigue_score)) {
      "Burden assessment unavailable."
    } else if (fatigue_score > 0.7) {
      "High burden detected. Consider: (1) reducing questions, (2) simplifying criteria, (3) session breaks."
    } else if (fatigue_score > 0.5) {
      "Moderate burden. Monitor for fatigue signs."
    } else {
      "Burden is manageable."
    }
  )
}

#' Validate healthcare treatment option configurations
#'
#' Generic validation with context-specific presets for different healthcare domains.
#'
#' @param profiles Data frame of treatment profiles.
#' @param domains List of criterion domains from engine.
#' @param required_criteria Character vector of required criteria (NULL = use context preset).
#' @param context One of "general", "ms", "oncology", "cardiology".
#'
#' @return List with validation results:
#'   * `valid` Logical, overall validity
#'   * `missing_criteria` Missing required criteria
#'   * `invalid_levels` Invalid level values
#'   * `warnings` Character vector of warnings
#'
#' @export
validate_treatment_profiles <- function(profiles,
                                        domains,
                                        required_criteria = NULL,
                                        context = c("general", "ms", "oncology", "cardiology")) {
  context <- match.arg(context)

  # Context-specific required criteria
  if (is.null(required_criteria)) {
    required_criteria <- switch(context,
      ms = c("efficacy", "side_effects", "monitoring", "convenience"),
      oncology = c("survival", "quality_of_life", "toxicity", "treatment_duration"),
      cardiology = c("mortality_reduction", "symptom_control", "lifestyle_impact"),
      general = names(domains)
    )

    # Make case-insensitive match
    domain_names_lower <- tolower(names(domains))
    required_lower <- tolower(required_criteria)
    required_criteria <- names(domains)[match(required_lower, domain_names_lower, nomatch = 0)]
    required_criteria <- required_criteria[required_criteria != ""]
  }

  # Check for missing criteria
  profile_criteria <- names(profiles)
  missing_criteria <- setdiff(required_criteria, profile_criteria)

  # Check for invalid levels
  invalid_levels <- list()
  for (crit in intersect(required_criteria, profile_criteria)) {
    if (crit %in% names(domains)) {
      invalid_vals <- setdiff(unique(profiles[[crit]]), domains[[crit]])
      if (length(invalid_vals) > 0) {
        invalid_levels[[crit]] <- invalid_vals
      }
    }
  }

  # Generate warnings
  warnings <- character()
  if (length(missing_criteria) > 0) {
    warnings <- c(warnings, paste("Missing required criteria:", paste(missing_criteria, collapse = ", ")))
  }
  if (length(invalid_levels) > 0) {
    for (crit in names(invalid_levels)) {
      warnings <- c(warnings, paste(
        "Invalid levels for", crit, ":",
        paste(invalid_levels[[crit]], collapse = ", ")
      ))
    }
  }

  valid <- length(missing_criteria) == 0 && length(invalid_levels) == 0

  list(
    valid = valid,
    context = context,
    missing_criteria = missing_criteria,
    invalid_levels = invalid_levels,
    warnings = warnings,
    n_profiles = nrow(profiles),
    n_criteria_covered = sum(required_criteria %in% profile_criteria)
  )
}

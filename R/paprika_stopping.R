# Classic PAPRIKA Stopping Criterion
# Checks if all profile pairs are resolved via transitive closure

#' Check if all profile pairs are resolved (classic PAPRIKA stopping criterion)
#'
#' Original PAPRIKA stops when every pair of alternatives can be ordered
#' via dominance or transitive closure of user preferences
#'
#' @param engine Paprika engine with profiles set
#' @return Logical: TRUE if all profile pairs are resolved and elicitation can stop
#' @export
paprika_all_profiles_resolved <- function(engine) {
    # Only applicable when profiles are set
    if (is.null(engine$profiles) || is.null(engine$profiles_idx)) {
        return(FALSE)
    }

    n_profiles <- nrow(engine$profiles)

    # Need at least one decision to resolve anything
    if (nrow(engine$decisions) == 0) {
        return(FALSE)
    }

    # Build transitive closure
    closure <- paprika_build_closure(engine)

    if (is.null(closure$matrix)) {
        return(FALSE)
    }

    # Build profile-to-alternative mapping
    # Profiles use engine$profiles_idx which maps to variable indices
    # We need to find which alternatives correspond to these profiles

    # For classic mode, we need to check if utilities can rank all profiles
    # Simpler approach: compute utilities and check for ties

    # Get weights if available
    weights <- NULL
    if (!is.null(engine$weights)) {
        weights <- setNames(engine$weights$Nutzen, engine$weights$Merkmal)
    } else if (!is.null(engine$results) && !is.null(engine$results$weights)) {
        weights <- engine$results$weights
    }

    if (is.null(weights)) {
        # No weights yet, can't determine resolution
        return(FALSE)
    }

    # Compute utility for each profile
    profile_utils <- numeric(n_profiles)
    for (p in 1:n_profiles) {
        idx <- engine$profiles_idx[p, ]
        profile_utils[p] <- sum(weights[idx], na.rm = TRUE)
    }

    # Check if all pairs are resolvable (no exact ties)
    tol <- 1e-6
    for (i in 1:(n_profiles - 1)) {
        for (j in (i + 1):n_profiles) {
            diff <- abs(profile_utils[i] - profile_utils[j])
            if (diff < tol) {
                # Tie detected - not fully resolved
                return(FALSE)
            }
        }
    }

    # All profiles can be strictly ordered
    TRUE
}

#' Check if classic PAPRIKA should stop (outcome-based criterion)
#'
#' @param engine Paprika engine
#' @return Logical: TRUE if stopping criterion is met
#' @keywords internal
paprika_should_stop_classic <- function(engine) {
    # Classic mode specific stopping
    if (!identical(engine$settings$mode, "classic")) {
        return(FALSE)
    }

    # Check if all profiles are resolved
    if (!is.null(engine$profiles)) {
        return(paprika_all_profiles_resolved(engine))
    }

    # If no profiles set, fall back to budget/candidate exhaustion
    FALSE
}

# Adaptive Information Gain Threshold
# Allows high selectivity early, relaxes over time to ensure coverage

#' Compute adaptive EIG threshold based on question progress
#'
#' Returns the current information gain threshold based on how many questions
#' have been asked. Starts with high threshold (selective), decays to lower
#' threshold (more permissive) to ensure coverage while maintaining efficiency.
#'
#' @param n_decisions Number of decisions collected so far
#' @param settings Engine settings containing stop$eig_adaptive configuration
#' @return Numeric threshold value for current decision count
#' @keywords internal
compute_adaptive_eig_threshold <- function(n_decisions, settings) {
    adaptive_cfg <- settings$stop$eig_adaptive %||% list()

    # If disabled or not configured, return fixed threshold
    if (!isTRUE(adaptive_cfg$enabled)) {
        return(settings$stop$eig_threshold %||% 0.002)
    }

    start_thr <- adaptive_cfg$start_threshold %||% 0.02
    end_thr <- adaptive_cfg$end_threshold %||% 0.005
    min_q <- adaptive_cfg$min_questions %||% 15
    max_q <- settings$max_q %||% 50
    decay_type <- adaptive_cfg$decay %||% "exponential"

    # Full strength until min_questions
    if (n_decisions <= min_q) {
        return(start_thr)
    }

    # After max_q, use end threshold
    if (n_decisions >= max_q) {
        return(end_thr)
    }

    # Progress from min_q to max_q
    progress <- (n_decisions - min_q) / max(1, max_q - min_q)
    progress <- pmin(pmax(progress, 0), 1) # Clamp to [0,1]

    if (decay_type == "linear") {
        # Linear decay: threshold(t) = start - (start - end) * progress
        threshold <- start_thr - (start_thr - end_thr) * progress
    } else {
        # Exponential decay: threshold(t) = end + (start - end) * exp(-lambda * progress)
        # lambda chosen so that at progress=0.5, we're halfway between start and end in log space
        lambda <- 2 * log((start_thr - end_thr) / (0.5 * (start_thr - end_thr)))
        threshold <- end_thr + (start_thr - end_thr) * exp(-lambda * progress)
    }

    threshold
}

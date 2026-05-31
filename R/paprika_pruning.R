# Dominated Alternative Pruning for PAPRIKA
# Ensures recommendations don't include strictly inferior options

#' Check if profile A dominates profile B based on computed utilities
#'
#' @param util_a Numeric vector of utilities for profile A (one per criterion)
#' @param util_b Numeric vector of utilities for profile B (one per criterion)
#' @param tol Numerical tolerance for comparisons
#' @return Logical: TRUE if A dominates B
#' @keywords internal
paprika_profile_dominates <- function(util_a, util_b, tol = 1e-9) {
    # A dominates B if:
    # 1. A is at least as good on all criteria (util_a >= util_b - tol)
    # 2. A is strictly better on at least one criterion (util_a > util_b + tol)

    at_least_as_good <- all(util_a >= util_b - tol)
    strictly_better_somewhere <- any(util_a > util_b + tol)

    at_least_as_good && strictly_better_somewhere
}

#' Identify and mark dominated profiles for exclusion
#'
#' @param profiles Data frame of treatment profiles
#' @param weights Named vector of computed part-worth utilities
#' @param var_names Character vector of variable names (criterion:level)
#' @param domains Named list of criterion -> ordered levels
#' @return List with:
#'   - dominated_indices: Integer vector of dominated profile row indices
#'   - dominance_pairs: Data frame showing which profiles dominate which
#'   - utilities: Matrix of utilities per profile per criterion
#' @export
paprika_prune_dominated <- function(profiles, weights, var_names, domains) {
    if (is.null(profiles) || nrow(profiles) == 0) {
        return(list(
            dominated_indices = integer(0),
            dominance_pairs = data.frame(
                dominator = integer(0),
                dominated = integer(0),
                stringsAsFactors = FALSE
            ),
            utilities = matrix(0, nrow = 0, ncol = 0)
        ))
    }

    n_profiles <- nrow(profiles)
    criteria <- names(domains)

    # Build utility matrix: rows = profiles, cols = criteria
    util_matrix <- matrix(0, nrow = n_profiles, ncol = length(criteria))
    colnames(util_matrix) <- criteria
    rownames(util_matrix) <- rownames(profiles) %||% paste0("Profile_", 1:n_profiles)

    # Compute utility for each profile on each criterion
    for (p in 1:n_profiles) {
        for (c in seq_along(criteria)) {
            crit <- criteria[c]
            level <- as.character(profiles[p, crit])

            # Find the weight for this criterion:level
            key <- paste0(crit, ":", level)
            idx <- match(key, var_names)

            if (!is.na(idx)) {
                util_matrix[p, c] <- weights[idx]
            } else {
                # Level not in weights (probably reference level = 0)
                util_matrix[p, c] <- 0
            }
        }
    }

    # Find dominance relationships
    dominated <- integer(0)
    dominance_pairs <- data.frame(
        dominator = integer(0),
        dominated = integer(0),
        dominator_name = character(0),
        dominated_name = character(0),
        stringsAsFactors = FALSE
    )

    # Check all pairs
    for (i in 1:(n_profiles - 1)) {
        for (j in (i + 1):n_profiles) {
            util_i <- util_matrix[i, ]
            util_j <- util_matrix[j, ]

            if (paprika_profile_dominates(util_i, util_j)) {
                # i dominates j
                dominated <- c(dominated, j)
                dominance_pairs <- rbind(dominance_pairs, data.frame(
                    dominator = i,
                    dominated = j,
                    dominator_name = rownames(util_matrix)[i],
                    dominated_name = rownames(util_matrix)[j],
                    stringsAsFactors = FALSE
                ))
            } else if (paprika_profile_dominates(util_j, util_i)) {
                # j dominates i
                dominated <- c(dominated, i)
                dominance_pairs <- rbind(dominance_pairs, data.frame(
                    dominator = j,
                    dominated = i,
                    dominator_name = rownames(util_matrix)[j],
                    dominated_name = rownames(util_matrix)[i],
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    # Remove duplicates (a profile might be dominated by multiple others)
    dominated <- unique(dominated)

    list(
        dominated_indices = dominated,
        dominance_pairs = dominance_pairs,
        utilities = util_matrix
    )
}

#' Apply dominated profile pruning to engine results
#'
#' Modifies engine diagnostics to mark dominated profiles and adjust rankings
#'
#' @param engine Paprika engine with computed results and profiles
#' @return Updated engine with dominance information
#' @keywords internal
paprika_apply_pruning <- function(engine) {
    # Only applicable when profiles are set
    if (is.null(engine$profiles)) {
        return(engine)
    }

    # Extract weights and variable names
    if (!is.null(engine$results$weights)) {
        weights <- engine$results$weights
        var_names <- engine$results$var_names
    } else if (!is.null(engine$weights)) {
        # Fallback for classic mode
        weights <- setNames(engine$weights$Nutzen, engine$weights$Merkmal)
        var_names <- engine$weights$Merkmal
    } else {
        # No weights computed yet
        return(engine)
    }

    # Perform pruning analysis
    pruning_result <- paprika_prune_dominated(
        profiles = engine$profiles,
        weights = weights,
        var_names = var_names,
        domains = engine$domains
    )

    # Store in diagnostics
    if (is.null(engine$diagnostics)) {
        engine$diagnostics <- list()
    }

    engine$diagnostics$dominated_profiles <- pruning_result$dominated_indices
    engine$diagnostics$dominance_pairs <- pruning_result$dominance_pairs
    engine$diagnostics$profile_utilities_by_criterion <- pruning_result$utilities

    # If there are dominated profiles, adjust winner probabilities
    if (length(pruning_result$dominated_indices) > 0 &&
        !is.null(engine$diagnostics$winner_probabilities)) {
        # Set dominated profiles to 0 probability
        engine$diagnostics$winner_probabilities[pruning_result$dominated_indices] <- 0

        # Renormalize remaining probabilities
        remaining_prob <- engine$diagnostics$winner_probabilities
        if (sum(remaining_prob) > 0) {
            engine$diagnostics$winner_probabilities <- remaining_prob / sum(remaining_prob)
        }

        # Same for top3 probabilities
        if (!is.null(engine$diagnostics$profile_top3_prob)) {
            engine$diagnostics$profile_top3_prob[pruning_result$dominated_indices] <- 0
            remaining_top3 <- engine$diagnostics$profile_top3_prob
            if (sum(remaining_top3) > 0) {
                engine$diagnostics$profile_top3_prob <- remaining_top3 / sum(remaining_top3)
            }
        }
    }

    engine
}

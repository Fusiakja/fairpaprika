# Active Pareto Set Tracking for PAPRIKA
# Maintains "potentially optimal set" during elicitation

#' Update Pareto (non-dominated) set after each decision
#'
#' Incrementally updates which profiles remain potentially optimal
#' based on current weight estimates. This provides real-time feedback
#' during elicitation without waiting for final computation.
#'
#' @param engine Paprika engine with profiles and current decisions
#' @return List with:
#'   - pareto_indices: Row indices of non-dominated profiles
#'   - dominated_indices: Row indices of dominated profiles
#'   - pareto_count: Number of profiles still in contention
#'   - eliminated_this_step: Profiles newly eliminated (since last update)
#' @keywords internal
paprika_update_pareto_set <- function(engine) {
    # Only applicable when profiles are set
    if (is.null(engine$profiles)) {
        return(list(
            pareto_indices = integer(0),
            dominated_indices = integer(0),
            pareto_count = 0,
            eliminated_this_step = integer(0)
        ))
    }

    n_profiles <- nrow(engine$profiles)

    # Need at least a few decisions to estimate utilities
    if (nrow(engine$decisions) < 2) {
        # Initially, all profiles are potentially optimal
        return(list(
            pareto_indices = 1:n_profiles,
            dominated_indices = integer(0),
            pareto_count = n_profiles,
            eliminated_this_step = integer(0)
        ))
    }

    # Try to compute weights from current decisions
    # Use a lightweight solve without full diagnostics
    tryCatch(
        {
            out <- solve_partworths(engine$domains, engine$decisions, engine$settings)

            if (!isTRUE(out$ok) || is.null(out$weights)) {
                # Can't compute yet, keep all profiles
                return(list(
                    pareto_indices = 1:n_profiles,
                    dominated_indices = integer(0),
                    pareto_count = n_profiles,
                    eliminated_this_step = integer(0)
                ))
            }

            # Compute utilities for each profile
            profile_utils <- numeric(n_profiles)

            for (p in 1:n_profiles) {
                idx <- engine$profiles_idx[p, ]
                profile_utils[p] <- sum(out$weights[idx], na.rm = TRUE)
            }

            # Check dominance based on criterion-level utilities
            # Build utility-by-criterion matrix
            criteria <- names(engine$domains)
            util_matrix <- matrix(0, nrow = n_profiles, ncol = length(criteria))
            colnames(util_matrix) <- criteria

            var_names <- names(out$weights)

            for (p in 1:n_profiles) {
                for (c in seq_along(criteria)) {
                    crit <- criteria[c]
                    level <- as.character(engine$profiles[p, crit])

                    # Find weight for this criterion:level
                    key <- paste0(crit, ":", level)
                    idx <- match(key, var_names)

                    if (!is.na(idx)) {
                        util_matrix[p, c] <- out$weights[idx]
                    } else {
                        util_matrix[p, c] <- 0
                    }
                }
            }

            # Find dominated profiles
            dominated <- integer(0)
            tol <- 1e-6

            for (i in 1:(n_profiles - 1)) {
                for (j in (i + 1):n_profiles) {
                    util_i <- util_matrix[i, ]
                    util_j <- util_matrix[j, ]

                    # Check if i dominates j
                    if (all(util_i >= util_j - tol) && any(util_i > util_j + tol)) {
                        dominated <- c(dominated, j)
                    }
                    # Check if j dominates i
                    else if (all(util_j >= util_i - tol) && any(util_j > util_i + tol)) {
                        dominated <- c(dominated, i)
                    }
                }
            }

            dominated <- unique(dominated)
            pareto <- setdiff(1:n_profiles, dominated)

            # Check what was eliminated since last update
            previous_pareto <- engine$diagnostics$pareto_set$pareto_indices %||% 1:n_profiles
            eliminated_this_step <- setdiff(previous_pareto, pareto)

            list(
                pareto_indices = pareto,
                dominated_indices = dominated,
                pareto_count = length(pareto),
                eliminated_this_step = eliminated_this_step,
                profile_utilities = profile_utils,
                utility_matrix = util_matrix
            )
        },
        error = function(e) {
            # On error, keep all profiles
            list(
                pareto_indices = 1:n_profiles,
                dominated_indices = integer(0),
                pareto_count = n_profiles,
                eliminated_this_step = integer(0)
            )
        }
    )
}

#' Get human-readable Pareto set summary
#'
#' @param engine Paprika engine with pareto set computed
#' @return Character string describing the current Pareto set
#' @export
paprika_pareto_summary <- function(engine) {
    if (is.null(engine$diagnostics$pareto_set)) {
        return("Pareto set not yet computed")
    }

    ps <- engine$diagnostics$pareto_set

    if (is.null(engine$profiles)) {
        return("No profiles set")
    }

    pareto_names <- rownames(engine$profiles)[ps$pareto_indices]

    if (length(pareto_names) == 0) {
        return("No profiles remain (error)")
    }

    msg <- sprintf(
        "Potentially optimal: %d/%d profiles",
        ps$pareto_count,
        nrow(engine$profiles)
    )

    if (ps$pareto_count <= 5) {
        msg <- paste0(msg, "\n  ", paste(pareto_names, collapse = ", "))
    }

    if (length(ps$eliminated_this_step) > 0) {
        eliminated_names <- rownames(engine$profiles)[ps$eliminated_this_step]
        msg <- paste0(msg, sprintf(
            "\n  Just eliminated: %s",
            paste(eliminated_names, collapse = ", ")
        ))
    }

    msg
}

# Classic PAPRIKA Question Selection (Corrected)
# Implements Hansen & Ombler's dominance-based pairwise ranking algorithm

#' Check if alternative a dominates alternative b
#'
#' a dominates b if a is at least as good on all criteria and strictly better on at least one
#'
#' @param a Named vector of levels for alternative a
#' @param b Named vector of levels for alternative b
#' @param domains Named list of criterion -> ordered levels (worst to best)
#' @return Logical: TRUE if a dominates b
#' @keywords internal
paprika_dominates <- function(a, b, domains) {
    criteria <- names(domains)
    at_least_as_good <- TRUE
    strictly_better_somewhere <- FALSE

    for (c in criteria) {
        levels <- domains[[c]]
        a_idx <- match(a[c], levels)
        b_idx <- match(b[c], levels)

        if (is.na(a_idx) || is.na(b_idx)) {
            stop(sprintf("Invalid level for criterion %s: a=%s, b=%s", c, a[c], b[c]))
        }

        if (a_idx < b_idx) {
            # a is worse than b on this criterion
            at_least_as_good <- FALSE
            break
        } else if (a_idx > b_idx) {
            # a is better than b on this criterion
            strictly_better_somewhere <- TRUE
        }
    }

    # a dominates b if it's at least as good everywhere AND strictly better somewhere
    at_least_as_good && strictly_better_somewhere
}

#' Generate all undominated two-criterion pairs for PAPRIKA
#'
#' True PAPRIKA: alternatives differ on EXACTLY 2 criteria, with one at worst level
#'
#' @param domains Named list of criterion -> ordered levels
#' @param criteria Character vector of criterion names
#' @return List of candidate question pairs
#' @keywords internal
paprika_generate_pairs <- function(domains, criteria) {
    pairs <- list()

    # Baseline: all criteria at worst level
    baseline <- vapply(criteria, function(c) domains[[c]][1], character(1))
    names(baseline) <- criteria

    # For each pair of criteria (i, j)
    for (i in 1:(length(criteria) - 1)) {
        for (j in (i + 1):length(criteria)) {
            ci <- criteria[i]
            cj <- criteria[j]

            levels_i <- domains[[ci]]
            levels_j <- domains[[cj]]

            # Original PAPRIKA: adjacent tradeoffs only (stage-2 "adjacent" cross)
            # For each adjacent level step on both criteria
            for (li in 2:length(levels_i)) {
                for (lj in 2:length(levels_j)) {
                    # Alternative A: level li on i, level lj-1 on j, baseline on rest
                    a <- baseline
                    a[ci] <- levels_i[li]
                    a[cj] <- levels_j[lj - 1]

                    # Alternative B: level li-1 on i, level lj on j, baseline on rest
                    b <- baseline
                    b[ci] <- levels_i[li - 1]
                    b[cj] <- levels_j[lj]

                    # Check dominance (should not dominate for valid PAPRIKA pairs)
                    if (!paprika_dominates(a, b, domains) && !paprika_dominates(b, a, domains)) {
                        pairs[[length(pairs) + 1]] <- list(
                            a = a,
                            b = b,
                            crit_pair = c(ci, cj),
                            label = sprintf(
                                "%s[%s vs %s] & %s[%s vs %s]",
                                ci, levels_i[li], levels_i[li - 1],
                                cj, levels_j[lj - 1], levels_j[lj]
                            )
                        )
                    }
                }
            }
        }
    }

    pairs
}

#' Build transitive closure of preferences
#'
#' Uses decisions to infer implied preferences via transitivity
#'
#' @param engine Paprika engine with decisions
#' @return List with preference graph (adjacency matrix)
#' @keywords internal
paprika_build_closure <- function(engine) {
    n_alts <- nrow(engine$alternatives)

    # Initialize preference matrix: pref[i,j] = 1 if i > j, -1 if j > i, 0 if equal/unknown
    pref_matrix <- matrix(0, nrow = n_alts, ncol = n_alts)

    decisions <- engine$decisions
    if (nrow(decisions) == 0) {
        return(list(matrix = pref_matrix, implied_count = 0))
    }

    # Build key->index mapping
    alt_keys <- engine$alt_keys
    if (is.null(alt_keys)) {
        # Fallback: generate keys from alternatives table matching decisions format
        alt_keys <- character(n_alts)
        for (i in 1:n_alts) {
            # Use the same function as engine_add_decision
            alt_keys[i] <- alt_to_string(engine$alternatives[i, , drop = FALSE], engine$criteria)
        }
    }

    key_to_idx <- setNames(1:n_alts, alt_keys)

    # Populate direct preferences
    for (d in seq_len(nrow(decisions))) {
        a_key <- as.character(decisions$A1[d])
        b_key <- as.character(decisions$A2[d])
        pref <- as.character(decisions$pref[d])

        i <- key_to_idx[[a_key]]
        j <- key_to_idx[[b_key]]

        if (is.null(i) || is.null(j) || is.na(i) || is.na(j)) {
            warning(sprintf("Unknown alternative key: %s or %s", a_key, b_key))
            next
        }

        if (pref == "A") {
            pref_matrix[i, j] <- 1
            pref_matrix[j, i] <- -1
        } else if (pref == "B") {
            pref_matrix[i, j] <- -1
            pref_matrix[j, i] <- 1
        } else if (pref == "E") {
            # Equal preferences don't create ordering
            pref_matrix[i, j] <- 0
            pref_matrix[j, i] <- 0
        }
    }

    # Floyd-Warshall for transitive closure
    # If i > j and j > k, then i > k
    for (k in 1:n_alts) {
        for (i in 1:n_alts) {
            for (j in 1:n_alts) {
                if (pref_matrix[i, k] == 1 && pref_matrix[k, j] == 1) {
                    if (pref_matrix[i, j] == 0) {
                        pref_matrix[i, j] <- 1
                        pref_matrix[j, i] <- -1
                    }
                }
            }
        }
    }

    # Count implied preferences
    implied_count <- sum(pref_matrix != 0) / 2 - nrow(decisions)

    list(matrix = pref_matrix, implied_count = max(0, implied_count))
}

#' Check if a preference is implied by transitivity
#'
#' @param engine Paprika engine with decisions
#' @param i Index of alternative a
#' @param j Index of alternative b
#' @param closure Pre-computed closure (optional, will compute if NULL)
#' @return Character: "A", "B", "E", or NULL if unknown
#' @keywords internal
paprika_implied_preference <- function(engine, i, j, closure = NULL) {
    if (is.null(closure)) {
        closure <- paprika_build_closure(engine)
    }

    pref_val <- closure$matrix[i, j]

    if (pref_val == 1) {
        return("A")
    }
    if (pref_val == -1) {
        return("B")
    }
    if (pref_val == 0) {
        # Check if it was explicitly answered as "E"
        # Get alternative keys for comparison
        alt_keys <- engine$alt_keys
        if (is.null(alt_keys)) {
            alt_keys <- character(nrow(engine$alternatives))
            for (idx in seq_len(nrow(engine$alternatives))) {
                alt_keys[idx] <- alt_to_string(engine$alternatives[idx, , drop = FALSE], engine$criteria)
            }
        }

        key_i <- alt_keys[i]
        key_j <- alt_keys[j]

        decisions <- engine$decisions
        for (d in seq_len(nrow(decisions))) {
            if (decisions$pref[d] == "E") {
                if ((decisions$A1[d] == key_i && decisions$A2[d] == key_j) ||
                    (decisions$A1[d] == key_j && decisions$A2[d] == key_i)) {
                    return("E")
                }
            }
        }
    }

    NULL
}

#' Initialize PAPRIKA question bank for classic mode
#'
#' @param engine Paprika engine
#' @return Updated engine with PAPRIKA candidate bank
#' @export
paprika_init_classic_bank <- function(engine) {
    if (!identical(engine$settings$mode, "classic")) {
        return(engine)
    }

    # Generate all undominated two-criterion pairs
    pairs <- paprika_generate_pairs(engine$domains, engine$criteria)

    # Convert to candidate format compatible with existing code
    candidates <- list()

    # Create alternative lookup
    alt_lookup <- function(levels_vec) {
        for (i in seq_len(nrow(engine$alternatives))) {
            if (all(engine$alternatives[i, ] == levels_vec)) {
                return(i)
            }
        }
        NA_integer_
    }

    for (pair in pairs) {
        i <- alt_lookup(pair$a)
        j <- alt_lookup(pair$b)

        if (!is.na(i) && !is.na(j)) {
            candidates[[length(candidates) + 1]] <- list(
                i = i,
                j = j,
                type = "paprika_pair",
                label = pair$label,
                crit_pair = paste(pair$crit_pair, collapse = "_")
            )
        }
    }

    engine$candidates <- candidates
    engine$paprika <- list(
        total_pairs = length(candidates),
        asked = 0,
        implied = 0,
        closure = NULL
    )

    engine
}

#' Filter PAPRIKA bank by dominance and transitivity
#'
#' Removes questions that can be inferred from previous answers
#'
#' @param engine Paprika engine
#' @return Updated engine with filtered candidate bank
#' @keywords internal
paprika_filter_bank <- function(engine) {
    if (is.null(engine$candidates) || length(engine$candidates) == 0) {
        return(engine)
    }

    # Build transitive closure
    closure <- paprika_build_closure(engine)

    # Update paprika diagnostics
    if (!is.null(engine$paprika)) {
        engine$paprika$closure <- closure
        engine$paprika$asked <- nrow(engine$decisions)
    }

    filtered <- list()
    implied_count <- 0

    for (cand in engine$candidates) {
        # Check if preference is implied via closure
        pref <- paprika_implied_preference(engine, cand$i, cand$j, closure)

        if (is.null(pref)) {
            # Not implied, keep it
            filtered[[length(filtered) + 1]] <- cand
        } else {
            implied_count <- implied_count + 1
        }
    }

    if (!is.null(engine$paprika)) {
        engine$paprika$implied <- implied_count
    }

    engine$candidates <- filtered
    engine
}

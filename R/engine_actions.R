#' Add a decision to the engine
#'
#' Stores the respondent's preference for the current question and advances
#' engine state. In full mode, this also updates cached polytope samples and
#' implied ranking closure.
#'
#' @param engine A `paprika_engine` with a current question set.
#' @param pref Character: one of `"A"`, `"B"`, or `"E"` (equal).
#' @return Updated `paprika_engine` with the decision recorded.
#' @export
engine_add_decision <- function(engine, pref) {
  engine <- validate_engine(engine)
  if (is.null(engine$current)) stop("No current question. Call engine_next_question() first.")
  pref <- match.arg(pref, c("A", "B", "E"))

  A1 <- alt_to_string(engine$current$a, engine$criteria)
  A2 <- alt_to_string(engine$current$b, engine$criteria)

  if (pref == "A") {
    engine$decisions <- rbind(engine$decisions, data.frame(A1 = A1, A2 = A2, pref = "A"))
  } else if (pref == "B") {
    # flip so "A" always means first is preferred in stored direction
    engine$decisions <- rbind(engine$decisions, data.frame(A1 = A2, A2 = A1, pref = "A"))
  } else {
    engine$decisions <- rbind(engine$decisions, data.frame(A1 = A1, A2 = A2, pref = "E"))
  }

  # Update implied rankings (transitive closure) and invalidate caches (full mode)
  if (!is.null(engine$closure) && !is.null(engine$current$i) && !is.null(engine$current$j)) {
    i <- engine$current$i
    j <- engine$current$j
    upd <- NULL
    if (pref == "A") {
      upd <- .fp_closure_add_edge(engine$closure, i, j)
    } else if (pref == "B") {
      upd <- .fp_closure_add_edge(engine$closure, j, i)
    }
    if (!is.null(upd)) {
      engine$closure <- upd$reach
      if (isTRUE(upd$conflict)) engine$diagnostics$closure_conflict <- TRUE
    }
  }
  if (identical(engine$settings$mode, "full")) {
    if (!is.null(engine$cache)) {
      engine$cache$samples <- NULL
      engine$cache$n_decisions <- -1L
      engine$cache$outcomes <- NULL
      engine$cache$implied <- NULL
    }
  }


  engine$current <- NULL
  # Update audit with coverage/balance metrics after decision
  if (length(engine$audit)) {
    needed <- .fp_needed_pairs(engine$criteria)
    counts <- .fp_pair_counts(engine)
    coverage_rate <- if (length(needed)) sum(counts[needed] > 0, na.rm = TRUE) / length(needed) else NA_real_
    gap <- if (length(counts)) max(counts) - min(counts) else NA_real_
    gini <- function(x) {
      x <- x[is.finite(x)]
      if (!length(x)) {
        return(NA_real_)
      }
      n <- length(x)
      mu <- mean(x)
      if (mu == 0) {
        return(0)
      }
      sum(abs(outer(x, x, "-"))) / (2 * n^2 * mu)
    }
    gini_val <- gini(as.numeric(counts))
    idx <- length(engine$audit)
    engine$audit[[idx]]$coverage_rate <- coverage_rate
    engine$audit[[idx]]$exposure_gap <- gap
    engine$audit[[idx]]$exposure_gini <- gini_val

    cov_curve <- vapply(engine$audit, function(x) x$coverage_rate %||% NA_real_, numeric(1))
    gap_curve <- vapply(engine$audit, function(x) x$exposure_gap %||% NA_real_, numeric(1))
    gini_curve <- vapply(engine$audit, function(x) x$exposure_gini %||% NA_real_, numeric(1))
    # interaction coverage/exposure
    interaction_pairs <- character()
    if (isTRUE(engine$settings$interactions$enabled %||% TRUE) && isTRUE(engine$interactions_active)) {
      if (!is.null(engine$interactions_pairs_active) && length(engine$interactions_pairs_active)) {
        interaction_pairs <- unique(vapply(engine$interactions_pairs_active, function(p) paste(sort(p), collapse = "::"), character(1)))
      } else {
        interaction_pairs <- unique(vapply(engine$settings$interactions$pairs %||% list(), function(p) paste(sort(p), collapse = "::"), character(1)))
      }
    }
    counts_inter <- if (length(interaction_pairs)) counts[interaction_pairs] else integer()
    if (length(counts_inter)) counts_inter[is.na(counts_inter)] <- 0L
    coverage_inter <- if (length(interaction_pairs)) sum(counts_inter > 0, na.rm = TRUE) / length(interaction_pairs) else NA_real_
    gap_inter <- if (length(counts_inter)) max(counts_inter, na.rm = TRUE) - min(counts_inter, na.rm = TRUE) else NA_real_
    gini_inter <- if (length(counts_inter)) gini(as.numeric(counts_inter)) else NA_real_

    dom_rate <- NA_real_
    if (!is.null(engine$decisions) && nrow(engine$decisions)) {
      diffs <- vapply(seq_len(nrow(engine$decisions)), function(i) .paprika_count_diffs(engine$decisions$A1[i], engine$decisions$A2[i]), integer(1))
      strict_two <- sum(engine$decisions$pref == "A" & diffs == 2, na.rm = TRUE)
      dom_rate <- strict_two / nrow(engine$decisions)
    }

    order_sens <- NA_real_
    topk_vec <- vapply(engine$audit, function(x) x$top_k %||% NA_real_, numeric(1))
    pick_idx <- vapply(engine$audit, function(x) x$pick_idx %||% NA_real_, numeric(1))
    if (length(topk_vec)) {
      rnd_flags <- topk_vec > 1 & pick_idx > 1
      order_sens <- mean(rnd_flags, na.rm = TRUE)
      if (!is.finite(order_sens)) order_sens <- NA_real_
    }
    interaction_questions <- sum(vapply(engine$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
    balance_relaxed_steps <- sum(vapply(engine$audit, function(x) isTRUE(x$balance_relaxed), logical(1)))
    forced_interactions <- sum(vapply(engine$audit, function(x) isTRUE(x$forced_interaction), logical(1)))
    interaction_share <- if (nrow(engine$decisions) > 0) interaction_questions / nrow(engine$decisions) else NA_real_
    fairness_reasons <- vapply(engine$audit, function(x) x$fairness_relaxed_reason %||% NA_character_, character(1))
    fairness_reasons <- fairness_reasons[!is.na(fairness_reasons)]
    fairness_relaxed_counts <- if (length(fairness_reasons)) as.list(table(fairness_reasons)) else list()

    # collapse sampling warnings if present
    samp_warn <- engine$diagnostics$sampling_warnings %||% character()
    if (length(samp_warn)) samp_warn <- unique(samp_warn)

    engine$diagnostics$audit_run <- list(
      coverage_curve = cov_curve,
      exposure_gap_curve = gap_curve,
      exposure_gini_curve = gini_curve,
      interaction_coverage = coverage_inter,
      interaction_gap = gap_inter,
      interaction_gini = gini_inter,
      interaction_share = interaction_share,
      interaction_budget = list(
        max_share = engine$settings$interactions$mix$max_share %||% NA_real_,
        window = engine$settings$interactions$mix$window %||% NA_integer_
      ),
      dominance_rate = dom_rate,
      order_sensitivity = order_sens,
      balance_relaxed = any(vapply(engine$audit, function(x) isTRUE(x$balance_relaxed), logical(1))),
      balance_relaxed_steps = balance_relaxed_steps,
      interaction_questions = interaction_questions,
      forced_interactions = forced_interactions,
      fairness_relaxed_counts = fairness_relaxed_counts,
      slack_revisit = engine$diagnostics$slack_revisit %||% NULL,
      seeds = engine$seed,
      path_dependency = order_sens,
      sampling_warnings = samp_warn
    )
  }

  # Update Pareto set for classic mode with profiles
  if (identical(engine$settings$mode, "classic") && !is.null(engine$profiles)) {
    pareto_result <- paprika_update_pareto_set(engine)
    engine$diagnostics$pareto_set <- pareto_result
  }

  engine
}

#' Undo the most recent decisions
#'
#' Removes the last `n` recorded decisions (and matching audit entries), clears
#' cached samples/weights, and rebuilds the implied-ranking closure.
#'
#' @param engine A `paprika_engine`.
#' @param n Integer, how many decisions to undo (default: 1).
#' @return Updated `paprika_engine` with decisions removed.
#' @export
engine_undo_decisions <- function(engine, n = 1L) {
  engine <- validate_engine(engine)
  n <- as.integer(n)
  if (is.na(n) || n <= 0) {
    return(engine)
  }
  k <- nrow(engine$decisions)
  if (k == 0) {
    return(engine)
  }
  drop_n <- min(k, n)

  # Drop decisions and matching audit/used-pair entries
  if (drop_n == k) {
    engine$decisions <- decisions_empty()
  } else {
    engine$decisions <- engine$decisions[seq_len(k - drop_n), , drop = FALSE]
  }
  if (length(engine$audit)) {
    keep_audit <- max(0L, length(engine$audit) - drop_n)
    engine$audit <- if (keep_audit) engine$audit[seq_len(keep_audit)] else list()
  }
  if (length(engine$used_pairs)) {
    keep_used <- max(0L, length(engine$used_pairs) - drop_n)
    engine$used_pairs <- if (keep_used) engine$used_pairs[seq_len(keep_used)] else character()
  }

  # Clear current state and caches
  engine$current <- NULL
  engine$last_pick <- NULL
  engine$weights <- NULL
  engine$diagnostics$solve_status <- NULL
  engine$diagnostics$audit_run <- NULL
  engine$diagnostics$undo_count <- (engine$diagnostics$undo_count %||% 0L) + drop_n
  engine$diagnostics$undo_since_compute <- TRUE
  engine$diagnostics$undo_epoch <- (engine$diagnostics$undo_epoch %||% 0L) + 1L
  if (!is.null(engine$cache)) {
    engine$cache$samples <- NULL
    engine$cache$n_decisions <- -1L
    engine$cache$outcomes <- NULL
    engine$cache$implied <- NULL
  }
  engine$stop_state <- new.env(parent = emptyenv())

  # Rebuild closure from remaining decisions
  if (!is.null(engine$closure)) {
    closure_cfg <- engine$settings$closure %||% list()
    use_dominance <- if (identical(engine$settings$mode, "classic")) FALSE else (closure_cfg$dominance %||% TRUE)
    closure <- .fp_closure_init(nrow(engine$alternatives))
    dom_upd <- if (isTRUE(use_dominance)) .fp_closure_add_dominance(closure, engine$alternatives, engine$domains) else list(reach = closure, conflict = FALSE, added = 0L)
    closure <- dom_upd$reach
    closure_conflict <- isTRUE(dom_upd$conflict)
    engine$diagnostics$closure_dominance_edges <- dom_upd$added %||% 0L
    if (closure_conflict) engine$diagnostics$closure_conflict <- TRUE else engine$diagnostics$closure_conflict <- NULL

    if (nrow(engine$decisions)) {
      for (i in seq_len(nrow(engine$decisions))) {
        d <- engine$decisions[i, , drop = FALSE]
        ia <- match(d$A1, engine$alt_keys)
        ib <- match(d$A2, engine$alt_keys)
        if (is.na(ia) || is.na(ib)) next
        if (d$pref == "A") {
          upd <- .fp_closure_add_edge(closure, ia, ib)
        } else {
          upd <- NULL
        }
        if (!is.null(upd)) {
          closure <- upd$reach
          if (isTRUE(upd$conflict)) engine$diagnostics$closure_conflict <- TRUE
        }
      }
    }
    engine$closure <- closure
  }

  engine
}

#' Check whether the engine has finished asking questions
#'
#' Evaluates stopping criteria for both classic and full modes, including
#' question budget, minimum questions, pair coverage (if enabled), and full-mode
#' stopping logic based on polytope sampling.
#'
#' @param engine A `paprika_engine`.
#' @return `TRUE` if no further questions should be asked, otherwise `FALSE`.
#' @export
engine_done <- function(engine) {
  engine <- validate_engine(engine)
  s <- engine$settings
  n <- nrow(engine$decisions)
  st <- engine$stop_state %||% new.env(parent = emptyenv())

  # Strict PAPRIKA mode: use original profile-resolution stopping
  strict_mode <- isTRUE(s$classic$strict_paprika) && s$mode == "classic"
  if (strict_mode) {
    # Original PAPRIKA: stop when all profile pairs are ordered by computed utilities
    if (!is.null(engine$profiles) && nrow(engine$profiles) > 0) {
      if (isTRUE(paprika_all_profiles_resolved(engine))) {
        .engine_set_stop_state(st, reason = "strict_profiles_resolved", n = n)
        return(TRUE)
      }
      return(FALSE)
    }
    # Fallback: bank exhaustion
    if (s$mode == "classic") {
      if (length(engine$candidates) == 0) {
        .engine_set_stop_state(st, reason = "classic_bank_exhausted", n = n)
        return(TRUE)
      }
      return(FALSE)
    }
  }

  # Enhanced mode: check nested stop$min_q first, fallback to top-level
  min_q_threshold <- s$stop$min_q %||% s$min_q
  if (n < min_q_threshold) {
    return(FALSE)
  }

  # For full mode, delegate to engine_should_stop for sophisticated criteria
  if (identical(s$mode, "full")) {
    return(engine_should_stop(engine))
  }

  # Classic mode: use bank exhaustion + fairness coverage
  if (isTRUE(s$fair$enabled) && isTRUE(s$fair$pair_coverage)) {
    h <- engine_health(engine)
    if (!isTRUE(h$pairs_covered)) {
      return(FALSE)
    }
  }

  # Do not stop just because min_q is reached; keep going while questions remain
  if (engine_has_more_questions(engine)) {
    return(FALSE)
  }

  .engine_set_stop_state(st, reason = "classic_bank_exhausted", n = n)
  TRUE
}

# R/full_paprika_selector.R
#
# Full-mode question selection:
# - sample the feasible polytope (Hit-and-Run)
# - score candidate trade-offs by expected information gain (IG)
# - incorporate procedural fairness via pair-coverage constraints/bonuses
#
# Stop criteria:
# - robust winner probability (if profiles are registered)
# - otherwise: backward-compatible (min_q + pair coverage + budget)

.fp_entropy <- function(p) {
  p <- p[p > 0]
  if (!length(p)) {
    return(0)
  }
  -sum(p * log(p))
}

.fp_predict_outcome <- function(delta, tau_equal, eps_strict) {
  # 1=A, 2=B, 3=E (no NA gray zone: tie if within tau_equal)
  # Note: eps_strict is the LP constraint margin; tau_equal governs the response model / IG bins.
  out <- rep(3L, length(delta))
  out[delta >= tau_equal] <- 1L
  out[delta <= -tau_equal] <- 2L
  out
}

.fp_samples_winners <- function(W, profiles_idx, profiles_idx_full = NULL) {
  # Use full indices if available, fallback to additive-only (matrix)
  prof_idx <- profiles_idx_full %||% lapply(seq_len(nrow(profiles_idx)), function(p) profiles_idx[p, ])
  P <- if (is.list(prof_idx)) length(prof_idx) else nrow(profiles_idx)
  util <- matrix(0, nrow = nrow(W), ncol = P)
  if (is.list(prof_idx)) {
    for (p in seq_len(P)) util[, p] <- rowSums(W[, prof_idx[[p]], drop = FALSE])
  } else {
    for (p in seq_len(P)) util[, p] <- rowSums(W[, prof_idx[p, ], drop = FALSE])
  }
  max.col(util, ties.method = "first")
}

.fp_samples_topk <- function(W, profiles_idx, k, profiles_idx_full = NULL) {
  if (is.null(dim(W))) W <- matrix(W, nrow = 1)
  # Use full indices if available, fallback to additive-only (matrix)
  prof_idx <- profiles_idx_full %||% lapply(seq_len(nrow(profiles_idx)), function(p) profiles_idx[p, ])
  P <- if (is.list(prof_idx)) length(prof_idx) else nrow(profiles_idx)
  S <- nrow(W)
  if (is.null(dim(profiles_idx)) || P == 0 || S == 0) {
    return(matrix(0, nrow = P, ncol = S))
  }
  util <- matrix(0, nrow = S, ncol = P)
  if (is.list(prof_idx)) {
    for (p in seq_len(P)) util[, p] <- rowSums(W[, prof_idx[[p]], drop = FALSE])
  } else {
    for (p in seq_len(P)) util[, p] <- rowSums(W[, prof_idx[p, ], drop = FALSE])
  }
  topk_mat <- matrix(0, nrow = P, ncol = S)
  for (s in seq_len(S)) {
    ord <- order(util[s, ], decreasing = TRUE)
    if (!length(ord)) next
    top <- ord[seq_len(min(k, length(ord)))]
    top <- top[top >= 1 & top <= nrow(topk_mat)]
    if (!length(top)) next
    topk_mat[top, s] <- 1
  }
  topk_mat
}

.fp_winner_entropy <- function(winners, P) {
  p <- tabulate(winners, nbins = P) / length(winners)
  .fp_entropy(p)
}

.fp_candidate_eig <- function(delta, winners, tau_equal, eps_strict) {
  # Expected reduction in winner-entropy after asking (A,B)
  out <- .fp_predict_outcome(delta, tau_equal, eps_strict)
  keep <- !is.na(out)
  if (!any(keep)) {
    return(0)
  }

  S <- sum(keep)
  P <- max(winners)
  H0 <- .fp_winner_entropy(winners, P)

  Hpost <- 0

  for (o in 1:3) {
    idx <- which(out[keep] == o)
    if (!length(idx)) next
    p_o <- length(idx) / S
    H_o <- .fp_winner_entropy(winners[keep][idx], P)
    Hpost <- Hpost + p_o * H_o
  }
  H0 - Hpost
}

.fp_candidate_eig_topk <- function(delta, topk_mat, tau_equal, eps_strict, k = 3) {
  out <- .fp_predict_outcome(delta, tau_equal, eps_strict)
  keep <- !is.na(out)
  if (!any(keep)) {
    return(0)
  }

  S <- sum(keep)
  if (S == 0) {
    return(0)
  }
  topk_keep <- topk_mat[, keep, drop = FALSE]
  base_prob <- rowMeans(topk_keep)
  H0 <- .fp_entropy(base_prob)

  Hpost <- 0
  for (o in 1:3) {
    idx <- which(out[keep] == o)
    if (!length(idx)) next
    p_o <- length(idx) / S
    topk_o <- rowMeans(topk_keep[, idx, drop = FALSE])
    H_o <- .fp_entropy(topk_o)
    Hpost <- Hpost + p_o * H_o
  }
  H0 - Hpost
}

.fp_entropy_se <- function(p, n) {
  # delta-method SE for entropy of categorical p with sample size n
  if (n <= 0 || !length(p)) {
    return(NA_real_)
  }
  p <- p[p > 0]
  if (!length(p)) {
    return(NA_real_)
  }
  g <- -(1 + log(p))
  var <- sum(p * (1 - p) * g^2)
  for (i in seq_along(p)) {
    for (j in seq_along(p)) {
      if (i != j) {
        var <- var - p[i] * p[j] * g[i] * g[j]
      }
    }
  }
  sqrt(max(var, 0)) / sqrt(n)
}

.fp_needed_pairs <- function(criteria) {
  out <- character()
  for (i in 1:(length(criteria) - 1)) {
    for (j in (i + 1):length(criteria)) {
      out <- c(out, paste(sort(c(criteria[i], criteria[j])), collapse = "::"))
    }
  }
  out
}

.fp_pair_counts <- function(engine) {
  d <- engine$decisions
  # initialize with all needed pairs to avoid missing/NA names downstream
  needed <- .fp_needed_pairs(engine$criteria)
  counts <- setNames(integer(length(needed)), needed)
  if (is.null(d) || !nrow(d)) {
    return(counts)
  }

  for (r in seq_len(nrow(d))) {
    cc <- .paprika_diff_criteria(d$A1[r], d$A2[r])
    if (length(cc) != 2) next
    key <- paste(sort(cc), collapse = "::")
    val <- counts[key] %||% 0L
    if (is.na(val)) val <- 0L
    counts[key] <- val + 1L
  }
  counts
}

.fp_uncovered_exists <- function(needed, counts) {
  # A pair is uncovered if it is missing or has count 0.
  if (is.null(needed) || !length(needed)) {
    return(FALSE)
  }
  vals <- counts[needed]
  vals[is.na(vals)] <- 0L
  any(vals == 0L)
}

.fp_coverage_bonus <- function(count) {
  1 / (1 + count) # higher when count is low
}

.fp_exposure_balance_allow <- function(counts, cp, q_total, gap_limit = 1) {
  # Hard-ish quotas:
  # - Until balanced, avoid widening exposure gaps beyond gap_limit.
  # - Encourage each pair to reach floor(q_total / num_pairs) - 1.
  all_pairs <- names(counts)
  if (is.null(all_pairs) || !length(all_pairs)) {
    return(TRUE)
  }
  npairs <- length(all_pairs)
  ccount <- counts[cp] %||% 0L
  if (is.na(ccount)) ccount <- 0L

  target_min <- max(0L, floor(q_total / npairs) - 1L)
  under_target <- counts < target_min
  if (any(under_target, na.rm = TRUE) && ccount >= target_min) {
    return(FALSE)
  }

  min_c <- min(counts, na.rm = TRUE)
  max_c <- max(counts, na.rm = TRUE)
  spread <- max_c - min_c
  if ((spread > gap_limit) && ccount >= max_c) {
    return(FALSE)
  }

  TRUE
}

.fp_fairness_gain <- function(count, max_count) {
  coverage <- if (count == 0L) 1 else 0
  exposure <- if (max_count > 0) (max_count - count) / max_count else 1
  coverage + exposure
}

.fp_topk_pair_counts <- function(profiles, top_idx, criteria) {
  counts <- integer()
  if (length(top_idx) <= 1) {
    return(counts)
  }

  for (i in 1:(length(top_idx) - 1)) {
    for (j in (i + 1):length(top_idx)) {
      pi <- top_idx[i]
      pj <- top_idx[j]
      diffs <- diff_criteria(profiles[pi, , drop = FALSE], profiles[pj, , drop = FALSE], criteria)
      if (length(diffs) < 2) next
      pairs <- combn(diffs, 2, simplify = FALSE)
      for (p in pairs) {
        key <- paste(sort(p), collapse = "::")
        val <- counts[key] %||% 0L
        if (is.na(val)) val <- 0L
        counts[key] <- val + 1L
      }
    }
  }
  counts
}

.fp_utility_gain <- function(cp, pair_counts) {
  if (is.null(pair_counts) || !length(pair_counts)) {
    return(0)
  }
  max_count <- max(pair_counts)
  if (max_count <= 0) {
    return(0)
  }
  count_val <- pair_counts[cp]
  if (is.na(count_val)) count_val <- 0
  count_val / max_count
}

.fp_implied_constraints <- function(reach, i, j) {
  if (is.null(reach) || is.na(i) || is.na(j)) {
    return(0L)
  }
  pred <- which(reach[, i])
  succ <- which(reach[j, ])
  if (!length(pred) || !length(succ)) {
    return(0L)
  }
  new_edges <- !reach[pred, succ, drop = FALSE]
  sum(new_edges)
}

.fp_cost_penalty <- function(reach, i, j) {
  implied <- .fp_implied_constraints(reach, i, j)
  1 / (1 + implied)
}

.fp_implied_proxy <- function(reach, i, j, counts, cp) {
  if (!is.null(reach) && nrow(reach) >= i && ncol(reach) >= j) {
    # Degree centrality on unresolved edges
    deg_i <- sum(!reach[i, ])
    deg_j <- sum(!reach[, j])
    # Triad count approximation: neighbors of i vs j unresolved
    neigh_i <- which(!reach[i, ])
    neigh_j <- which(!reach[, j])
    triad <- length(setdiff(intersect(neigh_i, neigh_j), c(i, j)))
    return(deg_i + deg_j + triad)
  }
  if (is.null(counts) || !length(counts)) {
    return(0)
  }
  ccount <- counts[cp] %||% 0
  max_c <- max(counts)
  max(max_c - ccount, 0)
}

.fp_interaction_entropy <- function(weights, var_names, pair) {
  if (is.null(weights) || is.null(var_names)) {
    return(0)
  }
  # interaction vars are named ci::li::cj::lj (both orders possible)
  pat1 <- paste0("^", pair[1], "::.*::", pair[2], "::")
  pat2 <- paste0("^", pair[2], "::.*::", pair[1], "::")
  vars <- var_names[grepl(pat1, var_names) | grepl(pat2, var_names)]
  if (!length(vars)) {
    return(0)
  }
  idx <- match(vars, var_names)
  if (any(is.na(idx))) {
    return(0)
  }
  w <- weights
  if (is.null(dim(w))) {
    # single weight vector -> make 1-row matrix
    w <- matrix(w, nrow = 1L, dimnames = list(NULL, var_names))
  }
  w <- w[, idx, drop = FALSE]
  sign_cat <- apply(w, 2, function(x) {
    s <- sign(x)
    c(
      pos = mean(s > 0),
      neg = mean(s < 0),
      zero = mean(abs(x) < 1e-8)
    )
  })
  p <- rowMeans(sign_cat)
  .fp_entropy(p)
}

.fp_maybe_activate_interactions <- function(engine, samples) {
  if (!isTRUE(engine$settings$interactions$enabled)) {
    return(engine)
  }
  if (isTRUE(engine$interactions_active)) {
    return(engine)
  }
  # Ensure pairs is a list before using it
  pairs_all <- engine$settings$interactions$pairs
  if (!is.list(pairs_all)) {
    pairs_all <- list()
  }
  if (!length(pairs_all)) {
    return(engine)
  }
  trig <- engine$settings$interactions$activate %||% list()
  n_q <- nrow(engine$decisions)
  if (is.finite(trig$min_questions %||% NA_real_) && n_q < (trig$min_questions %||% 0)) {
    return(engine)
  }

  trigger <- FALSE
  if (isTRUE(trig$slack) && isTRUE(engine$diagnostics$slack_flag)) trigger <- TRUE

  if (!trigger && !is.null(samples)) {
    # winner entropy trigger (needs profiles)
    if (!is.null(engine$profiles_idx) && !is.null(samples$weights)) {
      winners <- .fp_samples_winners(samples$weights, engine$profiles_idx, engine$profiles_idx_full)
      H <- .fp_winner_entropy(winners, nrow(engine$profiles_idx))
      thr <- trig$winner_entropy %||% NA_real_
      if (isTRUE(is.finite(thr) && is.finite(H) && H >= thr)) trigger <- TRUE
    }
  }

  if (trigger) {
    max_pairs <- engine$settings$interactions$max_pairs %||% length(pairs_all)
    engine$interactions_active <- TRUE
    engine$interactions_pairs_active <- pairs_all[seq_len(min(length(pairs_all), max_pairs))]
    engine$diagnostics$interactions_triggered <- list(
      at_questions = n_q,
      reason = if (isTRUE(engine$diagnostics$slack_flag)) "slack" else "entropy",
      active_pairs = engine$interactions_pairs_active
    )
  }
  engine
}

#' Full-mode tradeoff picker (polytope-sampling + IG + fairness)
#' @keywords internal
engine_pick_tradeoff_polytope <- function(engine, samples = NULL) {
  engine <- validate_engine(engine)
  s <- engine$settings
  use_closure <- !isTRUE(engine$diagnostics$slack_flag)

  # Interaction activation is controlled by .fp_maybe_activate_interactions
  # based on entropy/slack triggers. Don't forcibly enable here.

  if (is.null(engine$candidates) || !length(engine$candidates)) {
    return(NULL)
  }
  if (is.null(engine$alt_var_idx) && is.null(engine$alt_var_idx_full)) stop("Full mode requires engine$alt_var_idx (constructed in engine_create).")

  # samples
  if (is.null(samples)) {
    if (!is.null(engine$posterior_samples)) {
      samples <- engine$posterior_samples
    } else {
      sel <- s$selector
      samples <- tryCatch(
        engine_polytope_sample(engine,
          n = sel$n_samples,
          burnin = sel$burnin,
          thin = sel$thin,
          seed = engine$seed
        ),
        error = function(e) NULL
      )
    }
  }
  if (is.null(samples)) {
    return(NULL)
  }
  # Use scaled weights for consistency with thresholds and reporting
  W_full <- samples$weights_scaled %||% samples$w_scaled %||% samples$weights
  use_profiles <- !is.null(engine$profiles_idx)
  topk_mat_full <- NULL
  if (use_profiles) {
    topk_mat_full <- .fp_samples_topk(W_full, engine$profiles_idx, k = 3, profiles_idx_full = engine$profiles_idx_full)
  }

  # fairness (pair coverage)
  needed <- .fp_needed_pairs(engine$criteria)
  counts <- .fp_pair_counts(engine)
  if (length(needed)) {
    counts <- counts[needed]
    counts[is.na(counts)] <- 0L
  }
  interaction_pairs <- character()
  if (isTRUE(engine$interactions_active)) {
    if (!is.null(engine$interactions_pairs_active) && length(engine$interactions_pairs_active)) {
      interaction_pairs <- unique(vapply(engine$interactions_pairs_active, function(p) paste(sort(p), collapse = "::"), character(1)))
    } else if (isTRUE(s$interactions$enabled %||% TRUE)) {
      # Ensure pairs is actually a list (not FALSE or other scalar)
      pairs_val <- s$interactions$pairs
      if (is.list(pairs_val) && length(pairs_val) > 0) {
        interaction_pairs <- unique(vapply(pairs_val, function(p) paste(sort(p), collapse = "::"), character(1)))
      }
    }
  }

  # Count ONLY interaction questions for interaction coverage (not general pair counts)
  counts_inter <- if (length(interaction_pairs)) {
    interaction_counts <- setNames(rep(0L, length(interaction_pairs)), interaction_pairs)
    for (audit_item in engine$audit) {
      if (isTRUE(audit_item$interaction_pair) && !is.null(audit_item$crit_pair)) {
        cp <- audit_item$crit_pair
        if (cp %in% names(interaction_counts)) {
          interaction_counts[cp] <- interaction_counts[cp] + 1L
        }
      }
    }
    interaction_counts
  } else {
    integer()
  }

  max_pair_count <- if (length(counts)) max(counts) else 0L
  uncovered_exists <- isTRUE(s$fair$enabled) && isTRUE(s$fair$pair_coverage) && .fp_uncovered_exists(needed, counts)
  # Soft interaction coverage: track but do not hard-filter
  asked_interactions <- sum(vapply(engine$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
  min_interactions <- as.integer(s$interactions$min_questions %||% 0L)
  # Note: inter_idx is defined only after candidate pool is built; refine bound later using available interactions
  interaction_uncovered_soft <- isTRUE(s$fair$enabled) && length(interaction_pairs) && isTRUE(s$fair$interaction_coverage) && .fp_uncovered_exists(interaction_pairs, counts_inter)
  # Combine min_questions quota with interaction coverage
  uncovered_inter <- isTRUE(engine$interactions_active) && length(interaction_pairs) &&
    ((asked_interactions < min_interactions) || interaction_uncovered_soft)
  enforce_balance <- isTRUE(s$fair$enabled) && isTRUE(s$fair$exposure_balance)
  gap_limit <- s$fair$exposure_gap_limit %||% 1
  fairness_stage_relaxed <- isTRUE(s$fair$enabled) &&
    isTRUE(s$fair$anneal_after_coverage) &&
    !uncovered_exists &&
    !interaction_uncovered_soft

  score_cfg <- s$selector$score %||% list()
  alpha <- score_cfg$alpha %||% 1
  beta <- score_cfg$beta
  if (is.null(beta)) beta <- s$selector$fairness_lambda %||% 0
  gamma <- score_cfg$gamma %||% 0
  delta_cost <- score_cfg$delta %||% 0
  kappa <- score_cfg$kappa %||% 0
  zeta <- score_cfg$zeta %||% 0
  eta_burden <- score_cfg$eta %||% 0
  if (fairness_stage_relaxed) {
    beta <- beta * (s$fair$anneal_beta_scale %||% 0.25)
    if (isTRUE(s$fair$anneal_disable_balance)) {
      enforce_balance <- FALSE
      gap_limit <- Inf
    }
  }
  utility_top_k <- as.integer(s$selector$utility_top_k %||% 3L)
  interaction_pairs <- interaction_pairs

  # rolling interaction mix
  mix_window <- s$interactions$mix$window %||% 12L
  mix_max_share <- s$interactions$mix$max_share %||% 0.3
  interaction_coverage_bonus <- s$interactions$coverage_bonus %||% 1
  interaction_first_bonus <- s$interactions$first_bonus %||% 0.5
  interactions_already <- any(vapply(engine$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
  recent_audit <- if (length(engine$audit)) tail(engine$audit, mix_window) else list()
  recent_share <- if (length(recent_audit)) {
    mean(vapply(recent_audit, function(x) isTRUE(x$interaction_pair), logical(1)))
  } else {
    0
  }

  tau <- s$tau_equal
  eps <- s$eps_strict %||% tau

  pool <- s$selector$candidate_pool %||% NA_integer_
  pair_cap <- s$selector$pair_cap %||% NA_integer_
  cand_idx <- seq_along(engine$candidates)
  if (is.finite(pool) && pool > 0L && length(cand_idx) > pool) {
    cps <- vapply(engine$candidates, function(c) c$crit_pair %||% "", character(1))

    # Only prioritize unseen pairs if pair_coverage is enabled
    if (isTRUE(s$fair$enabled) && isTRUE(s$fair$pair_coverage)) {
      unseen_idx <- cand_idx[cps %in% names(counts)[counts == 0L]]
      seen_idx <- setdiff(cand_idx, unseen_idx)

      # Always keep unseen pairs, but cap at pool size to avoid defeating performance limit
      keep <- if (length(unseen_idx) <= pool) unseen_idx else sample(unseen_idx, pool)
      remaining <- pool - length(keep)
      if (remaining > 0 && length(seen_idx)) {
        groups <- split(seen_idx, cps[seen_idx])
        per_group <- ceiling(remaining / max(1L, length(groups)))
        sampled_seen <- unlist(lapply(groups, function(g) {
          cap <- per_group
          if (is.finite(pair_cap) && pair_cap > 0) cap <- min(cap, pair_cap)
          if (length(g) <= cap) g else sample(g, cap)
        }), use.names = FALSE)
        if (length(sampled_seen) > remaining) sampled_seen <- sampled_seen[seq_len(remaining)]
        keep <- c(keep, sampled_seen)
      }
    } else {
      # No coverage bias: sample uniformly from all candidates
      keep <- sample(cand_idx, pool)
    }

    # if we undershoot pool (e.g., many unseen only), top up randomly
    if (length(keep) < pool) {
      remaining_idx <- setdiff(cand_idx, keep)
      if (length(remaining_idx)) {
        top_up <- sample(remaining_idx, min(pool - length(keep), length(remaining_idx)))
        keep <- c(keep, top_up)
      }
    }
    cand_idx <- unique(keep)
  }
  # secondary per-pair cap even if pool not hit
  if (is.finite(pair_cap) && pair_cap > 0) {
    cps <- vapply(engine$candidates, function(c) c$crit_pair %||% "", character(1))
    groups <- split(cand_idx, cps[cand_idx])
    capped <- unlist(lapply(groups, function(g) {
      if (length(g) <= pair_cap) g else sample(g, pair_cap)
    }), use.names = FALSE)
    cand_idx <- unique(capped)
  }
  cand_list <- engine$candidates[cand_idx]

  var_names_full <- colnames(W_full) %||% samples$var_names

  score_candidates <- function(cands, Wmat, balance_on = enforce_balance, balance_limit = gap_limit, relaxed = FALSE, outcome_cache = NULL, require_inter_coverage = uncovered_inter, relax_reason = NULL, bypass_closure = FALSE, allow_na_outcomes = FALSE) {
    if (!nrow(Wmat)) {
      return(list())
    }
    use_profiles_local <- !is.null(engine$profiles_idx)
    winners_local <- NULL
    win_prob_local <- NULL
    if (use_profiles_local) {
      winners_local <- .fp_samples_winners(Wmat, engine$profiles_idx, engine$profiles_idx_full)
      win_prob_local <- tabulate(winners_local, nbins = nrow(engine$profiles_idx)) / length(winners_local)
    }

    utility_pairs_local <- integer()
    if (use_profiles_local) {
      ord <- order(win_prob_local, decreasing = TRUE)
      if (length(ord)) {
        top_idx <- ord[seq_len(min(utility_top_k, length(ord)))]
        utility_pairs_local <- .fp_topk_pair_counts(engine$profiles, top_idx, engine$criteria)
      }
    }

    scored_local <- list()
    for (cand in cands) {
      i <- cand$i
      j <- cand$j
      if (!isTRUE(engine$interactions_active) && identical(cand$type, "interaction_conditional")) next
      if (i %in% (engine$eligibility$excluded_alts %||% integer()) ||
        j %in% (engine$eligibility$excluded_alts %||% integer())) {
        next
      }

      a <- engine$alternatives[i, , drop = FALSE]
      b <- engine$alternatives[j, , drop = FALSE]
      k <- pair_key(a, b, engine$criteria)

      if (k %in% engine$used_pairs) next

      cp <- cand$crit_pair
      ccount <- counts[cp] %||% 0L
      if (is.na(ccount)) ccount <- 0L
      # When pair coverage is incomplete, prioritize unseen pairs even if closure implies them
      if (!(uncovered_exists && ccount == 0L)) {
        if (use_closure && !bypass_closure && !is.null(engine$closure) && .fp_closure_resolved(engine$closure, i, j)) next
      }
      if (uncovered_exists && ccount > 0L && !require_inter_coverage) next
      if (require_inter_coverage && !(cp %in% interaction_pairs)) next
      if (length(interaction_pairs) && !(cp %in% interaction_pairs)) {
        # budget: keep fraction of interaction questions; allow relax when only interactions remain
        max_frac <- engine$settings$interactions$max_fraction %||% 1
        if (is.finite(max_frac) && max_frac >= 0) {
          asked <- sum(vapply(engine$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
          frac <- if (nrow(engine$decisions) > 0) asked / nrow(engine$decisions) else 0
          if (frac > max_frac && isTRUE(engine$interactions_active)) next
        }
      }
      if (balance_on && !.fp_exposure_balance_allow(counts, cp, nrow(engine$decisions) + 1L, gap_limit = balance_limit)) next

      a_idx <- if (!is.null(engine$alt_var_idx_full)) engine$alt_var_idx_full[[i]] else engine$alt_var_idx[i, ]
      b_idx <- if (!is.null(engine$alt_var_idx_full)) engine$alt_var_idx_full[[j]] else engine$alt_var_idx[j, ]
      delta <- rowSums(Wmat[, a_idx, drop = FALSE]) - rowSums(Wmat[, b_idx, drop = FALSE])

      cached <- NULL
      if (!is.null(outcome_cache) && length(outcome_cache)) cached <- outcome_cache[[k]]
      if (!is.null(cached)) {
        p_out <- cached
      } else {
        out <- as.integer(.fp_predict_outcome(delta, tau_equal = tau, eps_strict = eps))
        keep_out <- !is.na(out)
        if (!any(keep_out)) {
          if (allow_na_outcomes) {
            p_out <- rep(1 / 3, 3)
          } else {
            next
          }
        } else {
          p_out <- tabulate(out[keep_out], nbins = 3) / sum(keep_out)
        }
        if (!is.null(outcome_cache)) outcome_cache[[k]] <<- p_out
      }

      if (use_profiles_local) {
        ig <- .fp_candidate_eig(delta, winners_local, tau_equal = tau, eps_strict = eps)
        if (identical(s$selector$eig_target, "top3") && !is.null(topk_mat_full)) {
          ig <- .fp_candidate_eig_topk(delta, topk_mat_full, tau_equal = tau, eps_strict = eps, k = 3)
        }
      } else {
        ig <- .fp_entropy(p_out)
      }
      ig_se <- .fp_entropy_se(p_out, nrow(Wmat))

      fair_gain <- if (isTRUE(s$fair$enabled)) .fp_fairness_gain(ccount, max_pair_count) else 0
      util_gain <- if (use_profiles_local) .fp_utility_gain(cp, utility_pairs_local) else 0
      cost_penalty <- if (use_closure) .fp_cost_penalty(engine$closure, i, j) else 0
      # Check both criterion pair AND question type to avoid mislabeling regular tradeoffs
      is_interaction <- length(interaction_pairs) && (cp %in% interaction_pairs) &&
        (identical(cand$type, "interaction_conditional") || identical(cand$type, "interaction_joint"))
      if (is_interaction && is.finite(mix_max_share) && mix_max_share >= 0 && recent_share > mix_max_share && !require_inter_coverage) {
        next
      }
      burden_mult <- 1
      if (is_interaction) {
        if (identical(cand$type, "interaction_joint")) {
          burden_mult <- s$interactions$burden$joint %||% 1.5
        } else {
          burden_mult <- s$interactions$burden$conditional %||% 1
        }
      }
      interaction_unc <- if (is_interaction && !is.null(samples)) {
        # Use current samples (W_full) instead of posterior_samples (which is null until compute)
        # cp is "ci::cj", split into c(ci, cj) for .fp_interaction_entropy
        pair <- strsplit(cp, "::", fixed = TRUE)[[1]]
        .fp_interaction_entropy(W_full, var_names_full, pair)
      } else {
        0
      }
      interaction_burden <- if (is_interaction) burden_mult else 0
      interaction_bonus <- 0
      if (is_interaction) {
        if (ccount == 0L && interaction_coverage_bonus > 0) interaction_bonus <- interaction_bonus + interaction_coverage_bonus
        if (!interactions_already && interaction_first_bonus > 0) interaction_bonus <- interaction_bonus + interaction_first_bonus
      }

      implied_a <- if (use_closure) .fp_implied_constraints(engine$closure, i, j) else 0
      implied_b <- if (use_closure) .fp_implied_constraints(engine$closure, j, i) else 0
      proxy <- implied_cache[[k]] %||% .fp_implied_proxy(engine$closure, i, j, counts, cp)
      implied_cache[[k]] <<- proxy
      exp_implied <- if (use_closure) (p_out[1] %||% 0) * implied_a + (p_out[2] %||% 0) * implied_b else proxy

      score <- alpha * ig + beta * fair_gain + gamma * util_gain - delta_cost * cost_penalty + kappa * exp_implied +
        zeta * interaction_unc - eta_burden * interaction_burden + interaction_bonus

      scored_local[[length(scored_local) + 1L]] <- list(
        score = score,
        ig = ig,
        fair_bonus = fair_gain,
        utility_gain = util_gain,
        cost_penalty = cost_penalty,
        interaction_unc = interaction_unc,
        interaction_pair = is_interaction,
        expected_implied = exp_implied,
        key = k,
        i = i, j = j,
        a = a, b = b,
        meta = list(
          type = "tradeoff",
          label = cand$label %||% "",
          crit_pair = cp,
          probs = p_out,
          ig = ig,
          ig_se = ig_se,
          fair_bonus = fair_gain,
          utility_gain = util_gain,
          cost_penalty = cost_penalty,
          interaction_unc = interaction_unc,
          interaction_pair = is_interaction,
          expected_implied = exp_implied,
          balance_relaxed = relaxed,
          fairness_relaxed_reason = if (relaxed) (relax_reason %||% if (!balance_on) "fairness_balance_relaxed" else "fairness_relaxed") else NULL,
          interaction_bonus = interaction_bonus,
          score = score,
          score_components = list(
            alpha = alpha, beta = beta, gamma = gamma, delta = delta_cost, kappa = kappa, zeta = zeta, eta = eta_burden,
            eig = ig, fairness = fair_gain, utility = util_gain, cost = cost_penalty,
            expected_implied = exp_implied, interaction_unc = interaction_unc,
            interaction_bonus = interaction_bonus,
            interaction_pair = is_interaction, interaction_burden = interaction_burden, ig_se = ig_se
          )
        )
      )
    }
    scored_local
  }

  adaptive <- s$selector$adaptive %||% list()
  n_coarse <- adaptive$n_coarse %||% NA_integer_
  refine_top <- adaptive$refine_top %||% NA_integer_
  ig_ci_width <- adaptive$ig_ci_width %||% NULL
  # outcome cache keyed by pair key, tied to decision count
  outcome_cache <- NULL
  implied_cache <- NULL
  if (!is.null(engine$cache$outcomes) && isTRUE(engine$cache$n_decisions == nrow(engine$decisions))) {
    outcome_cache <- engine$cache$outcomes
    implied_cache <- engine$cache$implied
  } else {
    engine$cache$outcomes <- list()
    engine$cache$implied <- list()
  }

  score_with_strategy <- function(cands, require_inter = FALSE) {
    if (!length(cands)) {
      return(list())
    }
    if (is.finite(n_coarse) && n_coarse > 0L && n_coarse < nrow(W_full)) {
      if (!is.null(engine$seed)) set.seed(engine$seed + nrow(engine$decisions) + 99L)
      idx_coarse <- sample.int(nrow(W_full), n_coarse)
      scored_local <- score_candidates(cands, W_full[idx_coarse, , drop = FALSE], outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter)
      if (!length(scored_local) && enforce_balance) {
        scored_local <- score_candidates(cands, W_full[idx_coarse, , drop = FALSE], balance_on = TRUE, balance_limit = gap_limit + 1, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter, relax_reason = "balance_relaxed")
      }
      if (is.finite(refine_top) && refine_top > 0 && length(scored_local)) {
        sc_scores <- vapply(scored_local, `[[`, numeric(1), "score")
        sc_keys <- vapply(scored_local, `[[`, character(1), "key")
        ci_width <- vapply(scored_local, function(x) if (!is.null(x$meta$ig_se)) 2 * 1.96 * (x$meta$ig_se) else NA_real_, numeric(1))

        need_refine <- order(sc_scores, decreasing = TRUE)[seq_len(min(refine_top, length(scored_local)))]
        if (!is.null(ig_ci_width)) {
          unsure <- which(is.finite(ci_width) & ci_width > ig_ci_width)
          need_refine <- unique(c(need_refine, unsure))
        }

        if (!length(need_refine) && all(ci_width <= ig_ci_width, na.rm = TRUE)) {
          engine$diagnostics$refinement_skipped <- TRUE
        } else if (length(need_refine)) {
          refine_keys <- sc_keys[need_refine]
          refine_cands <- list()
          for (ci in cands) {
            k <- pair_key(engine$alternatives[ci$i, , drop = FALSE], engine$alternatives[ci$j, , drop = FALSE], engine$criteria)
            if (k %in% refine_keys) refine_cands[[length(refine_cands) + 1L]] <- ci
          }
          refined <- score_candidates(refine_cands, W_full, outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter)
          if (!length(refined) && enforce_balance) {
            refined <- score_candidates(refine_cands, W_full, balance_on = TRUE, balance_limit = gap_limit + 1, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter, relax_reason = "balance_relaxed")
          }
          for (itm in refined) {
            idx <- match(itm$key, sc_keys)
            if (!is.na(idx)) scored_local[[idx]] <- itm
          }
        }
      }
      return(scored_local)
    }
    scored_local <- score_candidates(cands, W_full, outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter)
    if (!length(scored_local) && enforce_balance) {
      scored_local <- score_candidates(cands, W_full, balance_on = TRUE, balance_limit = gap_limit + 1, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = require_inter, relax_reason = "balance_relaxed")
    }
    scored_local
  }

  inter_idx <- which(vapply(cand_list, function(c) identical(c$type, "interaction_conditional") || identical(c$type, "interaction_joint"), logical(1)))
  base_idx <- setdiff(seq_along(cand_list), inter_idx)

  # If we still need interaction questions and have candidates, force the next interaction
  if (uncovered_inter && length(inter_idx) && asked_interactions < min_interactions) {
    ci <- cand_list[inter_idx][[1]]
    a <- engine$alternatives[ci$i, , drop = FALSE]
    b <- engine$alternatives[ci$j, , drop = FALSE]
    k <- pair_key(a, b, engine$criteria)
    return(list(
      a = a, b = b, i = ci$i, j = ci$j, key = k,
      meta = list(
        type = ci$type %||% "interaction_conditional",
        label = ci$label %||% "",
        crit_pair = ci$crit_pair %||% "",
        probs = c(1 / 3, 1 / 3, 1 / 3),
        ig = NA_real_,
        ig_se = NA_real_,
        fair_bonus = 0,
        utility_gain = 0,
        cost_penalty = 0,
        interaction_unc = NA_real_,
        interaction_pair = TRUE,
        expected_implied = NA_real_,
        balance_relaxed = TRUE,
        fairness_relaxed_reason = "interaction_forced_pick",
        interaction_bonus = 0,
        score = 0,
        score_components = list(),
        top_k = 1L,
        pick_idx = 1L,
        seed_used = engine$seed,
        forced_interaction = TRUE
      )
    ))
  }
  scored_inter <- score_with_strategy(cand_list[inter_idx], require_inter = uncovered_inter)
  scored_base <- score_with_strategy(cand_list[base_idx], require_inter = FALSE)

  # If interaction quota is active but none survive, fall back to base candidates
  if (uncovered_inter && !length(scored_inter)) {
    uncovered_inter <- FALSE
    scored_base <- score_with_strategy(cand_list[base_idx], require_inter = FALSE)
  }
  # If no interaction candidates exist at all, drop the quota immediately
  if (uncovered_inter && !length(inter_idx)) {
    uncovered_inter <- FALSE
    scored_base <- score_with_strategy(cand_list[base_idx], require_inter = FALSE)
  }

  if (uncovered_inter && !length(scored_inter) && length(inter_idx)) {
    # Relax filters aggressively for interactions to satisfy min_interactions
    scored_inter <- score_candidates(cand_list[inter_idx], W_full, balance_on = FALSE, balance_limit = gap_limit, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = FALSE, relax_reason = "interaction_minimum_relaxed", bypass_closure = TRUE, allow_na_outcomes = TRUE)
    if (!length(scored_inter)) {
      # Final safeguard: force-pick the first valid interaction candidate
      # Apply basic guards: used_pairs, eligibility, closure
      forced_cand <- NULL
      for (ci in cand_list[inter_idx]) {
        if (ci$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
          ci$j %in% (engine$eligibility$excluded_alts %||% integer())) {
          next
        }
        a <- engine$alternatives[ci$i, , drop = FALSE]
        b <- engine$alternatives[ci$j, , drop = FALSE]
        k <- pair_key(a, b, engine$criteria)
        if (k %in% engine$used_pairs) next
        if (use_closure && !is.null(engine$closure) && .fp_closure_resolved(engine$closure, ci$i, ci$j)) next

        forced_cand <- list(ci = ci, a = a, b = b, k = k)
        break
      }
      if (!is.null(forced_cand)) {
        ci <- forced_cand$ci
        scored_inter <- list(list(
          score = 0,
          ig = NA_real_,
          fair_bonus = 0,
          utility_gain = 0,
          cost_penalty = 0,
          interaction_unc = NA_real_,
          interaction_pair = TRUE,
          expected_implied = NA_real_,
          key = forced_cand$k,
          i = ci$i, j = ci$j,
          a = forced_cand$a, b = forced_cand$b,
          meta = list(
            type = ci$type %||% "interaction_conditional",
            label = ci$label %||% "",
            crit_pair = ci$crit_pair %||% "",
            probs = c(1 / 3, 1 / 3, 1 / 3),
            ig = NA_real_,
            ig_se = NA_real_,
            fair_bonus = 0,
            utility_gain = 0,
            cost_penalty = 0,
            interaction_unc = NA_real_,
            interaction_pair = TRUE,
            expected_implied = NA_real_,
            balance_relaxed = TRUE,
            fairness_relaxed_reason = "interaction_forced",
            interaction_bonus = 0,
            score = 0,
            score_components = list()
          )
        ))
      }
    }
  }
  if (!length(scored_inter) && !length(scored_base) && length(cand_list)) {
    # last resort: relax fairness on all while keeping a transparent audit flag
    scored_base <- score_candidates(cand_list, W_full, balance_on = FALSE, balance_limit = gap_limit, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = FALSE, relax_reason = "fairness_relaxed_all")
  }

  if (!uncovered_inter) {
    scored <- c(scored_inter, scored_base)
  } else {
    scored <- scored_inter
    if (!length(scored) && length(scored_base)) scored <- scored_base
    if (!length(scored) && length(cand_list)) {
      # last-resort: allow any remaining candidates
      scored <- score_candidates(cand_list, W_full, balance_on = FALSE, balance_limit = gap_limit, relaxed = TRUE, outcome_cache = engine$cache$outcomes, require_inter_coverage = FALSE, relax_reason = "interaction_minimum_relaxed_all", allow_na_outcomes = TRUE, bypass_closure = TRUE)
      if (!length(scored)) {
        # truly last resort: pick the first valid remaining candidate with guards
        forced_cand <- NULL
        for (ci in cand_list) {
          if (ci$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
            ci$j %in% (engine$eligibility$excluded_alts %||% integer())) {
            next
          }
          a <- engine$alternatives[ci$i, , drop = FALSE]
          b <- engine$alternatives[ci$j, , drop = FALSE]
          k <- pair_key(a, b, engine$criteria)
          if (k %in% engine$used_pairs) next
          if (use_closure && !is.null(engine$closure) && .fp_closure_resolved(engine$closure, ci$i, ci$j)) next

          forced_cand <- list(ci = ci, a = a, b = b, k = k)
          break
        }
        if (!is.null(forced_cand)) {
          ci <- forced_cand$ci
          scored <- list(list(
            score = 0,
            ig = NA_real_,
            fair_bonus = 0,
            utility_gain = 0,
            cost_penalty = 0,
            interaction_unc = NA_real_,
            interaction_pair = length(interaction_pairs) && (ci$crit_pair %in% interaction_pairs),
            expected_implied = NA_real_,
            key = forced_cand$k,
            i = ci$i, j = ci$j,
            a = forced_cand$a, b = forced_cand$b,
            meta = list(
              type = ci$type %||% "tradeoff",
              label = ci$label %||% "",
              crit_pair = ci$crit_pair %||% "",
              probs = c(1 / 3, 1 / 3, 1 / 3),
              ig = NA_real_,
              ig_se = NA_real_,
              fair_bonus = 0,
              utility_gain = 0,
              cost_penalty = 0,
              interaction_unc = NA_real_,
              interaction_pair = length(interaction_pairs) && (ci$crit_pair %in% interaction_pairs),
              expected_implied = NA_real_,
              balance_relaxed = TRUE,
              fairness_relaxed_reason = "forced_any_candidate",
              interaction_bonus = 0,
              score = 0,
              score_components = list(),
              top_k = 1L,
              pick_idx = 1L,
              seed_used = engine$seed
            )
          ))
        }
      }
    }
  }
  if (!length(scored) && length(cand_list)) {
    # Final fallback: pick the first valid candidate with guards
    forced_cand <- NULL
    for (ci in cand_list) {
      if (ci$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
        ci$j %in% (engine$eligibility$excluded_alts %||% integer())) {
        next
      }
      a <- engine$alternatives[ci$i, , drop = FALSE]
      b <- engine$alternatives[ci$j, , drop = FALSE]
      k <- pair_key(a, b, engine$criteria)
      if (k %in% engine$used_pairs) next
      if (use_closure && !is.null(engine$closure) && .fp_closure_resolved(engine$closure, ci$i, ci$j)) next

      forced_cand <- list(ci = ci, a = a, b = b, k = k)
      break
    }
    if (!is.null(forced_cand)) {
      ci <- forced_cand$ci
      scored <- list(list(
        score = 0,
        ig = NA_real_,
        fair_bonus = 0,
        utility_gain = 0,
        cost_penalty = 0,
        interaction_unc = NA_real_,
        interaction_pair = length(interaction_pairs) && (ci$crit_pair %in% interaction_pairs),
        expected_implied = NA_real_,
        key = forced_cand$k,
        i = ci$i, j = ci$j,
        a = forced_cand$a, b = forced_cand$b,
        meta = list(
          type = ci$type %||% "tradeoff",
          label = ci$label %||% "",
          crit_pair = ci$crit_pair %||% "",
          probs = c(1 / 3, 1 / 3, 1 / 3),
          ig = NA_real_,
          ig_se = NA_real_,
          fair_bonus = 0,
          utility_gain = 0,
          cost_penalty = 0,
          interaction_unc = NA_real_,
          interaction_pair = length(interaction_pairs) && (ci$crit_pair %in% interaction_pairs),
          expected_implied = NA_real_,
          balance_relaxed = TRUE,
          fairness_relaxed_reason = "forced_final_fallback",
          interaction_bonus = 0,
          score = 0,
          score_components = list(),
          top_k = 1L,
          pick_idx = 1L,
          seed_used = engine$seed
        )
      ))
    }
  }
  if (!length(scored)) {
    return(NULL)
  }

  # persist outcome cache for reuse if we are still at same decision count next time
  engine$cache$outcomes <- outcome_cache
  engine$cache$implied <- implied_cache

  scores <- vapply(scored, `[[`, numeric(1), "score")
  ord <- order(scores, decreasing = TRUE)
  scored <- scored[ord]
  top_k <- as.integer(s$selector$top_k %||% 1L)
  top_k <- max(1L, min(top_k, length(scored)))

  # Randomize within top-K (procedural fairness: reduce ordering bias)
  pick_idx <- 1L
  if (top_k > 1L) {
    if (!is.null(engine$seed)) set.seed(engine$seed + nrow(engine$decisions))
    pick_idx <- sample.int(top_k, 1L)
  }

  pick <- scored[[pick_idx]]
  pick$meta$top_k <- top_k
  pick$meta$pick_idx <- pick_idx
  pick$meta$seed_used <- engine$seed

  # Return cache info so caller can persist it (R copy-on-modify means engine mods are lost)
  pick$meta$cache_outcomes <- outcome_cache
  pick$meta$cache_implied <- implied_cache
  pick$meta$cache_n_decisions <- nrow(engine$decisions)

  list(a = pick$a, b = pick$b, i = pick$i, j = pick$j, key = pick$key, meta = pick$meta)
}

#' Full-mode stop criterion
#' @keywords internal
engine_should_stop <- function(engine, samples = NULL) {
  engine <- validate_engine(engine)
  s <- engine$settings
  n <- nrow(engine$decisions)
  st <- engine$stop_state %||% new.env(parent = emptyenv())

  if (engine$phase == "done") {
    .engine_set_stop_state(st, reason = "phase_done", n = n)
    return(TRUE)
  }
  if (n >= s$max_q) {
    .engine_set_stop_state(st, reason = "max_q", n = n)
    return(TRUE)
  }
  # Check nested stop$min_q if present, otherwise fall back to top-level min_q
  min_q_threshold <- s$stop$min_q %||% s$min_q
  if (n < min_q_threshold) {
    return(FALSE)
  }

  if (isTRUE(s$fair$enabled) && isTRUE(s$fair$pair_coverage)) {
    h <- engine_health(engine)
    if (!isTRUE(h$pairs_covered)) {
      return(FALSE)
    }
  }

  # Without profiles: continue until bank exhaustion or budget (not just min_q)
  if (is.null(engine$profiles_idx)) {
    # Allow continuing if candidates remain and budget not exceeded
    if (length(engine$candidates) > 0 && n < s$max_q) {
      return(FALSE)
    }
    .engine_set_stop_state(st, reason = "question_bank_exhausted", n = n)
    return(TRUE)
  }

  # samples (reuse cache from selection to avoid split-brain and double sampling)
  if (is.null(samples)) {
    # First check if engine has cached samples from selection
    if (!is.null(engine$cache$samples) && identical(engine$cache$n_decisions, n)) {
      samples <- engine$cache$samples
    } else {
      # Only resample if cache is stale or missing
      sel <- s$selector
      samples <- tryCatch(
        engine_polytope_sample(engine,
          n = sel$n_samples,
          burnin = sel$burnin,
          thin = sel$thin,
          seed = engine$seed
        ),
        error = function(e) NULL
      )
    }
  }
  if (is.null(samples)) {
    return(FALSE)
  }
  W <- samples$weights
  use_profiles <- !is.null(engine$profiles_idx)
  P <- nrow(engine$profiles_idx)

  # winner probability
  winners <- .fp_samples_winners(W, engine$profiles_idx, engine$profiles_idx_full)
  win_prob <- tabulate(winners, nbins = P) / length(winners)
  best <- which.max(win_prob)
  best_prob <- win_prob[best]

  # winner confidence streak
  win_conf_prob <- s$stop$win_conf_prob %||% s$stop$win_prob %||% 0.95
  win_conf_streak_need <- as.integer(s$stop$win_conf_streak %||% 2L)
  cur_streak <- st$win_conf_streak %||% 0L
  if (best_prob >= win_conf_prob) {
    cur_streak <- cur_streak + 1L
  } else {
    cur_streak <- 0L
  }
  st$win_conf_streak <- cur_streak
  if (win_conf_streak_need > 0 && cur_streak >= win_conf_streak_need) {
    .engine_set_stop_state(
      st,
      reason = "win_conf_streak",
      n = n,
      best_prob = best_prob
    )
    return(TRUE)
  }

  # Single-profile case: stop once we have enough questions and coverage.
  if (P == 1) {
    if (best_prob >= (s$stop$win_prob %||% 0.95)) {
      .engine_set_stop_state(
        st,
        reason = "single_profile_win_prob",
        n = n,
        best_prob = best_prob
      )
      return(TRUE)
    }
    return(FALSE)
  }

  # margin robustness (5% quantile of best - runner-up)
  util <- matrix(0, nrow = nrow(W), ncol = P)
  for (p in seq_len(P)) util[, p] <- rowSums(W[, engine$profiles_idx[p, ], drop = FALSE])
  ord <- order(win_prob, decreasing = TRUE)
  second <- ord[2]
  delta_bs <- util[, best] - util[, second]
  q05 <- stats::quantile(delta_bs, probs = 0.05, names = FALSE)

  if (best_prob >= (s$stop$win_prob %||% 0.95) &&
    q05 > (s$stop$margin_q05 %||% 0.0)) {
    .engine_set_stop_state(
      st,
      reason = "winner_margin",
      n = n,
      best_prob = best_prob,
      margin_q05 = q05
    )
    return(TRUE)
  }

  topk_thr <- s$stop$topk_prob %||% NA_real_
  if (use_profiles && is.finite(topk_thr)) {
    topk_mat <- .fp_samples_topk(W, engine$profiles_idx, k = 3, profiles_idx_full = engine$profiles_idx_full)
    topk_prob <- rowMeans(topk_mat)
    ord <- order(win_prob, decreasing = TRUE)
    top_ids <- ord[seq_len(min(3, length(ord)))]
    if (all(topk_prob[top_ids] >= topk_thr)) {
      .engine_set_stop_state(
        st,
        reason = "top3_stability",
        n = n,
        best_prob = best_prob,
        top3_min_prob = min(topk_prob[top_ids], na.rm = TRUE)
      )
      return(TRUE)
    }
  }

  # weight interval stop (top-level spans tight)
  weight_eps <- s$stop$weight_span_eps %||% NA_real_
  if (is.finite(weight_eps)) {
    vn <- colnames(W) %||% samples$var_names
    crit <- engine$criteria
    # posterior width of criterion weights (range over levels per sample)
    crit_ranges <- matrix(NA_real_, nrow = nrow(W), ncol = length(crit))
    colnames(crit_ranges) <- crit
    for (k in seq_along(crit)) {
      cn <- crit[k]
      lv <- engine$domains[[cn]]
      idxs <- match(paste(cn, lv, sep = ":"), vn)
      if (any(is.na(idxs))) next
      wsub <- W[, idxs, drop = FALSE]
      crit_ranges[, k] <- apply(wsub, 1, function(x) max(x) - min(x))
    }
    keep_k <- s$stop$weight_span_criteria %||% crit
    keep_k <- intersect(crit, keep_k)
    if (length(keep_k)) {
      q <- apply(crit_ranges[, keep_k, drop = FALSE], 2, function(x) {
        stats::quantile(x, probs = c(0.05, 0.95), names = FALSE)
      })
      if (!is.null(dim(q))) {
        span_width <- q[2, ] - q[1, ]
      } else {
        span_width <- q[2] - q[1]
        names(span_width) <- keep_k
      }
      if (all(span_width <= weight_eps, na.rm = TRUE)) {
        .engine_set_stop_state(
          st,
          reason = "weight_span",
          n = n,
          best_prob = best_prob
        )
        return(TRUE)
      }
    }
  }

  # IG threshold stop (if next question adds almost no info)
  # Use adaptive threshold if enabled, otherwise fixed threshold
  eig_thr <- compute_adaptive_eig_threshold(n, s)
  pick_meta <- NULL
  if (!is.null(engine$current) && !is.null(engine$current$meta)) {
    pick_meta <- engine$current$meta
  } else if (!is.null(engine$last_pick) && isTRUE(engine$last_pick$n_decisions == n)) {
    pick_meta <- engine$last_pick$meta
  }
  if (eig_thr > 0) {
    if (is.null(pick_meta)) {
      pick <- engine_pick_tradeoff_polytope(engine, samples = samples)
      if (is.null(pick)) {
        .engine_set_stop_state(
          st,
          reason = "question_bank_exhausted",
          n = n,
          best_prob = best_prob,
          margin_q05 = q05
        )
        return(TRUE)
      }
      pick_meta <- pick$meta
    }
    if (!is.null(pick_meta$ig) && pick_meta$ig < eig_thr) {
      .engine_set_stop_state(
        st,
        reason = "eig_threshold",
        n = n,
        best_prob = best_prob,
        margin_q05 = q05,
        next_ig = pick_meta$ig
      )
      return(TRUE)
    }

    # no-progress streak: expected IG below threshold for several steps
    np_thr <- s$stop$no_progress_ig %||% eig_thr
    np_need <- as.integer(s$stop$no_progress_streak %||% 0L)
    if (np_thr > 0 && np_need > 0) {
      streak <- st$no_progress_streak %||% 0L
      if (!is.null(pick_meta$ig) && pick_meta$ig < np_thr) {
        streak <- streak + 1L
      } else {
        streak <- 0L
      }
      st$no_progress_streak <- streak
      if (streak >= np_need) {
        .engine_set_stop_state(
          st,
          reason = "no_progress_eig",
          n = n,
          best_prob = best_prob,
          margin_q05 = q05,
          next_ig = pick_meta$ig
        )
        return(TRUE)
      }
    }
  }

  FALSE
}

#' @keywords internal
.engine_set_stop_state <- function(st,
                                   reason,
                                   n,
                                   best_prob = NA_real_,
                                   margin_q05 = NA_real_,
                                   top3_min_prob = NA_real_,
                                   next_ig = NA_real_) {
  st$stop_reason <- as.character(reason)
  st$stop_n_questions <- as.integer(n)
  st$stop_best_prob <- as.numeric(best_prob)
  st$stop_margin_q05 <- as.numeric(margin_q05)
  st$stop_top3_min_prob <- as.numeric(top3_min_prob)
  st$stop_next_ig <- as.numeric(next_ig)
  invisible(st)
}

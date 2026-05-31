rescale_weights_range100 <- function(domains, w_named) {
  # Range-based scaling: sum(range_k)=100
  sp <- strsplit(names(w_named), ":", fixed = TRUE)
  crit_of <- vapply(sp, `[`, character(1), 1)

  rng_by_k <- tapply(as.numeric(w_named), crit_of, function(x) max(x) - min(x))
  scale <- 100 / (sum(rng_by_k) + 1e-12)
  w_named * scale
}

importance_from_weights <- function(w_df) {
  stopifnot(all(c("Merkmal", "Nutzen") %in% names(w_df)))
  # Filter out interaction terms (contain ::) - only use additive weights
  is_interaction <- grepl("::", w_df$Merkmal, fixed = TRUE)
  w_additive <- w_df[!is_interaction, , drop = FALSE]

  if (nrow(w_additive) == 0) {
    return(setNames(numeric(0), character(0)))
  }

  sp <- strsplit(w_additive$Merkmal, ":", fixed = TRUE)
  K <- vapply(sp, `[[`, character(1), 1)
  rng <- tapply(w_additive$Nutzen, K, function(x) max(x) - min(x))
  rng[is.na(rng)] <- 0
  if (sum(rng) <= 0) {
    return(setNames(rep(0, length(rng)), names(rng)))
  }
  100 * rng / sum(rng)
}

.topk_probabilities <- function(util, k = 3) {
  P <- nrow(util)
  S <- ncol(util)
  if (P == 0 || S == 0) {
    return(rep(NA_real_, P))
  }
  ranks <- apply(util, 2, function(u) rank(-u, ties.method = "min"))
  rowMeans(ranks <= k)
}

.sensitivity_knockout <- function(W, profiles_idx, criteria, top_k = 3) {
  P <- nrow(profiles_idx)
  S <- nrow(W)
  if (P == 0 || S == 0) {
    return(rep(NA_real_, length(criteria)))
  }

  util_base <- matrix(0, nrow = P, ncol = S)
  for (p in seq_len(P)) util_base[p, ] <- rowSums(W[, profiles_idx[p, ], drop = FALSE])
  base_topk <- apply(util_base, 2, function(u) head(order(-u), top_k))

  sens <- numeric(length(criteria))
  names(sens) <- criteria
  for (k in seq_along(criteria)) {
    idx <- profiles_idx[, k]
    Wk <- W
    Wk[, idx] <- 0
    util_k <- matrix(0, nrow = P, ncol = S)
    for (p in seq_len(P)) util_k[p, ] <- rowSums(Wk[, profiles_idx[p, ], drop = FALSE])
    topk_k <- apply(util_k, 2, function(u) head(order(-u), top_k))
    same <- mapply(function(a, b) length(intersect(a, b)) / top_k, split(base_topk, col(base_topk)), split(topk_k, col(topk_k)))
    sens[k] <- 1 - mean(same)
  }
  sens
}

.profile_contributions <- function(w_mean, var_names, profiles_idx, criteria, top_ids) {
  wv <- setNames(w_mean, var_names)
  out <- list()
  for (pid in top_ids) {
    idx <- profiles_idx[pid, ]
    contrib <- numeric(length(criteria))
    names(contrib) <- criteria
    for (k in seq_along(criteria)) {
      var <- var_names[idx[k]]
      contrib[k] <- wv[[var]]
    }
    contrib <- sort(contrib, decreasing = TRUE)
    out[[length(out) + 1L]] <- list(profile_id = pid, contributions = contrib)
  }
  out
}

.interaction_contributions <- function(profile_row, interactions, w_mean, var_names) {
  out <- numeric()
  if (is.null(interactions) || !length(interactions)) {
    return(out)
  }
  for (pair in interactions) {
    if (length(pair) != 2) next
    ci <- pair[1]
    cj <- pair[2]
    if (is.null(profile_row[[ci]]) || is.null(profile_row[[cj]])) next
    var <- paste(ci, as.character(profile_row[[ci]]), cj, as.character(profile_row[[cj]]), sep = "::")
    idx <- match(var, var_names)
    if (is.na(idx)) next
    out[var] <- w_mean[idx]
  }
  out[abs(out) > 0]
}

.counterfactual_criteria <- function(sens, n = 2) {
  if (is.null(sens) || !length(sens)) {
    return(character())
  }
  sens <- sens[is.finite(sens)]
  if (!length(sens)) {
    return(character())
  }
  head(names(sort(sens, decreasing = TRUE)), n)
}

.robustness_label <- function(win_prob, top3_prob) {
  if (is.na(win_prob) && is.na(top3_prob)) {
    return("uncertain")
  }
  if (isTRUE(win_prob >= 0.8)) {
    return("robust")
  }
  if (isTRUE(top3_prob >= 0.8)) {
    return("stable-top3")
  }
  if (isTRUE(top3_prob >= 0.5)) {
    return("plausible")
  }
  "fragile"
}

.format_interaction_string <- function(inter_vec, tol = 0.05) {
  if (is.null(inter_vec) || !length(inter_vec)) {
    return("Interaktionen vernachl\u00E4ssigbar")
  }
  inter_vec <- inter_vec[abs(inter_vec) >= tol]
  if (!length(inter_vec)) {
    return("Interaktionen vernachl\u00E4ssigbar")
  }
  parts <- vapply(names(inter_vec), function(nm) {
    tok <- strsplit(nm, "::", fixed = TRUE)[[1]]
    if (length(tok) == 4) {
      sprintf("%s=%s \u00D7 %s=%s: %.2f", tok[1], tok[2], tok[3], tok[4], inter_vec[[nm]])
    } else {
      sprintf("%s: %.2f", nm, inter_vec[[nm]])
    }
  }, character(1))
  paste("Interaktion relevant bei", paste(parts, collapse = "; "))
}

.format_profile_explanation <- function(pid, ex, engine) {
  name <- rownames(engine$profiles)[pid] %||% paste0("Option ", pid)
  contrib_str <- paste(names(head(ex$top_contributions, 3)), collapse = ", ")
  against_str <- if (!is.null(ex$against) && length(ex$against)) {
    paste(names(ex$against), collapse = ", ")
  } else {
    "keine klaren Gegenargumente"
  }
  tol_inter <- engine$settings$interactions$relevance_tol %||% 0.05
  inter_pairs <- engine$settings$interactions$pairs %||% list()
  asked_inter <- any(vapply(engine$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
  inter_str <- if (length(inter_pairs) && !asked_inter) {
    "Interaktionen nicht gepr\u00FCft (keine Interaktionsfragen gestellt)"
  } else {
    .format_interaction_string(ex$interaction_contributions, tol = tol_inter)
  }
  # Fairness note: expose balance relax if it happened
  fair_note <- NULL
  audit_run <- engine$diagnostics$audit_run %||% list()
  if (isTRUE(audit_run$balance_relaxed)) {
    fair_note <- "Fairness-Filter zeitweise gelockert"
  }
  cf_str <- if (!is.null(ex$counterfactual$criteria) && length(ex$counterfactual$criteria)) {
    paste("Entscheidung abh\u00E4ngig von", paste(ex$counterfactual$criteria, collapse = ", "))
  } else {
    "Ergebnis stabil gegen\u00FCber einzelnen Kriterien"
  }
  smooth_prob <- function(p) {
    if (is.null(p) || is.na(p)) {
      return(NA_real_)
    }
    if (identical(engine$settings$mode, "classic")) {
      return(p) # OG classic: no clamp
    }
    nsamp <- engine$settings$selector$summary$n_samples %||% engine$settings$selector$n_samples %||% 100
    eps <- max(0.02, 1 / max(100, nsamp)) # avoid hard 0/1 due to finite sampling
    pmin(pmax(p, eps), 1 - eps)
  }
  win_prob_fmt <- smooth_prob(ex$win_prob %||% ex$uncertainty$winner_prob %||% NA_real_)
  top3_prob_fmt <- smooth_prob(ex$top3_prob %||% ex$uncertainty$top3_prob %||% NA_real_)
  robustness <- .robustness_label(win_prob_fmt, top3_prob_fmt) %||% ex$robustness %||% ex$uncertainty$label %||% "uncertain"
  parts <- c(
    sprintf("%s: passt zu deinen Priorit\u00E4ten wegen %s", name, contrib_str),
    sprintf("Gegenargumente: %s", against_str),
    sprintf("Unsicherheit: %s (P(win)=%.2f, P(top3)=%.2f)", robustness, win_prob_fmt, top3_prob_fmt),
    sprintf("Interaktion: %s", inter_str),
    sprintf("Counterfactual: %s", cf_str)
  )
  if (!is.null(fair_note)) parts <- c(parts, sprintf("Fairness: %s", fair_note))
  paste(parts, collapse = " | ")
}

.fp_is_dominant <- function(a_row, b_row, domains) {
  crit <- names(domains)
  better_or_equal <- TRUE
  strictly_better <- FALSE
  for (cn in crit) {
    lv <- domains[[cn]]
    ia <- match(a_row[[cn]], lv)
    ib <- match(b_row[[cn]], lv)
    if (is.na(ia) || is.na(ib)) next
    if (ia < ib) better_or_equal <- FALSE
    if (ia > ib) strictly_better <- TRUE
  }
  better_or_equal && strictly_better
}

.engine_sanity_checks <- function(engine, w_mean) {
  warnings <- character()
  dom_flags <- data.frame()

  # Dominance sanity: a dominates b but has lower utility
  if ((!is.null(engine$alt_var_idx) || !is.null(engine$alt_var_idx_full)) && nrow(engine$alternatives) >= 2) {
    util <- numeric(nrow(engine$alternatives))
    for (i in seq_len(nrow(engine$alternatives))) {
      idx <- if (!is.null(engine$alt_var_idx_full)) engine$alt_var_idx_full[[i]] else engine$alt_var_idx[i, ]
      util[i] <- sum(w_mean[idx])
    }
    for (i in 1:(nrow(engine$alternatives) - 1)) {
      for (j in (i + 1):nrow(engine$alternatives)) {
        a <- engine$alternatives[i, , drop = FALSE]
        b <- engine$alternatives[j, , drop = FALSE]
        if (.fp_is_dominant(a, b, engine$domains) && is.finite(util[i]) && is.finite(util[j]) && util[i] < util[j]) {
          dom_flags <- rbind(dom_flags, data.frame(dominator = engine$alt_keys[i], dominated = engine$alt_keys[j], stringsAsFactors = FALSE))
        }
        if (.fp_is_dominant(b, a, engine$domains) && is.finite(util[i]) && is.finite(util[j]) && util[j] < util[i]) {
          dom_flags <- rbind(dom_flags, data.frame(dominator = engine$alt_keys[j], dominated = engine$alt_keys[i], stringsAsFactors = FALSE))
        }
      }
    }
    if (nrow(dom_flags)) {
      warnings <- c(warnings, "Dominance sanity violated: dominated option has higher utility.")
    }
  }

  # Interaction magnitude sanity
  inter_sum <- engine$diagnostics$interaction_summary %||% list()
  max_inter <- inter_sum$max_abs %||% 0
  bound <- engine$settings$interactions$max_abs %||% NA_real_
  if (is.finite(bound) && max_inter > bound) {
    warnings <- c(warnings, sprintf("Interaction magnitude exceeds bound (max %.3f > bound %.3f)", max_inter, bound))
  }

  list(warnings = warnings, dominance = dom_flags)
}

#' Compute weights for a PAPRIKA engine
#'
#' Solves the PAPRIKA linear program for the current set of decisions and
#' attaches scaled weights and basic diagnostics to the engine.
#'
#' @param engine A `paprika_engine` created with `engine_create()`.
#'
#' @return The updated `paprika_engine` with `weights` and `diagnostics`
#'   fields populated. If no feasible solution exists, `weights` is `NULL`
#'   and `diagnostics$solve_status` stores the solver status code.
#' @export
engine_compute <- function(engine) {
  engine <- validate_engine(engine)

  # Classic PAPRIKA: deterministic LP solve, no sampling
  if (identical(engine$settings$mode, "classic")) {
    out <- solve_partworths(engine$domains, engine$decisions, engine$settings)
    engine$diagnostics$solve_status <- out$status %||% 1L
    if (!isTRUE(out$ok)) {
      engine$weights <- NULL
      return(engine)
    }

    # EXPOSE SLACK DIAGNOSTICS for Inconsistency Checking
    if (!is.null(out$slack_info)) engine$diagnostics$slack <- out$slack_info
    has_slack <- (!is.null(out$slack_info) && length(out$slack_info)) || (!is.null(out$slack) && length(out$slack))

    if (has_slack) {
      engine$diagnostics$slack_flag <- TRUE
      total_slack <- sum(out$slack, na.rm = TRUE)
      max_slack <- max(out$slack, na.rm = TRUE)
      per_decision <- list()

      if (!is.null(out$slack_info)) {
        dec_idx <- vapply(out$slack_info, function(x) x$idx %||% NA_integer_, integer(1))
        sl_vals <- vapply(out$slack_info, function(x) x$slack %||% NA_real_, numeric(1))
        df <- data.frame(decision = dec_idx, slack = sl_vals)
        df <- df[order(-df$slack), , drop = FALSE]
        decision_is_valid <- !is.na(df$decision)
        if (any(decision_is_valid)) {
          df_clean <- df[decision_is_valid, , drop = FALSE]
          per_decision <- aggregate(slack ~ decision, data = df_clean, sum)
          per_decision <- per_decision[order(-per_decision$slack), , drop = FALSE]
        } else {
          per_decision <- data.frame(decision = integer(), slack = numeric())
        }
      }

      engine$diagnostics$slack_stats <- list(
        sum = total_slack,
        max = max_slack,
        per_decision = per_decision,
        top5 = head(per_decision, 5)
      )
      # Store the top conflicts for the UI to suggest revisiting
      engine$diagnostics$slack_revisit <- engine$diagnostics$slack_stats$top5
      engine$diagnostics$slack_message <- "Inkonsistenzen festgestellt. Lösung wurde mit minimalen Abweichungen approximiert."
    } else {
      engine$diagnostics$slack_flag <- FALSE
    }

    w <- rescale_weights_range100(engine$domains, out$weights)
    engine$weights <- data.frame(Merkmal = names(w), Nutzen = as.numeric(w), stringsAsFactors = FALSE)
    engine$diagnostics$importance <- importance_from_weights(engine$weights)

    # Check for degenerate weights (classic mode specific diagnostic)
    # This helps users identify when their decision set is insufficient
    ranges <- with(engine$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
    zero_criteria <- names(ranges)[ranges < 1e-4]

    if (length(zero_criteria) > 0) {
      warning(
        "Classic PAPRIKA: Criterion weights appear degenerate for: ",
        paste(zero_criteria, collapse = ", "), ". ",
        "The decision constraints may be insufficient to uniquely determine all weights. ",
        "Consider adding more discriminating pairwise comparisons involving these criteria."
      )
      engine$diagnostics$degenerate_criteria <- zero_criteria
    }

    # Deterministic utilities for profiles (or alternatives if no profiles set)
    util_vec <- numeric()
    names(util_vec) <- character()
    if (!is.null(engine$profiles_idx)) {
      util_profiles <- numeric(nrow(engine$profiles_idx))
      for (p in seq_len(nrow(engine$profiles_idx))) {
        util_profiles[p] <- sum(out$weights[engine$profiles_idx[p, ]])
      }
      names(util_profiles) <- rownames(engine$profiles) %||% paste0("Profile ", seq_along(util_profiles))
      util_vec <- util_profiles
    } else {
      alt_idx <- engine$alt_var_idx
      if (is.null(alt_idx)) {
        alt_idx <- tryCatch(.fp_alt_var_idx(engine$alternatives, engine$criteria, engine$var_idx), error = function(e) NULL)
      }
      if (!is.null(alt_idx)) {
        util_alt <- numeric(nrow(alt_idx))
        for (a in seq_len(nrow(alt_idx))) util_alt[a] <- sum(out$weights[alt_idx[a, ]])
        names(util_alt) <- engine$alt_keys %||% paste0("Alt ", seq_along(util_alt))
        util_vec <- util_alt
      }
    }
    if (length(util_vec)) {
      ord <- order(util_vec, decreasing = TRUE)
      win_prob <- rep(0, length(util_vec))
      win_prob[ord[1]] <- 1
      top3 <- ord[seq_len(min(3, length(ord)))]
      top3_prob <- rep(0, length(util_vec))
      top3_prob[top3] <- 1
      names(win_prob) <- names(util_vec)
      names(top3_prob) <- names(util_vec)
      engine$diagnostics$winner_probabilities <- win_prob
      if (!is.null(engine$profiles_idx)) {
        engine$diagnostics$profile_top3_prob <- top3_prob
      }
      engine$diagnostics$deterministic_utilities <- util_vec
    }

    # Apply dominated alternative pruning
    engine <- paprika_apply_pruning(engine)

    return(engine)
  }

  # Polytope summary (default): sample feasible weights, aggregate uncertainty
  s <- engine$settings
  summ <- s$selector$summary %||% list()
  nsamp <- summ$n_samples %||% s$selector$n_samples %||% 600L
  burn <- summ$burnin %||% s$selector$burnin %||% 200L
  thin <- summ$thin %||% s$selector$thin %||% 2L
  q_probs <- summ$quantiles %||% c(0.05, 0.95)
  if (length(q_probs) < 2) q_probs <- c(0.05, 0.95)

  sampling_warnings <- character()
  samples <- withCallingHandlers(
    tryCatch(
      engine_polytope_sample(engine, n = nsamp, burnin = burn, thin = thin, seed = engine$seed),
      error = function(e) NULL
    ),
    warning = function(w) {
      sampling_warnings <<- c(sampling_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (is.null(samples)) {
    # fallback attempt: smaller sample to salvage a posterior summary
    samples <- withCallingHandlers(
      tryCatch(
        engine_polytope_sample(engine, n = max(50L, ceiling(nsamp / 4)), burnin = max(10L, ceiling(burn / 2)), thin = thin, seed = engine$seed),
        error = function(e) NULL
      ),
      warning = function(w) {
        sampling_warnings <<- c(sampling_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }

  if (is.null(samples)) {
    # Fallback: single feasible point (backward compatible)
    out <- solve_partworths(engine$domains, engine$decisions, engine$settings)
    engine$diagnostics$solve_status <- out$status %||% 1L
    if (length(sampling_warnings)) engine$diagnostics$sampling_warnings <- sampling_warnings
    if (!is.null(out$slack_info)) engine$diagnostics$slack <- out$slack_info

    # Set or clear slack_flag based on current solve
    has_slack <- (!is.null(out$slack_info) && length(out$slack_info)) || (!is.null(out$slack) && length(out$slack))
    if (has_slack) {
      engine$diagnostics$slack_flag <- TRUE
    } else {
      # Clear flag if solve is strictly feasible (no slack needed)
      engine$diagnostics$slack_flag <- FALSE
    }

    if (!is.null(out$slack) && length(out$slack)) {
      total_slack <- sum(out$slack, na.rm = TRUE)
      max_slack <- max(out$slack, na.rm = TRUE)
      per_decision <- list()
      if (!is.null(out$slack_info)) {
        dec_idx <- vapply(out$slack_info, function(x) x$idx %||% NA_integer_, integer(1))
        sl_vals <- vapply(out$slack_info, function(x) x$slack %||% NA_real_, numeric(1))
        df <- data.frame(decision = dec_idx, slack = sl_vals)
        df <- df[order(-df$slack), , drop = FALSE]
        decision_is_valid <- !is.na(df$decision)
        if (any(decision_is_valid)) {
          df_clean <- df[decision_is_valid, , drop = FALSE]
          per_decision <- aggregate(slack ~ decision, data = df_clean, sum)
          per_decision <- per_decision[order(-per_decision$slack), , drop = FALSE]
        } else {
          per_decision <- data.frame(decision = integer(), slack = numeric())
        }
      }
      engine$diagnostics$slack_stats <- list(
        sum = total_slack,
        max = max_slack,
        per_decision = per_decision,
        top5 = head(per_decision, 5)
      )
      engine$diagnostics$slack_revisit <- engine$diagnostics$slack_stats$top5
      warn_sum <- engine$settings$slack$warn_sum %||% NA_real_
      warn_max <- engine$settings$slack$warn_max %||% NA_real_
      if (is.finite(warn_sum) && total_slack > warn_sum) {
        warning("Total slack exceeds warn_sum threshold: ", total_slack)
      }
      if (is.finite(warn_max) && max_slack > warn_max) {
        warning("Max slack exceeds warn_max threshold: ", max_slack)
      }
      engine$diagnostics$slack_flag <- TRUE
      engine$diagnostics$slack_message <- "Inconsistent preferences detected; closest feasible model computed with slack. Structural constraints remain hard."
    }
    if (!isTRUE(out$ok)) {
      engine$weights <- NULL
      return(engine)
    }
    w <- rescale_weights_range100(engine$domains, out$weights)
    engine$weights <- data.frame(Merkmal = names(w), Nutzen = as.numeric(w), stringsAsFactors = FALSE)
    engine$diagnostics$importance <- importance_from_weights(engine$weights)

    # Deterministic utilities/winners for fallback (copied from classic logic)
    util_vec <- numeric()
    names(util_vec) <- character()
    if (!is.null(engine$profiles_idx)) {
      util_profiles <- numeric(nrow(engine$profiles_idx))
      for (p in seq_len(nrow(engine$profiles_idx))) {
        # Use out$weights consistent with classic mode
        util_profiles[p] <- sum(out$weights[engine$profiles_idx[p, ]])
      }
      names(util_profiles) <- rownames(engine$profiles) %||% paste0("Profile ", seq_along(util_profiles))
      util_vec <- util_profiles
    } else {
      alt_idx <- engine$alt_var_idx
      if (is.null(alt_idx)) {
        alt_idx <- tryCatch(.fp_alt_var_idx(engine$alternatives, engine$criteria, engine$var_idx), error = function(e) NULL)
      }
      if (!is.null(alt_idx)) {
        util_alt <- numeric(nrow(alt_idx))
        for (a in seq_len(nrow(alt_idx))) util_alt[a] <- sum(out$weights[alt_idx[a, ]])
        names(util_alt) <- engine$alt_keys %||% paste0("Alt ", seq_along(util_alt))
        util_vec <- util_alt
      }
    }
    if (length(util_vec)) {
      ord <- order(util_vec, decreasing = TRUE)
      win_prob <- rep(0, length(util_vec))
      win_prob[ord[1]] <- 1
      top3 <- ord[seq_len(min(3, length(ord)))]
      top3_prob <- rep(0, length(util_vec))
      top3_prob[top3] <- 1
      names(win_prob) <- names(util_vec)
      names(top3_prob) <- names(util_vec)
      engine$diagnostics$winner_probabilities <- win_prob
      if (!is.null(engine$profiles_idx)) {
        engine$diagnostics$profile_top3_prob <- top3_prob
      }
      engine$diagnostics$deterministic_utilities <- util_vec
    }

    return(engine)
  }

  if (length(sampling_warnings)) engine$diagnostics$sampling_warnings <- sampling_warnings

  W <- samples$weights_scaled %||% samples$w_scaled %||% samples$weights %||% samples$w
  if (is.null(dim(W))) W <- matrix(W, nrow = 1)
  var_names <- colnames(W) %||% samples$var_names

  # Mean weights
  w_mean <- colMeans(W)
  qmat <- t(vapply(q_probs, function(p) apply(W, 2, stats::quantile, probs = p, names = FALSE), numeric(ncol(W))))
  colnames(qmat) <- var_names
  rownames(qmat) <- paste0("q", format(round(100 * q_probs, 1), trim = TRUE))

  engine$weights <- data.frame(Merkmal = var_names, Nutzen = as.numeric(w_mean), stringsAsFactors = FALSE)
  engine$weights_q <- data.frame(Merkmal = var_names, t(qmat), check.names = FALSE, stringsAsFactors = FALSE)
  engine$diagnostics$solve_status <- 0
  engine$diagnostics$importance <- importance_from_weights(engine$weights)
  engine$diagnostics$undo_since_compute <- FALSE
  # Interaction relevance summary
  inter_vars <- grep("::", var_names, value = TRUE, fixed = TRUE)
  if (length(inter_vars)) {
    tol <- engine$settings$interactions$relevance_tol %||% 0.05
    stats <- lapply(inter_vars, function(v) {
      idx <- match(v, var_names)
      m <- w_mean[idx]
      q05 <- qmat[paste0("q", 5), idx] %||% NA_real_
      q95 <- qmat[paste0("q", 95), idx] %||% NA_real_
      list(var = v, mean = m, q05 = q05, q95 = q95, abs_mean = abs(m))
    })
    df <- do.call(rbind, lapply(stats, as.data.frame))
    df$relevant <- df$abs_mean > tol
    additive_ok <- all(!df$relevant)
    top_inter <- df[order(-df$abs_mean), , drop = FALSE]
    engine$diagnostics$interaction_summary <- list(
      interactions = df,
      additive_ok = additive_ok,
      max_abs = max(df$abs_mean, na.rm = TRUE),
      message = if (additive_ok) "Interactions negligible; additive model suffices." else "Interactions relevant for some level combinations."
    )
  } else {
    engine$diagnostics$interaction_summary <- list(interactions = data.frame(), additive_ok = TRUE, max_abs = 0)
  }

  # Winner probabilities for alternatives (or profiles if set)
  alt_idx <- engine$alt_var_idx
  alt_idx_full <- engine$alt_var_idx_full
  if (is.null(alt_idx) && is.null(alt_idx_full)) {
    alt_idx <- tryCatch(.fp_alt_var_idx(engine$alternatives, engine$criteria, engine$var_idx), error = function(e) NULL)
    alt_idx_full <- tryCatch(.fp_alt_var_idx_full(engine$alternatives, engine$criteria, engine$var_idx, engine$settings$interactions %||% list()), error = function(e) NULL)
  }
  if (!is.null(alt_idx) || !is.null(alt_idx_full)) {
    n_alt <- if (!is.null(alt_idx_full)) length(alt_idx_full) else nrow(alt_idx)
    util <- matrix(0, nrow = nrow(W), ncol = n_alt)
    for (a in seq_len(n_alt)) {
      idx <- if (!is.null(alt_idx_full)) alt_idx_full[[a]] else alt_idx[a, ]
      util[, a] <- rowSums(W[, idx, drop = FALSE])
    }
    winners <- max.col(util, ties.method = "first")
    win_prob <- tabulate(winners, nbins = n_alt) / nrow(W)
    engine$diagnostics$winner_probabilities <- win_prob

    # If profiles set, compute top-k stability and sensitivity/explanations
    if (!is.null(engine$profiles_idx)) {
      inter_pairs <- engine$settings$interactions$pairs %||% list()
      # Use full indices (includes interactions) if available, fallback to additive-only
      P <- nrow(engine$profiles_idx) # Define P here
      prof_idx <- engine$profiles_idx_full %||% lapply(seq_len(P), function(p) engine$profiles_idx[p, ])
      util_profiles <- matrix(0, nrow = P, ncol = nrow(W)) # Corrected dimensions
      if (is.list(prof_idx)) {
        # profiles_idx_full is a list of variable indices (including interactions)
        for (p in seq_len(P)) {
          util_profiles[p, ] <- rowSums(W[, prof_idx[[p]], drop = FALSE])
        }
      } else {
        # Fallback to matrix indexing (additive-only, backward compat)
        for (p in seq_len(P)) {
          util_profiles[p, ] <- rowSums(W[, prof_idx[p, ], drop = FALSE])
        }
      }
      top3_prob <- .topk_probabilities(util_profiles, k = 3)
      # override winner probabilities to profile winners
      winners <- apply(util_profiles, 2, which.max)
      win_prob <- tabulate(winners, nbins = nrow(engine$profiles_idx)) / length(winners)
      engine$diagnostics$winner_probabilities <- win_prob
      engine$diagnostics$profile_top3_prob <- top3_prob

      sens <- .sensitivity_knockout(W, engine$profiles_idx, engine$criteria, top_k = 3)
      engine$diagnostics$sensitivity_knockout <- sens

      top_ids <- order(win_prob, decreasing = TRUE)
      top_ids <- top_ids[seq_len(min(3, length(top_ids)))]
      contrib <- .profile_contributions(w_mean, var_names, engine$profiles_idx, engine$criteria, top_ids)
      cf_crit <- .counterfactual_criteria(sens, n = 2)
      # Compute max potential utility per criterion for gap analysis
      w_ranges <- tapply(w_mean, sub(":.*", "", var_names), max)
      tol_inter <- engine$settings$interactions$relevance_tol %||% 0.05

      expl <- lapply(seq_along(top_ids), function(i) {
        pid <- top_ids[i]
        inter_c <- .interaction_contributions(engine$profiles[pid, , drop = FALSE], inter_pairs, w_mean, var_names)

        # PROS: High absolute contribution (Good level + High Importance)
        curr_contrib <- contrib[[i]]$contributions
        best_contrib <- head(sort(curr_contrib, decreasing = TRUE), 3)

        # CONS: High "Utility Gap" (Loss relative to best possible level)
        # Gap = MaxPossible - Current
        # Using the criterion names from contrib vector
        gaps <- numeric(length(curr_contrib))
        names(gaps) <- names(curr_contrib)
        for (cn in names(curr_contrib)) {
          max_val <- w_ranges[[cn]] %||% 0
          curr_val <- curr_contrib[[cn]]
          gaps[cn] <- max_val - curr_val
        }
        # Filter gaps: only list as Con if gap is significant
        gaps <- gaps[gaps > (0.1 * sum(w_mean))] # Ignore small gaps (<10% of total utility)
        worst_contrib <- head(sort(gaps, decreasing = TRUE), 2)

        rob_lab <- .robustness_label(win_prob[pid], top3_prob[pid])
        list(
          profile_id = pid,
          win_prob = win_prob[pid],
          top3_prob = top3_prob[pid],
          top_contributions = best_contrib, # Still pass full vector or just names? Formatting expects vector with names
          interaction_contributions = inter_c,
          interaction_relevant = any(abs(inter_c) >= tol_inter),
          stability = list(win_prob = win_prob[pid], top3_prob = top3_prob[pid], sensitivity = sens),
          against = worst_contrib, # Now contains Gaps
          robustness = rob_lab,
          uncertainty = list(label = rob_lab, winner_prob = win_prob[pid], top3_prob = top3_prob[pid]),
          counterfactual = list(criteria = cf_crit)
        )
      })
      engine$diagnostics$profile_explanations <- expl
      # human-readable templates
      engine$diagnostics$profile_explanations_text <- vapply(seq_along(expl), function(i) .format_profile_explanation(top_ids[i], expl[[i]], engine), character(1))
    }
  }

  # Eligibility/dealbreaker explanations
  if (!is.null(engine$eligibility$excluded_alts) && length(engine$eligibility$excluded_alts)) {
    excl_ids <- engine$eligibility$excluded_alts
    reasons <- engine$eligibility$reasons %||% vector("list", length(engine$alternatives))
    expl <- lapply(excl_ids, function(idx) {
      list(
        alt = engine$alt_keys[[idx]] %||% idx,
        reasons = reasons[[idx]] %||% character()
      )
    })
    engine$diagnostics$eligibility_explanations <- expl
  } else {
    engine$diagnostics$eligibility_explanations <- list()
  }
  engine$diagnostics$sanity <- .engine_sanity_checks(engine, w_mean)

  engine
}

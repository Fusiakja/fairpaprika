decisions_empty <- function() {
  data.frame(
    A1 = character(), A2 = character(), pref = character(),
    stringsAsFactors = FALSE
  )
}

# Build initial queues (anchor + systematic pairwise)
engine_build_queues <- function(engine) {
  engine <- validate_engine(engine)
  crits <- engine$criteria
  dom <- engine$domains

  base <- vapply(crits, function(cn) dom[[cn]][1], character(1))
  names(base) <- crits

  anchor_q <- list()
  pairwise_q <- list()

  row_index_of <- function(levels_named) {
    alts <- engine$alternatives
    i <- which(apply(alts, 1, function(r) all(as.character(r) == as.character(levels_named[crits]))))
    if (length(i)) i[1] else NA_integer_
  }

  # Anchor: each pair of criteria: one has top, other has top (baseline elsewhere)
  for (i in 1:(length(crits) - 1)) {
    for (j in (i + 1):length(crits)) {
      a <- base
      b <- base
      a[crits[i]] <- tail(dom[[crits[i]]], 1)
      b[crits[j]] <- tail(dom[[crits[j]]], 1)

      ia <- row_index_of(a)
      ib <- row_index_of(b)
      if (!is.na(ia) && !is.na(ib)) {
        anchor_q[[length(anchor_q) + 1]] <- list(
          i = ia, j = ib, type = "anchor",
          label = paste0(crits[i], "HIGH vs ", crits[j], "HIGH")
        )
      }
    }
  }

  # Pairwise: ensure coverage for each criterion pair
  for (i in 1:(length(crits) - 1)) {
    for (j in (i + 1):length(crits)) {
      ci <- crits[i]
      cj <- crits[j]

      a <- base
      a[ci] <- tail(dom[[ci]], 1)
      b <- base
      b[cj] <- tail(dom[[cj]], 1)
      ia <- row_index_of(a)
      ib <- row_index_of(b)

      if (!is.na(ia) && !is.na(ib)) {
        pairwise_q[[length(pairwise_q) + 1]] <- list(
          i = ia, j = ib, type = "pairwise",
          label = paste0(ci, "HIGH vs ", cj, "HIGH")
        )
      }

      # Optional finer questions if 3-level exists
      if (length(dom[[ci]]) >= 3) {
        a2 <- base
        a2[ci] <- dom[[ci]][2]
        b2 <- base
        b2[cj] <- tail(dom[[cj]], 1)
        ja <- row_index_of(a2)
        jb <- row_index_of(b2)
        if (!is.na(ja) && !is.na(jb)) {
          pairwise_q[[length(pairwise_q) + 1]] <- list(
            i = ja, j = jb, type = "pairwise",
            label = paste0(ci, "MID vs ", cj, "HIGH")
          )
        }
      }
      if (length(dom[[cj]]) >= 3) {
        a3 <- base
        a3[ci] <- tail(dom[[ci]], 1)
        b3 <- base
        b3[cj] <- dom[[cj]][2]
        ka <- row_index_of(a3)
        kb <- row_index_of(b3)
        if (!is.na(ka) && !is.na(kb)) {
          pairwise_q[[length(pairwise_q) + 1]] <- list(
            i = ka, j = kb, type = "pairwise",
            label = paste0(ci, "HIGH vs ", cj, "MID")
          )
        }
      }
    }
  }

  engine$queues$anchor <- anchor_q
  engine$queues$pairwise <- pairwise_q
  engine$queues$tradeoff <- list()
  engine$phase <- if (length(anchor_q)) "anchor" else "pairwise"
  # ensure interaction questions exist in full-mode candidates; already added in engine_create for full mode
  engine
}

engine_has_more_questions <- function(engine) {
  engine <- validate_engine(engine)
  phase <- engine$phase
  if (phase == "anchor") {
    return(length(engine$queues$anchor) > 0)
  }
  if (phase == "pairwise") {
    return(length(engine$queues$pairwise) > 0)
  }
  if (phase == "tradeoff") {
    if (identical(engine$settings$mode, "classic")) {
      return(length(engine$candidates) > 0)
    }
    return(TRUE) # full-mode tradeoff generated adaptively
  }
  FALSE
}

#' Get the next question from the engine
#'
#' Advances the engine state and returns the next question to ask, if any.
#' Respects the current phase, used pairs, fairness constraints, and optional
#' allowance to exceed the configured question budget.
#'
#' @param engine A `paprika_engine`.
#' @param allow_extra Logical; if `TRUE`, allow questions beyond `max_q`.
#' @return A list with updated `engine` and `question` payload (or `NULL` if no question).
#' @export
engine_next_question <- function(engine, allow_extra = FALSE) {
  engine <- validate_engine(engine)
  s <- engine$settings
  # clear stale last_pick if decisions advanced without setting current
  if (!is.null(engine$last_pick) && !is.null(engine$last_pick$n_decisions)) {
    if (engine$last_pick$n_decisions != nrow(engine$decisions)) {
      engine$last_pick <- NULL
    }
  }
  # If slack flagged, be conservative: ignore closure for selection steps
  if (isTRUE(engine$diagnostics$slack_flag)) {
    engine$closure <- NULL
  }
  if (!allow_extra && engine_done(engine)) {
    engine$phase <- "done"
    engine$current <- NULL
    return(list(engine = engine, question = NULL))
  }

  if (!allow_extra && nrow(engine$decisions) >= s$max_q) {
    engine$phase <- "done"
    engine$current <- NULL
    return(list(engine = engine, question = NULL))
  }

  # helper: skip used/implied etc. (implied optional hook)
  is_used <- function(a, b) {
    k <- pair_key(a, b, engine$criteria)
    k %in% engine$used_pairs
  }

  # 1) anchor
  if (engine$phase == "anchor") {
    while (length(engine$queues$anchor)) {
      item <- engine$queues$anchor[[1]]
      engine$queues$anchor <- engine$queues$anchor[-1]
      a <- engine$alternatives[item$i, , drop = FALSE]
      b <- engine$alternatives[item$j, , drop = FALSE]
      if (item$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
        item$j %in% (engine$eligibility$excluded_alts %||% integer())) {
        next
      }
      if (!is.null(engine$closure) && .fp_closure_resolved(engine$closure, item$i, item$j)) next

      if (is_used(a, b)) next

      engine$current <- list(
        a = a, b = b, i = item$i, j = item$j,
        key = pair_key(a, b, engine$criteria),
        meta = item
      )
      engine$used_pairs <- c(engine$used_pairs, engine$current$key)
      return(list(engine = engine, question = engine_question_payload(engine)))
    }
    engine$phase <- "pairwise"
  }

  # 2) pairwise
  if (engine$phase == "pairwise") {
    while (length(engine$queues$pairwise)) {
      item <- engine$queues$pairwise[[1]]
      engine$queues$pairwise <- engine$queues$pairwise[-1]
      a <- engine$alternatives[item$i, , drop = FALSE]
      b <- engine$alternatives[item$j, , drop = FALSE]
      if (item$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
        item$j %in% (engine$eligibility$excluded_alts %||% integer())) {
        next
      }
      if (!is.null(engine$closure) && .fp_closure_resolved(engine$closure, item$i, item$j)) next
      if (is_used(a, b)) next

      # optional fairness constraints on question formation
      # Skip in strict PAPRIKA mode
      strict_mode <- isTRUE(s$classic$strict_paprika) && s$mode == "classic"
      if (!strict_mode && isTRUE(s$fair$enabled)) {
        ok <- valid_tradeoff(engine$domains, a, b, engine$criteria,
          require_two = isTRUE(s$fair$require_two_differences),
          require_opposite = isTRUE(s$fair$require_opposite_directions)
        )
        if (!ok) next
      }

      engine$current <- list(
        a = a, b = b, i = item$i, j = item$j,
        key = pair_key(a, b, engine$criteria),
        meta = item
      )
      engine$used_pairs <- c(engine$used_pairs, engine$current$key)
      return(list(engine = engine, question = engine_question_payload(engine)))
    }

    # after pairwise, either finish or go tradeoff
    if (identical(s$mode, "classic")) {
      engine$phase <- "tradeoff"
    } else {
      engine$phase <- "tradeoff"
    }
  }

  # 3) tradeoff (heuristic pick)
  if (engine$phase == "tradeoff") {
    if (identical(s$mode, "classic")) {
      # PAPRIKA: Filter candidates by dominance and transitivity before picking
      engine <- paprika_filter_bank(engine)

      # Deterministic classic PAPRIKA: iterate through candidate bank in order
      if (length(engine$candidates)) {
        remaining <- list()
        picked <- NULL
        for (cand in engine$candidates) {
          a <- engine$alternatives[cand$i, , drop = FALSE]
          b <- engine$alternatives[cand$j, , drop = FALSE]
          if (cand$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
            cand$j %in% (engine$eligibility$excluded_alts %||% integer())) {
            next
          }
          if (is_used(a, b)) next
          if (!is.null(engine$closure) && .fp_closure_resolved(engine$closure, cand$i, cand$j)) next

          # Skip fairness in strict PAPRIKA mode
          strict_mode <- isTRUE(s$classic$strict_paprika) && s$mode == "classic"
          if (!strict_mode && isTRUE(s$fair$enabled)) {
            ok <- valid_tradeoff(engine$domains, a, b, engine$criteria,
              require_two = isTRUE(s$fair$require_two_differences),
              require_opposite = isTRUE(s$fair$require_opposite_directions)
            )
            if (!ok) next
          }

          if (is.null(picked)) {
            picked <- cand
          } else {
            remaining[[length(remaining) + 1L]] <- cand
          }
        }
        # Update candidate list to remaining non-implied/non-used items
        engine$candidates <- remaining

        if (is.null(picked)) {
          # Fallback: apply guards to find first valid candidate
          use_closure <- !is.null(engine$closure) && !isTRUE(engine$diagnostics$slack_flag)
          for (cand in engine$candidates) {
            if (cand$i %in% (engine$eligibility$excluded_alts %||% integer()) ||
              cand$j %in% (engine$eligibility$excluded_alts %||% integer())) {
              next
            }
            a <- engine$alternatives[cand$i, , drop = FALSE]
            b <- engine$alternatives[cand$j, , drop = FALSE]
            k <- pair_key(a, b, engine$criteria)
            if (k %in% engine$used_pairs) next
            if (use_closure && .fp_closure_resolved(engine$closure, cand$i, cand$j)) next

            picked <- cand
            break
          }
        }

        if (is.null(picked)) {
          engine$phase <- "done"
          engine$current <- NULL
          return(list(engine = engine, question = NULL))
        }

        meta <- list(
          type = picked$type %||% "tradeoff",
          label = picked$label %||% "",
          crit_pair = picked$crit_pair %||% "",
          probs = c(1 / 3, 1 / 3, 1 / 3),
          ig = NA_real_,
          ig_se = NA_real_,
          fair_bonus = 0,
          utility_gain = 0,
          cost_penalty = 0,
          interaction_unc = NA_real_,
          interaction_pair = identical(picked$type, "interaction_conditional") || identical(picked$type, "interaction_joint"),
          expected_implied = NA_real_,
          balance_relaxed = FALSE,
          fairness_relaxed_reason = NA_character_,
          interaction_bonus = 0,
          score = 0,
          top_k = 1L,
          pick_idx = length(engine$audit) + 1L,
          seed_used = engine$seed
        )

        engine$current <- list(
          a = engine$alternatives[picked$i, , drop = FALSE],
          b = engine$alternatives[picked$j, , drop = FALSE],
          i = picked$i, j = picked$j,
          key = pair_key(engine$alternatives[picked$i, , drop = FALSE], engine$alternatives[picked$j, , drop = FALSE], engine$criteria),
          meta = meta
        )
        engine$used_pairs <- c(engine$used_pairs, engine$current$key)
        engine$last_pick <- list(meta = meta, n_decisions = nrow(engine$decisions))
        engine$audit[[length(engine$audit) + 1L]] <- list(
          key = engine$current$key,
          type = meta$type,
          crit_pair = meta$crit_pair,
          ig = meta$ig,
          ig_se = meta$ig_se,
          fair = meta$fair_bonus,
          util = meta$utility_gain,
          cost = meta$cost_penalty,
          implied = meta$expected_implied,
          interaction_pair = meta$interaction_pair,
          interaction_unc = meta$interaction_unc,
          probs = meta$probs,
          balance_relaxed = meta$balance_relaxed,
          forced_interaction = meta$forced_interaction %||% FALSE,
          fairness_relaxed_reason = meta$fairness_relaxed_reason,
          top_k = meta$top_k,
          pick_idx = meta$pick_idx,
          seed_used = meta$seed_used,
          undo_epoch = engine$diagnostics$undo_epoch %||% 0L,
          after_undo = isTRUE(engine$diagnostics$undo_since_compute)
        )
        return(list(engine = engine, question = engine_question_payload(engine)))
      }
      engine$phase <- "done"
      engine$current <- NULL
      return(list(engine = engine, question = NULL))
    }

    if (identical(s$mode, "full")) {
      # reuse cached samples if possible (selection + stop share the same draws)
      samples <- NULL
      if (!is.null(engine$cache$samples) && identical(engine$cache$n_decisions, nrow(engine$decisions))) {
        samples <- engine$cache$samples
      } else {
        if (!is.null(engine$posterior_samples)) {
          samples <- engine$posterior_samples
          engine$cache$samples <- samples
          engine$cache$n_decisions <- nrow(engine$decisions)
        } else {
          sel <- s$selector
          samples <- tryCatch(
            engine_polytope_sample(
              engine,
              n = sel$n_samples,
              burnin = sel$burnin,
              thin = sel$thin,
              seed = engine$seed
            ),
            error = function(e) NULL
          )
          # fallback with tiny sample if sampling failed
          if (is.null(samples)) {
            samples <- tryCatch(
              engine_polytope_sample(
                engine,
                n = 40L,
                burnin = 15L,
                thin = 1L,
                seed = engine$seed
              ),
              error = function(e) NULL
            )
          }
          engine$cache$samples <- samples
          engine$cache$n_decisions <- nrow(engine$decisions)
        }
      }
      engine <- .fp_maybe_activate_interactions(engine, samples)
      if (is.null(samples)) {
        engine$diagnostics$sampling_error <- TRUE
        pick <- engine_pick_tradeoff(engine)
      } else {
        pick <- engine_pick_tradeoff_polytope(engine, samples = samples)
      }
    } else {
      pick <- engine_pick_tradeoff(engine)
    }

    if (is.null(pick)) {
      # fallback: if candidates still exist, pick the first available to avoid NULL
      if (length(engine$candidates)) {
        ci <- engine$candidates[[1]]
        a <- engine$alternatives[ci$i, , drop = FALSE]
        b <- engine$alternatives[ci$j, , drop = FALSE]
        engine$current <- list(
          a = a, b = b, i = ci$i, j = ci$j,
          key = pair_key(a, b, engine$criteria),
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
            interaction_pair = identical(ci$type, "interaction_conditional") || identical(ci$type, "interaction_joint"),
            expected_implied = NA_real_,
            balance_relaxed = TRUE,
            fairness_relaxed_reason = "forced_any_candidate_global",
            interaction_bonus = 0,
            score = 0,
            top_k = 1L,
            pick_idx = 1L,
            seed_used = engine$seed
          )
        )
        engine$last_pick <- list(meta = engine$current$meta, n_decisions = nrow(engine$decisions))
        engine$audit[[length(engine$audit) + 1L]] <- list(
          key = engine$current$key,
          type = engine$current$meta$type %||% "tradeoff",
          crit_pair = engine$current$meta$crit_pair %||% NA_character_,
          ig = engine$current$meta$ig %||% NA_real_,
          ig_se = engine$current$meta$ig_se %||% NA_real_,
          fair = engine$current$meta$fair_bonus %||% NA_real_,
          util = engine$current$meta$utility_gain %||% NA_real_,
          cost = engine$current$meta$cost_penalty %||% NA_real_,
          implied = engine$current$meta$expected_implied %||% NA_real_,
          interaction_pair = engine$current$meta$interaction_pair %||% FALSE,
          interaction_unc = engine$current$meta$interaction_unc %||% NA_real_,
          probs = engine$current$meta$probs %||% NA,
          balance_relaxed = engine$current$meta$balance_relaxed %||% FALSE,
          forced_interaction = engine$current$meta$forced_interaction %||% FALSE,
          fairness_relaxed_reason = engine$current$meta$fairness_relaxed_reason %||% NA_character_,
          top_k = engine$current$meta$top_k %||% NA_integer_,
          pick_idx = engine$current$meta$pick_idx %||% NA_integer_,
          seed_used = engine$current$meta$seed_used %||% NA_integer_,
          undo_epoch = engine$diagnostics$undo_epoch %||% 0L,
          after_undo = isTRUE(engine$diagnostics$undo_since_compute)
        )
        engine$used_pairs <- c(engine$used_pairs, engine$current$key)
        return(list(engine = engine, question = engine_question_payload(engine)))
      }
      engine$phase <- "done"
      engine$current <- NULL
      return(list(engine = engine, question = NULL))
    }
    engine$current <- pick
    engine$last_pick <- list(meta = pick$meta, n_decisions = nrow(engine$decisions))

    # Persist cache info from picker (returned in meta to survive R copy-on-modify)
    if (!is.null(pick$meta$cache_outcomes)) {
      engine$cache$outcomes <- pick$meta$cache_outcomes
      engine$cache$implied <- pick$meta$cache_implied
      engine$cache$n_decisions <- pick$meta$cache_n_decisions
    }

    engine$audit[[length(engine$audit) + 1L]] <- list(
      key = pick$key,
      type = pick$meta$type %||% "tradeoff",
      crit_pair = pick$meta$crit_pair %||% NA_character_,
      ig = pick$meta$ig %||% NA_real_,
      ig_se = pick$meta$ig_se %||% NA_real_,
      fair = pick$meta$fair_bonus %||% NA_real_,
      util = pick$meta$utility_gain %||% NA_real_,
      cost = pick$meta$cost_penalty %||% NA_real_,
      implied = pick$meta$expected_implied %||% NA_real_,
      interaction_pair = pick$meta$interaction_pair %||% FALSE,
      interaction_unc = pick$meta$interaction_unc %||% NA_real_,
      probs = pick$meta$probs %||% NA,
      balance_relaxed = pick$meta$balance_relaxed %||% FALSE,
      forced_interaction = pick$meta$forced_interaction %||% FALSE,
      fairness_relaxed_reason = pick$meta$fairness_relaxed_reason %||% NA_character_,
      top_k = pick$meta$top_k %||% NA_integer_,
      pick_idx = pick$meta$pick_idx %||% NA_integer_,
      seed_used = pick$meta$seed_used %||% NA_integer_,
      undo_epoch = engine$diagnostics$undo_epoch %||% 0L,
      after_undo = isTRUE(engine$diagnostics$undo_since_compute)
    )
    engine$used_pairs <- c(engine$used_pairs, engine$current$key)
    return(list(engine = engine, question = engine_question_payload(engine)))
  }

  engine$current <- NULL
  list(engine = engine, question = NULL)
}

engine_question_payload <- function(engine) {
  # This is what Shiny (or any UI) consumes.
  cur <- engine$current
  if (is.null(cur)) {
    return(NULL)
  }

  a <- cur$a
  b <- cur$b
  diffs <- diff_criteria(a, b, engine$criteria)

  list(
    key = cur$key,
    type = cur$meta$type %||% "tradeoff",
    label = cur$meta$label %||% "",
    a = as.list(a[1, , drop = TRUE]),
    b = as.list(b[1, , drop = TRUE]),
    differing_criteria = diffs
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

engine_pick_tradeoff <- function(engine) {
  # placeholder: choose a valid tradeoff that maximizes "new info"
  # In your app: score by uncovered gaps, implied pairs, etc.
  s <- engine$settings
  alts <- engine$alternatives
  crit <- engine$criteria

  best <- NULL
  best_score <- -Inf

  for (i in 1:(nrow(alts) - 1)) {
    for (j in (i + 1):nrow(alts)) {
      a <- alts[i, , drop = FALSE]
      b <- alts[j, , drop = FALSE]

      # Skip fairness in strict PAPRIKA mode
      strict_mode <- isTRUE(s$classic$strict_paprika) && s$mode == "classic"
      if (!strict_mode && isTRUE(s$fair$enabled)) {
        ok <- valid_tradeoff(engine$domains, a, b, crit,
          require_two = isTRUE(s$fair$require_two_differences),
          require_opposite = isTRUE(s$fair$require_opposite_directions)
        )
        if (!ok) next
      }

      k <- pair_key(a, b, crit)
      if (k %in% engine$used_pairs) next

      # score hook: you can replace by uncovered gaps, entropy, pair coverage, etc.
      score <- length(diff_criteria(a, b, crit))
      if (score > best_score) {
        best_score <- score
        best <- list(a = a, b = b, key = k, meta = list(type = "tradeoff", label = ""))
      }
    }
  }

  best
}

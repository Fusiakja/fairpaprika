# R/full_paprika_bank.R
#
# Stage-2 "full PAPRIKA" question bank:
# - exactly 2 criteria differ
# - opposite directions (trade-off)
# - avoid dominated pairs by construction (under monotonic domains)

.fp_build_var_index <- function(domains, interactions = list()) {
  crit <- names(domains)
  var_names <- unlist(lapply(crit, function(cn) paste(cn, domains[[cn]], sep = ":")))
  idx <- setNames(seq_along(var_names), var_names)
  if (!is.null(interactions) && length(interactions)) {
    for (pair in interactions) {
      if (length(pair) != 2) next
      ci <- pair[1]
      cj <- pair[2]
      if (!(ci %in% crit && cj %in% crit)) next
      for (li in domains[[ci]]) {
        for (lj in domains[[cj]]) {
          nm <- paste(ci, li, cj, lj, sep = "::")
          var_names <- c(var_names, nm)
          idx[[nm]] <- length(idx) + 1L
        }
      }
    }
  }
  list(var_names = var_names, var_idx = idx)
}

.fp_alt_var_idx <- function(alts, criteria, var_idx) {
  K <- length(criteria)
  out <- matrix(NA_integer_, nrow = nrow(alts), ncol = K)
  colnames(out) <- criteria
  for (k in seq_len(K)) {
    cn <- criteria[k]
    keys <- paste(cn, as.character(alts[[cn]]), sep = ":")
    out[, k] <- unname(var_idx[keys])
  }
  out
}

.fp_alt_var_idx_full <- function(alts, criteria, var_idx, interactions = list()) {
  inter_pairs <- interactions
  if (is.list(interactions) && !is.null(interactions$pairs)) inter_pairs <- interactions$pairs
  lapply(seq_len(nrow(alts)), function(r) {
    idxs <- unname(var_idx[paste(criteria, as.character(alts[r, criteria]), sep = ":")])
    if (!is.null(inter_pairs) && length(inter_pairs)) {
      for (pair in inter_pairs) {
        if (length(pair) != 2) next
        ci <- pair[1]
        cj <- pair[2]
        if (!(ci %in% criteria && cj %in% criteria)) next
        li <- as.character(alts[r, ci])
        lj <- as.character(alts[r, cj])
        nm <- paste(ci, li, cj, lj, sep = "::")
        idx_inter <- var_idx[[nm]]
        if (!is.null(idx_inter) && !is.na(idx_inter)) idxs <- c(idxs, idx_inter)
      }
    }
    idxs
  })
}

.fp_build_stage2_bank <- function(domains,
                                  move = c("adjacent", "all"),
                                  baseline = c("worst", "mid")) {
  move <- match.arg(move)
  baseline <- match.arg(baseline)

  crit <- names(domains)
  base_level <- vapply(crit, function(cn) {
    lv <- domains[[cn]]
    if (baseline == "worst") lv[1] else lv[ceiling(length(lv) / 2)]
  }, character(1))
  names(base_level) <- crit

  # Deduplicate alternatives via environment hash
  alt_env <- new.env(parent = emptyenv())
  alts_list <- list()
  alt_keys <- character()

  get_alt_id <- function(levels_named) {
    key <- paste(sprintf("%s:%s", crit, as.character(levels_named[crit])), collapse = ",")
    if (exists(key, envir = alt_env, inherits = FALSE)) {
      return(get(key, envir = alt_env, inherits = FALSE))
    }
    id <- length(alts_list) + 1L
    assign(key, id, envir = alt_env)
    alt_keys <<- c(alt_keys, key)
    alts_list[[id]] <<- as.character(levels_named[crit])
    id
  }

  candidates <- list()

  for (i in 1:(length(crit) - 1)) {
    for (j in (i + 1):length(crit)) {
      ci <- crit[i]
      cj <- crit[j]
      Li <- domains[[ci]]
      Lj <- domains[[cj]]

      if (move == "adjacent") {
        for (ii in 2:length(Li)) {
          for (jj in 2:length(Lj)) {
            # Trade-off: A better on ci, worse on cj (vs B)
            A <- base_level
            B <- base_level
            A[ci] <- Li[ii]
            A[cj] <- Lj[jj - 1]
            B[ci] <- Li[ii - 1]
            B[cj] <- Lj[jj]

            a_id <- get_alt_id(A)
            b_id <- get_alt_id(B)

            cp <- paste(sort(c(ci, cj)), collapse = "::")
            candidates[[length(candidates) + 1L]] <- list(
              i = a_id, j = b_id,
              crit_pair = cp,
              type = "tradeoff",
              label = paste0(
                ci, ": ", Li[ii], " vs ", Li[ii - 1], " | ",
                cj, ": ", Lj[jj], " vs ", Lj[jj - 1]
              )
            )
          }
        }
      } else {
        # Larger bank: extreme cross (still 2-diff, opposite)
        for (ii in 2:length(Li)) {
          for (jj in 2:length(Lj)) {
            A <- base_level
            B <- base_level
            A[ci] <- Li[ii]
            A[cj] <- Lj[1]
            B[ci] <- Li[1]
            B[cj] <- Lj[jj]

            a_id <- get_alt_id(A)
            b_id <- get_alt_id(B)

            cp <- paste(sort(c(ci, cj)), collapse = "::")
            candidates[[length(candidates) + 1L]] <- list(
              i = a_id, j = b_id,
              crit_pair = cp,
              type = "tradeoff",
              label = paste0(ci, "=", Li[ii], " vs ", cj, "=", Lj[jj])
            )
          }
        }
      }
    }
  }

  alts <- as.data.frame(do.call(rbind, alts_list), stringsAsFactors = FALSE)
  colnames(alts) <- crit

  list(alternatives = alts, alt_keys = alt_keys, candidates = candidates)
}

# Build minimal interaction question family: conditional tradeoff (i vs j under low/high k)
.fp_build_interaction_bank <- function(domains, pairs) {
  crit <- names(domains)
  if (is.null(pairs) || !length(pairs)) {
    return(list(alternatives = data.frame(), alt_keys = character(), candidates = list()))
  }

  base_level <- vapply(crit, function(cn) domains[[cn]][1], character(1))
  names(base_level) <- crit

  alt_env <- new.env(parent = emptyenv())
  alts_list <- list()
  alt_keys <- character()
  get_alt_id <- function(levels_named) {
    key <- paste(sprintf("%s:%s", crit, as.character(levels_named[crit])), collapse = ",")
    if (exists(key, envir = alt_env, inherits = FALSE)) {
      return(get(key, envir = alt_env, inherits = FALSE))
    }
    id <- length(alts_list) + 1L
    assign(key, id, envir = alt_env)
    alt_keys <<- c(alt_keys, key)
    alts_list[[id]] <<- as.character(levels_named[crit])
    id
  }

  candidates <- list()
  for (pair in pairs) {
    if (length(pair) != 2) next
    ci <- pair[1]
    cj <- pair[2]
    if (!(ci %in% crit && cj %in% crit)) next
    ctx <- setdiff(crit, c(ci, cj))
    if (!length(ctx)) next
    ck <- ctx[1]
    Li <- domains[[ci]]
    Lj <- domains[[cj]]
    Lk <- domains[[ck]]
    if (length(Li) < 2 || length(Lj) < 2 || length(Lk) < 2) next
    low_k <- Lk[1]
    high_k <- utils::tail(Lk, 1)
    hi_i <- utils::tail(Li, 1)
    lo_i <- Li[1]
    hi_j <- utils::tail(Lj, 1)
    lo_j <- Lj[1]

    build_pair <- function(k_level, suffix) {
      A <- base_level
      B <- base_level
      A[ci] <- hi_i
      A[cj] <- lo_j
      A[ck] <- k_level
      B[ci] <- lo_i
      B[cj] <- hi_j
      B[ck] <- k_level
      a_id <- get_alt_id(A)
      b_id <- get_alt_id(B)
      list(
        i = a_id, j = b_id,
        crit_pair = paste(sort(c(ci, cj)), collapse = "::"),
        type = "interaction_conditional",
        label = paste0("Interaction ", ci, "/", cj, " | ", ck, "=", k_level, " (", suffix, ")"),
        context = ck,
        context_level = k_level
      )
    }

    candidates[[length(candidates) + 1L]] <- build_pair(low_k, "low")
    candidates[[length(candidates) + 1L]] <- build_pair(high_k, "high")
  }

  if (!length(alts_list)) {
    alts <- data.frame(matrix(ncol = length(crit), nrow = 0))
    colnames(alts) <- crit
  } else {
    alts <- as.data.frame(do.call(rbind, alts_list), stringsAsFactors = FALSE)
    colnames(alts) <- crit
  }
  list(alternatives = alts, alt_keys = alt_keys, candidates = candidates)
}

# Build joint-improvement vs single improvement interaction questions
.fp_build_joint_bank <- function(domains, pairs) {
  crit <- names(domains)
  out_cands <- list()
  alt_env <- new.env(parent = emptyenv())
  alts_list <- list()
  alt_keys <- character()
  get_alt_id <- function(levels_named) {
    key <- paste(sprintf("%s:%s", crit, as.character(levels_named[crit])), collapse = ",")
    if (exists(key, envir = alt_env, inherits = FALSE)) {
      return(get(key, envir = alt_env, inherits = FALSE))
    }
    id <- length(alts_list) + 1L
    assign(key, id, envir = alt_env)
    alt_keys <<- c(alt_keys, key)
    alts_list[[id]] <<- as.character(levels_named[crit])
    id
  }

  base_level <- vapply(crit, function(cn) domains[[cn]][1], character(1))
  names(base_level) <- crit

  for (pair in pairs) {
    if (length(pair) != 2) next
    ci <- pair[1]
    cj <- pair[2]
    if (!(ci %in% crit && cj %in% crit)) next
    Li <- domains[[ci]]
    Lj <- domains[[cj]]
    # Require at least 3 levels to avoid dominance; use mid vs high/low tradeoff
    if (length(Li) < 3 || length(Lj) < 3) next
    mid_i <- Li[2]
    hi_i <- utils::tail(Li, 1)
    lo_i <- Li[1]
    mid_j <- Lj[2]
    hi_j <- utils::tail(Lj, 1)
    lo_j <- Lj[1]

    # Joint mild improvement (mid, mid) vs single strong on ci (hi, low)
    A <- base_level
    B <- base_level
    A[ci] <- mid_i
    A[cj] <- mid_j
    B[ci] <- hi_i
    B[cj] <- lo_j
    a_id <- get_alt_id(A)
    b_id <- get_alt_id(B)
    out_cands[[length(out_cands) + 1L]] <- list(
      i = a_id, j = b_id,
      crit_pair = paste(sort(c(ci, cj)), collapse = "::"),
      type = "interaction_joint",
      label = paste0("Joint vs single ", ci, "/", cj, " (mid/mid vs high/low)")
    )

    # Joint mild improvement vs single strong on cj (low/high)
    B2 <- base_level
    B2[ci] <- lo_i
    B2[cj] <- hi_j
    a_id2 <- get_alt_id(A)
    b_id2 <- get_alt_id(B2)
    out_cands[[length(out_cands) + 1L]] <- list(
      i = a_id2, j = b_id2,
      crit_pair = paste(sort(c(ci, cj)), collapse = "::"),
      type = "interaction_joint",
      label = paste0("Joint vs single ", ci, "/", cj, " (mid/mid vs low/high)")
    )
  }

  if (length(alts_list)) {
    alts <- as.data.frame(do.call(rbind, alts_list), stringsAsFactors = FALSE)
    colnames(alts) <- crit
  } else {
    alts <- data.frame(matrix(ncol = length(crit), nrow = 0))
    colnames(alts) <- crit
  }
  list(alternatives = alts, alt_keys = alt_keys, candidates = out_cands)
}

#' Register real-world profiles (options) for decision-focused IG + stop criteria
#'
#' If you set profiles, full-mode question selection can maximize information gain
#' about the *winner among these profiles* (instead of just answer-entropy),
#' and stopping can be based on robust winner probability.
#'
#' @param engine A `paprika_engine`.
#' @param profiles data.frame with one row per real option; must contain all criteria columns.
#' @return Updated `paprika_engine`.
#' @export
engine_set_profiles <- function(engine, profiles) {
  engine <- validate_engine(engine)
  stopifnot(is.data.frame(profiles))

  crit <- engine$criteria
  if (!all(crit %in% names(profiles))) {
    stop("profiles must contain all criteria columns: ", paste(crit, collapse = ", "))
  }

  if (is.null(engine$var_idx)) {
    vi <- .fp_build_var_index(engine$domains, engine$settings$interactions %||% list())
    engine$var_names <- vi$var_names
    engine$var_idx <- vi$var_idx
  }

  # Store both additive-only indices (for backward compat) and full indices (with interactions)
  P <- nrow(profiles)
  K <- length(crit)
  prof_idx_additive <- matrix(NA_integer_, nrow = P, ncol = K)
  colnames(prof_idx_additive) <- crit

  for (k in seq_len(K)) {
    cn <- crit[k]
    keys <- paste(cn, as.character(profiles[[cn]]), sep = ":")
    prof_idx_additive[, k] <- unname(engine$var_idx[keys])
    if (anyNA(prof_idx_additive[, k])) {
      bad <- unique(keys[is.na(prof_idx_additive[, k])])
      stop("profiles contain unknown levels for criterion '", cn, "': ", paste(bad, collapse = ", "))
    }
  }

  # Include interaction terms using alt_var_idx_full
  prof_idx_full <- .fp_alt_var_idx_full(profiles, crit, engine$var_idx, engine$settings$interactions %||% list())

  engine$profiles <- profiles
  engine$profiles_idx <- prof_idx_additive # Keep for backward compat
  engine$profiles_idx_full <- prof_idx_full # Full indices including interactions
  engine
}

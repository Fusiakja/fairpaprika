#' Simulation and ablation benchmarks (Phase 5.4)
#'
#' Lightweight simulation of patient sessions to compare settings configs.
#' Generates monotone synthetic preferences, simulates responses with optional noise,
#' runs the engine, and reports accuracy/effort metrics.
#' @param domains A list of domain levels.
#' @param configs A list of configuration lists (must contain $settings).
#' @param n_patients Number of synthetic patients to simulate per config.
#' @param noise Standard deviation of noise added to utility differences.
#' @param max_profiles Maximum number of profiles to use in simulation.
#' @export
benchmark_ablation <- function(domains,
                               configs,
                               n_patients = 3,
                               noise = 0,
                               max_profiles = 8) {
  stopifnot(is.list(configs), length(configs) > 0)
  res <- list()
  for (cfg_idx in seq_along(configs)) {
    cfg <- configs[[cfg_idx]]
    cfg_name <- cfg$name %||% paste0("cfg", cfg_idx)
    settings <- cfg$settings %||% list()
    metrics <- replicate(4, NA_real_)
    n_q <- numeric(n_patients)
    top1 <- numeric(n_patients)
    top3 <- numeric(n_patients)
    cover <- numeric(n_patients)
    for (p in seq_len(n_patients)) {
      sim <- .simulate_session(domains, settings, noise = noise, max_profiles = max_profiles)
      n_q[p] <- sim$n_questions
      top1[p] <- sim$top1_correct
      top3[p] <- sim$top3_overlap
      cover[p] <- sim$coverage
    }
    res[[cfg_name]] <- list(
      n_questions_mean = mean(n_q, na.rm = TRUE),
      top1_acc = mean(top1, na.rm = TRUE),
      top3_overlap = mean(top3, na.rm = TRUE),
      coverage = mean(cover, na.rm = TRUE)
    )
  }
  as.data.frame(do.call(rbind, lapply(names(res), function(nm) c(config = nm, res[[nm]]))), stringsAsFactors = FALSE)
}

.sample_monotone_weights <- function(domains) {
  w <- numeric()
  for (cn in names(domains)) {
    lv <- domains[[cn]]
    steps <- rexp(length(lv) - 1)
    vals <- c(0, cumsum(steps))
    vals <- vals / max(vals + 1e-9)
    names(vals) <- paste(cn, lv, sep = ":")
    w <- c(w, vals)
  }
  w
}

.utility_alt <- function(alt_row, w_named) {
  keys <- paste(names(alt_row), as.character(alt_row), sep = ":")
  sum(w_named[keys])
}

.simulate_session <- function(domains, settings, noise = 0, max_profiles = 8, true_w = NULL, profiles = NULL) {
  settings$max_q <- settings$max_q %||% 6L
  settings$min_q <- settings$min_q %||% 1L
  if (is.null(true_w)) true_w <- .sample_monotone_weights(domains)
  # build small profile set (cartesian cap)
  prof <- profiles
  if (is.null(prof)) {
    prof <- expand.grid(domains, stringsAsFactors = FALSE)
    if (nrow(prof) > max_profiles) prof <- prof[sample.int(nrow(prof), max_profiles), , drop = FALSE]
  }
  eng <- engine_create(domains, settings = settings, seed = sample.int(1e6, 1))
  eng <- engine_set_profiles(eng, prof)
  # simulate questions/answers
  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break
    a_row <- as.data.frame(q$a, stringsAsFactors = FALSE)
    b_row <- as.data.frame(q$b, stringsAsFactors = FALSE)
    ua <- .utility_alt(a_row, true_w)
    ub <- .utility_alt(b_row, true_w)
    if (noise > 0) {
      ua <- ua + rnorm(1, sd = noise)
      ub <- ub + rnorm(1, sd = noise)
    }
    pref <- if (abs(ua - ub) < 1e-6) "E" else if (ua > ub) "A" else "B"
    eng <- engine_add_decision(eng, pref)
    if (engine_done(eng)) break
  }
  n_questions <- nrow(eng$decisions)
  eng <- engine_compute(eng)
  # true top-k
  true_util <- apply(prof, 1, .utility_alt, w_named = true_w)
  true_ord <- order(true_util, decreasing = TRUE)
  est_probs <- eng$diagnostics$winner_probabilities %||% rep(0, nrow(prof))
  est_ord <- order(est_probs, decreasing = TRUE)
  top1_correct <- as.numeric(true_ord[1] == est_ord[1])
  top3_overlap <- mean(est_ord[seq_len(min(3, length(est_ord)))] %in% true_ord[seq_len(min(3, length(true_ord)))])
  needed <- .fp_needed_pairs(names(domains))
  counts <- .fp_pair_counts(eng)
  cover <- if (length(needed)) sum(counts[needed] > 0, na.rm = TRUE) / length(needed) else NA_real_
  list(
    n_questions = n_questions, top1_correct = top1_correct, top3_overlap = top3_overlap, coverage = cover,
    est_top1 = est_ord[1], est_top3 = est_ord[seq_len(min(3, length(est_ord)))], winner_prob = est_probs
  )
}

#' Seed/order sensitivity report
#'
#' Runs repeated simulations with a fixed synthetic patient and profiles, varying seeds
#' to quantify path dependence.
#' @param domains A list of domain levels.
#' @param settings A list of engine settings.
#' @param seeds Integer vector of seeds to use.
#' @param noise Standard deviation of noise added to utility differences.
#' @param max_profiles Maximum number of profiles to generate.
#' @export
seed_stability_report <- function(domains, settings = list(), seeds = 1:5, noise = 0, max_profiles = 8) {
  true_w <- .sample_monotone_weights(domains)
  profiles <- expand.grid(domains, stringsAsFactors = FALSE)
  if (nrow(profiles) > max_profiles) profiles <- profiles[sample.int(nrow(profiles), max_profiles), , drop = FALSE]

  results <- lapply(seeds, function(sd) {
    sim <- .simulate_session(domains, settings, noise = noise, max_profiles = max_profiles, true_w = true_w, profiles = profiles)
    sim$seed <- sd
    sim
  })
  top1_ids <- vapply(results, function(x) x$est_top1 %||% NA_integer_, integer(1))
  top3_sets <- lapply(results, function(x) x$est_top3 %||% integer())
  top1_var <- length(unique(top1_ids))
  top3_jaccard <- NA_real_
  if (length(top3_sets) >= 2) {
    pairs <- combn(seq_along(top3_sets), 2, simplify = FALSE)
    j <- vapply(pairs, function(p) {
      a <- top3_sets[[p[1]]]
      b <- top3_sets[[p[2]]]
      if (!length(a) || !length(b)) {
        return(NA_real_)
      }
      length(intersect(a, b)) / length(unique(c(a, b)))
    }, numeric(1))
    top3_jaccard <- mean(j, na.rm = TRUE)
  }
  # per-option top3 frequency and rank variance
  P <- nrow(profiles)
  top3_freq <- numeric(P)
  rank_var <- numeric(P)
  interaction_share <- numeric(length(results))
  exposure_gap <- numeric(length(results))
  n_questions <- numeric(length(results))
  for (i in seq_along(results)) {
    est_top3 <- results[[i]]$est_top3 %||% integer()
    top3_freq[est_top3] <- top3_freq[est_top3] + 1
    prob <- results[[i]]$winner_prob %||% rep(0, P)
    rks <- rank(-prob, ties.method = "average")
    rank_var <- rank_var + (rks - mean(rks))^2
    interaction_share[i] <- results[[i]]$interaction_share %||% NA_real_
    exposure_gap[i] <- results[[i]]$exposure_gap %||% NA_real_
    n_questions[i] <- results[[i]]$n_questions %||% NA_real_
  }
  top3_freq <- top3_freq / length(results)
  rank_var <- rank_var / max(1, length(results))
  inter_mean <- mean(interaction_share, na.rm = TRUE)
  inter_sd <- stats::sd(interaction_share, na.rm = TRUE)
  gap_sd <- stats::sd(exposure_gap, na.rm = TRUE)
  nq_sd <- stats::sd(n_questions, na.rm = TRUE)
  list(
    seeds = seeds,
    top1_unique = top1_var,
    top3_jaccard = top3_jaccard,
    n_questions_mean = mean(vapply(results, `[[`, numeric(1), "n_questions"), na.rm = TRUE),
    n_questions_sd = nq_sd,
    top3_freq = top3_freq,
    rank_variance = rank_var,
    interaction_share_mean = inter_mean,
    interaction_share_sd = inter_sd,
    exposure_gap_sd = gap_sd,
    runs = results
  )
}

#' Permutation stress test for order sensitivity
#'
#' Given a fixed decision set, permute the order and assess outcome variability.
#' @param domains A list of domain levels.
#' @param decisions A data frame of existing decisions.
#' @param profiles Optional data frame of profiles to evaluate.
#' @param settings A list of engine settings.
#' @param n_perm Number of permutations to run.
#' @export
permutation_stress_test <- function(domains, decisions, profiles = NULL, settings = list(), n_perm = 10) {
  stopifnot(is.data.frame(decisions))
  res <- list()
  if (is.null(profiles)) profiles <- expand.grid(domains, stringsAsFactors = FALSE)
  n_perm <- max(1L, as.integer(n_perm))
  for (i in seq_len(n_perm)) {
    perm <- sample.int(nrow(decisions))
    eng <- engine_create(domains, settings = settings, seed = i)
    eng$decisions <- decisions[perm, , drop = FALSE]
    eng <- engine_set_profiles(eng, profiles)
    eng <- engine_compute(eng)
    probs <- eng$diagnostics$winner_probabilities %||% rep(0, nrow(profiles))
    ord <- order(probs, decreasing = TRUE)
    res[[i]] <- list(seed = i, top1 = ord[1], top3 = ord[seq_len(min(3, length(ord)))], probs = probs)
  }
  top1_ids <- vapply(res, function(x) x$top1, integer(1))
  top3_sets <- lapply(res, function(x) x$top3)
  top1_var <- length(unique(top1_ids))
  top3_jaccard <- NA_real_
  if (length(top3_sets) >= 2) {
    pairs <- combn(seq_along(top3_sets), 2, simplify = FALSE)
    j <- vapply(pairs, function(p) {
      a <- top3_sets[[p[1]]]
      b <- top3_sets[[p[2]]]
      length(intersect(a, b)) / length(unique(c(a, b)))
    }, numeric(1))
    top3_jaccard <- mean(j, na.rm = TRUE)
  }
  list(
    top1_unique = top1_var,
    top3_jaccard = top3_jaccard,
    runs = res
  )
}

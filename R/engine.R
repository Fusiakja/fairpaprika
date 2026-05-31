#' Create a PAPRIKA engine (S3)
#'
#' @param domains named list. Each entry: criterion -> ordered levels (worst..best).
#' @param settings list. Algorithm settings (eps, tau, max_q, min_q, etc.).
#' @param seed optional integer for reproducibility of randomized question selection.
#'
#' @details
#' - `tau_equal` controls outcome classification / information gain (indifference band).
#' - `eps_strict` controls the LP margin for strict preferences (constraints); it does not affect outcome bins.
#' - When inconsistent preferences are detected, a slack LP finds the closest feasible utility model; structural assumptions (monotonicity, anchors, normalization) remain hard.
#' @return an S3 object of class 'paprika_engine'
#' @export
engine_create <- function(domains,
                          settings = list(),
                          seed = NULL) {
  domains <- validate_domains(domains)

  defaults <- list(
    # Engine mode
    mode = "full", # "full" (default) or "classic"
    full = list(
      bank_move = "adjacent", # "adjacent" (default) or "all"
      bank_baseline = "worst" # "worst" or "mid"
    ),

    # Budget
    min_q = 16L,
    max_q = 24L,

    # Constraint parameters
    eps_strict = 1e-3,
    tau_equal = 1e-3,
    epsilon_monotone = 1e-6,
    normalize_sum_top = 1, # sum(top-levels)=1
    regularize = FALSE, # Legacy: L1 regularization on top-level deviations
    regularize_balanced = list(
      enabled = FALSE, # Off by default (respects pure voice)
      type = "criterion_balance", # Prefer balanced criterion importance
      strength = 0.1, # Weak prior (0.01-0.5 range, lower = weaker)
      target = "uniform" # "uniform" = all criteria equal importance
    ),
    slack = list(
      enabled = TRUE,
      penalty = 1, # objective weight on slack sum
      warn_sum = 0.05, # warn if total slack exceeds this
      warn_max = 0.01 # warn if any single slack exceeds this
    ), # allow minimal-violation slack if infeasible
    interactions = list(
      enabled = FALSE, # master switch for interactions (disabled by default for clinical robustness)
      pairs = list(), # optional sparse pair interactions
      max_pairs = 2L, # budget of concurrently active interaction pairs
      max_fraction = 0.25, # cap share of interaction questions (procedural burden)
      max_abs = 1.5, # hard bound on interaction partworths (complexity control)
      relevance_tol = 0.05, # threshold to deem an interaction relevant in outputs
      mix = list(
        window = 12L, # rolling window for interaction share
        max_share = 0.3 # max share of interactions in window before penalizing
      ),
      min_questions = 0L, # minimum number of interaction questions to ask (hard budget)
      families = list(conditional = TRUE, joint = TRUE),
      burden = list(conditional = 1, joint = 1.5), # relative burden multipliers by family
      activate = list(
        winner_entropy = 1.0, # trigger if winner entropy stays above this (needs profiles)
        slack = TRUE, # trigger if slack/inconsistency detected
        min_questions = 6L # don't trigger before this many questions
      ),
      coverage_bonus = 5, # soft bonus for uncovered interaction pairs
      first_bonus = 3 # bonus for first interaction question
    ),

    # Full-mode selector (polytope sampling + IG)
    selector = list(
      method = "polytope_eig",
      n_samples = 600L,
      burnin = 200L,
      thin = 2L,
      top_k = 25L,
      candidate_pool = 150L, # optional random pool size when coverage is done
      pair_cap = 15L, # max candidates per criterion pair after prefilter
      eig_target = "winner", # "winner" or "top3" IG target
      fairness_lambda = 0.25, # moderate fairness emphasis
      utility_top_k = 4L, # focus on top-4 winners for utility targeting
      score = list(
        alpha = 1, # information gain weight
        beta = NULL, # fairness weight (defaults to fairness_lambda)
        gamma = 2.5, # utility focus weight (balanced)
        delta = 0.25, # cost penalty weight (moderate)
        kappa = 0.75, # implied constraints bonus (tempered)
        zeta = 1.0, # interaction uncertainty weight
        eta = 0.5 # burden penalty for interaction questions
      ),
      summary = list(
        n_samples = NULL, # override sampler draws used for summaries (defaults to selector$n_samples)
        burnin = NULL, # override sampler burnin
        thin = NULL, # override sampler thinning
        quantiles = c(0.05, 0.95) # uncertainty intervals for weights
      ),
      adaptive = list(
        n_coarse = 150L, # coarse sample count for broad candidate scan
        refine_top = 50L, # refine top candidates with full samples
        ig_ci_width = 0.02 # CI width threshold to force refinement (entropy IG)
      )
    ),

    # Full-mode stop criteria
    stop = list(
      win_prob = 0.95,
      margin_q05 = 0.0,
      eig_threshold = 0.002,
      eig_adaptive = list(
        enabled = FALSE, # Disabled by default (use fixed threshold)
        start_threshold = 0.02, # High selectivity early on
        end_threshold = 0.005, # More relaxed near max_q
        decay = "exponential", # "exponential" or "linear"
        min_questions = 15 # Full strength threshold until this many questions
      ),
      win_conf_prob = 0.95,
      win_conf_streak = 2L,
      weight_span_eps = 1.0,
      weight_span_criteria = NULL,
      no_progress_ig = 0.001,
      no_progress_streak = 3L,
      topk_prob = 0.8
    ),
    fair = list(
      enabled = TRUE,
      require_two_differences = TRUE,
      require_opposite_directions = TRUE,
      pair_coverage = TRUE, # ensure each criterion pair is “seen”
      exposure_balance = TRUE, # enforce balanced exposure across pairs
      exposure_gap_limit = 0, # max gap between most/least exposed (when feasible)
      interaction_coverage = TRUE # ensure interaction pairs are exposed at least once
    ),
    classic = list(
      bank_baseline = "worst",
      use_anchor_phase = FALSE,
      strict_paprika = FALSE, # TRUE = disable all enhancements, match original PAPRIKA
      use_regularization = TRUE, # Use weak regularization for tie-breaking when LP is underdetermined
      regularization_strength = 0.01 # Very weak, just to prefer balanced weights over arbitrary corners
    )
  )

  # Preserve user-specified interaction pairs even if modifyList drops unnamed lists
  user_inter_pairs <- NULL
  if (!is.null(settings$interactions) && "pairs" %in% names(settings$interactions)) {
    user_inter_pairs <- settings$interactions$pairs
  }
  settings <- modifyList(defaults, settings)
  if (!is.null(user_inter_pairs) && length(user_inter_pairs)) {
    settings$interactions$pairs <- user_inter_pairs
  }
  # tau_equal governs response/IG bins; eps_strict is the LP margin for strict prefs.

  if (!is.null(seed)) set.seed(seed)

  vi <- .fp_build_var_index(domains, settings$interactions$pairs %||% list())

  # Validate pair coverage feasibility
  K <- length(names(domains))
  n_pairs <- K * (K - 1) / 2
  if (isTRUE(settings$fair$enabled) &&
    isTRUE(settings$fair$pair_coverage) &&
    is.finite(settings$max_q) &&
    settings$max_q < n_pairs) {
    warning(
      "max_q (", settings$max_q, ") is smaller than number of criterion pairs (", n_pairs,
      "); pair coverage cannot be guaranteed. Increase max_q or disable pair_coverage."
    )
  }

  if (identical(settings$mode, "full")) {
    bank <- .fp_build_stage2_bank(
      domains,
      move = settings$full$bank_move %||% "adjacent",
      baseline = settings$full$bank_baseline %||% "worst"
    )
    # optional interaction question bank (conditional tradeoffs, joint improvements)
    if (isTRUE(settings$interactions$enabled) && length(settings$interactions$pairs %||% list())) {
      cand_list <- list()
      alt_env <- new.env(parent = emptyenv())
      alts_list <- list()

      add_alt <- function(row_vec) {
        key <- paste(sprintf("%s:%s", names(domains), as.character(row_vec)), collapse = ",")
        if (exists(key, envir = alt_env, inherits = FALSE)) {
          return(get(key, envir = alt_env, inherits = FALSE))
        }
        id <- length(alts_list) + 1L
        assign(key, id, envir = alt_env)
        alts_list[[id]] <<- as.character(row_vec)
        id
      }

      # start with base alts/candidates
      for (r in seq_len(nrow(bank$alternatives))) {
        add_alt(bank$alternatives[r, , drop = TRUE])
      }
      cand_list <- c(cand_list, bank$candidates)

      if (isTRUE(settings$interactions$families$conditional)) {
        ibank <- .fp_build_interaction_bank(domains, settings$interactions$pairs)
        for (r in seq_len(nrow(ibank$alternatives))) {
          row_vec <- ibank$alternatives[r, , drop = TRUE]
          add_alt(row_vec)
        }
        for (cand in ibank$candidates) {
          a_row <- ibank$alternatives[cand$i, , drop = TRUE]
          b_row <- ibank$alternatives[cand$j, , drop = TRUE]
          cand$i <- add_alt(a_row)
          cand$j <- add_alt(b_row)
          cand_list[[length(cand_list) + 1L]] <- cand
        }
      }

      if (isTRUE(settings$interactions$families$joint)) {
        jbank <- .fp_build_joint_bank(domains, settings$interactions$pairs)
        for (r in seq_len(nrow(jbank$alternatives))) {
          row_vec <- jbank$alternatives[r, , drop = TRUE]
          add_alt(row_vec)
        }
        for (cand in jbank$candidates) {
          a_row <- jbank$alternatives[cand$i, , drop = TRUE]
          b_row <- jbank$alternatives[cand$j, , drop = TRUE]
          cand$i <- add_alt(a_row)
          cand$j <- add_alt(b_row)
          cand_list[[length(cand_list) + 1L]] <- cand
        }
      }

      alts <- as.data.frame(do.call(rbind, alts_list), stringsAsFactors = FALSE)
      colnames(alts) <- names(domains)
      keys <- alternatives_keys(alts, names(domains))
      candidates <- cand_list
    } else {
      alts <- bank$alternatives
      keys <- alternatives_keys(alts, names(domains))
      candidates <- bank$candidates
    }

    alt_var_idx <- .fp_alt_var_idx(alts, names(domains), vi$var_idx)
    alt_var_idx_full <- .fp_alt_var_idx_full(alts, names(domains), vi$var_idx, settings$interactions %||% list())
    closure <- .fp_closure_init(nrow(alts))
    closure_cfg <- settings$closure %||% list()
    use_dominance <- closure_cfg$dominance %||% TRUE
    dom_upd <- if (isTRUE(use_dominance)) .fp_closure_add_dominance(closure, alts, domains) else list(reach = closure, conflict = FALSE, added = 0L)
    closure <- dom_upd$reach
    closure_conflict <- isTRUE(dom_upd$conflict)
    closure_dom_edges <- dom_upd$added %||% 0L
    phase <- "tradeoff"
    queues <- list(anchor = list(), pairwise = list(), tradeoff = list())
  } else {
    # Classic: use the stage-2 bank (adjacent tradeoffs), configurable baseline
    classic_baseline <- settings$classic$bank_baseline %||% settings$full$bank_baseline %||% "worst"
    bank <- .fp_build_stage2_bank(
      domains,
      move = settings$full$bank_move %||% "adjacent",
      baseline = classic_baseline
    )
    alts <- bank$alternatives
    keys <- alternatives_keys(alts, names(domains))
    alt_var_idx <- .fp_alt_var_idx(alts, names(domains), vi$var_idx)
    alt_var_idx_full <- .fp_alt_var_idx_full(alts, names(domains), vi$var_idx, settings$interactions %||% list())
    closure <- .fp_closure_init(nrow(alts))
    closure_conflict <- FALSE
    closure_dom_edges <- 0L
    candidates <- bank$candidates
    phase <- if (isTRUE(settings$classic$use_anchor_phase)) "anchor" else "tradeoff"
    queues <- list(anchor = list(), pairwise = list(), tradeoff = list())
  }

  eng <- list(
    domains = domains,
    criteria = names(domains),
    alternatives = alts,
    alt_keys = keys,

    # Full-mode helpers / caches (harmless in classic mode)
    var_names = vi$var_names,
    var_idx = vi$var_idx,
    alt_var_idx = alt_var_idx, # matrix: alternative x criteria -> var index
    alt_var_idx_full = alt_var_idx_full, # list: alternative -> c(additive + interaction idx)
    candidates = candidates, # list of candidate questions (full mode)
    closure = closure, # transitive closure for implied rankings (full mode)
    cache = list(samples = NULL, n_decisions = -1L, outcomes = NULL, implied = NULL),
    posterior_samples = NULL,
    eligibility = list(
      rules = NULL,
      excluded_alts = integer(),
      excluded_profiles = integer(),
      reasons = list()
    ),
    profiles = NULL,
    profiles_idx = NULL,

    # State
    decisions = decisions_empty(),
    used_pairs = character(),
    phase = phase, # "anchor"|"pairwise"|"tradeoff"|"done"
    queues = queues,
    current = NULL, # current question: list(a=..., b=..., key=..., meta=...)
    last_pick = NULL, # cache of last pick + n_decisions for stop checks
    audit = list(), # per-question audit trail
    stop_state = new.env(parent = emptyenv()), # persistent stop-rule counters
    interactions_active = FALSE,
    interactions_pairs_active = list(),

    # Computed
    weights = NULL, # data.frame(Merkmal, Nutzen)
    diagnostics = list(),
    settings = settings,
    seed = seed
  )
  class(eng) <- "paprika_engine"
  if (isTRUE(closure_conflict)) eng$diagnostics$closure_conflict <- TRUE
  eng$diagnostics$closure_dominance_edges <- closure_dom_edges %||% 0L

  if (!identical(eng$settings$mode, "full") && !identical(eng$settings$mode, "classic")) eng <- engine_build_queues(eng)
  if (identical(eng$settings$mode, "classic")) {
    # Validate eps_strict for classic mode
    # Classic PAPRIKA requires strict preferences (eps_strict > 0) to prevent degenerate solutions
    # where some criterion weights collapse to zero while still satisfying all constraints
    if (is.null(eng$settings$eps_strict) || eng$settings$eps_strict <= 0) {
      warning(
        "Classic PAPRIKA requires eps_strict > 0 for unique weight determination. ",
        "Setting eps_strict = 1e-3"
      )
      eng$settings$eps_strict <- 1e-3
    }

    if (isTRUE(eng$settings$classic$use_anchor_phase)) {
      eng <- engine_build_queues(eng)
    } else {
      # Use PAPRIKA-style systematic enumeration
      eng <- paprika_init_classic_bank(eng)
    }
  }
  eng
}

#' Reset engine to initial state (keep domains/settings)
#'
#' @param engine A `paprika_engine` object.
#' @return A reset `paprika_engine`.
#' @export
engine_reset <- function(engine) {
  engine <- validate_engine(engine)
  engine$decisions <- decisions_empty()
  engine$used_pairs <- character()

  engine$current <- NULL
  engine$last_pick <- NULL
  engine$weights <- NULL
  engine$diagnostics <- list()
  engine$stop_state <- new.env(parent = emptyenv())
  engine$posterior_samples <- NULL
  engine$eligibility$excluded_alts <- integer()
  engine$eligibility$excluded_profiles <- integer()
  engine$interactions_active <- FALSE
  engine$interactions_pairs_active <- list()

  # reset caches / closure (full mode)
  if (!is.null(engine$cache)) {
    engine$cache$samples <- NULL
    engine$cache$n_decisions <- -1L
  }
  if (!is.null(engine$closure)) {
    engine$closure <- .fp_closure_init(nrow(engine$alternatives))
    closure_cfg <- engine$settings$closure %||% list()
    use_dominance <- if (identical(engine$settings$mode, "classic")) FALSE else (closure_cfg$dominance %||% TRUE)
    dom_upd <- if (isTRUE(use_dominance)) .fp_closure_add_dominance(engine$closure, engine$alternatives, engine$domains) else list(reach = engine$closure, conflict = FALSE, added = 0L)
    engine$closure <- dom_upd$reach
    if (isTRUE(dom_upd$conflict)) engine$diagnostics$closure_conflict <- TRUE
    engine$diagnostics$closure_dominance_edges <- dom_upd$added %||% 0L
  }

  if (identical(engine$settings$mode, "full")) {
    engine$phase <- "tradeoff"
    engine$queues <- list(anchor = list(), pairwise = list(), tradeoff = list())
    return(engine)
  }

  # classic mode: either start directly in tradeoff (OG-style) or rebuild anchor/pairwise if enabled
  if (isTRUE(engine$settings$classic$use_anchor_phase)) {
    engine$phase <- "anchor"
    engine$queues <- list(anchor = list(), pairwise = list(), tradeoff = list())
    engine <- engine_build_queues(engine)
  } else {
    engine$phase <- "tradeoff"
    engine$queues <- list(anchor = list(), pairwise = list(), tradeoff = list())
  }
  engine
}

#' Print a PAPRIKA engine summary
#'
#' @param x A `paprika_engine`.
#' @param ... Passed to methods.
#' @export
print.paprika_engine <- function(x, ...) {
  cat("<paprika_engine>\n")
  cat(" Criteria:", paste(x$criteria, collapse = ", "), "\n")
  cat(" Alternatives:", nrow(x$alternatives), "\n")
  cat(" Decisions:", nrow(x$decisions), "\n")
  cat(" Phase:", x$phase, "\n")
  if (!is.null(x$current)) cat(" Current question:", x$current$key, "\n")
  cat(" Weights:", if (!is.null(x$weights)) "computed" else "not computed", "\n\n")

  # Optional but very handy while developing: show basic validation/health.
  # This is printed (not stored) so it never affects downstream logic.
  if (exists("engine_health", mode = "function")) {
    print(engine_health(x))
  }
  invisible(x)
}

#' Summarize a PAPRIKA engine
#'
#' @param object A `paprika_engine`.
#' @param ... Passed to methods.
#' @export
summary.paprika_engine <- function(object, ...) {
  object <- validate_engine(object)

  h <- if (exists("engine_health", mode = "function")) engine_health(object) else NULL

  list(
    n_criteria = length(object$criteria),
    n_alternatives = nrow(object$alternatives),
    n_decisions = nrow(object$decisions),
    phase = object$phase,
    has_weights = !is.null(object$weights),
    health = h,
    diagnostics = object$diagnostics
  )
}

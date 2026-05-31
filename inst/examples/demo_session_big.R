# demo_session_big.R
#
# Large synthetic end-to-end test for fairpaprika
# - 8 criteria (mixed directionality)
# - 60 profiles (options)
# - interactions enabled for selected pairs
# - synthetic respondent (hidden additive + interaction utility) with noise
#
# Purpose: stress-test selector, fairness constraints, fallback logic, and
# seed/order sensitivity.

# Always load the local source version so the demo reflects the current code changes,
# even if a released version is installed.
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  message("devtools not available; falling back to installed fairpaprika (may not include local changes).")
  library(fairpaprika)
}

cat("NEW DEMO")
library(fairpaprika)

# 1. Create engine with domains
domains <- list(
  Effect       = c("Low", "Medium", "High"),
  SideEffects  = c("High", "Medium", "Low"),
  Convenience  = c("Low", "Medium", "High"),
  Monitoring   = c("High", "Medium", "Low")
)

eng <- engine_create(domains, seed = 42)

# 2. Collect some decisions (simulate preference elicitation)
for (i in 1:10) {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  q <- nxt$question
  if (is.null(q)) break
  # Simulate choosing "A" for all questions
  eng <- engine_add_decision(eng, pref = "A")
  if (engine_done(eng)) break
}

# 3. Now bootstrap will work!
eng <- engine_compute(eng)

# Test parallel bootstrap
boot <- engine_bootstrap(eng, B = 200, parallel = TRUE, n_cores = 4)
print(boot)

# Test bootstrap convergence
conv <- bootstrap_convergence(boot)
conv$converged


# Test coordinate Hit-and-Run
samples_coord <- engine_polytope_sample(eng, n = 500, method = "coordinate", progress = TRUE)

# Test multi-chain with diagnostics
# 1. Collect more decisions first
for (i in 1:20) {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  if (is.null(nxt$question)) break
  eng <- engine_add_decision(eng, pref = "A")
}
eng <- engine_compute(eng)

# 2. Use coordinate method with longer burn-in
samples_mc <- engine_polytope_sample(
  eng, 
  n = 200, 
  burnin = 1000,  # much longer burn-in
  method = "coordinate",
  chains = 4
)
diag <- polytope_diagnostics(samples_mc)
print(diag$summary)

# Test sensitivity analysis
sens <- sensitivity_analysis(eng, param = "eps_strict", range = c(0.5, 2.0), steps = 5)
print(sens$summary)

# Single chain is fine for most purposes
samples <- engine_polytope_sample(eng, n = 1000, method = "coordinate")
# More stable configuration
diag <- polytope_diagnostics(samples)
print(diag)

library(fairpaprika)

# 1. Setup: Create engine and collect some decisions
domains <- list(
  Effect       = c("Low", "Medium", "High"),
  SideEffects  = c("High", "Medium", "Low"),
  Convenience  = c("Low", "Medium", "High"),
  Monitoring   = c("High", "Medium", "Low")
)

eng <- engine_create(domains, seed = 42)

# Collect ~15 decisions
for (i in 1:15) {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  if (is.null(nxt$question)) break
  # Simulate choices (vary them realistically)
  choice <- sample(c("A", "A", "A", "E"), 1, prob = c(0.7, 0.1, 0.1, 0.1))
  eng <- engine_add_decision(eng, pref = choice)
  if (engine_done(eng)) break
}

eng <- engine_compute(eng)

# 2. Test SDM Decision Quality
quality <- sdm_decision_quality(eng)
print(quality$overall_quality)
print(quality$quality_components)
print(quality$confidence)
print(quality$deliberation_quality)

# 3. Test Patient Journey Tracking
journey <- sdm_journey_report(eng)
print(journey$cognitive_load)
print(journey$revision_patterns)
print(journey$fatigue_indicators)

# View trajectory of top choice
sapply(journey$trajectory, function(x) x$top_option)

# 4. Test Decision Burden
burden <- sdm_decision_burden(eng)
print(burden$fatigue_score)
print(burden$recommendations)

# 5. Test Treatment Validation - MS context
profiles_ms <- data.frame(
  Effect = c("High", "Medium", "Low"),
  SideEffects = c("Low", "Medium", "High"),
  Convenience = c("High", "Medium", "Low"),
  Monitoring = c("Low", "Medium", "High")
)

# This should validate successfully
validation <- validate_treatment_profiles(
  profiles_ms, 
  domains, 
  context = "ms"
)
print(validation$valid)
print(validation$warnings)

# Test with missing criterion (should fail)
profiles_bad <- data.frame(
  Effect = c("High", "Medium"),
  SideEffects = c("Low", "High")
  # Missing Convenience and Monitoring
)

validation_bad <- validate_treatment_profiles(
  profiles_bad,
  domains,
  context = "ms"
)
print(validation_bad$valid)  # FALSE
print(validation_bad$warnings)

# 6. Test with different contexts
validate_treatment_profiles(profiles_ms, domains, context = "general")
validate_treatment_profiles(profiles_ms, domains, context = "oncology")  # Will show some warnings

# 7. Comprehensive test combining everything
cat("\n=== Comprehensive SDM Report ===\n")
cat("Decision Quality Score:", quality$overall_quality, "\n")
cat("Preference Clarity:", quality$preference_clarity, "\n")
cat("Confidence:", quality$confidence$confidence_score, "\n")
cat("Burden Score:", burden$fatigue_score, "\n")
cat("Total Questions:", journey$fatigue_indicators$total_questions, "\n")
cat("Undo Rate:", journey$revision_patterns$undo_rate, "\n")


cat("OLD DEMO")
set.seed(3)

# ----------------------------
# 1) Domains (ordered worst -> best)
#    NOTE: For "risk-like" criteria where lower is better (e.g., SideEffects),
#    we put worst->best as High -> ... -> Low (as in demo_session.R). :contentReference[oaicite:1]{index=1}
# ----------------------------
domains <- list(
  Effectiveness      = c("Low", "Medium", "High", "VeryHigh"),
  RelapseControl     = c("Low", "Medium", "High", "VeryHigh"),
  DisabilityProgress = c("High", "Medium", "Low", "VeryLow"),     # lower is better
  SideEffects        = c("High", "Medium", "Low", "VeryLow"),     # lower is better
  SeriousInfections  = c("High", "Medium", "Low", "VeryLow"),     # lower is better
  MonitoringBurden   = c("High", "Medium", "Low", "VeryLow"),     # lower is better
  Convenience        = c("Low", "Medium", "High", "VeryHigh"),
  QoLImpact          = c("Low", "Medium", "High", "VeryHigh")
)

crit_names <- names(domains)

# ----------------------------
# 2) Build many profiles (options)
# ----------------------------
n_profiles <- 25L

# Helper: sample levels with mild structure (avoid totally uniform random)
sample_levels <- function(levels, n, bias_best = 0.0) {
  # bias_best in [0,1]: higher -> more weight on better levels
  k <- length(levels)
  # weights increase toward best
  w <- (1:k) ^ (1 + 3 * bias_best)
  w <- w / sum(w)
  sample(levels, size = n, replace = TRUE, prob = w)
}

profiles <- data.frame(
  Effectiveness      = sample_levels(domains$Effectiveness,      n_profiles, bias_best = 0.40),
  RelapseControl     = sample_levels(domains$RelapseControl,     n_profiles, bias_best = 0.35),
  DisabilityProgress = sample_levels(domains$DisabilityProgress, n_profiles, bias_best = 0.30),
  SideEffects        = sample_levels(domains$SideEffects,        n_profiles, bias_best = 0.20),
  SeriousInfections  = sample_levels(domains$SeriousInfections,  n_profiles, bias_best = 0.10),
  MonitoringBurden   = sample_levels(domains$MonitoringBurden,   n_profiles, bias_best = 0.15),
  Convenience        = sample_levels(domains$Convenience,        n_profiles, bias_best = 0.25),
  QoLImpact          = sample_levels(domains$QoLImpact,          n_profiles, bias_best = 0.30),
  stringsAsFactors = FALSE
)
rownames(profiles) <- paste0("Therapy", seq_len(n_profiles))

# Optional: enforce uniqueness / reduce duplicates a bit
profiles <- unique(profiles)
while (nrow(profiles) < n_profiles) {
  add <- profiles[ sample(seq_len(nrow(profiles)), 1), , drop = FALSE ]
  # perturb one criterion randomly
  j <- sample(crit_names, 1)
  add[[j]] <- sample(domains[[j]], 1)
  profiles <- unique(rbind(profiles, add))
}
profiles <- profiles[seq_len(n_profiles), , drop = FALSE]
rownames(profiles) <- paste0("Therapy", seq_len(n_profiles))

# ----------------------------
# 3) Configure engine (interactions enabled)
# ----------------------------
# Choose a few interaction pairs (engine-level)
# Example: Effectiveness x SideEffects (patients may tolerate risk only with high benefit),
#          MonitoringBurden x Convenience (burden and convenience interplay),
#          RelapseControl x SeriousInfections (trade-off context).
interaction_pairs <- list(
  c("Effectiveness", "SideEffects"),
  c("MonitoringBurden", "Convenience"),
  c("RelapseControl", "SeriousInfections"),
  c("Convenience", "QoLImpact")
)

settings <- list(
  mode = "full",
  interactions = list(
    enabled = TRUE,               # enable interaction pairs for the demo
    pairs = interaction_pairs,
    families = list(conditional = TRUE, joint = FALSE),
    max_pairs = 4L,
    mix = list(window = 12L, max_share = 0.30),   # tighter cap for balance
    coverage_bonus = 4,            # softer bonus for interactions
    first_bonus = 2,               # small early nudge
    min_questions = 1              # single mandatory interaction, then compete fairly
  ),
  fair = list(
    pair_coverage = FALSE,         # Coverage aktiv für prozedurale Fairness
    exposure_balance = TRUE,
    exposure_gap_limit = 1,        # tighter gap to avoid repeated pairs
    interaction_coverage = TRUE   # Interaktionspaare sollen mindestens einmal erscheinen
  ),
  selector = list(
    n_samples = 120L,
    burnin = 50L,
    thin = 1L,
    top_k = 1L,                   # deterministic pick to reduce path dependence
    candidate_pool = 100L,        # stratified pool
    pair_cap = 2L,                # stricter repeat limit per pair
    adaptive = list(ig_ci_width = 0.02),
    score = list(beta = 2)        # stronger fairness weight in scoring
  ),
  stop = list(
    # moderate stop; allow fair coverage before stopping
    win_prob = 0.9,
    win_conf_prob = 0.9,
    win_conf_streak = 2L,
    topk_prob = 0.8,
    eig_threshold = 0.008,
    no_progress_ig = 3,
    weight_span_eps = NA_real_
  ),
  max_q = 28L,
  min_q = 10L
)

eng <- engine_create(domains, settings = settings, seed = 123)
eng <- engine_set_profiles(eng, profiles)
# For the demo, jump directly into the tradeoff phase so interactions are eligible immediately
eng$phase <- "tradeoff"
eng$queues$anchor <- list()
eng$queues$pairwise <- list()
# Optional: toggle dominance-implied closure
# eng <- engine_create(domains, settings = modifyList(settings, list(closure = list(dominance = FALSE))), seed = 123)
cat("Candidate types before seeding:\n")
print(table(vapply(eng$candidates, function(c) c$type %||% "tradeoff", character(1))))



# ----------------------------
# 4) Synthetic respondent (hidden truth)
#    Additive + a small set of pairwise interaction boosts/penalties
# ----------------------------
# Map level -> numeric (worst=0 .. best=1)
level_to_score <- function(level, levels) {
  idx <- match(level, levels)
  if (anyNA(idx)) stop("Unknown level encountered")
  (idx - 1) / (length(levels) - 1)
}

# Make hidden additive weights (sum to 1)
w_true <- runif(length(crit_names))
w_true <- w_true / sum(w_true)
names(w_true) <- crit_names

# True interaction coefficients per pair (small magnitude)
# Positive means "synergy" (the joint combo is valued more than additive would suggest).
# Negative means "redundancy/penalty".
int_pairs <- interaction_pairs
int_coef <- c(0.20, 0.10, -0.12, -0.15)   # tweak as desired
names(int_coef) <- vapply(int_pairs, paste, collapse = "×", FUN.VALUE = character(1))

# Interaction feature: product of (scaled scores) for the two criteria
interaction_term <- function(row, pair) {
  i <- pair[1]; j <- pair[2]
  si <- level_to_score(row[[i]], domains[[i]])
  sj <- level_to_score(row[[j]], domains[[j]])
  si * sj
}

utility_true <- function(row) {
  s <- 0
  # additive
  for (cn in crit_names) {
    s <- s + w_true[[cn]] * level_to_score(row[[cn]], domains[[cn]])
  }
  # interactions
  for (p in seq_along(int_pairs)) {
    nm <- paste(int_pairs[[p]], collapse = "×")
    s <- s + int_coef[[nm]] * interaction_term(row, int_pairs[[p]])
  }
  s
}

# Stochastic choice model for questions:
# pref = A/B/E with logistic + indifference band
respond_to_question <- function(q, tau = 0.03, ...) {
  # deterministic respondent aligned with utility_true (no stochastic flip)
  a <- as.list(q$a)
  b <- as.list(q$b)
  du <- utility_true(a) - utility_true(b)
  if (abs(du) <= tau) return("E")
  if (du > 0) "A" else "B"
}

# Force interactions active and seed with one interaction question so at least one
# interaction is asked in the demo.
eng$interactions_active <- TRUE
eng$interactions_pairs_active <- settings$interactions$pairs[seq_len(min(length(settings$interactions$pairs), settings$interactions$max_pairs %||% length(settings$interactions$pairs)))]
# Seed phase removed; selector enforces min interaction questions.
# If you want to demonstrate correction/undo, you can call:
# eng <- engine_undo_decisions(eng, n = 1L)

# ----------------------------
# 5) Run a longer elicitation loop
# ----------------------------
# respect settings
max_q <- settings$max_q %||% 25L
qs <- vector("list", max_q)
prefs <- character(0)
warn_messages <- character()
withCallingHandlers({
  for (t in seq_len(max_q)) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) {
      message("No more questions (NULL) at step ", t, ".")
      break
    }
    qs[[t]] <- q

    pref <- respond_to_question(q, tau = 0.03)
    prefs <- c(prefs, pref)
    eng <- engine_add_decision(eng, pref = pref)

    if (engine_done(eng)) {
      message("Engine done at step ", t, ".")
      break
    }
  }

  qs <<- Filter(Negate(is.null), qs)
  # Compute results
  eng <<- engine_compute(eng)
}, warning = function(w) {
  warn_messages <<- c(warn_messages, conditionMessage(w))
  invokeRestart("muffleWarning")
})

# ----------------------------
# 6) Print core diagnostics
# ----------------------------
cat("=== TRUE additive weights (hidden) ===\n")
print(round(w_true, 3))

cat("\n=== TRUE interaction coefs (hidden) ===\n")
print(int_coef)

cat("\n=== Winner probabilities (engine) ===\n")
print(eng$diagnostics$winner_probabilities)

cat("\n=== Top-3 explanations (engine) ===\n")
print(eng$diagnostics$profile_explanations_text)

cat("\n=== Procedural justice report (session) ===\n")
print(engine_procedural_justice(eng)$session)
if (length(warn_messages)) {
  cat("\n=== Warnungen (gebündelt) ===\n")
  cat(unique(warn_messages), sep = "\n")
}

interaction_count <- sum(vapply(eng$audit, function(x) isTRUE(x$interaction_pair), logical(1)))
cat("\n=== Interaktionsfragen im Demo ===\n")
cat("Anzahl Interaktionsfragen: ", interaction_count, "\n")
if (interaction_count < 5) {
  cat("Hinweis: Weniger als 5 Interaktionsfragen gestellt – erhöhe ggf. coverage/first_bonus oder lockere Balance-Filter.\n")
}

cat("\n=== Session summary ===\n")
cat("Questions asked:", length(qs), "\n")
cat("Pref counts:\n")
print(table(prefs))


# ----------------------------
# 7) (Optional) Seed sensitivity quick check
# ----------------------------
# Replicate a few seeds to see top-3 stability quickly (lightweight smoke test)
quick_seed_sensitivity <- function(seeds = 1:10) {
  top3s <- list()
  for (sd in seeds) {
    eng2 <- engine_create(domains, settings = settings, seed = sd)
    eng2 <- engine_set_profiles(eng2, profiles)

    for (t in 1:30) {
      nxt <- engine_next_question(eng2)
      eng2 <- nxt$engine
      q <- nxt$question
      if (is.null(q)) break
      pref <- respond_to_question(q, tau = 0.03)
      eng2 <- engine_add_decision(eng2, pref = pref)
      if (engine_done(eng2)) break
    }
    eng2 <- engine_compute(eng2)
    # If you store top-3 somewhere else, adapt here.
    wp <- eng2$diagnostics$winner_probabilities
    top3 <- names(sort(wp, decreasing = TRUE))[1:3]
    top3s[[as.character(sd)]] <- top3
  }
  top3s
}

seed_report <- seed_stability_report(domains, settings = settings, seeds = 1:4, max_profiles = 10)
cat("\n=== Seed stability report (light) ===\n")
print(list(
  seeds = seed_report$seeds,
  top1_unique = seed_report$top1_unique,
  top3_jaccard = seed_report$top3_jaccard,
  n_questions_mean = seed_report$n_questions_mean
))
if (!is.null(seed_report$top3_sets)) {
  top1s <- vapply(seed_report$top3_sets, function(x) x[1], character(1))
  top1_mode <- names(sort(table(top1s), decreasing = TRUE))[1]
  top3_freq <- sort(table(unlist(seed_report$top3_sets)) / length(seed_report$top3_sets), decreasing = TRUE)
  cat("\n=== Seed aggregate ===\n")
  cat("Top1 mode across seeds:", top1_mode, "\n")
  cat("Top-3 inclusion freq:\n")
  print(round(top3_freq, 2))
}

table(vapply(eng$audit, function(x) x$interaction_pair, logical(1)))

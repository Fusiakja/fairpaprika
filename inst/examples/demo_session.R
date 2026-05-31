# Demo dataset and run for fairpaprika
#
# This script builds a small synthetic example with 4 criteria, 5 profiles,
# and a short decision sequence. It can be sourced to verify end-to-end flow.

library(fairpaprika)

# 1) Domains (ordered worst -> best)
domains <- list(
  Effect       = c("Low", "Medium", "High"),
  SideEffects  = c("High", "Medium", "Low"),
  Convenience  = c("Low", "Medium", "High"),
  Monitoring   = c("High", "Medium", "Low")
)

# 2) Profiles (real options)
profiles <- data.frame(
  Effect      = c("High", "Medium", "High", "Low", "Medium"),
  SideEffects = c("Low", "Medium", "High", "Medium", "Low"),
  Convenience = c("High", "High", "Medium", "Low", "Medium"),
  Monitoring  = c("Low", "Medium", "High", "High", "Medium"),
  row.names   = paste0("Option", 1:5),
  stringsAsFactors = FALSE
)

# 3) Run engine with a small synthetic decision set (no interactions for simplicity)
settings <- list(
  interactions = list(enabled = FALSE),
  fair = list(pair_coverage = TRUE),
  selector = list(top_k = 3L)
)

eng <- engine_create(domains, settings = settings, seed = 42)
eng <- engine_set_profiles(eng, profiles)

# Simulate a short elicitation run (always choose A)
qs <- list()
for (t in 1:6) {
  nxt <- engine_next_question(eng)
  eng <- nxt$engine
  q <- nxt$question
  if (is.null(q)) break
  qs[[length(qs) + 1]] <- q
  eng <- engine_add_decision(eng, pref = "A")
  if (engine_done(eng)) break
}
eng <- engine_compute(eng)

cat("Winner probabilities:\n")
print(eng$diagnostics$winner_probabilities)

cat("\nTop-3 explanations:\n")
print(eng$diagnostics$profile_explanations_text)

cat("\nProcedural justice report:\n")
print(engine_procedural_justice(eng)$session)

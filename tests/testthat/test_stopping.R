devtools::load_all(".")

# Test classic PAPRIKA adaptive stopping
cat("=== Testing Classic PAPRIKA Adaptive Stopping ===\n\n")

domains <- list(
    Efficacy = c("Low", "Medium", "High"),
    Safety = c("Risky", "Moderate", "Safe"),
    Cost = c("Expensive", "Affordable", "Cheap")
)

# Create 3 therapy profiles with clear ranking
therapies <- data.frame(
    Efficacy = c("High", "Medium", "Low"),
    Safety = c("Safe", "Moderate", "Risky"),
    Cost = c("Cheap", "Affordable", "Expensive"),
    stringsAsFactors = FALSE
)
rownames(therapies) <- c("Therapy_A", "Therapy_B", "Therapy_C")

cat("Therapy Profiles:\n")
print(therapies)
cat("\nTherapy A should dominate all others (best on all criteria)\n\n")

# Classic mode with high max_q to allow adaptive stopping
settings <- list(
    mode = "classic",
    min_q = 3,
    max_q = 50 # High budget to see if we stop early
)

eng <- engine_create(domains, settings = settings, seed = 123)
eng <- engine_set_profiles(eng, therapies)

cat("Starting elicitation...\n")

# Answer questions consistently (prefer higher indices = better)
count <- 0
while (!engine_done(eng) && count < 20) {
    nxt <- engine_next_question(eng)
    if (is.null(nxt$question)) break

    a <- nxt$question$a
    b <- nxt$question$b

    # Score based on level indices (higher = better)
    score_a <- 0
    score_b <- 0
    for (crit in names(a)) {
        levels <- domains[[crit]]
        score_a <- score_a + match(a[[crit]], levels)
        score_b <- score_b + match(b[[crit]], levels)
    }

    pref <- if (score_a > score_b) "A" else if (score_b > score_a) "B" else "E"

    eng <- nxt$engine
    eng <- engine_add_decision(eng, pref = pref)
    count <- count + 1

    # Check if done after each decision
    if (engine_done(eng)) {
        cat(sprintf("\n✓ STOPPED EARLY after %d questions (outcome-based stopping)\n", count))
        break
    }
}

if (count >= 20) {
    cat(sprintf("\n✗ Hit loop limit (%d questions)\n", count))
} else if (!engine_done(eng)) {
    cat(sprintf("\n? Exited loop after %d questions but not done\n", count))
}

# Compute final results
eng <- engine_compute(eng)

cat("\n=== RESULTS ===\n")
cat(sprintf("Questions asked: %d / %d max\n", nrow(eng$decisions), settings$max_q))
cat(sprintf(
    "Reduction: %.1f%% (asked %d instead of %d)\n",
    (1 - nrow(eng$decisions) / settings$max_q) * 100,
    nrow(eng$decisions),
    settings$max_q
))

if (!is.null(eng$diagnostics$winner_probabilities)) {
    cat("\nWinner probabilities:\n")
    probs <- eng$diagnostics$winner_probabilities
    for (i in seq_along(probs)) {
        cat(sprintf("  %s: %.3f\n", names(probs)[i], probs[i]))
    }
}

if (!is.null(eng$diagnostics$dominated_profiles)) {
    dominated <- eng$diagnostics$dominated_profiles
    if (length(dominated) > 0) {
        cat(sprintf("\nDominated profiles: %d\n", length(dominated)))
        for (idx in dominated) {
            cat(sprintf("  - %s\n", rownames(therapies)[idx]))
        }
    }
}

cat("\n=== EFFICIENCY METRICS ===\n")
diag <- paprika_diagnostics(eng)
cat(sprintf("Question efficiency: %.1f%%\n", diag$efficiency$question_efficiency * 100))
cat(sprintf("Implied preferences: %d\n", diag$efficiency$implied_preferences))

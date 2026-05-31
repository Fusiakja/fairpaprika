devtools::load_all(".")

# Test dominated alternative pruning
cat("=== Testing Dominated Alternative Pruning ===\n\n")

domains <- list(
    Effectiveness = c("Low", "Medium", "High"),
    SideEffects = c("Severe", "Moderate", "Mild"),
    Cost = c("Expensive", "Moderate", "Cheap")
)

# Create 4 therapy profiles
therapies <- data.frame(
    Effectiveness = c("High", "Medium", "High", "Low"),
    SideEffects = c("Mild", "Moderate", "Severe", "Severe"),
    Cost = c("Cheap", "Moderate", "Moderate", "Expensive"),
    stringsAsFactors = FALSE
)
rownames(therapies) <- c("Therapy_A", "Therapy_B", "Therapy_C", "Therapy_D")

cat("Therapy Profiles:\n")
print(therapies)
cat("\n")

# Therapy D should be dominated by all others (worst on all criteria)
# Therapy C might be dominated by A (both High effectiveness, but C has worse side effects)

settings <- list(
    mode = "classic",
    max_q = 20
)

eng <- engine_create(domains, settings = settings)
eng <- engine_set_profiles(eng, therapies)

# Simulate some decisions favoring Effectiveness
# We'll prefer high effectiveness
nxt <- engine_next_question(eng)
count <- 0
while (!is.null(nxt$question) && count < 10) {
    # Simple heuristic: prefer High > Medium > Low
    a <- nxt$question$a
    b <- nxt$question$b

    # Score based on order
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
    nxt <- engine_next_question(eng)
    count <- count + 1
}

cat(sprintf("Answered %d questions\n\n", count))

# Compute results
eng <- engine_compute(eng)

cat("=== RESULTS ===\n\n")

if (!is.null(eng$diagnostics$dominated_profiles)) {
    dominated <- eng$diagnostics$dominated_profiles

    if (length(dominated) > 0) {
        cat(sprintf("✓ DOMINATED PROFILES DETECTED: %d\n", length(dominated)))
        cat("Dominated therapies:\n")
        for (idx in dominated) {
            cat(sprintf("  - %s (row %d)\n", rownames(therapies)[idx], idx))
        }
        cat("\n")

        if (!is.null(eng$diagnostics$dominance_pairs)) {
            cat("Dominance relationships:\n")
            print(eng$diagnostics$dominance_pairs)
            cat("\n")
        }

        # Check utilities
        if (!is.null(eng$diagnostics$profile_utilities_by_criterion)) {
            cat("Utilities by criterion:\n")
            print(round(eng$diagnostics$profile_utilities_by_criterion, 2))
            cat("\n")
        }

        # Check winner probabilities
        if (!is.null(eng$diagnostics$winner_probabilities)) {
            cat("Winner probabilities (dominated = 0):\n")
            probs <- eng$diagnostics$winner_probabilities
            for (i in seq_along(probs)) {
                cat(sprintf("  %s: %.3f\n", names(probs)[i], probs[i]))
            }
        }
    } else {
        cat("✓ No dominated profiles found (all competitive)\n")
    }
} else {
    cat("✗ Pruning not applied (diagnostics missing)\n")
}

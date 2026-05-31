devtools::load_all(".")

# Test enhanced PAPRIKA diagnostics
cat("=== Testing Enhanced PAPRIKA Diagnostics ===\n\n")

domains <- list(
    Quality = c("Poor", "Good", "Excellent"),
    Price = c("Expensive", "Moderate", "Cheap"),
    Speed = c("Slow", "Medium", "Fast")
)

settings <- list(mode = "classic", max_q = 30)
eng <- engine_create(domains, settings = settings, seed = 123)

# Simulate a session with some deliberate inconsistency
cat("Simulating decision session...\n")

# Make 8 decisions
for (i in 1:8) {
    nxt <- engine_next_question(eng)
    if (is.null(nxt$question)) break

    # First 7: prefer based on sum of level indices
    # 8th: deliberately inconsistent
    a <- nxt$question$a
    b <- nxt$question$b

    score_a <- 0
    score_b <- 0
    for (crit in names(a)) {
        levels <- domains[[crit]]
        score_a <- score_a + match(a[[crit]], levels)
        score_b <- score_b + match(b[[crit]], levels)
    }

    if (i == 8) {
        # Force inconsistency: flip the preference
        pref <- if (score_a > score_b) "B" else "A"
    } else {
        pref <- if (score_a > score_b) "A" else if (score_b > score_a) "B" else "E"
    }

    eng <- nxt$engine
    eng <- engine_add_decision(eng, pref = pref)
}

cat(sprintf("\nAnswered %d questions\n\n", nrow(eng$decisions)))

# Run diagnostics
diag <- paprika_diagnostics(eng)

cat("=== EFFICIENCY METRICS ===\n")
cat(sprintf("Questions asked: %d\n", diag$efficiency$questions_asked))
cat(sprintf("Total possible pairs: %d\n", diag$efficiency$total_possible_pairs))
cat(sprintf(
    "Question efficiency: %.2f%% (%.2fx reduction)\n",
    diag$efficiency$question_efficiency * 100,
    diag$efficiency$reduction_factor
))
cat("\n")

cat("=== CLOSURE EFFECTIVENESS ===\n")
cat(sprintf("Directly asked: %d preferences\n", diag$efficiency$questions_asked))
cat(sprintf("Implied by transitivity: %d preferences\n", diag$efficiency$implied_preferences))
cat(sprintf("Total known: %d preferences\n", diag$efficiency$total_preferences))
cat(sprintf(
    "Closure effectiveness: %.2f%%\n",
    diag$efficiency$closure_effectiveness * 100
))
cat("\n")

cat("=== INCONSISTENCY DETECTION ===\n")
if (diag$inconsistencies$has_inconsistencies) {
    cat(sprintf(
        "✗ INCONSISTENCIES DETECTED: %d\n\n",
        diag$inconsistencies$inconsistent_count
    ))
    cat("Details:\n")
    print(diag$inconsistencies$inconsistency_details)
} else {
    cat("✓ No inconsistencies detected\n")
}
cat("\n")

cat("=== SUMMARY ===\n")
cat(diag$summary)
cat("\n")

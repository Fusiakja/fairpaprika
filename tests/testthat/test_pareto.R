devtools::load_all(".")

# Test active Pareto set tracking
cat("=== Testing Active Pareto Set Tracking ===\n\n")

domains <- list(
    Efficacy = c("Low", "Medium", "High"),
    Safety = c("Risky", "Moderate", "Safe"),
    Cost = c("Expensive", "Affordable", "Cheap")
)

# Create 5 therapy profiles with varying dominance
therapies <- data.frame(
    Efficacy = c("High", "High", "Medium", "Low", "Low"),
    Safety = c("Safe", "Moderate", "Safe", "Moderate", "Risky"),
    Cost = c("Cheap", "Affordable", "Cheap", "Expensive", "Expensive"),
    stringsAsFactors = FALSE
)
rownames(therapies) <- c("Therapy_A", "Therapy_B", "Therapy_C", "Therapy_D", "Therapy_E")

cat("Therapy Profiles:\n")
print(therapies)
cat("\nExpected dominance:\n")
cat("  A dominates all (best on all)\n")
cat("  E dominated by all (worst on all)\n")
cat("  B, C, D compete in middle\n\n")

settings <- list(mode = "classic", min_q = 2, max_q = 30)
eng <- engine_create(domains, settings = settings, seed = 123)
eng <- engine_set_profiles(eng, therapies)

cat("=== ELICITATION SESSION ===\n\n")

# Answer questions
for (q_num in 1:12) {
    nxt <- engine_next_question(eng)
    if (is.null(nxt$question)) {
        cat("\nNo more questions available\n")
        break
    }

    # Simple heuristic: prefer higher level indices
    a <- nxt$question$a
    b <- nxt$question$b

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

    # Show Pareto set after each decision
    cat(sprintf("Question %d: ", q_num))
    summary <- paprika_pareto_summary(eng)
    cat(summary, "\n\n")

    if (engine_done(eng)) {
        cat("✓ Stopping criterion met (all profiles resolved)\n")
        break
    }
}

cat("\n=== FINAL RESULTS ===\n\n")
eng <- engine_compute(eng)

if (!is.null(eng$diagnostics$winner_probabilities)) {
    cat("Winner probabilities:\n")
    probs <- eng$diagnostics$winner_probabilities
    for (i in seq_along(probs)) {
        cat(sprintf("  %s: %.3f\n", names(probs)[i], probs[i]))
    }
}

if (!is.null(eng$diagnostics$pareto_set)) {
    cat("\nFinal Pareto set:\n")
    cat(paprika_pareto_summary(eng), "\n")
}

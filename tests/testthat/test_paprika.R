devtools::load_all(".")

# Test PAPRIKA implementation
domains <- list(
    Effect = c("None", "Small", "Big"),
    SideEffects = c("Severe", "Moderate", "None"),
    Cost = c("High", "Medium", "Low")
)

cat("=== Testing PAPRIKA Classic Mode ===\n\n")

# Create engine in classic mode
settings <- list(
    mode = "classic",
    min_q = 0L,
    max_q = 50L
)

eng <- engine_create(domains, settings = settings, seed = 42)

cat("Engine created successfully\n")
cat(sprintf("Total alternatives: %d\n", nrow(eng$alternatives)))
cat(sprintf("Total candidates in PAPRIKA bank: %d\n", length(eng$candidates)))

if (!is.null(eng$paprika)) {
    cat(sprintf("PAPRIKA total pairs: %d\n", eng$paprika$total_pairs))
}

# Try to get first question
cat("\n=== First Question ===\n")
nxt <- engine_next_question(eng)
if (!is.null(nxt$question)) {
    cat("Question type:", nxt$question$type, "\n")
    cat("A:", paste(names(nxt$question$a), nxt$question$a, sep = "=", collapse = ", "), "\n")
    cat("B:", paste(names(nxt$question$b), nxt$question$b, sep = "=", collapse = ", "), "\n")
} else {
    cat("ERROR: No question generated!\n")
}

# Add a decision and check filtering
if (!is.null(nxt$question)) {
    cat("\n=== After First Answer ===\n")
    eng <- nxt$engine
    eng <- engine_add_decision(eng, pref = "A")

    cat(sprintf("Decisions made: %d\n", nrow(eng$decisions)))

    # Get next question
    nxt2 <- engine_next_question(eng)
    if (!is.null(nxt2$question)) {
        cat("Second question generated successfully\n")
        cat(sprintf("Remaining candidates: %d\n", length(nxt2$engine$candidates)))
    } else {
        cat("No second question (bank exhausted or done)\n")
    }
}

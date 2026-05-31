devtools::load_all(".")

# Test that epsilon discrimination works
domains <- list(
    A = c("Low", "High"),
    B = c("Low", "High")
)

cat("=== Testing Minimum Discrimination (Epsilon Gaps) ===\n\n")

# Create simple scenario
settings <- list(
    mode = "classic",
    eps_strict = 1e-3, # This should enforce w(High) >= w(Low) + 0.001
    max_q = 10
)

eng <- engine_create(domains, settings = settings)

# Manual decisions: prefer (A:High, B:Low) over (A:Low, B:High)
# This creates constraint: w(A:High) - w(A:Low) + w(B:Low) - w(B:High) >= eps_strict

# Add decision via key
# Find the alternatives
print(eng$alternatives)
cat("\n")

# Find the correct alternative indices
alt1_idx <- which(eng$alternatives$A == "High" & eng$alternatives$B == "Low")
alt2_idx <- which(eng$alternatives$A == "Low" & eng$alternatives$B == "High")

cat(sprintf("Alternative 1 (A:High, B:Low): index %d\n", alt1_idx))
cat(sprintf("Alternative 2 (A:Low, B:High): index %d\n", alt2_idx))

# Get keys
key1 <- eng$alt_keys[alt1_idx]
key2 <- eng$alt_keys[alt2_idx]

cat(sprintf("Keys: %s vs %s\n\n", key1, key2))

# Add decision
eng$decisions <- data.frame(
    A1 = key1,
    A2 = key2,
    pref = "A",
    stringsAsFactors = FALSE
)

# Compute weights
eng <- engine_compute(eng)

if (!is.null(eng$weights)) {
    cat("Weights computed successfully!\n")
    print(eng$weights)

    # Extract specific weights
    w_names <- eng$results$var_names %||% eng$weights$Merkmal
    w_vals <- eng$results$weights %||% eng$weights$Nutzen

    w_A_high <- w_vals[which(grepl("A.*High", w_names))[1]]
    w_A_low <- w_vals[which(grepl("A.*Low", w_names))[1]]

    cat(sprintf("\nw(A:High) = %.6f\n", w_A_high))
    cat(sprintf("w(A:Low) = %.6f\n", w_A_low))
    cat(sprintf("Gap = %.6f\n", w_A_high - w_A_low))
    cat(sprintf("Expected gap >= eps_strict = %.6f\n", settings$eps_strict))

    if ((w_A_high - w_A_low) >= settings$eps_strict - 1e-9) {
        cat("\n✓ Epsilon discrimination WORKING! Gap enforced.\n")
    } else {
        cat("\n✗ WARNING: Gap smaller than epsilon!\n")
    }
} else {
    cat("ERROR: No weights computed\n")
}

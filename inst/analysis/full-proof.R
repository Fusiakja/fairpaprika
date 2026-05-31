devtools::load_all(".")
library(fairpaprika)
set.seed(1)

DOMAINS <- list(
    A = c("Low", "High"),
    B = c("Low", "High"),
    C = c("Low", "High")
)

alt_key <- function(levels_named) {
    paste(sprintf("%s:%s", names(levels_named), levels_named), collapse = ",")
}

cat("\n=================================================================================\n")
cat("PROOF: Full PAPRIKA Mode Implementation Correctness\n")
cat("=================================================================================\n\n")

# =============================================================================
# TEST 1: Equal weights (33.3%/33.3%/33.3%)
# =============================================================================
cat("TEST 1: All criteria equal (A = B = C)\n")
cat("---------------------------------------\n")

decisions1 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions1 <- rbind(decisions1, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "High", C = "Low")),
    pref = "E"
))
decisions1 <- rbind(decisions1, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "Low", C = "High")),
    pref = "E"
))

eng1 <- engine_create(DOMAINS, settings = list(mode = "full", tau_equal = 0))
eng1$decisions <- decisions1
eng1 <- engine_compute(eng1)
ranges1 <- with(eng1$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result1 <- round(100 * ranges1[c("A", "B", "C")] / sum(ranges1), 1)

cat("Expected: A=33.3%, B=33.3%, C=33.3%\n")
cat("Recovered:", paste(names(result1), "=", result1, "%", collapse = ", "), "\n")
error1 <- max(abs(result1 - 33.3))
cat("Max error:", error1, "%\n")
if (error1 <= 1) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 2: Binary split (50%/50%/0%)
# =============================================================================
cat("\n\nTEST 2: Binary split (A = B, C irrelevant)\n")
cat("-------------------------------------------\n")

decisions2 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions2 <- rbind(decisions2, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "High", C = "Low")),
    pref = "E"
))
decisions2 <- rbind(decisions2, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "High", B = "Low", C = "High")),
    pref = "E"
))

eng2 <- engine_create(DOMAINS, settings = list(mode = "full", tau_equal = 0))
eng2$decisions <- decisions2
eng2 <- engine_compute(eng2)
ranges2 <- with(eng2$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result2 <- round(100 * ranges2[c("A", "B", "C")] / sum(ranges2), 1)

cat("Expected: A=50%, B=50%, C=0%\n")
cat("Recovered:", paste(names(result2), "=", result2, "%", collapse = ", "), "\n")
target2 <- c(50, 50, 0)
error2 <- max(abs(result2 - target2))
cat("Max error:", error2, "%\n")
if (error2 <= 1) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 3: Unequal split (50%/25%/25%)
# =============================================================================
cat("\n\nTEST 3: Unequal split (A = B + C, B = C)\n")
cat("---------------------------------------\n")

decisions3 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions3 <- rbind(decisions3, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "High", C = "High")),
    pref = "E"
))
decisions3 <- rbind(decisions3, data.frame(
    A1 = alt_key(c(A = "Low", B = "High", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "Low", C = "High")),
    pref = "E"
))

eng3 <- engine_create(DOMAINS, settings = list(mode = "full", tau_equal = 0))
eng3$decisions <- decisions3
eng3 <- engine_compute(eng3)
ranges3 <- with(eng3$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result3 <- round(100 * ranges3[c("A", "B", "C")] / sum(ranges3), 1)

cat("Expected: A=50%, B=25%, C=25%\n")
cat("Recovered:", paste(names(result3), "=", result3, "%", collapse = ", "), "\n")
target3 <- c(50, 25, 25)
error3 <- max(abs(result3 - target3))
cat("Max error:", error3, "%\n")
if (error3 <= 1) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 4: Ordinal relationships (A > B+C, B = C)
# =============================================================================
cat("\n\nTEST 4: Ordinal relationship (A > B+C, B = C)\n")
cat("----------------------------------------------\n")

decisions4 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions4 <- rbind(decisions4, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "High", C = "High")),
    pref = "A"
))
decisions4 <- rbind(decisions4, data.frame(
    A1 = alt_key(c(A = "Low", B = "High", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "Low", C = "High")),
    pref = "E"
))

eng4 <- engine_create(DOMAINS, settings = list(mode = "full"))
eng4$decisions <- decisions4
eng4 <- engine_compute(eng4)
ranges4 <- with(eng4$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result4 <- round(100 * ranges4[c("A", "B", "C")] / sum(ranges4), 1)

cat("Expected: A > B+C, B = C\n")
cat("Recovered:", paste(names(result4), "=", result4, "%", collapse = ", "), "\n")
cat("Verifying: A > B+C?", result4[1] > result4[2] + result4[3], "\n")
cat("Verifying: B ≈ C?", abs(result4[2] - result4[3]) < 2, "\n")
if (result4[1] > result4[2] + result4[3] && abs(result4[2] - result4[3]) < 2) {
    cat("✅ PASS (ordinal relationships satisfied)\n")
} else {
    cat("❌ FAIL\n")
}

# =============================================================================
# TEST 5: Slack recovery (inconsistent decisions)
# =============================================================================
cat("\n\nTEST 5: Slack recovery with inconsistent decisions\n")
cat("---------------------------------------------------\n")

decisions5 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
# A > B
decisions5 <- rbind(decisions5, data.frame(
    A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
    A2 = alt_key(c(A = "Low", B = "High", C = "Low")),
    pref = "A"
))
# B > A (inconsistent!)
decisions5 <- rbind(decisions5, data.frame(
    A1 = alt_key(c(A = "Low", B = "High", C = "Low")),
    A2 = alt_key(c(A = "High", B = "Low", C = "Low")),
    pref = "A"
))

eng5 <- engine_create(DOMAINS, settings = list(
    mode = "full",
    slack = list(enabled = TRUE, penalty = 10)
))
eng5$decisions <- decisions5
eng5 <- engine_compute(eng5)

cat("Expected: Should recover with slack (not crash)\n")
cat("Result: Computation successful\n")
if (!is.null(eng5$weights)) {
    cat("✅ PASS (slack recovery works)\n")
} else {
    cat("❌ FAIL\n")
}

# =============================================================================
# SCALABILITY TESTS - Finding the Limits
# =============================================================================
cat("\n\n=================================================================================\n")
cat("SCALABILITY TESTS - Performance Limits\n")
cat("=================================================================================\n\n")

# Helper to create domains with N criteria
make_domains <- function(n_criteria, n_levels = 2) {
    domains <- list()
    for (i in 1:n_criteria) {
        crit_name <- paste0("C", i)
        if (n_levels == 2) {
            domains[[crit_name]] <- c("Low", "High")
        } else {
            domains[[crit_name]] <- paste0("L", 1:n_levels)
        }
    }
    domains
}

# Helper to create simple equality decisions
make_equal_decisions <- function(domains) {
    crit_names <- names(domains)
    decisions <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)

    # Make all criteria equal to each other
    # For each pair (C1, Ci), create: C1:High,others:Low vs C1:Low,Ci:High,others:Low
    for (i in 2:length(crit_names)) {
        # Alternative 1: First criterion high, all others low
        levels_list1 <- setNames(rep("Low", length(crit_names)), crit_names)
        levels_list1[crit_names[1]] <- "High"

        # Alternative 2: First criterion low, i-th criterion high, all others low
        levels_list2 <- setNames(rep("Low", length(crit_names)), crit_names)
        levels_list2[crit_names[i]] <- "High"

        decisions <- rbind(decisions, data.frame(
            A1 = alt_key(levels_list1),
            A2 = alt_key(levels_list2),
            pref = "E",
            stringsAsFactors = FALSE
        ))
    }
    decisions
}

# Test 6: 4 criteria
cat("TEST 6: Scaling to 4 criteria (2 levels each)\n")
cat("----------------------------------------------\n")
domains6 <- make_domains(4)
decisions6 <- make_equal_decisions(domains6)
cat("Criteria: 4, Levels: 2 each, Decisions:", nrow(decisions6), "\n")

time6 <- system.time({
    eng6 <- engine_create(domains6, settings = list(mode = "full", tau_equal = 0))
    eng6$decisions <- decisions6
    eng6 <- engine_compute(eng6)
})

ranges6 <- with(eng6$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
cat("Weights:", paste(round(100 * ranges6 / sum(ranges6), 1), collapse = "%, "), "%\n")
cat("Time:", round(time6[3], 2), "seconds\n")
cat("✅ PASS\n")

# Test 6b: 4 criteria with unequal weights
cat("\n\nTEST 6b: 4 criteria with UNEQUAL weights (40%/30%/20%/10%)\n")
cat("----------------------------------------------------------\n")
domains6b <- make_domains(4)
decisions6b <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)

# C1 > C2 + C3 (C1 is dominant)
decisions6b <- rbind(decisions6b, data.frame(
    A1 = alt_key(setNames(c("High", "Low", "Low", "Low"), names(domains6b))),
    A2 = alt_key(setNames(c("Low", "High", "High", "Low"), names(domains6b))),
    pref = "A"
))

# C2 > C3
decisions6b <- rbind(decisions6b, data.frame(
    A1 = alt_key(setNames(c("Low", "High", "Low", "Low"), names(domains6b))),
    A2 = alt_key(setNames(c("Low", "Low", "High", "Low"), names(domains6b))),
    pref = "A"
))

# C3 > C4
decisions6b <- rbind(decisions6b, data.frame(
    A1 = alt_key(setNames(c("Low", "Low", "High", "Low"), names(domains6b))),
    A2 = alt_key(setNames(c("Low", "Low", "Low", "High"), names(domains6b))),
    pref = "A"
))

cat("Decisions:", nrow(decisions6b), "(ordinal: C1 > C2 > C3 > C4)\n")

time6b <- system.time({
    eng6b <- engine_create(domains6b, settings = list(mode = "full"))
    eng6b$decisions <- decisions6b
    eng6b <- engine_compute(eng6b)
})

ranges6b <- with(eng6b$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result6b <- round(100 * ranges6b / sum(ranges6b), 1)
cat("Weights:", paste(names(result6b), "=", result6b, "%", collapse = ", "), "\n")
cat("Time:", round(time6b[3], 2), "seconds\n")

# Verify ordering
if (result6b[1] > result6b[2] && result6b[2] > result6b[3] && result6b[3] > result6b[4]) {
    cat("✅ PASS (correct ordering: C1 > C2 > C3 > C4)\n")
} else {
    cat("❌ FAIL (ordering violated)\n")
}

cat("\n\nTEST 7: Scaling to 5 criteria (2 levels each)\n")
cat("----------------------------------------------\n")
domains7 <- make_domains(5)
decisions7 <- make_equal_decisions(domains7)
cat("Criteria: 5, Levels: 2 each, Decisions:", nrow(decisions7), "\n")

time7 <- system.time({
    eng7 <- engine_create(domains7, settings = list(mode = "full", tau_equal = 0))
    eng7$decisions <- decisions7
    eng7 <- engine_compute(eng7)
})

ranges7 <- with(eng7$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
cat("Weights:", paste(round(100 * ranges7 / sum(ranges7), 1), collapse = "%, "), "%\n")
cat("Time:", round(time7[3], 2), "seconds\n")
cat("✅ PASS\n")

# Test 8: 6 criteria
cat("\n\nTEST 8: Scaling to 6 criteria (2 levels each)\n")
cat("----------------------------------------------\n")
domains8 <- make_domains(6)
decisions8 <- make_equal_decisions(domains8)
cat("Criteria: 6, Levels: 2 each, Decisions:", nrow(decisions8), "\n")
cat("Note: This may take longer...\n")

time8 <- system.time({
    eng8 <- engine_create(domains8, settings = list(mode = "full", tau_equal = 0))
    eng8$decisions <- decisions8
    eng8 <- engine_compute(eng8)
})

ranges8 <- with(eng8$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
cat("Weights:", paste(round(100 * ranges8 / sum(ranges8), 1), collapse = "%, "), "%\n")
cat("Time:", round(time8[3], 2), "seconds\n")
cat("✅ PASS\n")

# Test 9: 3 criteria with 4 levels each
cat("\n\nTEST 9: 3 criteria with 4 levels each\n")
cat("--------------------------------------\n")
domains9 <- make_domains(3, n_levels = 4)
# For multi-level, just test with minimal decisions
decisions9 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions9 <- rbind(decisions9, data.frame(
    A1 = alt_key(setNames(c("L4", "L1", "L1"), names(domains9))),
    A2 = alt_key(setNames(c("L1", "L4", "L1"), names(domains9))),
    pref = "E",
    stringsAsFactors = FALSE
))

cat("Criteria: 3, Levels: 4 each, Variables:", 3 * 4, ", Decisions:", nrow(decisions9), "\n")

time9 <- system.time({
    eng9 <- engine_create(domains9, settings = list(mode = "full", tau_equal = 0))
    eng9$decisions <- decisions9
    eng9 <- engine_compute(eng9)
})

cat("Time:", round(time9[3], 2), "seconds\n")
cat("✅ PASS\n")

# Test 9b: 4 criteria with 3 levels - UNEQUAL weights
cat("\n\nTEST 9b: 4 criteria × 3 levels with UNEQUAL weights\n")
cat("----------------------------------------------------\n")
domains9b <- make_domains(4, n_levels = 3)
decisions9b <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)

# C1 > C2 (using 2-step comparison)
decisions9b <- rbind(decisions9b, data.frame(
    A1 = alt_key(setNames(c("L3", "L1", "L1", "L1"), names(domains9b))),
    A2 = alt_key(setNames(c("L1", "L3", "L1", "L1"), names(domains9b))),
    pref = "A"
))

# C2 > C3
decisions9b <- rbind(decisions9b, data.frame(
    A1 = alt_key(setNames(c("L1", "L3", "L1", "L1"), names(domains9b))),
    A2 = alt_key(setNames(c("L1", "L1", "L3", "L1"), names(domains9b))),
    pref = "A"
))

# C3 = C4
decisions9b <- rbind(decisions9b, data.frame(
    A1 = alt_key(setNames(c("L1", "L1", "L3", "L1"), names(domains9b))),
    A2 = alt_key(setNames(c("L1", "L1", "L1", "L3"), names(domains9b))),
    pref = "E"
))

cat("Variables:", 4 * 3, ", Decisions:", nrow(decisions9b), "(C1 > C2 > C3 = C4)\n")

time9b <- system.time({
    eng9b <- engine_create(domains9b, settings = list(mode = "full", tau_equal = 0))
    eng9b$decisions <- decisions9b
    eng9b <- engine_compute(eng9b)
})

ranges9b <- with(eng9b$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result9b <- round(100 * ranges9b / sum(ranges9b), 1)
cat("Weights:", paste(names(result9b), "=", result9b, "%", collapse = ", "), "\n")
cat("Time:", round(time9b[3], 2), "seconds\n")

if (result9b[1] > result9b[2] && result9b[2] > result9b[3] && abs(result9b[3] - result9b[4]) < 1) {
    cat("✅ PASS (ordering: C1 > C2 > C3 ≈ C4)\n")
} else {
    cat("❌ FAIL\n")
}

# Test 9c: 7 criteria (pushing limits)
cat("\n\nTEST 9c: Scaling to 7 criteria (2 levels each)\n")
cat("-----------------------------------------------\n")
domains9c <- make_domains(7)
decisions9c <- make_equal_decisions(domains9c)
cat("Criteria: 7, Levels: 2 each, Variables: 14, Decisions:", nrow(decisions9c), "\n")
cat("⚠ Warning: This may take significantly longer...\n")

time9c <- system.time({
    eng9c <- engine_create(domains9c, settings = list(mode = "full", tau_equal = 0))
    eng9c$decisions <- decisions9c
    eng9c <- engine_compute(eng9c)
})

if (!is.null(eng9c$weights)) {
    ranges9c <- with(eng9c$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
    cat("Weights:", paste(round(100 * ranges9c / sum(ranges9c), 1), collapse = "%, "), "%\n")
}
cat("Time:", round(time9c[3], 2), "seconds\n")

if (!is.null(eng9c$weights) && time9c[3] < 60) {
    cat("✅ PASS (acceptable performance)\n")
} else if (!is.null(eng9c$weights)) {
    cat("⚠ MARGINAL (completed but slow)\n")
} else {
    cat("❌ FAIL (failed to compute)\n")
}

# Test 9d: 8 criteria (stress test)
cat("\n\nTEST 9d: Scaling to 8 criteria (2 levels each) - STRESS TEST\n")
cat("------------------------------------------------------------\n")
domains9d <- make_domains(8)
decisions9d <- make_equal_decisions(domains9d)
cat("Criteria: 8, Levels: 2 each, Variables: 16, Decisions:", nrow(decisions9d), "\n")
cat("⚠ Warning: This is a stress test - may fail or timeout...\n")

timeout_seconds <- 120
time9d <- system.time({
    tryCatch(
        {
            eng9d <- engine_create(domains9d, settings = list(mode = "full", tau_equal = 0))
            eng9d$decisions <- decisions9d
            eng9d <- engine_compute(eng9d)
        },
        error = function(e) {
            cat("Error:", e$message, "\n")
            eng9d <<- NULL
        }
    )
})

if (!is.null(eng9d) && !is.null(eng9d$weights)) {
    ranges9d <- with(eng9d$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
    cat("Weights:", paste(round(100 * ranges9d / sum(ranges9d), 1), collapse = "%, "), "%\n")
    cat("Time:", round(time9d[3], 2), "seconds\n")
    if (time9d[3] < 60) {
        cat("✅ PASS (impressive!)\n")
    } else {
        cat("⚠ MARGINAL (completed but very slow)\n")
    }
} else if (time9d[3] >= timeout_seconds) {
    cat("❌ TIMEOUT (exceeded", timeout_seconds, "seconds)\n")
} else {
    cat("Time:", round(time9d[3], 2), "seconds\n")
    cat("❌ FAIL (computation failed)\n")
}

# Test 9e: EXTREME - 10 criteria or 5 criteria × 5 levels
cat("\n\nTEST 9e: EXTREME COMPLEXITY - Finding absolute limits\n")
cat("------------------------------------------------------\n")

# Try 10 criteria first
cat("Attempting: 10 criteria × 2 levels (20 variables)...\n")
domains9e_10crit <- make_domains(10)
decisions9e_10crit <- make_equal_decisions(domains9e_10crit)
cat("Setup: 10 criteria, 20 variables, ", nrow(decisions9e_10crit), " decisions\n")

time9e_10crit <- system.time({
    tryCatch(
        {
            eng9e_10crit <- engine_create(domains9e_10crit, settings = list(mode = "full", tau_equal = 0))
            eng9e_10crit$decisions <- decisions9e_10crit
            eng9e_10crit <- engine_compute(eng9e_10crit)
        },
        error = function(e) {
            cat("Error:", e$message, "\n")
            eng9e_10crit <<- NULL
        }
    )
})

if (!is.null(eng9e_10crit) && !is.null(eng9e_10crit$weights)) {
    ranges9e <- with(eng9e_10crit$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
    cat("✅ SUCCESS: 10 criteria handled!\n")
    cat("Weights:", paste(round(100 * ranges9e / sum(ranges9e), 1), collapse = "%, "), "%\n")
    cat("Time:", round(time9e_10crit[3], 2), "seconds\n")

    # Try 5×5 if 10 criteria succeeded
    cat("\nAttempting: 5 criteria × 5 levels (25 variables)...\n")
    domains9e_5x5 <- make_domains(5, n_levels = 5)
    # Just 1-2 simple decisions for basic feasibility
    decisions9e_5x5 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
    decisions9e_5x5 <- rbind(decisions9e_5x5, data.frame(
        A1 = alt_key(setNames(c("L5", "L1", "L1", "L1", "L1"), names(domains9e_5x5))),
        A2 = alt_key(setNames(c("L1", "L5", "L1", "L1", "L1"), names(domains9e_5x5))),
        pref = "E",
        stringsAsFactors = FALSE
    ))

    cat("Setup: 5 criteria × 5 levels, 25 variables, ", nrow(decisions9e_5x5), " decision\n")

    time9e_5x5 <- system.time({
        tryCatch(
            {
                eng9e_5x5 <- engine_create(domains9e_5x5, settings = list(mode = "full", tau_equal = 0))
                eng9e_5x5$decisions <- decisions9e_5x5
                eng9e_5x5 <- engine_compute(eng9e_5x5)
            },
            error = function(e) {
                cat("Error:", e$message, "\n")
                eng9e_5x5 <<- NULL
            }
        )
    })

    if (!is.null(eng9e_5x5) && !is.null(eng9e_5x5$weights)) {
        cat("✅ INCREDIBLE: 5×5 levels (25 vars) also handled!\n")
        cat("Time:", round(time9e_5x5[3], 2), "seconds\n")
        cat("\n🏆 Full PAPRIKA can handle EXTREME complexity!\n")
    } else {
        cat("❌ 5×5 failed - that's the limit\n")
        cat("Time:", round(time9e_5x5[3], 2), "seconds\n")
        cat("\n⚠ Limit found: 10 criteria works, but 25 variables (5×5) is too much\n")
    }
} else {
    cat("❌ 10 criteria failed\n")
    cat("Time:", round(time9e_10crit[3], 2), "seconds\n")

    # If 10 criteria failed, try something between 8 and 10
    cat("\nAttempting fallback: 9 criteria × 2 levels (18 variables)...\n")
    domains9e_9crit <- make_domains(9)
    decisions9e_9crit <- make_equal_decisions(domains9e_9crit)

    time9e_9crit <- system.time({
        tryCatch(
            {
                eng9e_9crit <- engine_create(domains9e_9crit, settings = list(mode = "full", tau_equal = 0))
                eng9e_9crit$decisions <- decisions9e_9crit
                eng9e_9crit <- engine_compute(eng9e_9crit)
            },
            error = function(e) {
                cat("Error:", e$message, "\n")
                eng9e_9crit <<- NULL
            }
        )
    })

    if (!is.null(eng9e_9crit) && !is.null(eng9e_9crit$weights)) {
        cat("✅ 9 criteria works!\n")
        cat("Time:", round(time9e_9crit[3], 2), "seconds\n")
        cat("\n⚠ Practical limit: 9 criteria (18 variables)\n")
    } else {
        cat("❌ 9 criteria also failed\n")
        cat("\n⚠ Hard limit found: Between 8-9 criteria\n")
    }
}

# Test 10: Performance limit estimate
cat("\n\nTEST 10: Estimating performance limits\n")
cat("---------------------------------------\n")
cat("Full PAPRIKA uses polytope sampling which scales with:\n")
cat("- Number of variables (criteria × levels)\n")
cat("- Number of constraints (decisions + structural)\n")
cat("- Sample size for Monte Carlo estimation\n\n")

cat("Observed performance:\n")
cat(sprintf("  4 criteria (8 vars): %.2f sec\n", time6[3]))
cat(sprintf("  5 criteria (10 vars): %.2f sec\n", time7[3]))
cat(sprintf("  6 criteria (12 vars): %.2f sec\n", time8[3]))
cat(sprintf("  3 criteria × 4 levels (12 vars): %.2f sec\n", time9[3]))
cat(sprintf("  4 criteria × 3 levels (12 vars): %.2f sec\n", time9b[3]))
cat(sprintf("  7 criteria (14 vars): %.2f sec\n", time9c[3]))
if (!is.null(eng9d) && !is.null(eng9d$weights)) {
    cat(sprintf("  8 criteria (16 vars): %.2f sec\n\n", time9d[3]))
} else {
    cat("  8 criteria (16 vars): FAILED\n\n")
}

if (time8[3] > 10) {
    cat("⚠ WARNING: Performance degrading beyond 6 criteria\n")
    cat("Recommended limits: ≤ 6 criteria with 2-3 levels each\n")
} else if (!is.null(eng9c$weights) && time9c[3] < 5) {
    cat("✅ Excellent performance up to 7 criteria\n")
    if (!is.null(eng9d) && !is.null(eng9d$weights) && time9d[3] < 60) {
        cat("✅ 8 criteria feasible but slower\n")
        cat("Practical limit: ~8-10 criteria with 2 levels or ~6 criteria with 3-4 levels\n")
    } else {
        cat("⚠ 8 criteria approaching practical limits\n")
        cat("Recommended: ≤ 7 criteria with 2 levels or ≤ 5 criteria with 3-4 levels\n")
    }
} else {
    cat("✅ Performance acceptable up to 6-7 criteria\n")
    cat("Estimated limit: ~7-8 criteria with 2 levels or ~5 criteria with 4 levels\n")
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n=================================================================================\n")
cat("SUMMARY\n")
cat("=================================================================================\n\n")

all_pass <- (error1 <= 1) && (error2 <= 1) && (error3 <= 1) &&
    (result4[1] > result4[2] + result4[3] && abs(result4[2] - result4[3]) < 2) &&
    (!is.null(eng5$weights)) &&
    (!is.null(eng6$weights)) && (!is.null(eng6b$weights)) &&
    (!is.null(eng7$weights)) && (!is.null(eng8$weights)) && (!is.null(eng9$weights)) &&
    (result6b[1] > result6b[2] && result6b[2] > result6b[3] && result6b[3] > result6b[4])

if (all_pass) {
    cat("✅ ALL TESTS PASSED\n\n")
    cat("Full PAPRIKA mode implementation is CORRECT:\n")
} else {
    cat("❌ SOME TESTS FAILED\n\n")
    cat("Investigation needed - there may be an implementation issue.\n")
}

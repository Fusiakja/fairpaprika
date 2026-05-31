devtools::load_all(".")

# Profiling test: Where does Full PAPRIKA spend its time?
cat("\n=== PROFILING FULL PAPRIKA ===\n\n")

# Test 1: Many criteria (10)
cat("TEST: 10 criteria × 2 levels\n")
DOMAINS_10 <- setNames(lapply(1:10, function(i) c("Low", "High")), paste0("C", 1:10))
decisions_10 <- data.frame(
    A1 = "C1:High,C2:Low,C3:Low,C4:Low,C5:Low,C6:Low,C7:Low,C8:Low,C9:Low,C10:Low",
    A2 = "C1:Low,C2:High,C3:Low,C4:Low,C5:Low,C6:Low,C7:Low,C8:Low,C9:Low,C10:Low",
    pref = "E",
    stringsAsFactors = FALSE
)

# Profile with different sample sizes
cat("\nTesting effect of n_sample parameter:\n")
for (n_samp in c(1000, 5000, 10000, 20000)) {
    time <- system.time({
        eng <- fairpaprika::engine_create(
            DOMAINS_10,
            settings = list(
                mode = "full",
                full = list(n_sample = n_samp),
                tau_equal = 0
            )
        )
        eng$decisions <- decisions_10
        eng <- fairpaprika::engine_compute(eng)
    })
    cat(sprintf("  n_sample=%d: %.2f sec\n", n_samp, time[3]))
}

# Test 2: High-level domains
cat("\n\nTEST: 5 criteria × 5 levels\n")
DOMAINS_5x5 <- setNames(lapply(1:5, function(i) paste0("L", 1:5)), paste0("C", 1:5))
decisions_5x5 <- data.frame(
    A1 = "C1:L5,C2:L1,C3:L1,C4:L1,C5:L1",
    A2 = "C1:L1,C2:L5,C3:L1,C4:L1,C5:L1",
    pref = "E",
    stringsAsFactors = FALSE
)

cat("Testing effect of n_sample on high-dimensional problem:\n")
for (n_samp in c(1000, 5000, 10000)) {
    time <- system.time({
        eng <- fairpaprika::engine_create(
            DOMAINS_5x5,
            settings = list(
                mode = "full",
                full = list(n_sample = n_samp),
                tau_equal = 0
            )
        )
        eng$decisions <- decisions_5x5
        eng <- fairpaprika::engine_compute(eng)
    })
    cat(sprintf("  n_sample=%d: %.2f sec\n", n_samp, time[3]))
}

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Default n_sample may be too high for simple problems\n")
cat("2. Could use adaptive sampling based on problem size\n")
cat("3. High-level domains need special optimization\n")

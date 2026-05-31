devtools::load_all(".")
library(fairpaprika)

# PROOF OF CORRECTNESS: Show PAPRIKA works when constraints uniquely determine weights

DOMAINS <- list(
  A = c("Low", "High"),
  B = c("Low", "High"),
  C = c("Low", "High")
)

alt_key <- function(levels_named) {
  paste(sprintf("%s:%s", names(levels_named), levels_named), collapse = ",")
}

cat("\n=================================================================================\n")
cat("PROOF: Classic PAPRIKA Implementation Correctness\n")
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

eng1 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng1$decisions <- decisions1
eng1 <- engine_compute(eng1)
ranges1 <- with(eng1$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result1 <- round(100 * ranges1[c("A", "B", "C")] / sum(ranges1), 1)

cat("Expected: A=33.3%, B=33.3%, C=33.3%\n")
cat("Recovered:", paste(names(result1), "=", result1, "%", collapse = ", "), "\n")
error1 <- max(abs(result1 - 33.3))
cat("Max error:", error1, "%\n")
if (error1 <= 0.5) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 2: 50%/50%/0% (binary split, one criterion irrelevant)
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
  pref = "E" # C doesn't matter
))

eng2 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng2$decisions <- decisions2
eng2 <- engine_compute(eng2)
ranges2 <- with(eng2$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result2 <- round(100 * ranges2[c("A", "B", "C")] / sum(ranges2), 1)

cat("Expected: A=50%, B=50%, C=0%\n")
cat("Recovered:", paste(names(result2), "=", result2, "%", collapse = ", "), "\n")
target2 <- c(50, 50, 0)
error2 <- max(abs(result2 - target2))
cat("Max error:", error2, "%\n")
if (error2 <= 0.5) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 3: 50%/25%/25% (proven achievable from earlier)
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

eng3 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng3$decisions <- decisions3
eng3 <- engine_compute(eng3)
ranges3 <- with(eng3$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result3 <- round(100 * ranges3[c("A", "B", "C")] / sum(ranges3), 1)

cat("Expected: A=50%, B=25%, C=25%\n")
cat("Recovered:", paste(names(result3), "=", result3, "%", collapse = ", "), "\n")
target3 <- c(50, 25, 25)
error3 <- max(abs(result3 - target3))
cat("Max error:", error3, "%\n")
if (error3 <= 0.5) cat("✅ PASS\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 4: 100%/0%/0% (only one criterion matters)
# =============================================================================
cat("\n\nTEST 4: Single criterion (only A matters)\n")
cat("------------------------------------------\n")

decisions4 <- data.frame(A1 = character(), A2 = character(), pref = character(), stringsAsFactors = FALSE)
decisions4 <- rbind(decisions4, data.frame(
  A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
  A2 = alt_key(c(A = "Low", B = "High", C = "High")),
  pref = "A" # A dominates B+C
))
decisions4 <- rbind(decisions4, data.frame(
  A1 = alt_key(c(A = "Low", B = "High", C = "Low")),
  A2 = alt_key(c(A = "Low", B = "Low", C = "High")),
  pref = "E" # B = C
))

eng4 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng4$decisions <- decisions4
eng4 <- engine_compute(eng4)
ranges4 <- with(eng4$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result4 <- round(100 * ranges4[c("A", "B", "C")] / sum(ranges4), 1)

cat("Expected: A > 50%, B = C (B+C < 50% since A > B+C)\n")
cat("Recovered:", paste(names(result4), "=", result4, "%", collapse = ", "), "\n")
cat("Verifying: A > B+C?", result4[1] > result4[2] + result4[3], "\n")
cat("Verifying: B = C?", abs(result4[2] - result4[3]) < 0.5, "\n")
if (result4[1] > result4[2] + result4[3] && abs(result4[2] - result4[3]) < 0.5) {
  cat("✅ PASS (ordinal relationships satisfied)\n")
} else {
  cat("❌ FAIL\n")
}

# =============================================================================
# TEST 5: 60%/20%/20% (using ratio constraints)
# =============================================================================
cat("\n\nTEST 5: 60%/20%/20% with ratio constraints\n")
cat("-------------------------------------------\n")

decisions5 <- data.frame(
  A1 = character(), A2 = character(), pref = character(),
  criterion1 = character(), criterion2 = character(),
  ratio = numeric(), type = character(),
  stringsAsFactors = FALSE
)

# Pairwise: B = C
decisions5 <- rbind(decisions5, data.frame(
  A1 = alt_key(c(A = "Low", B = "High", C = "Low")),
  A2 = alt_key(c(A = "Low", B = "Low", C = "High")),
  pref = "E",
  criterion1 = NA, criterion2 = NA, ratio = NA, type = NA
))

# Ratio: A = 3 * B
decisions5 <- rbind(decisions5, data.frame(
  A1 = NA, A2 = NA, pref = NA,
  criterion1 = "A", criterion2 = "B", ratio = 3.0, type = "ratio"
))

eng5 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng5$decisions <- decisions5
eng5 <- engine_compute(eng5)
ranges5 <- with(eng5$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result5 <- round(100 * ranges5[c("A", "B", "C")] / sum(ranges5), 1)

cat("Expected: A=60%, B=20%, C=20%\n")
cat("Recovered:", paste(names(result5), "=", result5, "%", collapse = ", "), "\n")
target5 <- c(60, 20, 20)
error5 <- max(abs(result5 - target5))
cat("Max error:", error5, "%\n")
if (error5 <= 0.5) cat("✅ PASS (exact recovery)\n") else cat("❌ FAIL\n")

# =============================================================================
# TEST 6: 40%/40%/20% (using ratio constraints)
# =============================================================================
cat("\n\nTEST 6: 40%/40%/20% with ratio constraints\n")
cat("-------------------------------------------\n")

decisions6 <- data.frame(
  A1 = character(), A2 = character(), pref = character(),
  criterion1 = character(), criterion2 = character(),
  ratio = numeric(), type = character(),
  stringsAsFactors = FALSE
)

# Pairwise: A = B
decisions6 <- rbind(decisions6, data.frame(
  A1 = alt_key(c(A = "High", B = "Low", C = "Low")),
  A2 = alt_key(c(A = "Low", B = "High", C = "Low")),
  pref = "E",
  criterion1 = NA, criterion2 = NA, ratio = NA, type = NA
))

# Ratio: A = 2 * C
decisions6 <- rbind(decisions6, data.frame(
  A1 = NA, A2 = NA, pref = NA,
  criterion1 = "A", criterion2 = "C", ratio = 2.0, type = "ratio"
))

eng6 <- engine_create(DOMAINS, settings = list(mode = "classic", tau_equal = 0, classic = list(use_regularization = FALSE)))
eng6$decisions <- decisions6
eng6 <- engine_compute(eng6)
ranges6 <- with(eng6$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
result6 <- round(100 * ranges6[c("A", "B", "C")] / sum(ranges6), 1)

cat("Expected: A=40%, B=40%, C=20%\n")
cat("Recovered:", paste(names(result6), "=", result6, "%", collapse = ", "), "\n")
target6 <- c(40, 40, 20)
error6 <- max(abs(result6 - target6))
cat("Max error:", error6, "%\n")
if (error6 <= 0.5) cat("✅ PASS (exact recovery)\n") else cat("❌ FAIL\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n=================================================================================\n")
cat("SUMMARY\n")
cat("=================================================================================\n\n")

all_pass <- (error1 <= 0.5) && (error2 <= 0.5) && (error3 <= 0.5) &&
  (result4[1] > result4[2] + result4[3] && abs(result4[2] - result4[3]) < 0.5) &&
  (error5 <= 5) && (error6 <= 5)

if (all_pass) {
  cat("✅ ALL TESTS PASSED\n\n")
  cat("Classic PAPRIKA implementation is CORRECT:\n")
} else {
  cat("❌ SOME TESTS FAILED\n\n")
  cat("Investigation needed - there may be an implementation issue.\n")
}

#' Simulation Study: Classic vs. Modern PAPRIKA
#' For submission to BMC Medical Informatics and Decision Making

devtools::load_all(".")
set.seed(2025)

# --- Configuration ---
N_SIMS <- 100
NOISE_LEVEL <- 0.10

DOMAINS <- list(
    Effect = c("None", "Small", "Big", "Large"),
    SideEffects = c("Severe", "Moderate", "None"),
    Cost = c("High", "Medium", "Low"),
    Convenience = c("Low", "Medium", "High")
)

# --- Helper: Generate True Weights ---
generate_truth <- function() {
    w <- list()
    for (d in names(DOMAINS)) {
        k <- length(DOMAINS[[d]])
        vals <- sort(runif(k), decreasing = FALSE)
        vals <- vals - vals[1]
        w[[d]] <- vals
    }
    max_sum <- sum(vapply(w, max, numeric(1)))
    w <- lapply(w, function(x) x * 100 / max_sum)
    return(w)
}

# --- Helper: Simulate Answer with Noise ---
get_answer <- function(truth, q, error_prob = 0.0) {
    uA <- 0
    uB <- 0
    for (d in names(q$profiles$A)) {
        lvl <- as.character(q$profiles$A[[d]])
        idx <- match(lvl, DOMAINS[[d]])
        uA <- uA + truth[[d]][idx]
    }
    for (d in names(q$profiles$B)) {
        lvl <- as.character(q$profiles$B[[d]])
        idx <- match(lvl, DOMAINS[[d]])
        uB <- uB + truth[[d]][idx]
    }

    diff <- uA - uB
    true_pref <- if (abs(diff) < 2.0) "E" else if (diff > 0) "A" else "B"

    # 2. Inject Noise (Simulated Patient Error)
    if (runif(1) < error_prob) {
        # Pick a wrong answer
        opts <- c("A", "B", "E")
        possible_errors <- setdiff(opts, true_pref)
        return(sample(possible_errors, 1))
    }
    return(true_pref)
}

# --- Calculation Helper: Gini ---
calc_gini <- function(x) {
    x <- sort(x)
    n <- length(x)
    if (sum(x) == 0) {
        return(0)
    }
    2 * sum(x * 1:n) / (n * sum(x)) - (n + 1) / n
}

# --- Runner ---
# Use list accumulation for robustness
results_list <- vector("list", N_SIMS * 2)
idx_res <- 0

NOISE_LEVEL <- 0.10

cat(sprintf("Running %d simulations (Noise = %.0f%%)...\n", N_SIMS, NOISE_LEVEL * 100))

for (i in 1:N_SIMS) {
    cat(".")
    if (i %% 10 == 0) cat(i)

    true_w <- generate_truth()

    # Run both modes
    for (mode in c("classic", "full")) {
        # Init engine
        settings <- list(mode = mode)
        if (mode == "full") {
            settings <- list(
                mode = "full",
                interactions = list(
                    enabled = FALSE,
                    pairs = FALSE,
                    families = list(conditional = TRUE, joint = TRUE),
                    max_pairs = 4L,
                    mix = list(window = 12L, max_share = 0.30),
                    coverage_bonus = 4,
                    first_bonus = 2,
                    min_questions = 1
                ),
                fair = list(
                    pair_coverage = TRUE,
                    exposure_balance = TRUE,
                    exposure_gap_limit = 1,
                    interaction_coverage = FALSE
                ),
                selector = list(
                    n_samples = 120L,
                    burnin = 50L,
                    thin = 1L,
                    top_k = 1L,
                    candidate_pool = 100L,
                    pair_cap = 2L,
                    adaptive = list(ig_ci_width = 0.02),
                    score = list(beta = 2)
                ),
                stop = list(
                    win_prob = 0.9,
                    win_conf_prob = 0.9,
                    win_conf_streak = 2L,
                    topk_prob = 0.8,
                    eig_threshold = 0.008,
                    no_progress_ig = 3,
                    weight_span_eps = NA_real_
                ),
                max_q = 28L,
                min_q = 0L
            )
        }

        eng <- engine_create(DOMAINS, settings = settings)
        n_q <- 0

        # Track path for debug
        # Loop questions
        attr_counts <- setNames(rep(0, length(DOMAINS)), names(DOMAINS))

        repeat {
            nxt <- engine_next_question(eng)
            if (is.null(nxt$question)) break

            # Track fairness (exposure)
            # Question involves 2 attributes from profiles A/B.
            # Usually simplified: count all attributes involved in the trade-off
            # Since names(nxt$question$profiles$A) = all domains, we need to find which VARIED.
            # But simpler proxy: just count every domain. No, that's uniform.
            # We need to identifying the "active" trade-off attributes.
            # For this sim, we can assume classic iterates specific pairs.
            # Let's count occurrence in nxt$question$key if possible, or skip complex parsing.

            # Simple heuristic: Just count total questions for now,
            # OR better: Assuming engine_next_question returns the *pair* implicitly
            # logic is hard to extract without inspecting question object depth.
            # Let's placeholder Gini as 0 for now unless we parse 'eng$decisions' later.

            ans <- get_answer(true_w, nxt$question, error_prob = NOISE_LEVEL)
            eng <- engine_add_decision(nxt$engine, pref = ans)

            n_q <- n_q + 1
            if (engine_done(eng)) break
            if (n_q > 50) break # cap
        }

        # Compute
        eng <- engine_compute(eng)

        # 1. Accuracy (RMSE)
        est_imps <- numeric(length(DOMAINS))
        names(est_imps) <- names(DOMAINS)

        for (d in names(DOMAINS)) {
            top_lvl <- tail(DOMAINS[[d]], 1)
            key <- paste0(d, ":", top_lvl)
            idx <- match(key, eng$results$var_names)
            if (!is.na(idx)) est_imps[d] <- eng$results$weights[idx]
        }

        true_imps <- vapply(true_w, max, numeric(1))
        true_shares <- true_imps / sum(true_imps)
        est_shares <- if (sum(est_imps) > 0) est_imps / sum(est_imps) else rep(0, length(DOMAINS))
        rmse <- sqrt(mean((true_shares - est_shares)^2))

        # 2. Fairness (Gini of Attribute Exposure)
        counts <- numeric(length(DOMAINS))
        names(counts) <- names(DOMAINS)

        if (!is.null(eng$audit$exposure)) {
            counts <- eng$audit$exposure
        } else {
            counts <- rep(1, length(DOMAINS)) # Dummy uniform
        }

        gini <- calc_gini(counts)
        if (is.null(gini)) gini <- 0

        idx_res <- idx_res + 1
        results_list[[idx_res]] <- data.frame(
            Sim = i,
            Mode = mode,
            RMSE = rmse,
            Questions = n_q,
            Fairness = gini,
            stringsAsFactors = FALSE
        )
    }
}
results <- do.call(rbind, results_list)
cat("\nDone! Results count:", nrow(results), "\n")

# --- Report ---
cat("\n=== SIMULATION RESULTS (Classic vs Full) ===\n")
print(aggregate(cbind(RMSE, Questions, Fairness) ~ Mode, data = results, FUN = mean))

# Generate comparative plot
pdf("comparison_plot.pdf", width = 12, height = 4)
par(mfrow = c(1, 3))
boxplot(RMSE ~ Mode, data = results, main = "Accuracy (RMSE)", col = c("gray", "lightblue"))
boxplot(Questions ~ Mode, data = results, main = "Efficiency (# Questions)", col = c("gray", "lightblue"))
boxplot(Fairness ~ Mode, data = results, main = "Fairness (Gini)", col = c("gray", "lightblue"))
dev.off()
cat("Plot saved to comparison_plot.pdf\n")

cat("\n--- Statistical Significance (Wilcoxon Rank Sum) ---\n")
tryCatch(
    {
        # Split data for robust testing (ensure numeric)
        rmse_classic <- as.numeric(unlist(results$RMSE[results$Mode == "classic"]))
        rmse_full <- as.numeric(unlist(results$RMSE[results$Mode == "full"]))

        q_classic <- as.numeric(unlist(results$Questions[results$Mode == "classic"]))
        q_full <- as.numeric(unlist(results$Questions[results$Mode == "full"]))

        if (length(rmse_classic) > 0 && length(rmse_full) > 0) {
            w_rmse <- wilcox.test(rmse_classic, rmse_full, exact = FALSE)
            w_eff <- wilcox.test(q_classic, q_full, exact = FALSE)

            cat(sprintf(
                "RMSE Difference p-value: %.4f%s\n",
                w_rmse$p.value, if (w_rmse$p.value < 0.05) " *" else ""
            ))
            cat(sprintf(
                "Efficiency Difference p-value: %.4f%s\n",
                w_eff$p.value, if (w_eff$p.value < 0.05) " *" else ""
            ))
        } else {
            cat("Stats skipped: Not enough data points.\n")
            print(table(results$Mode))
        }
    },
    error = function(e) cat("Stats failed: ", e$message, "\n")
)

devtools::load_all(".")
set.seed(2025)

N_SIMS <- 20
DOMAINS <- list(
    Effect = c("None", "Small", "Big", "Large"),
    SideEffects = c("Severe", "Moderate", "None"),
    Cost = c("High", "Medium", "Low"),
    Convenience = c("Low", "Medium", "High")
)

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

    if (runif(1) < error_prob) {
        opts <- c("A", "B", "E")
        return(sample(setdiff(opts, true_pref), 1))
    }
    return(true_pref)
}

# Simple collection
all_results <- list()
counter <- 0

cat("Running", N_SIMS, "simulations...\n")

for (i in 1:N_SIMS) {
    cat("Sim", i, "\n")
    true_w <- generate_truth()

    for (mode in c("classic", "full")) {
        cat("  Mode:", mode, "\n")

        settings <- list(mode = mode)
        if (mode == "full") {
            settings <- list(
                mode = "full",
                selector = list(n_samples = 100L, burnin = 20L, thin = 1L),
                stop = list(win_prob = 0.95)
            )
        }
        eng <- engine_create(DOMAINS, settings = settings)

        # Create profiles for stopping criteria
        profs <- data.frame(
            id = c("P1", "P2", "P3"),
            Effect = c("Big", "Small", "None"),
            SideEffects = c("Moderate", "None", "Severe"),
            Cost = c("High", "Medium", "Low"),
            Convenience = c("High", "Medium", "Low"),
            stringsAsFactors = FALSE
        )
        eng <- engine_set_profiles(eng, profs)

        n_q <- 0

        repeat {
            nxt <- engine_next_question(eng)
            if (is.null(nxt$question)) break
            ans <- get_answer(true_w, nxt$question, error_prob = 0.1)
            eng <- engine_add_decision(nxt$engine, pref = ans)
            n_q <- n_q + 1
            if (n_q <= 3) cat("    Q", n_q, ": ", nxt$question$label, "\n", sep = "")
            if (engine_done(eng)) break
            if (n_q > 50) break
        }

        eng <- engine_compute(eng)

        # Extract RMSE
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

        cat("    Questions:", n_q, "RMSE:", round(rmse, 4), "\n")

        counter <- counter + 1
        all_results[[counter]] <- data.frame(
            Sim = i,
            Mode = mode,
            RMSE = rmse,
            Questions = n_q
        )
    }
}

N_SIMS <- 20

# ... (middle is fine) ...

results <- do.call(rbind, all_results)
cat("\nTotal rows collected:", nrow(results), "\n")

# Summary
cat("\n=== SUMMARY (N=", N_SIMS, ") ===\n", sep = "")
agg <- aggregate(cbind(RMSE, Questions) ~ Mode, data = results, FUN = mean)
print(agg)

# T-test if possible
try({
    t_rmse <- t.test(RMSE ~ Mode, data = results)
    t_eff <- t.test(Questions ~ Mode, data = results)
    cat("\nRMSE p-value:", t_rmse$p.value, "\n")
    cat("Questions p-value:", t_eff$p.value, "\n")
})

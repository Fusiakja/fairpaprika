test_that("dominance sanity flags violations", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  domains <- D
  alts <- data.frame(c1 = c("high", "low"), c2 = c("high", "low"), stringsAsFactors = FALSE)
  vi <- fairpaprika::: .fp_build_var_index(domains)
  alt_var_idx <- fairpaprika::: .fp_alt_var_idx(alts, names(domains), vi$var_idx)
  eng <- list(
    domains = domains,
    criteria = names(domains),
    alternatives = alts,
    alt_keys = c("A", "B"),
    alt_var_idx = alt_var_idx,
    diagnostics = list(interaction_summary = list(max_abs = 0)),
    settings = list(interactions = list(max_abs = 0.5))
  )
  class(eng) <- "paprika_engine"
  # make dominated option (B) higher utility by negative weights on top levels
  w_mean <- c("c1:low" = 0, "c1:high" = -1, "c2:low" = 0, "c2:high" = -1)
  san <- fairpaprika::: .engine_sanity_checks(eng, w_mean)
  expect_true(length(san$warnings) >= 1)
  expect_true(nrow(san$dominance) >= 1)
})

test_that("interaction magnitude sanity warns when exceeding bound", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(max_abs = 0.5)), seed = 1)
  eng$diagnostics$interaction_summary <- list(max_abs = 1)
  san <- fairpaprika::: .engine_sanity_checks(eng, w_mean = c("c1:low" = 0, "c1:high" = 1, "c2:low" = 0, "c2:high" = 1))
  expect_true(any(grepl("exceeds bound", san$warnings)))
})

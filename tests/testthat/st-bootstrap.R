test_that("engine_bootstrap returns enough successful replicates", {
  domains <- list(
    Effekt            = c("Niedrig","Mittel","Hoch"),
    Nebenwirkungen    = c("Viel","Wenig"),
    Risiken           = c("Viel","Wenig"),
    Anwendung         = c("Schwierig","Mittel","Einfach"),
    Monitoringaufwand = c("Viel","Wenig")
  )

  eng <- engine_create(domains, seed = 1)

  # simulate: always choose A (feasible but extreme)
  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    eng <- engine_add_decision(eng, "A")
    if (engine_done(eng)) break
  }

  boot <- engine_bootstrap(eng, B = 80, seed = 1, min_ok = 20, eps_strict = 0)
  expect_true(boot$ok)
  expect_true(boot$ok_count >= boot$min_ok)

  s <- bootstrap_summary(boot, "importance")
  expect_true(all(c("Mean","SD","Median","Lo","Hi") %in% names(s)))
})

test_that("bootstrap_rank_profiles returns Fit% and Top-k probabilities", {
  domains <- list(
    Effekt            = c("Niedrig","Mittel","Hoch"),
    Nebenwirkungen    = c("Viel","Wenig"),
    Risiken           = c("Viel","Wenig"),
    Anwendung         = c("Schwierig","Mittel","Einfach"),
    Monitoringaufwand = c("Viel","Wenig")
  )

  eng <- engine_create(domains, seed = 2)

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    eng <- engine_add_decision(eng, "A")
    if (engine_done(eng)) break
  }

  boot <- engine_bootstrap(eng, B = 60, seed = 2, min_ok = 15, eps_strict = 0)

  # small profile set (just to test ranking)
  prof <- data.frame(
    name = c("P1","P2","P3"),
    Effekt = c("Hoch","Mittel","Niedrig"),
    Nebenwirkungen = c("Wenig","Wenig","Viel"),
    Risiken = c("Wenig","Viel","Viel"),
    Anwendung = c("Einfach","Mittel","Schwierig"),
    Monitoringaufwand = c("Wenig","Viel","Viel"),
    stringsAsFactors = FALSE
  )

  r <- bootstrap_rank_profiles(boot, prof, id_col = "name", top_k = 2)
  expect_true(is.list(r) && "table" %in% names(r))
  expect_true(all(c("Fit_median","Fit_lo","Fit_hi","P_top1","P_topk") %in% names(r$table)))
})

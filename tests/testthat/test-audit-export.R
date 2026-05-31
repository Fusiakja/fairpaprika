test_that("audit export round-trips JSON", {
  domains <- list(C1 = c("low", "high"), C2 = c("low", "high"))
  eng <- engine_create(domains, seed = 13)
  eng <- simulate_engine(eng, chooser = always_choose_A, n = 2)

  tmp <- tempfile("audit_export")
  paths <- engine_audit_export(eng, tmp)
  expect_true(file.exists(paths$run))
  expect_true(file.exists(paths$questions))

  js <- jsonlite::fromJSON(paths$run)
  expect_true(is.list(js))
})

test_that("audit export handles long prob vectors", {
  domains <- list(C1 = c("low", "mid", "high"), C2 = c("low", "mid", "high"))
  eng <- engine_create(domains, seed = 21)
  profiles <- expand.grid(C1 = domains$C1, C2 = domains$C2, stringsAsFactors = FALSE)
  eng <- engine_set_profiles(eng, profiles)
  eng <- simulate_engine(eng, chooser = always_choose_A, n = 2)
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$diagnostics$profile_top3_prob))
  tmp <- tempfile("audit_export_long")
  paths <- engine_audit_export(eng, tmp)
  expect_true(file.exists(paths$questions))
})

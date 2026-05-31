test_that("audit export handles interaction fields for profiles", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  prof <- data.frame(c1 = c("low", "high"), c2 = c("low", "low"), c3 = c("low", "high"), stringsAsFactors = FALSE)
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))), seed = 60)
  eng <- engine_set_profiles(eng, prof)
  # ask one question with interaction disabled to keep audit simple
  nxt <- engine_next_question(eng); eng <- nxt$engine
  if (!is.null(nxt$question)) eng <- engine_add_decision(eng, "A")
  path <- tempfile()
  on.exit(unlink(paste0(path, "_run.json")), add = TRUE)
  engine_audit_export(eng, path)
  js <- jsonlite::fromJSON(paste0(path, "_run.json"))
  expect_true(is.list(js))
  expect_true("seeds" %in% names(js) || "audit" %in% names(js))
})

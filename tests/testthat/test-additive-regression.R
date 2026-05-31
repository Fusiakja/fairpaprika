test_that("additive and interaction-disabled runs match for same decisions", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  dec <- data.frame(
    A1 = c("c1:high,c2:low", "c1:low,c2:high"),
    A2 = c("c1:low,c2:low", "c1:high,c2:high"),
    pref = c("A", "B"),
    stringsAsFactors = FALSE
  )
  eng_add <- engine_create(D, settings = list(interactions = list(enabled = FALSE)), seed = 10)
  eng_add$decisions <- dec
  eng_add <- engine_compute(eng_add)

  eng_int_off <- engine_create(D, settings = list(interactions = list(enabled = FALSE)), seed = 10)
  eng_int_off$decisions <- dec
  eng_int_off <- engine_compute(eng_int_off)

  expect_equal(eng_add$weights, eng_int_off$weights)
})


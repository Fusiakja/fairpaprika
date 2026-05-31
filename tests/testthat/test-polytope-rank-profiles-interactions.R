test_that("polytope_rank_profiles handles interactions and unknown levels", {
  D <- list(
    c1 = c("low", "high"),
    c2 = c("low", "high")
  )
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))), seed = 4)
  eng <- simulate_engine(eng, chooser = function(q) if (q$a$c1 == "high") "A" else "B", n = 2)
  samples <- engine_polytope_sample(eng, n = 20, burnin = 10, thin = 1, seed = 5)
  prof <- data.frame(
    c1 = c("low", "high"),
    c2 = c("low", "high"),
    stringsAsFactors = FALSE
  )
  res <- polytope_rank_profiles(eng, prof, samples = samples)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), nrow(prof))
  # unknown level should error
  prof_bad <- prof
  prof_bad$c2[1] <- "mid"
  expect_error(polytope_rank_profiles(eng, prof_bad, samples = samples))
})


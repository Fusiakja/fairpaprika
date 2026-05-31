test_that("alt_var_idx_full includes interaction indices", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  eng <- engine_create(D, settings = list(interactions = list(pairs = list(c("c1", "c2")))), seed = 1)
  expect_true(!is.null(eng$alt_var_idx_full))
  # first alternative gets additive + one interaction variable
  expect_equal(length(eng$alt_var_idx_full[[1]]), length(D) + 1L)
})

test_that("interaction utility contribution changes delta", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"))
  vi <- .fp_build_var_index(D, interactions = list(c("c1", "c2")))
  alts <- data.frame(c1 = c("low", "high"), c2 = c("high", "low"), stringsAsFactors = FALSE)
  idx_full <- .fp_alt_var_idx_full(alts, names(D), vi$var_idx, interactions = list(c("c1", "c2")))
  # weight vector: only interaction (c1=low, c2=high) is non-zero
  w <- rep(0, length(vi$var_names))
  names(w) <- vi$var_names
  inter_name <- "c1::low::c2::high"
  w[inter_name] <- 1
  util1 <- sum(w[idx_full[[1]]])
  util2 <- sum(w[idx_full[[2]]])
  expect_true(util1 != util2)
})

test_that("interaction entropy detects mixed signs", {
  var_names <- c("c1:low", "c1:high", "c2:low", "c2:high", "c1::low::c2::high")
  w <- matrix(c(
    0.1, 0.2, 0.3, 0.4,  0.5,
    0.1, 0.2, 0.3, 0.4, -0.5
  ), nrow = 2, byrow = TRUE)
  colnames(w) <- var_names
  ent <- .fp_interaction_entropy(w, var_names, c("c1", "c2"))
  expect_gt(ent, 0)
})

test_that("implied proxy higher with closure centrality", {
  reach <- .fp_closure_init(4)
  reach[1, 2] <- TRUE
  reach[2, 3] <- TRUE
  reach[3, 4] <- TRUE
  counts <- c("C1::C2" = 1L)
  proxy_closure <- .fp_implied_proxy(reach, 1, 4, counts, "C1::C2")
  proxy_no_closure <- .fp_implied_proxy(NULL, 1, 4, counts, "C1::C2")
  expect_true(proxy_closure > proxy_no_closure)
})

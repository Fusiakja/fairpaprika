test_that("computed solution satisfies all constraints", {
  eng <- engine_create(demo_domains(), seed = 1)

  # 3) Simulate answers
  eng <- simulate_engine(eng, chooser = function(q) "A")

  # 4) Compute weights
  eng <- engine_compute(eng)

  # 5) Validate solution
  chk <- engine_check_solution(eng)
  expect_true(chk$ok)
})

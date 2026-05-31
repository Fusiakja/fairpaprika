test_that("classic mode uses true PAPRIKA systematic enumeration", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "classic", fair = list(pair_coverage = FALSE)), seed = 1)

  types <- character()
  diff_counts <- integer()

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break

    types <- c(types, q$type)
    # Count differing criteria
    diff_counts <- c(diff_counts, length(q$differing_criteria))

    eng <- engine_add_decision(eng, "A")
  }

  # Should have asked at least some questions
  expect_true(length(types) > 0)

  # PAPRIKA questions should be labeled as tradeoff or paprika_pair
  expect_true(all(types %in% c("tradeoff", "paprika_pair")))

  # All PAPRIKA questions involve exactly 2 criteria
  expect_true(all(diff_counts == 2))

  # Verify we can compute weights after answering questions
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))
})

test_that("classic mode warns and corrects eps_strict=0", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))

  expect_warning(
    eng <- engine_create(D, settings = list(mode = "classic", eps_strict = 0)),
    "eps_strict > 0"
  )

  expect_true(eng$settings$eps_strict > 0)
  expect_equal(eng$settings$eps_strict, 1e-3)
})

test_that("classic mode warns when weights are degenerate (regularization disabled)", {
  D <- list(
    Effect = c("Low", "High"),
    Safety = c("Low", "High"),
    Convenience = c("Low", "High")
  )

  # Disable regularization to test degeneracy warning
  eng <- engine_create(D, settings = list(
    mode = "classic",
    classic = list(use_regularization = FALSE)
  ))

  # Add only decisions that don't constrain Convenience
  alt_key <- function(levels_named) paste(sprintf("%s:%s", names(levels_named), levels_named), collapse = ",")

  eng$decisions <- data.frame(
    A1 = alt_key(c(Effect = "High", Safety = "Low", Convenience = "Low")),
    A2 = alt_key(c(Effect = "Low", Safety = "High", Convenience = "Low")),
    pref = "A",
    stringsAsFactors = FALSE
  )

  expect_warning(
    eng <- engine_compute(eng),
    "degenerate"
  )

  # Verify diagnostic was set
  expect_true("degenerate_criteria" %in% names(eng$diagnostics))
  expect_true("Convenience" %in% eng$diagnostics$degenerate_criteria)
})

test_that("classic mode with regularization prevents degeneracy", {
  D <- list(
    Effect = c("Low", "High"),
    Safety = c("Low", "High"),
    Convenience = c("Low", "High")
  )

  # With regularization enabled (default), should get balanced weights
  eng <- engine_create(D, settings = list(mode = "classic"))

  alt_key <- function(levels_named) paste(sprintf("%s:%s", names(levels_named), levels_named), collapse = ",")

  eng$decisions <- data.frame(
    A1 = alt_key(c(Effect = "High", Safety = "Low", Convenience = "Low")),
    A2 = alt_key(c(Effect = "Low", Safety = "High", Convenience = "Low")),
    pref = "A",
    stringsAsFactors = FALSE
  )

  eng <- engine_compute(eng)

  # Should NOT be degenerate
  expect_false("degenerate_criteria" %in% names(eng$diagnostics))

  # All criteria should have meaningful weights
  ranges <- with(eng$weights, tapply(Nutzen, sub(":.*", "", Merkmal), function(x) max(x) - min(x)))
  expect_true(all(ranges > 0.1)) # All criteria > 0.1% importance
})

test_that("classic mode filters implied preferences via transitivity", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "classic"), seed = 1)

  # Track candidate bank size
  initial_candidates <- length(eng$candidates)
  expect_true(initial_candidates > 0)

  # Answer some questions to trigger transitivity
  question_count <- 0
  while (question_count < 3 && !is.null(eng$candidates) && length(eng$candidates) > 0) {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break
    eng <- engine_add_decision(eng, "A")
    question_count <- question_count + 1
  }

  # Bank should shrink as preferences are implied (transitivity works)
  current_candidates <- length(eng$candidates)
  expect_true(current_candidates <= initial_candidates)
})

test_that("classic mode generates only 2-criterion pairs", {
  D <- list(
    A = c("low", "mid", "high"),
    B = c("low", "high"),
    C = c("low", "high")
  )
  eng <- engine_create(D, settings = list(mode = "classic"), seed = 1)

  all_2_criteria <- TRUE
  question_count <- 0

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break

    # Check exactly 2 criteria differ
    if (length(q$differing_criteria) != 2) {
      all_2_criteria <- FALSE
    }

    question_count <- question_count + 1
    eng <- engine_add_decision(eng, "A")
  }

  expect_true(all_2_criteria)
  expect_true(question_count > 0)

  # Verify completion and weight computation
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))
})

test_that("classic mode with use_anchor_phase works for backward compatibility", {
  D <- list(c1 = c("low", "high"), c2 = c("low", "high"), c3 = c("low", "high"))
  eng <- engine_create(D, settings = list(
    mode = "classic",
    classic = list(use_anchor_phase = TRUE),
    fair = list(pair_coverage = FALSE)
  ), seed = 1)

  types <- character()
  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break
    types <- c(types, q$type)
    eng <- engine_add_decision(eng, "A")
  }

  # With use_anchor_phase = TRUE, should ask anchor and pairwise questions
  expect_true(all(types %in% c("anchor", "pairwise", "tradeoff")))

  # Should have asked questions and be able to compute weights
  expect_true(length(types) > 0)
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))
})

test_that("classic mode PAPRIKA pairs cover all criterion combinations", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "classic"), seed = 1)

  # Track which criterion pairs have been seen
  pairs_seen <- list()

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break

    # Record the pair
    crits <- sort(q$differing_criteria)
    pair_key <- paste(crits, collapse = "_")
    pairs_seen[[pair_key]] <- TRUE

    eng <- engine_add_decision(eng, "A")
  }

  # For 3 criteria, should have seen all 3 pairs: A_B, A_C, B_C
  expect_true(length(pairs_seen) >= 3)

  # Verify we can compute weights
  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))
})

test_that("classic mode works with EPAMED-App domain configuration", {
  # Use the EXACT domain structure from the EPAMED-App
  D <- list(
    Effekt            = c("Niedrig", "Mittel", "Hoch"),
    Nebenwirkungen    = c("Viel", "Wenig"),
    Risiken           = c("Viel", "Wenig"),
    Anwendung         = c("Schwierig", "Mittel", "Einfach"),
    Monitoringaufwand = c("Viel", "Wenig")
  )

  eng <- engine_create(D, settings = list(
    mode = "classic",
    tau_equal = 0.001,
    classic = list(use_regularization = TRUE)
  ), seed = 42)

  question_count <- 0

  # Answer all questions
  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break

    question_count <- question_count + 1
    eng <- engine_add_decision(eng, "A")
  }

  # Should have asked questions
  expect_true(question_count > 0)

  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))

  # Verify all 5 criteria have non-zero weights
  ranges <- with(eng$weights, tapply(
    Nutzen, sub(":.*", "", Merkmal),
    function(x) max(x) - min(x)
  ))
  expect_equal(length(ranges), 5)
  expect_true(all(ranges > 0))

  # Verify criteria names match EPAMED-App
  expect_true(all(c(
    "Effekt", "Nebenwirkungen", "Risiken",
    "Anwendung", "Monitoringaufwand"
  ) %in% names(ranges)))
})

test_that("classic mode handles mixed 2-level and 3-level criteria", {
  D <- list(
    TwoLevel_A = c("Low", "High"),
    ThreeLevel_B = c("Low", "Mid", "High"),
    TwoLevel_C = c("Bad", "Good"),
    ThreeLevel_D = c("Worst", "Medium", "Best")
  )

  eng <- engine_create(D, settings = list(mode = "classic"), seed = 123)

  question_count <- 0
  all_2_criteria <- TRUE

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break

    # Verify 2-criterion pairs
    if (length(nxt$question$differing_criteria) != 2) {
      all_2_criteria <- FALSE
    }

    question_count <- question_count + 1
    eng <- engine_add_decision(eng, "A")
  }

  expect_true(question_count > 0)
  expect_true(all_2_criteria)

  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))

  # All 4 criteria should have weights
  ranges <- with(eng$weights, tapply(
    Nutzen, sub(":.*", "", Merkmal),
    function(x) max(x) - min(x)
  ))
  expect_equal(length(ranges), 4)
})

test_that("classic mode regularization produces consistent balanced weights", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))

  # With regularization - multiple runs should give consistent results
  weights_list <- list()
  alt_key <- function(levels) paste(sprintf("%s:%s", names(levels), levels), collapse = ",")

  for (i in 1:3) {
    eng <- engine_create(D, settings = list(
      mode = "classic",
      classic = list(use_regularization = TRUE)
    ), seed = i * 100)

    # Only one equality constraint - leaves degrees of freedom
    eng$decisions <- data.frame(
      A1 = alt_key(c(A = "high", B = "low", C = "low")),
      A2 = alt_key(c(A = "low", B = "high", C = "low")),
      pref = "E",
      stringsAsFactors = FALSE
    )

    eng <- engine_compute(eng)
    ranges <- with(eng$weights, tapply(
      Nutzen, sub(":.*", "", Merkmal),
      function(x) max(x) - min(x)
    ))
    weights_list[[i]] <- ranges / sum(ranges)
  }

  # All runs should give similar balanced results
  ref <- weights_list[[1]]
  for (i in 2:3) {
    max_diff <- max(abs(weights_list[[i]] - ref))
    expect_true(max_diff < 0.1,
      info = sprintf("Run %d differs from run 1 by %f", i, max_diff)
    )
  }
})

test_that("classic mode handles all-equal preferences gracefully", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))
  eng <- engine_create(D, settings = list(mode = "classic"), seed = 1)

  question_count <- 0

  repeat {
    nxt <- engine_next_question(eng)
    eng <- nxt$engine
    if (is.null(nxt$question)) break

    eng <- engine_add_decision(eng, "E") # Always equal
    question_count <- question_count + 1
  }

  expect_true(question_count > 0)

  eng <- engine_compute(eng)
  expect_true(!is.null(eng$weights))

  # Should get equal or nearly equal weights
  ranges <- with(eng$weights, tapply(
    Nutzen, sub(":.*", "", Merkmal),
    function(x) max(x) - min(x)
  ))
  normalized <- ranges / sum(ranges)

  # Check all weights are similar (within 10% of each other)
  max_weight <- max(normalized)
  min_weight <- min(normalized)
  expect_true(max_weight - min_weight < 0.1)
})

test_that("classic mode gives consistent weights for same decision pattern", {
  D <- list(A = c("low", "high"), B = c("low", "high"), C = c("low", "high"))

  get_weights <- function(seed_val) {
    eng <- engine_create(D, settings = list(
      mode = "classic",
      classic = list(use_regularization = TRUE)
    ), seed = seed_val)

    question_count <- 0

    repeat {
      nxt <- engine_next_question(eng)
      eng <- nxt$engine
      if (is.null(nxt$question)) break

      # Always prefer option A
      eng <- engine_add_decision(eng, "A")
      question_count <- question_count + 1
    }

    eng <- engine_compute(eng)
    ranges <- with(eng$weights, tapply(
      Nutzen, sub(":.*", "", Merkmal),
      function(x) max(x) - min(x)
    ))
    list(weights = ranges / sum(ranges), n_questions = question_count)
  }

  result1 <- get_weights(42)
  result2 <- get_weights(123)
  result3 <- get_weights(999)

  # All runs should ask the same number of questions (deterministic candidate order)
  # Note: This might vary slightly if randomization is used in candidate selection
  # For now, just check that results are similar

  # Weights should be very similar (same decision pattern)
  diff_1_2 <- max(abs(result1$weights - result2$weights))
  diff_2_3 <- max(abs(result2$weights - result3$weights))

  expect_true(diff_1_2 < 0.05,
    info = sprintf("Weights differ by %f between seed 42 and 123", diff_1_2)
  )
  expect_true(diff_2_3 < 0.05,
    info = sprintf("Weights differ by %f between seed 123 and 999", diff_2_3)
  )
})

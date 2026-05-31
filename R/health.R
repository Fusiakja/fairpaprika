# R/health.R


.paprika_parse_alt <- function(s) {
  tok <- strsplit(s, ",", fixed = TRUE)[[1]]
  kv  <- strsplit(tok, ":", fixed = TRUE)
  k   <- vapply(kv, `[`, character(1), 1)
  v   <- vapply(kv, function(x) paste(x[-1], collapse=":"), character(1))
  setNames(v, k)
}

# --- Helper: how many criteria differ? ---
.paprika_count_diffs <- function(A1, A2) {
  a <- .paprika_parse_alt(A1)
  b <- .paprika_parse_alt(A2)
  common <- intersect(names(a), names(b))
  sum(a[common] != b[common])
}

# --- Helper: which criteria differ? ---
.paprika_diff_criteria <- function(A1, A2) {
  a <- .paprika_parse_alt(A1)
  b <- .paprika_parse_alt(A2)
  common <- intersect(names(a), names(b))
  common[a[common] != b[common]]
}

# --- Coverage: each criterion pair must be seen at least once (A or E counts) ---
.paprika_all_pairs_covered <- function(decisions, criteria) {
  if (is.null(decisions) || !nrow(decisions)) return(FALSE)

  need <- character()
  for (i in 1:(length(criteria)-1)) for (j in (i+1):length(criteria)) {
    need <- c(need, paste(sort(c(criteria[i], criteria[j])), collapse="::"))
  }

  dAE <- decisions[decisions$pref %in% c("A","E"), , drop = FALSE]
  if (!nrow(dAE)) return(FALSE)

  seen <- apply(dAE[, c("A1","A2")], 1, function(x) {
    cc <- .paprika_diff_criteria(x[1], x[2])
    if (length(cc) == 2) paste(sort(cc), collapse="::") else NA_character_
  })
  seen <- unique(stats::na.omit(seen))

  all(need %in% seen)
}

#' Health / validation checks for the engine
#'
#' @param engine A `paprika_engine` object.
#' @return An object of class `paprika_health` with summary metrics.
#' @export
engine_health <- function(engine) {
  engine <- validate_engine(engine)
  d <- engine$decisions

  warn <- character()
  if (isTRUE(engine$diagnostics$undo_since_compute)) {
    warn <- c(warn, "Undo applied since last compute; recompute to refresh diagnostics.")
  }

  out <- list(
    n_decisions   = if (is.null(d)) 0L else nrow(d),
    n_equal       = if (is.null(d)) 0L else sum(d$pref == "E", na.rm = TRUE),
    n_strict      = if (is.null(d)) 0L else sum(d$pref == "A", na.rm = TRUE),
    pct_two_diff  = NA_real_,
    pairs_covered = FALSE,
    undo_count    = engine$diagnostics$undo_count %||% 0L,
    undo_since_compute = isTRUE(engine$diagnostics$undo_since_compute),
    solve_status  = engine$diagnostics$solve_status %||% NA_integer_,
    slack_n       = NA_integer_,
    slack_sum     = NA_real_,
    slack_max     = NA_real_,
    warnings      = warn
  )

  if (!is.null(d) && nrow(d)) {
    diffs <- vapply(seq_len(nrow(d)), function(i) .paprika_count_diffs(d$A1[i], d$A2[i]), integer(1))
    out$pct_two_diff  <- 100 * mean(diffs == 2)
    out$pairs_covered <- .paprika_all_pairs_covered(d, engine$criteria)
  }

  if (!is.null(engine$diagnostics$slack)) {
    s_info <- engine$diagnostics$slack
    slack_vals <- vapply(s_info, function(x) x$slack %||% NA_real_, numeric(1))
    out$slack_n <- sum(!is.na(slack_vals))
    out$slack_sum <- sum(slack_vals, na.rm = TRUE)
    out$slack_max <- if (length(slack_vals)) max(slack_vals, na.rm = TRUE) else NA_real_
  }

  class(out) <- "paprika_health"
  out
}

#' @export
print.paprika_health <- function(x, ...) {
  cat("Health checks\n")
  cat(sprintf(" Decisions: %d\n", x$n_decisions))
  cat(sprintf("  - Equal (E):  %d\n", x$n_equal))
  cat(sprintf("  - Strict (A): %d\n", x$n_strict))
  cat(sprintf(" Two-diff comparisons: %s\n",
              if (is.finite(x$pct_two_diff)) sprintf("%.1f%%", x$pct_two_diff) else "NA"))
  cat(sprintf(" Criteria-pair coverage: %s\n", if (isTRUE(x$pairs_covered)) "YES" else "NO"))
  if (!is.na(x$solve_status)) {
    cat(sprintf(" Solve status: %s\n", x$solve_status))
  }
  if (!is.null(x$undo_count) && x$undo_count > 0) {
    cat(sprintf(" Undo operations: %d%s\n", x$undo_count, if (isTRUE(x$undo_since_compute)) " (since last compute)" else ""))
  }
  if (length(x$warnings)) {
    cat(" Warnings:\n")
    for (w in x$warnings) cat(sprintf("  - %s\n", w))
  }
  if (is.finite(x$slack_n) && x$slack_n > 0) {
    cat(sprintf(" Slack: n=%d, sum=%.4f, max=%.4f\n", x$slack_n, x$slack_sum, x$slack_max))
  }
  invisible(x)
}

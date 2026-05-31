# R/full_paprika_closure.R
#
# Minimal transitive-closure (reachability) for "implied rankings" (A ≻ B).
# NOTE: This is deliberately simple (logical matrix). For very large banks,
# consider switching to bitsets.

.fp_closure_init <- function(N) {
  reach <- matrix(FALSE, nrow = N, ncol = N)
  diag(reach) <- TRUE
  reach
}

.fp_closure_add_dominance <- function(reach, alts, domains) {
  # Add dominance-implied edges (strict, all criteria >= and at least one >).
  # Dominance is anti-symmetric, so conflicts should not occur, but we
  # preserve the contract of .fp_closure_add_edge.
  if (is.null(reach) || is.null(alts) || nrow(alts) < 2) {
    return(list(reach = reach, conflict = FALSE, added = 0L))
  }
  added <- 0L
  conflict <- FALSE
  n <- nrow(alts)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      ai <- alts[i, , drop = FALSE]
      aj <- alts[j, , drop = FALSE]
      if (.fp_is_dominant(ai, aj, domains)) {
        upd <- .fp_closure_add_edge(reach, i, j)
        reach <- upd$reach
        conflict <- conflict || upd$conflict
        if (upd$changed) added <- added + 1L
      } else if (.fp_is_dominant(aj, ai, domains)) {
        upd <- .fp_closure_add_edge(reach, j, i)
        reach <- upd$reach
        conflict <- conflict || upd$conflict
        if (upd$changed) added <- added + 1L
      }
    }
  }
  list(reach = reach, conflict = conflict, added = added)
}

.fp_closure_resolved <- function(reach, a, b) {
  if (is.null(reach) || is.na(a) || is.na(b)) return(FALSE)
  isTRUE(reach[a, b] || reach[b, a])
}

.fp_closure_add_edge <- function(reach, a, b) {
  # Add a ≻ b; update transitive closure incrementally.
  if (is.null(reach) || is.na(a) || is.na(b)) {
    return(list(reach = reach, conflict = FALSE, changed = FALSE))
  }
  if (reach[a, b]) return(list(reach = reach, conflict = FALSE, changed = FALSE))
  if (reach[b, a]) return(list(reach = reach, conflict = TRUE,  changed = FALSE))

  pred <- which(reach[, a])
  succ <- which(reach[b, ])
  reach[pred, succ] <- TRUE

  list(reach = reach, conflict = FALSE, changed = TRUE)
}

#' Set hard eligibility rules for alternatives (and profiles)
#'
#' @param engine A `paprika_engine`.
#' @param rules list. Allowed keys:
#'  - `allowed_levels`: named list criterion -> vector of allowed levels.
#'  - `max_level`: named vector of maximum allowed level per criterion.
#'  - `exclude_profiles`: integer indices of profiles to exclude.
#' @return Updated engine with exclusions recorded.
#' @export
engine_set_eligibility <- function(engine, rules = list()) {
  engine <- validate_engine(engine)
  rules <- rules %||% list()
  alts <- engine$alternatives
  crit <- engine$criteria

  ok <- rep(TRUE, nrow(alts))
  reasons <- vector("list", nrow(alts))

  if (!is.null(rules$allowed_levels)) {
    for (k in seq_along(rules$allowed_levels)) {
      cn <- names(rules$allowed_levels)[k]
      if (!cn %in% crit) next
      allowed <- as.character(rules$allowed_levels[[k]])
      bad <- !(as.character(alts[[cn]]) %in% allowed)
      ok[bad] <- FALSE
      reasons[bad] <- lapply(reasons[bad], function(r) c(r, paste0(cn, " not allowed")))
    }
  }

  if (!is.null(rules$max_level)) {
    for (cn in names(rules$max_level)) {
      if (!cn %in% crit) next
      max_lv <- rules$max_level[[cn]]
      lv <- engine$domains[[cn]]
      max_pos <- match(max_lv, lv)
      bad <- match(as.character(alts[[cn]]), lv) > max_pos
      ok[bad] <- FALSE
      reasons[bad] <- lapply(reasons[bad], function(r) c(r, paste0(cn, " above max")))
    }
  }

  engine$eligibility$rules <- rules
  engine$eligibility$excluded_alts <- which(!ok)
  engine$eligibility$reasons <- reasons

  if (!is.null(rules$exclude_profiles) && length(engine$profiles_idx)) {
    engine$eligibility$excluded_profiles <- as.integer(rules$exclude_profiles)
  } else {
    engine$eligibility$excluded_profiles <- integer()
  }

  # record diagnostics
  if (length(engine$eligibility$excluded_alts)) {
    engine$diagnostics$eligibility_excluded <- engine$alt_keys[engine$eligibility$excluded_alts]
  } else {
    engine$diagnostics$eligibility_excluded <- character()
  }
  engine
}

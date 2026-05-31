.fp_entropy_safe <- function(p) {
  p <- p[p > 0 & is.finite(p)]
  if (!length(p)) {
    return(NA_real_)
  }
  -sum(p * log(p))
}

#' Procedural-justice report for a single session
#'
#' Computes fairness/exposure/coverage/burden metrics for one engine run.
#' @param engine A `paprika_engine`.
#' @export
engine_procedural_justice <- function(engine) {
  engine <- validate_engine(engine)
  aud <- engine$audit %||% list()
  n_q <- length(aud)
  # exposure over pairs
  needed <- .fp_needed_pairs(engine$criteria)
  counts <- .fp_pair_counts(engine)
  counts[needed[!(needed %in% names(counts))]] <- 0L
  counts <- counts[needed]
  counts[is.na(counts)] <- 0L
  cov_rate <- if (length(needed)) sum(counts > 0) / length(needed) else NA_real_
  gap <- if (length(counts)) max(counts) - min(counts) else NA_real_
  gini <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) {
      return(NA_real_)
    }
    n <- length(x)
    mu <- mean(x)
    if (mu == 0) {
      return(0)
    }
    sum(abs(outer(x, x, "-"))) / (2 * n^2 * mu)
  }
  exposure_entropy <- .fp_entropy_safe(counts / sum(counts))
  # interaction exposure
  inter_pairs <- character()
  if (isTRUE(engine$settings$interactions$enabled %||% FALSE)) {
    if (isTRUE(engine$interactions_active) && length(engine$interactions_pairs_active)) {
      inter_pairs <- unique(vapply(engine$interactions_pairs_active, function(p) paste(sort(p), collapse = "::"), character(1)))
    } else {
      inter_pairs <- unique(vapply(engine$settings$interactions$pairs %||% list(), function(p) paste(sort(p), collapse = "::"), character(1)))
    }
  }
  counts_inter <- if (length(inter_pairs)) counts[inter_pairs] else integer()
  if (length(counts_inter)) counts_inter[is.na(counts_inter)] <- 0L
  cov_inter <- if (length(inter_pairs)) sum(counts_inter > 0) / length(inter_pairs) else NA_real_
  gap_inter <- if (length(counts_inter)) max(counts_inter, na.rm = TRUE) - min(counts_inter, na.rm = TRUE) else NA_real_
  gini_inter <- if (length(counts_inter)) gini(as.numeric(counts_inter)) else NA_real_
  entropy_inter <- .fp_entropy_safe(counts_inter / sum(counts_inter))

  # family mix
  fam_counts <- table(vapply(aud, function(x) x$type %||% "tradeoff", character(1)))
  fam_mix <- as.list(fam_counts / sum(fam_counts))

  # burden
  cost_vec <- vapply(aud, function(x) x$cost %||% NA_real_, numeric(1))
  interaction_flags <- vapply(aud, function(x) isTRUE(x$interaction_pair), logical(1))
  avg_cost <- if (any(is.finite(cost_vec))) mean(cost_vec, na.rm = TRUE) else NA_real_
  complexity <- avg_cost + mean(interaction_flags, na.rm = TRUE)

  # neutrality/repro seeds
  seed_used <- unique(vapply(aud, function(x) as.integer(x$seed_used %||% NA_integer_), integer(1)))

  list(
    session = list(
      n_questions = n_q,
      coverage = cov_rate,
      exposure_gap = gap,
      exposure_gini = gini(as.numeric(counts)),
      exposure_entropy = exposure_entropy,
      interaction_coverage = cov_inter,
      interaction_gap = gap_inter,
      interaction_gini = gini_inter,
      interaction_entropy = entropy_inter,
      family_mix = fam_mix,
      burden = list(
        avg_cost = avg_cost,
        complexity = complexity,
        interaction_share = mean(interaction_flags, na.rm = TRUE)
      ),
      seeds = seed_used
    ),
    raw = list(
      pair_counts = counts,
      interaction_counts = counts_inter
    )
  )
}

#' Export procedural-justice report (JSON + optional text)
#' @param engine A `paprika_engine`.
#' @param path Path to export to (without extension).
#' @export
engine_procedural_justice_export <- function(engine, path) {
  rep <- engine_procedural_justice(engine)
  json_path <- paste0(path, "_justice.json")
  txt_path <- paste0(path, "_justice.txt")
  jsonlite::write_json(rep, json_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  lines <- c(
    sprintf("Questions: %s", rep$session$n_questions),
    sprintf("Coverage: %.2f", rep$session$coverage),
    sprintf("Exposure gap: %s", rep$session$exposure_gap),
    sprintf("Exposure entropy: %.3f", rep$session$exposure_entropy),
    sprintf("Family mix: %s", paste(names(rep$session$family_mix), sprintf("%.2f", unlist(rep$session$family_mix)), collapse = "; "))
  )
  writeLines(lines, txt_path)
  invisible(list(json = json_path, text = txt_path))
}

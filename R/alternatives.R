## Helpers for building and comparing PAPRIKA alternatives

# Build full set of alternatives from ordered domains
alternatives_build <- function(domains) {
  # expand.grid keeps factor-ish behavior unless stringsAsFactors=FALSE
  alts <- do.call(expand.grid, c(domains, stringsAsFactors = FALSE))
  # ensure column order is criteria order
  alts <- alts[, names(domains), drop = FALSE]
  alts
}

# Convert a single alternative row to the compact "criterion:level" string
alt_to_string <- function(row_df, criteria) {
  # row_df is 1-row data.frame, criteria in order
  paste(sprintf("%s:%s", criteria, as.character(row_df[1, criteria, drop = TRUE])),
        collapse = ",")
}

# Compute keyed strings for each alternative row
alternatives_keys <- function(alts, criteria) {
  vapply(seq_len(nrow(alts)),
         function(i) alt_to_string(alts[i, , drop = FALSE], criteria),
         FUN.VALUE = character(1))
}

# Stable key for an unordered pair of alternatives
pair_key <- function(a_row, b_row, criteria) {
  ka <- alt_to_string(a_row, criteria)
  kb <- alt_to_string(b_row, criteria)
  paste(sort(c(ka, kb)), collapse = "|||")
}

# Identify which criteria differ between two alternatives
diff_criteria <- function(a_row, b_row, criteria) {
  criteria[vapply(criteria, function(cn) {
    as.character(a_row[[cn]]) != as.character(b_row[[cn]])
  }, logical(1))]
}

# Numeric rank of a level within a criterion domain
rank_level <- function(domains, criterion, level) {
  match(level, domains[[criterion]])
}

# Validate that a tradeoff question meets structural requirements
valid_tradeoff <- function(domains, a_row, b_row, criteria,
                           require_two = TRUE,
                           require_opposite = TRUE) {
  diffs <- diff_criteria(a_row, b_row, criteria)
  if (require_two && length(diffs) != 2) return(FALSE)
  if (!require_opposite) return(TRUE)

  dirs <- vapply(diffs, function(cn) {
    sign(rank_level(domains, cn, a_row[[cn]]) - rank_level(domains, cn, b_row[[cn]]))
  }, numeric(1))

  all(dirs != 0) && (dirs[1] != dirs[2])
}

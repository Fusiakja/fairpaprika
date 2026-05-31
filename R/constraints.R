## Construct linear constraints from PAPRIKA decisions and domain structure

# Build constraint matrices/metadata for a given decision set
constraints_from_decisions <- function(domains, decisions,
                                       eps_strict = 1e-3,
                                       tau_equal = 1e-3,
                                       epsilon_monotone = 1e-6,
                                       normalize_sum_top = 1,
                                       interactions = list()) {
  # Be robust: callers may pass NULL (e.g., a fresh engine).
  if (is.null(decisions)) {
    decisions <- data.frame(
      A1 = character(),
      A2 = character(),
      pref = character(),
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(decisions)) {
    stop("decisions must be a data.frame (or NULL).")
  }
  # Minimal schema check (helps surface test/debug issues early)
  need_cols <- c("A1", "A2", "pref")
  if (!all(need_cols %in% names(decisions))) {
    stop("decisions must have columns: A1, A2, pref")
  }

  crit <- names(domains)
  # interactions can be passed as a list of pairs, or a list with $pairs and $max_abs
  inter_pairs <- interactions
  inter_bound <- NA_real_
  if (is.list(interactions) && !is.null(interactions$pairs)) {
    inter_pairs <- interactions$pairs
    inter_bound <- interactions$max_abs %||% NA_real_
  }
  var_names <- unlist(lapply(crit, function(k) paste(k, domains[[k]], sep = ":")))
  idx <- setNames(seq_along(var_names), var_names)
  # interaction vars
  if (!is.null(inter_pairs) && length(inter_pairs)) {
    for (pair in inter_pairs) {
      if (length(pair) != 2) next
      ci <- pair[1]
      cj <- pair[2]
      if (!(ci %in% crit && cj %in% crit)) next
      for (li in domains[[ci]]) {
        for (lj in domains[[cj]]) {
          nm <- paste(ci, li, cj, lj, sep = "::")
          if (is.na(idx[nm])) {
            idx[nm] <- length(idx) + 1L
            var_names <- c(var_names, nm)
          }
        }
      }
    }
  }
  n <- length(var_names)

  # Parse "criterion:level" strings into a named vector with validation
  parse_alt <- function(alt_str) {
    tok <- strsplit(alt_str, ",", fixed = TRUE)[[1]]
    kv <- strsplit(tok, ":", fixed = TRUE)
    k <- vapply(kv, `[`, character(1), 1)
    v <- vapply(kv, function(x) paste(x[-1], collapse = ":"), character(1))

    if (anyNA(k) || anyNA(v) || any(k == "")) {
      stop("Malformed alternative string: ", alt_str)
    }
    if (anyDuplicated(k)) {
      stop("Duplicate criteria in alternative string: ", alt_str)
    }
    levs <- setNames(v, k)

    extra <- setdiff(names(levs), crit)
    if (length(extra)) {
      stop(
        "Unknown criteria in alternative string: ",
        paste(extra, collapse = ", "), " (", alt_str, ")"
      )
    }
    missing <- setdiff(crit, names(levs))
    if (length(missing)) {
      stop(
        "Missing criteria in alternative string: ",
        paste(missing, collapse = ", "), " (", alt_str, ")"
      )
    }
    # validate level values against domains
    for (cn in crit) {
      if (!(levs[[cn]] %in% domains[[cn]])) {
        stop(
          "Unknown level '", levs[[cn]], "' for criterion '", cn,
          "' in alternative string: ", alt_str
        )
      }
    }
    levs[crit] # reorder to engine criterion order
  }

  # Build one constraint row for u(A1) - u(A2)
  row_diff <- function(A1_str, A2_str) {
    v1 <- parse_alt(A1_str)
    v2 <- parse_alt(A2_str)

    r <- numeric(n)

    for (j in seq_along(crit)) {
      k1 <- paste(crit[j], v1[[j]], sep = ":")
      k2 <- paste(crit[j], v2[[j]], sep = ":")
      i1 <- as.integer(idx[k1])
      i2 <- as.integer(idx[k2])
      if (is.na(i1) || is.na(i2)) {
        stop(
          "Internal error: missing var index for ", k1, " or ", k2,
          " (check domains/decisions consistency)."
        )
      }
      r[i1] <- r[i1] + 1
      r[i2] <- r[i2] - 1
    }
    if (!is.null(interactions) && length(interactions)) {
      for (pair in interactions) {
        if (length(pair) != 2) next
        ci <- pair[1]
        cj <- pair[2]
        if (!(ci %in% crit && cj %in% crit)) next
        nm1 <- paste(ci, v1[[ci]], cj, v1[[cj]], sep = "::")
        nm2 <- paste(ci, v2[[ci]], cj, v2[[cj]], sep = "::")
        i1 <- as.integer(idx[nm1])
        i2 <- as.integer(idx[nm2])
        if (!is.na(i1)) r[i1] <- r[i1] + 1
        if (!is.na(i2)) r[i2] <- r[i2] - 1
      }
    }

    r
  }

  A_list <- list()
  dir <- character()
  b <- numeric()
  origin <- list()

  # Separate ratio constraints from pairwise decisions
  # Ratio constraints have type="ratio" and specify criterion1, criterion2, ratio
  has_type_col <- "type" %in% names(decisions)

  pairwise_decisions <- decisions
  ratio_decisions <- NULL

  if (has_type_col) {
    is_ratio <- !is.na(decisions$type) & decisions$type == "ratio"
    if (any(is_ratio)) {
      ratio_decisions <- decisions[is_ratio, , drop = FALSE]
      pairwise_decisions <- decisions[!is_ratio, , drop = FALSE]
    }
  }

  # 1) preference constraints (pairwise comparisons)
  if (nrow(pairwise_decisions) > 0) {
    for (i in seq_len(nrow(pairwise_decisions))) {
      # r <- row_diff(pairwise_decisions$A1[i], pairwise_decisions$A2[i])

      pref <- pairwise_decisions$pref[i]
      A1 <- pairwise_decisions$A1[i]
      A2 <- pairwise_decisions$A2[i]

      # Support "B" as "second preferred" by swapping to store as "A"
      if (identical(pref, "B")) {
        tmp <- A1
        A1 <- A2
        A2 <- tmp
        pref <- "A"
      }
      if (!(pref %in% c("A", "E"))) {
        stop("Unknown pref value at row ", i, ": '", decisions$pref[i], "'. Expected A, B, or E.")
      }

      r <- row_diff(A1, A2)

      if (pref == "A") {
        A_list[[length(A_list) + 1]] <- r
        dir <- c(dir, ">=")
        b <- c(b, eps_strict)
        origin[[length(origin) + 1]] <- list(type = "decision", idx = i, pref = "A")
      } else if (pref == "E") {
        A_list[[length(A_list) + 1]] <- r
        dir <- c(dir, "<=")
        b <- c(b, tau_equal)
        origin[[length(origin) + 1]] <- list(type = "decision", idx = i, pref = "E_upper")

        A_list[[length(A_list) + 1]] <- r
        dir <- c(dir, ">=")
        b <- c(b, -tau_equal)
        origin[[length(origin) + 1]] <- list(type = "decision", idx = i, pref = "E_lower")
      }
    }
  }

  # 1b) ratio constraints: criterion1/criterion2 = ratio
  # Translates to: range(criterion1) = ratio * range(criterion2)
  # Or: (top1 - low1) = ratio * (top2 - low2)
  if (!is.null(ratio_decisions) && nrow(ratio_decisions) > 0) {
    for (i in seq_len(nrow(ratio_decisions))) {
      c1 <- ratio_decisions$criterion1[i]
      c2 <- ratio_decisions$criterion2[i]
      r <- ratio_decisions$ratio[i]

      # Validation
      if (is.na(c1) || is.na(c2) || is.na(r)) {
        stop("Ratio decision at row ", i, " has NA values. Need criterion1, criterion2, and ratio.")
      }
      if (!(c1 %in% crit)) {
        stop("Ratio decision at row ", i, ": criterion1 '", c1, "' not found in domains.")
      }
      if (!(c2 %in% crit)) {
        stop("Ratio decision at row ", i, ": criterion2 '", c2, "' not found in domains.")
      }
      if (r <= 0) {
        stop("Ratio decision at row ", i, ": ratio must be positive, got ", r)
      }

      # Find top and low levels for each criterion
      levels1 <- domains[[c1]]
      levels2 <- domains[[c2]]

      top1 <- paste(c1, levels1[length(levels1)], sep = ":")
      low1 <- paste(c1, levels1[1], sep = ":")
      top2 <- paste(c2, levels2[length(levels2)], sep = ":")
      low2 <- paste(c2, levels2[1], sep = ":")

      idx_top1 <- as.integer(idx[top1])
      idx_low1 <- as.integer(idx[low1])
      idx_top2 <- as.integer(idx[top2])
      idx_low2 <- as.integer(idx[low2])
      if (is.na(idx_top1) || is.na(idx_low1) || is.na(idx_top2) || is.na(idx_low2)) {
        stop("Internal error: missing indices for ratio constraint between ", c1, " and ", c2)
      }

      # Add constraint: (w_top1 - w_low1) - r * (w_top2 - w_low2) = 0
      # Rearrange: w_top1 - w_low1 - r*w_top2 + r*w_low2 = 0
      row <- numeric(n)
      row[idx_top1] <- 1
      row[idx_low1] <- -1
      row[idx_top2] <- -r
      row[idx_low2] <- r

      A_list[[length(A_list) + 1]] <- row
      dir <- c(dir, "=")
      b <- c(b, 0)
      origin[[length(origin) + 1]] <- list(
        type = "ratio",
        idx = i,
        criterion1 = c1,
        criterion2 = c2,
        ratio = r
      )
    }
  }

  # 2) anchors: lowest level = 0
  for (cn in crit) {
    low <- domains[[cn]][1]
    row0 <- numeric(n)
    idx0 <- as.integer(idx[paste(cn, low, sep = ":")])
    if (is.na(idx0)) stop("Internal error: missing anchor index for ", cn, ":", low)
    row0[idx0] <- 1
    A_list[[length(A_list) + 1]] <- row0
    dir <- c(dir, "=")
    b <- c(b, 0)
    origin[[length(origin) + 1]] <- list(type = "anchor", criterion = cn)
  }

  # 3) monotonicity: level_j - level_(j-1) >= epsilon_monotone
  for (cn in crit) {
    lv <- domains[[cn]]
    if (length(lv) >= 2) {
      for (j in 2:length(lv)) {
        rowm <- numeric(n)
        idx_from <- as.integer(idx[paste(cn, lv[j - 1], sep = ":")])
        idx_to <- as.integer(idx[paste(cn, lv[j], sep = ":")])
        if (is.na(idx_from) || is.na(idx_to)) stop("Internal error: missing monotone index for ", cn)
        rowm[idx_to] <- 1
        rowm[idx_from] <- -1
        A_list[[length(A_list) + 1]] <- rowm
        dir <- c(dir, ">=")
        b <- c(b, epsilon_monotone)
        origin[[length(origin) + 1]] <- list(type = "monotone", criterion = cn, from = lv[j - 1], to = lv[j])
      }
    }
  }

  # 4) normalization: sum(top-levels)=normalize_sum_top
  row_norm <- numeric(n)
  top_idx <- integer(length(crit))
  for (k in seq_along(crit)) {
    cn <- crit[k]
    top <- tail(domains[[cn]], 1)
    ii <- as.integer(idx[paste(cn, top, sep = ":")])
    if (is.na(ii)) stop("Internal error: missing normalization index for ", cn, ":", top)
    row_norm[ii] <- 1
    top_idx[k] <- ii
  }
  A_list[[length(A_list) + 1]] <- row_norm
  dir <- c(dir, "=")
  b <- c(b, normalize_sum_top)
  origin[[length(origin) + 1]] <- list(type = "normalization")

  # 5) interaction bounds (complexity control)
  if (is.finite(inter_bound) && inter_bound > 0) {
    inter_vars <- grep("::", var_names, fixed = TRUE, value = TRUE)
    for (nm in inter_vars) {
      idx_nm <- as.integer(idx[nm])
      if (is.na(idx_nm)) stop("Internal error: missing interaction index for ", nm)
      rowp <- numeric(n)
      rowp[idx_nm] <- 1
      rown <- numeric(n)
      rown[idx_nm] <- -1
      A_list[[length(A_list) + 1]] <- rowp
      dir <- c(dir, "<=")
      b <- c(b, inter_bound)
      origin[[length(origin) + 1]] <- list(type = "interaction_bound", var = nm, sign = "+")
      A_list[[length(A_list) + 1]] <- rown
      dir <- c(dir, "<=")
      b <- c(b, inter_bound)
      origin[[length(origin) + 1]] <- list(type = "interaction_bound", var = nm, sign = "-")
    }
  }

  list(
    var_names = var_names,
    idx = idx,
    A = do.call(rbind, A_list),
    dir = dir,
    b = b,
    top_idx = top_idx,
    origin = origin
  )
}

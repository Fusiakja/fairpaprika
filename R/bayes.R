#' Fit Bayesian preference model (logit with monotone increments) via cmdstanr
#' @param engine A `paprika_engine`.
#' @param iter Number of iterations.
#' @param chains Number of chains.
#' @param seed Random seed.
#' @param backend Backend to use ("stan" or "approx").
#' @export
engine_fit_posterior <- function(engine,
                                 iter = 1000,
                                 chains = 2,
                                 seed = NULL,
                                 backend = c("stan", "approx")) {
  engine <- validate_engine(engine)
  backend <- match.arg(backend)
  if (backend == "approx") {
    # Approximate posterior via polytope sampling
    sel <- engine$settings$selector
    samples <- engine_polytope_sample(
      engine,
      n = sel$n_samples,
      burnin = sel$burnin,
      thin = sel$thin,
      seed = seed %||% engine$seed
    )
    engine$posterior_samples <- samples
    engine$cache$samples <- samples
    engine$cache$n_decisions <- nrow(engine$decisions)
    return(engine)
  }
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr is required for Bayesian fitting. Please install cmdstanr.")
  }
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("posterior package required.")
  }
  stan_file <- system.file("stan", "paprika_logit.stan", package = "fairpaprika")
  if (stan_file == "") stop("Stan model not found.")

  crit <- engine$criteria
  K <- length(crit)
  L <- vapply(crit, function(cn) length(engine$domains[[cn]]), integer(1))
  N <- nrow(engine$decisions)
  if (N == 0) stop("No decisions to fit.")
  y <- integer(N)
  idxA <- matrix(0L, nrow = N, ncol = K)
  idxB <- matrix(0L, nrow = N, ncol = K)
  for (n in seq_len(N)) {
    pref <- engine$decisions$pref[n]
    A1 <- .paprika_parse_alt(engine$decisions$A1[n])
    A2 <- .paprika_parse_alt(engine$decisions$A2[n])
    if (pref == "A") {
      y[n] <- 1L
    } else if (pref == "B") {
      y[n] <- 2L # B is distinct outcome, not same as A
    } else {
      y[n] <- 3L # E or other
    }
    for (k in seq_len(K)) {
      cn <- crit[k]
      idxA[n, k] <- match(A1[[cn]], engine$domains[[cn]])
      idxB[n, k] <- match(A2[[cn]], engine$domains[[cn]])
    }
  }
  data_list <- list(K = K, L = L, N = N, y = y, idxA = idxA, idxB = idxB)
  # interactions
  inter_pairs <- engine$settings$interactions$pairs %||% list()
  inter_enabled <- isTRUE(engine$settings$interactions$enabled) && length(inter_pairs)
  inter_idx <- list()
  if (inter_enabled) {
    # index mapping for interactions: pair -> combination of levels
    for (p in seq_along(inter_pairs)) {
      pair <- inter_pairs[[p]]
      ci <- pair[1]
      cj <- pair[2]
      Li <- engine$domains[[ci]]
      Lj <- engine$domains[[cj]]
      for (li in seq_along(Li)) {
        for (lj in seq_along(Lj)) {
          inter_idx[[length(inter_idx) + 1L]] <- list(pair_idx = p, li = li, lj = lj, ci = ci, cj = cj)
        }
      }
    }
  }
  K_inter <- if (inter_enabled) length(inter_pairs) else 0L
  D_inter <- if (inter_enabled) length(inter_idx) else 0L
  if (inter_enabled) {
    data_list$K_inter <- K_inter
    data_list$inter_pairs <- t(vapply(inter_pairs, function(x) as.integer(match(x, crit)), integer(2)))
    data_list$D_inter <- D_inter
    data_list$inter_i <- as.integer(vapply(inter_idx, `[[`, integer(1), "pair_idx"))
    data_list$inter_li <- as.integer(vapply(inter_idx, `[[`, integer(1), "li"))
    data_list$inter_lj <- as.integer(vapply(inter_idx, `[[`, integer(1), "lj"))
  } else {
    data_list$K_inter <- 0L
    data_list$inter_pairs <- array(0L, dim = c(0, 2))
    data_list$D_inter <- 0L
    data_list$inter_i <- integer()
    data_list$inter_li <- integer()
    data_list$inter_lj <- integer()
  }
  # shrinkage prior settings (tunable via settings$interactions)
  slab_scale <- engine$settings$interactions$slab_scale %||% (if (inter_enabled) 0.5 else 1.0)
  slab_df <- engine$settings$interactions$slab_df %||% 4.0
  data_list$slab_scale <- slab_scale
  data_list$slab_df <- slab_df

  mod <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
  fit <- mod$sample(data = data_list, iter_sampling = iter, chains = chains, seed = seed)
  draws <- posterior::as_draws_matrix(fit$draws("w"))
  wmat <- posterior::extract_variable_matrix(draws, "w")
  base_names <- paste(rep(crit, times = L), unlist(lapply(L, seq_len)), sep = ":")
  inter_names <- character()
  if (inter_enabled) {
    for (itm in inter_idx) {
      inter_names <- c(inter_names, paste(itm$ci, engine$domains[[itm$ci]][itm$li], itm$cj, engine$domains[[itm$cj]][itm$lj], sep = "::"))
    }
  }
  colnames(wmat) <- c(base_names, inter_names)
  # scale weights to range100
  w_scaled <- t(apply(wmat, 1, function(wi) {
    names(wi) <- colnames(wmat)
    rescale_weights_range100(engine$domains, wi)
  }))
  engine$posterior_samples <- list(weights = wmat, weights_scaled = w_scaled, var_names = colnames(wmat))
  engine$cache$samples <- engine$posterior_samples
  engine$cache$n_decisions <- nrow(engine$decisions)
  engine
}

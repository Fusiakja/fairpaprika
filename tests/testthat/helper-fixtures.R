demo_domains <- function() {
  list(
    Effekt            = c("Niedrig","Mittel","Hoch"),
    Nebenwirkungen    = c("Viel","Wenig"),
    Risiken           = c("Viel","Wenig"),
    Anwendung         = c("Schwierig","Mittel","Einfach"),
    Monitoringaufwand = c("Viel","Wenig")
  )
}

# Minimal engine used by polytope tests (2 criteria, 2 levels).
# Adds one strict trade-off so "Effekt" tends to dominate "Nebenwirkungen".
demo_engine_simple <- function(seed = 1) {
  domains <- list(
    Effekt         = c("Niedrig", "Hoch"),
    Nebenwirkungen = c("Viel", "Wenig")
  )

  eng <- engine_create(
    domains,
    seed = seed,
    settings = list(
      # keep question machinery irrelevant for this fixture
      min_q = 0L,
      max_q = 1L,
      mode = "classic",
      fair = list(enabled = FALSE)
    )
  )

  # Prefer: (high effect + many side effects) over (low effect + few side effects)
  # This induces w(Effekt:Hoch) > w(Nebenwirkungen:Wenig).
  eng$decisions <- data.frame(
    A1 = "Effekt:Hoch,Nebenwirkungen:Viel",
    A2 = "Effekt:Niedrig,Nebenwirkungen:Wenig",
    pref = "A",
    stringsAsFactors = FALSE
  )
  eng
}

make_alt_string <- function(levels_named, criteria) {
  stopifnot(all(criteria %in% names(levels_named)))
  paste(sprintf("%s:%s", criteria, as.character(levels_named[criteria])), collapse = ",")
}

simulate_engine <- function(engine, chooser, n = NULL, allow_extra = FALSE) {
  # Convenience helper for tests.
  #
  # NOTE: Some tests call this as `simulate_engine(eng, ...)` without assigning
  # the return value. To keep those tests robust, we update the caller's
  # variable `eng` (or whatever symbol was passed) in-place when possible.
  engine_sym <- substitute(engine)
  # - If `n` is NULL: run until the engine is done (or no more questions).
  # - If `n` is provided: run exactly `n` decisions (or until no question exists).

  if (!is.null(n)) {
    n <- as.integer(n)
    if (is.na(n) || n < 0L) stop("`n` must be a non-negative integer.")
    allow_extra <- TRUE
  }

  i <- 0L
  repeat {
    if (!is.null(n) && i >= n) break

    nxt <- engine_next_question(engine, allow_extra = allow_extra)
    engine <- nxt$engine
    q <- nxt$question
    if (is.null(q)) break

    pref <- chooser(q)
    engine <- engine_add_decision(engine, pref = pref)
    i <- i + 1L

    if (is.null(n) && engine_done(engine)) break
  }

  if (is.symbol(engine_sym)) {
    assign(as.character(engine_sym), engine, envir = parent.frame())
  }

  engine
}

make_random_utility_model <- function(domains, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  crit <- names(domains)

  # monotone part-worths per criterion
  w <- list()
  for (cn in crit) {
    lv <- domains[[cn]]
    inc <- rgamma(length(lv) - 1, shape = 2, rate = 2)
    vals <- c(0, cumsum(inc))
    w[[cn]] <- stats::setNames(vals, lv)
  }
  w
}

utility_of_alt <- function(alt_named, model) {
  sum(vapply(names(model), function(cn) {
    lv <- alt_named[[cn]]
    as.numeric(model[[cn]][[lv]])
  }, numeric(1)))
}

choose_by_model <- function(q, model, tau = 0) {
  ua <- utility_of_alt(q$a, model)
  ub <- utility_of_alt(q$b, model)
  d <- ua - ub
  if (abs(d) <= tau) return("E")
  if (d > 0) "A" else "B"
}

# Deterministic chooser for the 2-criterion demo: prefers higher effect and fewer side effects
demo_monotone_chooser <- local({
  model <- list(
    Effekt = c(Niedrig = 0, Hoch = 1),
    Nebenwirkungen = c(Viel = 0, Wenig = 0.5)
  )
  function(q) choose_by_model(q, model)
})

# Simple chooser used in tests: always select option A.
always_choose_A <- function(q) "A"

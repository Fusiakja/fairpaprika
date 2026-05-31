#' Reset cached samples/outcomes/implied scores
#'
#' Call this after changing settings mid-run to avoid stale caches.
#' @param engine A `paprika_engine`.
#' @return Updated engine with caches cleared.
#' @export
engine_reset_caches <- function(engine) {
  engine <- validate_engine(engine)
  if (!is.null(engine$cache)) {
    engine$cache$samples <- NULL
    engine$cache$n_decisions <- -1L
    engine$cache$outcomes <- NULL
    engine$cache$implied <- NULL
  }
  engine$last_pick <- NULL
  engine
}

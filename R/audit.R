#' Export audit trail and run-level metrics
#'
#' @param engine A `paprika_engine`.
#' @return A list with `questions` (data.frame) and `run` (list of curves/metrics).
#' @export
engine_audit_report <- function(engine) {
  engine <- validate_engine(engine)
  qlog <- engine$audit %||% list()
  questions <- if (length(qlog)) {
    as.data.frame(do.call(rbind, lapply(qlog, function(x) {
      lapply(x, function(v) if (length(v) == 0) NA else v)
    })), stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
  run <- engine$diagnostics$audit_run %||% list()
  run$undo_count <- engine$diagnostics$undo_count %||% 0L
  run$undo_since_compute <- isTRUE(engine$diagnostics$undo_since_compute)
  list(questions = questions, run = run)
}

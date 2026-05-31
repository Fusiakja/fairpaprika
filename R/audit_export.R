#' Export audit report to disk
#'
#' @param engine A `paprika_engine`.
#' @param path Path without extension; will write `<path>_questions.csv` and `<path>_run.json`.
#' @export
engine_audit_export <- function(engine, path) {
  rep <- engine_audit_report(engine)
  qpath <- paste0(path, "_questions.csv")
  if (nrow(rep$questions)) {
    df <- rep$questions
    for (nm in names(df)) {
      if (is.list(df[[nm]])) {
        df[[nm]] <- vapply(df[[nm]], function(x) jsonlite::toJSON(x, auto_unbox = TRUE), character(1))
      }
    }
    utils::write.csv(df, qpath, row.names = FALSE)
  }
  rpath <- paste0(path, "_run.json")
  json <- jsonlite::toJSON(rep$run, pretty = TRUE, auto_unbox = TRUE, null = "null")
  writeLines(json, rpath)
  invisible(list(questions = qpath, run = rpath))
}

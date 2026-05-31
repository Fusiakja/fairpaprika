#' Get formatted justice metrics for SDM reporting
#'
#' Wraps `engine_procedural_justice` to provide the structured metrics expected
#' by the clinical demo and reporting tools.
#'
#' @param engine A `paprika_engine`.
#' @return List with formatted justice components:
#'   * `metrics` List containing `voice`, `neutrality`, and `transparency`.
#' @export
sdm_justice_metrics <- function(engine) {
    raw <- engine_procedural_justice(engine)

    # Determine process clarity based on audit trail presence
    clarity <- if (length(engine$audit) > 0) "High (Audit Trail Active)" else "Low (No Audit)"

    list(
        metrics = list(
            voice = list(
                attribute_coverage = raw$session$coverage %||% 0,
                interaction_coverage = raw$session$interaction_coverage %||% 0
            ),
            neutrality = list(
                exposure_gini = raw$session$exposure_gini %||% NA_real_,
                exposure_gap = raw$session$exposure_gap %||% NA_real_
            ),
            transparency = list(
                process_clarity = clarity,
                reproducibility = length(raw$session$seeds) > 0
            )
        ),
        raw_report = raw
    )
}

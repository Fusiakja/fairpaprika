#' Create a ratio decision constraint
#'
#' Specify that one criterion is a fixed multiple more important than another.
#' For example, decision_ratio("Effect", "Safety", 3.0) means "Effect is 3 times
#' as important as Safety" (i.e., range(Effect) = 3.0 * range(Safety)).
#'
#' @param criterion1 Name of first criterion (numerator)
#' @param criterion2 Name of second criterion (denominator)
#' @param ratio Positive number indicating importance ratio (criterion1 / criterion2)
#' @return A data frame row that can be rbind'd to decisions
#' @export
#' @examples
#' # Effect is 3x as important as Safety
#' ratio_dec <- decision_ratio("Effect", "Safety", 3.0)
#'
#' # Combine with pairwise decisions
#' decisions <- rbind(
#'     data.frame(A1 = "...", A2 = "...", pref = "A"),
#'     ratio_dec
#' )
decision_ratio <- function(criterion1, criterion2, ratio) {
    data.frame(
        A1 = NA_character_,
        A2 = NA_character_,
        pref = NA_character_,
        criterion1 = criterion1,
        criterion2 = criterion2,
        ratio = ratio,
        type = "ratio",
        stringsAsFactors = FALSE
    )
}

#' Format significant level stars for table display
#'
#' @param x
#' @param digits How many digits to display? (default = 3)
#' @export
#'

format_stars <- function(x) {
  x <- dplyr::mutate(x,
                     stars = ifelse(pvalue < .001, "***",
                                    ifelse(pvalue < .01, "**",
                                           ifelse(pvalue < .05, "*", ""))))
  return(x)
}

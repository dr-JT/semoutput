#' Descriptives table
#'
#' This function will display basic descriptive statistics for a dataframe
#' @param x dataframe
#' @export
#' @examples
#' sem_descriptives(x)

sem_descriptives <- function(x){
  x <- tidyr::gather(x, "Variable", "value")
  x <- dplyr::group_by(x, Variable)
  x <- dplyr::summarise(x,
                        n = dplyr::n(),
                        Mean = mean(value, na.rm=TRUE),
                        SD = sd(value, na.rm=TRUE),
                        min = min(value, na.rm=TRUE),
                        max = max(value, na.rm=TRUE),
                        Skewness = e1071::skewness(value, na.rm = TRUE, type = 2),
                        Kurtosis = e1071::kurtosis(value, na.rm = TRUE, type = 1),
                        '% Missing' = 100*(length(which(is.na(value)))/dplyr::n()))
  x <- knitr::kable(x, digits=2, format="html", caption="Descriptive Statistics")
  x <- kableExtra::kable_styling(x)
  return(x)
}

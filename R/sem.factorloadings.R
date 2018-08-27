#' A Results Output Function
#'
#' This function will display a table of Factor loadings
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem.factorloadings(x)

sem.factorloadings <- function(x, standardized = TRUE, print = TRUE){
  table <- lavaan::parameterEstimates(x, standardized = standardized)
  table <- dplyr::filter(table, op=="=~")
  table <- dplyr::mutate(table, stars = ifelse(pvalue < .001, "***",
                          ifelse(pvalue < .01, "**",
                                 ifelse(pvalue < .05, "*", ""))))
  table <- dplyr::select(table, 'Latent Factor'=lhs, Indicator=rhs, Beta=std.all, B=est, SE=se, z, 'sig'=stars)

  if (print==TRUE){
    table <- knitr::kable(table, digits=3, format="html", caption="Factor Loadings")
    table <- kableExtra::kable_styling(table)
  } else if (print==FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}

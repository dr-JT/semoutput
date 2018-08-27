#' A Results Output Function
#'
#' This function will display a table of Latent factor correlations
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @param factors list c() of factors included in the model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem.factorcor(x)

sem.factorcor <- function(x, standardized = TRUE, factors = c(), print = TRUE){
  table <- lavaan::parameterEstimates(x, standardized = standardized)
  table <- dplyr::filter(table, op=="~~", lhs %in% factors, !is.na(pvalue), lhs!=rhs)
  table <- dplyr::mutate(table, stars = ifelse(pvalue < .001, "***",
                                       ifelse(pvalue < .01, "**",
                                              ifelse(pvalue < .05, "*", ""))))
  table <- dplyr::select(table, 'Factor 1'=lhs, 'Factor 2'=rhs, r=est, sig=stars)

  if (print==TRUE){
    table <- knitr::kable(table, digits=3, format="html", caption="Latent Factor Correlations")
    table <- kableExtra::kable_styling(table, full_width = FALSE, position = "left")
  } else if (print==FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}

#' Residual correlation matrix
#'
#' This function will display a table of the residual correlation matrix of a lavaan cfa() or sem() model
#' @param x results from a cfa() or sem() lavaan model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem.residuals(x)

sem.residuals <- function(x, print = TRUE){
  table <- lavaan::residuals(x, type="cor")$cov
  table[upper.tri(table)] <- NA
  diag(table) <- NA

  if (print==TRUE){
    options(knitr.kable.NA='')
    table <- knitr::kable(table, digits=2, format="html")
    table <- kableExtra::kable_styling(table, full_width=FALSE, position = "left")
  } else if (print==FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}

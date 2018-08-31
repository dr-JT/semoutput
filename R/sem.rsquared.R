#' An SEM Output Function
#'
#' This function will display a table of R squared values (explained variance)
#' @param x results from a cfa() or sem() lavaan model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem.rsquared(x)

sem.rsquared <- function(x, print = TRUE){
  table <- lavaan::inspect(x, 'r2')
  table <- data.frame(table)
  table <- tibble::rownames_to_column(table)
  colnames(table) <- c("Variable", "R-Squared")

  if (print==TRUE){
    table <- knitr::kable(table, digits=3, format="html", caption="R-Squared Values", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width=FALSE, position="left")
  } else if (print==FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}

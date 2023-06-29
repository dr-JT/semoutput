#' R squared values
#'
#' This function will display a table of R squared values
#' @param x a cfa() or sem() lavaan model
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_rsquared <- function(x, print = TRUE){
  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::inspect(x, 'r2')
  table <- data.frame(table)
  table <- tibble::rownames_to_column(table)
  colnames(table) <- c("Variable", "R-Squared")
  table <- dplyr::filter(table, Variable %in% factors)

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "R-Squared Values", row.names = FALSE,
                            table.attr = 'data-quarto-disable-processing="true"')
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

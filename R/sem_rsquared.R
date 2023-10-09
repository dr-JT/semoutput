#' R squared values
#'
#' This function will display a table of R squared values
#' @param x a cfa() or sem() lavaan model
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_rsquared <- function(x, digits = 3, print = TRUE){

  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::inspect(x, 'r2')
  table <- data.frame(table)
  table <- tibble::rownames_to_column(table)
  colnames(table) <- c("Variable", "R_Squared")
  table <- dplyr::filter(table, Variable %in% factors)

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Explained Variance"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(R_Squared = "{{R^2}}") |>
        gt::cols_align(align = "left", columns = Variable) |>
        gt::fmt_number(decimals = digits)
    }

    if (print == FALSE) {
      table <- as.data.frame(table)
    }

  } else {
    table <- ""
  }

  return(table)
}

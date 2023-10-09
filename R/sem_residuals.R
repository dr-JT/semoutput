#' Residual correlation matrix
#'
#' This function will display a table of the residual correlation matrix
#' @param x a cfa() or sem() lavaan model
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_residuals <- function(x, digits = 3, print = TRUE) {

  table <- lavaan::residuals(x, type = "cor")$cov
  table[upper.tri(table)] <- NA
  diag(table) <- NA

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Residual Correlations"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
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

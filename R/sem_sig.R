#' Chi-square significance test
#'
#' This function will display a table of Model significance tests
#' @param x a cfa() or sem() lavaan model
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_sig <- function(x, digits = 3, print = TRUE){
  stats <- lavaan::fitMeasures(x, c("ntotal", "chisq", "pvalue", "df"))
  table <- data.frame(sample_size = stats[["ntotal"]],
                      Chi_Square = stats[["chisq"]],
                      df = stats[["df"]], p = stats[["pvalue"]])

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table_title <- "Model Significance"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(sample_size = "Sample Size",
                       Chi_Square = "{{:Chi:^2}}") |>
        gt::sub_small_vals(columns = p, threshold = .001) |>
        gt::fmt_number(decimals = digits) |>
        gt::fmt_number(columns = c(sample_size, df), decimals = 0)

    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

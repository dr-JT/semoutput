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
  table <- data.frame(N = stats[["ntotal"]],
                      Chi_Square = stats[["chisq"]],
                      df = stats[["df"]], p = stats[["pvalue"]])

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table_title <- "Model Significance"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(Chi_Square = gt::html("&chi;<sup>2</sup>")) |>
        gt::sub_small_vals(columns = p, threshold = .001) |>
        gt::fmt_number(decimals = digits, use_seps = FALSE) |>
        gt::fmt_number(columns = c(N, df), decimals = 0, use_seps = FALSE)

    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

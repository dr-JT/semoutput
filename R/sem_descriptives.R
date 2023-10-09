#' Descriptives table
#'
#' This function will display basic descriptive statistics for a dataframe
#' @param x dataframe
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_descriptives <- function(x, digits = 3, print = TRUE){
  col_order <- colnames(x)
  x <- tidyr::gather(x, "Variable", "value")
  x <- dplyr::group_by(x, Variable)
  table <- dplyr::summarise(x,
                            n = length(which(!is.na(value))),
                            Mean = mean(value, na.rm = TRUE),
                            SD = sd(value, na.rm = TRUE),
                            min = min(value, na.rm = TRUE),
                            max = max(value, na.rm = TRUE),
                            Skewness =
                              e1071::skewness(value, na.rm = TRUE, type = 2),
                            Kurtosis =
                              e1071::kurtosis(value, na.rm = TRUE, type = 2),
                            '% Missing' =
                              100 * (length(which(is.na(value))) / dplyr::n()))

  N <- dplyr::summarise(x, N.total = dplyr::n())
  N <- N$N.total[1]
  table <- dplyr::ungroup(table)
  table <- dplyr::arrange(table, match(Variable, col_order))

  if (print == TRUE) {
    table_title <- "Descriptive Statistics"

    table <- gt::gt(table) |>
      table_styling() |>
      gt::tab_header(title = table_title) |>
      gt::cols_merge_range(col_begin = min, col_end = max,
                           sep = " -- ") |>
      gt::cols_align(align = "left", columns = Variable) |>
      gt::fmt_number(decimals = digits) |>
      gt::tab_footnote(paste("Total N = ", N, sep = ""))
  }

  return(table)
}

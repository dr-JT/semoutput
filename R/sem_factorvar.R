#' Latent factor variances
#'
#' This function will display a table of Latent factor variances
#' @param x a cfa() or sem() lavaan model
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_factorvar <- function(x, digits = 3, print = TRUE) {

  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::parameterEstimates(x, standardized = TRUE)
  table <- dplyr::filter(table,
                         op == "~~",
                         lhs %in% factors,
                         !is.na(pvalue),
                         lhs == rhs)
  table <- format_stars(table)
  table <- dplyr::select(table, lhs, est, std.all, stars, pvalue)

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Latent Factor Variance/Residual Variance"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(lhs = "Factor",
                       est = "Variance",
                       std.all = "Std. Variance",
                       stars = "sig",
                       pvalue = "p") |>
        gt::cols_align(align = "left", columns = lhs) |>
        gt::sub_small_vals(columns = pvalue, threshold = .001) |>
        gt::fmt_number(decimals = digits, use_seps = FALSE)
    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

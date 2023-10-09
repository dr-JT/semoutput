#' Latent factor correlations
#'
#' This function will display a table of Latent factor correlations
#' @param x a cfa() or sem() lavaan model
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_factorcor <- function(x, ci_level = .95, digits = 3, print = TRUE) {

  ci_col_label <- paste(round(ci_level*100, 0), "% ", "CI", sep = "")

  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::standardizedSolution(x, level = 0.95)
  table <- dplyr::filter(table,
                         op == "~~",
                         lhs %in% factors,
                         !is.na(pvalue),
                         lhs != rhs)
  table <- format_stars(table)
  table <- dplyr::select(table, lhs, rhs, est.std, ci.lower, ci.upper,
                         stars, se, pvalue)

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Latent Factor Correlations"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_merge(columns = c(lhs, rhs), pattern = "{1} ~~ {2}") |>
        gt::cols_merge_range(col_begin = ci.lower, col_end = ci.upper,
                             sep = gt::html("&nbsp;&ndash;&nbsp")) |>
        gt::cols_label(lhs = "Factors",
                       est.std = "r",
                       ci.lower = ci_col_label,
                       stars = "sig",
                       se = "SE",
                       pvalue = "p") |>
        gt::cols_align(align = "left", columns = lhs) |>
        gt::sub_small_vals(columns = pvalue, threshold = .001) |>
        gt::fmt_number(decimals = digits) |>
        gt::tab_footnote("* p < .05") |>
        gt::tab_footnote("** p < .01") |>
        gt::tab_footnote("*** p < .001")
    }

    if (print == FALSE) {
      table <- as.data.frame(table)
    }

  } else {
    table <- ""
  }

  return(table)
}

#' SEM intercepts
#'
#' This function will display a table of variable intercepts
#' @param x a cfa() or sem() lavaan model
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_intercepts <- function(x, ci_level = 0.95, digits = 3, print = TRUE){

  ci_col_label <- paste(round(ci_level*100, 0), "% ", "CI", sep = "")

  table <- lavaan::parameterEstimates(x, level = ci_level) |>
    dplyr::filter(op == "~1") |>
    format_stars() |>
    dplyr::select(-op, -rhs) %>%
    dplyr::mutate(dplyr::across(c(se, ci.lower, ci.upper), .fns = \(xx) {ifelse(is.na(z), NA_real_, xx)}))


  if (nrow(table) > 0) {
    if (print == TRUE) {
      table_title <- "Variable Intercepts"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(lhs = "Variable",
                       est = "Intercept",
                       ci.lower = ci_col_label,
                       stars = "sig",
                       se = "SE",
                       z = "z",
                       pvalue = "p") |>
        gt::cols_align(align = "left", columns = c(lhs)) |>
        gt::sub_small_vals(columns = pvalue, threshold = .001) |>
        gt::fmt_number(decimals = digits, use_seps = FALSE) |>
        gt::tab_footnote("* p < .05; ** p < .01; *** p < .001") |>
        gt::cols_merge_range(col_begin = ci.lower,
                             col_end = ci.upper,
                             sep = gt::html("&nbsp;&mdash;&nbsp;"))


    }
    if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }
  return(table)
}

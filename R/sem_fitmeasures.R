#' Model fit statistics
#'
#' This function will display a table of Model fit measures
#' @param x a cfa() or sem() lavaan model
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_fitmeasures <- function(x, digits = 3, print = TRUE) {

  ci_col_label <- paste(round(ci_level*100, 0), "% ", "CI", sep = "")

  stats <- lavaan::fitMeasures(x, c("cfi", "rmsea", "rmsea.ci.lower",
                                    "rmsea.ci.upper", "srmr", "aic", "bic"),
                               fm.args = list(rmsea.ci.level = ci_level))

  table <- data.frame(CFI = stats[["cfi"]], RMSEA = stats[["rmsea"]],
                      RMSEA_Lower = stats[["rmsea.ci.lower"]],
                      RMSEA_Upper = stats[["rmsea.ci.upper"]],
                      SRMR = stats["srmr"],
                      AIC = stats[["aic"]], BIC = stats[["bic"]])

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Model Fit"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_merge_range(col_begin = RMSEA_Lower, col_end = RMSEA_Upper,
                             sep = " -- ") |>
        gt::cols_label(RMSEA_Lower = ci_col_label) |>
        gt::fmt_number(decimals = digits)

    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

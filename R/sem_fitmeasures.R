#' Model fit statistics
#'
#' This function will display a table of Model fit measures
#' @param x a cfa() or sem() lavaan model
#' @param robust logical. Include robust fit statistics?
#' @param ci_level What level of confidence interval to use (default = 0.90)
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_fitmeasures <- function(x, robust = FALSE,
                            ci_level = .90, digits = 3, print = TRUE) {

  ci_col_label <- paste(round(ci_level*100, 0), "% ", "CI", sep = "")

  stats <- lavaan::fitMeasures(x,
                               fm.args = list(rmsea.ci.level = ci_level))

  table <- data.frame(CFI = stats[["cfi"]], RMSEA = stats[["rmsea"]],
                      RMSEA_Lower = stats[["rmsea.ci.lower"]],
                      RMSEA_Upper = stats[["rmsea.ci.upper"]],
                      TLI = stats[["tli"]],
                      SRMR = stats["srmr"],
                      AIC = stats[["aic"]], BIC = stats[["bic"]])

  robust_table <- data.frame(CFI = stats[["cfi.robust"]],
                             RMSEA = stats[["rmsea.robust"]],
                             RMSEA_Lower = stats[["rmsea.ci.lower.robust"]],
                             RMSEA_Upper = stats[["rmsea.ci.upper.robust"]],
                             TLI = stats[["tli.robust"]])

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Model Fit"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_merge_range(col_begin = RMSEA_Lower, col_end = RMSEA_Upper,
                             sep = gt::html("&nbsp;&ndash;&nbsp")) |>
        gt::cols_label(RMSEA_Lower = ci_col_label) |>
        gt::fmt_number(decimals = digits)

      if (robust == TRUE) {

        robust_table_title <- "Robust Model Fit Statistics"

        robust_table <- gt::gt(robust_table) |>
          table_styling() |>
          gt::tab_header(title = robust_table_title) |>
          gt::cols_merge_range(col_begin = RMSEA_Lower, col_end = RMSEA_Upper,
                               sep = gt::html("&nbsp;&ndash;&nbsp")) |>
          gt::cols_label(RMSEA_Lower = ci_col_label) |>
          gt::fmt_number(decimals = digits)

        table <- gt::gt_group(table) |>
          gt::grp_add(robust_table)
      }

    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

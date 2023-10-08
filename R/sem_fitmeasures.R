#' Model fit statistics
#'
#' This function will display a table of Model fit measures
#' @param x a cfa() or sem() lavaan model
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_fitmeasures <- function(x, digits = 3, print = TRUE){
  stats <- lavaan::fitMeasures(x, c("cfi", "rmsea", "rmsea.ci.lower",
                                    "rmsea.ci.upper", "srmr", "aic", "bic"))
  table <- data.frame(CFI = stats[["cfi"]], RMSEA = stats[["rmsea"]],
                      RMSEA_Lower = stats[["rmsea.ci.lower"]],
                      RMSEA_Upper = stats[["rmsea.ci.upper"]],
                      SRMR = stats["srmr"],
                      AIC = stats[["aic"]], BIC = stats[["bic"]]) |>
    dplyr::mutate(RMSEA_Lower = round(RMSEA_Lower, digits),
                  RMSEA_Upper = round(RMSEA_Upper, digits)) |>
    tidyr::unite(RMSEA_CI, RMSEA_Lower, RMSEA_Upper, sep = ", ") |>
    dplyr::mutate(RMSEA_CI = paste("[", RMSEA_CI, "]", sep = ""))

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Model Fit"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(RMSEA_CI = "95% CI") |>
        gt::fmt_number(decimals = digits)

    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

#' Model fit statistics
#'
#' This function will display a table of Model fit measures
#' @param x a cfa() or sem() lavaan model
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_fitmeasures <- function(x, print = TRUE){
  stats <- lavaan::fitMeasures(x, c("cfi", "rmsea", "rmsea.ci.lower",
                                    "rmsea.ci.upper", "srmr", "aic", "bic"))
  table <- data.frame(CFI = stats[["cfi"]], RMSEA = stats[["rmsea"]],
                      'RMSEA Lower' = stats[["rmsea.ci.lower"]],
                      'RMSEA Upper' = stats[["rmsea.ci.upper"]],
                      SRMR = stats["srmr"],
                      AIC = stats[["aic"]], BIC = stats[["bic"]])
  if (nrow(table) > 0) {
    if (print == TRUE) {
      table <- knitr::kable(table, digits = 3,
                            caption = "Model Fit Measures", row.names = FALSE)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
    } else if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }

  return(table)
}

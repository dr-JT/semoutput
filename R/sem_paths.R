#' SEM regression paths
#'
#' This function will display a table of SEM regression paths
#' @param x a cfa() or sem() lavaan model
#' @param standardized logical. Include standardized loadings? (default = TRUE)
#' @param ci logical. display standardized or unstandardized confidence
#'     intervals? (default = "standardized"). Not needed if standardized=FALSE
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_paths <- function(x, standardized = TRUE, ci = "standardized",
                      ci_level = 0.95, digits = 3, print = TRUE){

  ci_col_name <- paste(ci_level * 100, "% CI", sep = "")

  if (x@call$se == "boot" | x@call$se == "bootstrap") {
    message("SEs, p-values, and confidence intervals may not be accuracte ",
            "for bootstrapped indirect effects. If using bootstrapping for ",
            "indirect effects it is advised to set se = \"standard\" and to ",
            "use semTools::monteCarloCI() instead. This will calculate ",
            "bias-corrected percentile confidence intervals.")
  }

  if (standardized == FALSE) {
    ci <- "unstandardized"
  }
  if (ci == "standardized") {
    table <- lavaan::standardizedSolution(x, level = ci_level)
    table <- dplyr::filter(table, op == "~" | op == ":=")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))),
                           dplyr::across(c(ci.lower, ci.upper), ~ round(.x, digits))) |>
      tidyr::unite("CI", ci.lower, ci.upper, sep = " - ")
    table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                           `Path Value` = est.std, SE = se, z, 'sig' = stars,
                           p = pvalue, CI)

    if (nrow(table) > 0) {
      if (print == TRUE) {
        colnames(table)[which(colnames(table) == "CI")] <- ci_col_name
        table <- knitr::kable(table, digits = digits, format = "html",
                              caption = "Regression Paths",
                              table.attr = 'data-quarto-disable-processing="true"')
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table, c(" ", " ",
                                                       "Standardized" = 6))
      }
    } else {
      table <- ""
    }
  }

  if (ci == "unstandardized") {
    table <- lavaan::parameterEstimates(x, standardized = TRUE)
    table <- dplyr::filter(table, op == "~" | op == ":=")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))),
                           dplyr::across(c(ci.lower, ci.upper), ~ round(.x, digits))) |>
      tidyr::unite("CI", ci.lower, ci.upper, sep = " - ")
    if (standardized == TRUE) {
      table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                             `Path Value` = est, SE = se, z, 'sig' = stars,
                             p = pvalue, CI,
                             `Path Value` = std.all)
      if (nrow(table) > 0) {
        if (print == TRUE) {
          colnames(table)[which(colnames(table) == "CI")] <- ci_col_name
          table <- knitr::kable(table, digits = digits, format = "html",
                                caption = "Regression Paths",
                                table.attr = 'data-quarto-disable-processing="true"')
          table <- kableExtra::kable_styling(table)
          table <- kableExtra::add_header_above(table,
                                                c(" ", " ",
                                                  "Unstandardized" = 6,
                                                  "Standardized" = 1))
        }
      } else {
        table <- ""
      }
    }

    if (standardized == FALSE) {
      table <- dplyr::select(table, Predictor = rhs, DV = lhs,
                             `Path Value` = est, SE = se, z, 'sig' = stars,
                             p = pvalue, CI)
      if (nrow(table) > 0) {
        if (print == TRUE) {
          colnames(table)[which(colnames(table) == "CI")] <- ci_col_name
          table <- knitr::kable(table, digits = digits, format = "html",
                                caption = "Regression Paths",
                                table.attr = 'data-quarto-disable-processing="true"')
          table <- kableExtra::kable_styling(table)
          table <- kableExtra::add_header_above(table,
                                                c(" ", " ",
                                                  "Unstandardized" = 6))
        }
      } else {
        table <- ""
      }
    }
  }
  if (print == FALSE) {
    if (nrow(table) > 0) table <- as.data.frame(table)
  }
  return(table)
}

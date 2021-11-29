#' Factor loadings
#'
#' This function will display a table of Factor loadings
#' @param x a cfa() or sem() lavaan model
#' @param standardized logical. Include standardized loadings? (default = TRUE)
#' @param ci logical. display standardized or unstandardized confidence
#'     intervals? (default = "standardized"). Not needed if standardized=FALSE
#' @param ci.level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_factorloadings <- function(x, standardized = TRUE, ci = "standardized",
                               ci.level = 0.95, digits = 3, print = TRUE){
  if (standardized == FALSE){
    ci <- "unstandardized"
  }
  if (ci == "standardized"){
    table <- lavaan::standardizedSolution(x, level = 0.95)
    table <- dplyr::filter(table, op == "=~")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))))
    table <- dplyr::select(table, 'Latent Factor' = lhs, Indicator = rhs,
                           Loadings = est.std, 'sig' = stars, p = pvalue,
                           Lower.CI = ci.lower, Upper.CI = ci.upper,
                           SE = se, z)
    if (nrow(table) > 0) {
      if (print == TRUE){
        table <- knitr::kable(table, digits = digits, format = "html",
                              caption = "Factor Loadings")
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table,
                                              c(" ", " ",
                                                "Standardized" = 7))
      }
    } else {
      table <- ""
    }
  }

  if (ci == "unstandardized"){
    table <- lavaan::parameterEstimates(x, standardized = standardized)
    table <- dplyr::filter(table, op == "=~")
    table <- dplyr::mutate(table,
                           stars = ifelse(pvalue < .001, "***",
                                          ifelse(pvalue < .01, "**",
                                                 ifelse(pvalue < .05, "*", ""))))
    if (standardized == TRUE){
      table <- dplyr::select(table, 'Latent Factor' = lhs, Indicator = rhs,
                             Loadings = est, 'sig' = stars, p = pvalue,
                             Lower.CI = ci.lower, Upper.CI = ci.upper,
                             SE = se, z, Loadings.std = std.all)

      if (nrow(table) > 0) {
        if (print == TRUE){
          table <- knitr::kable(table, digits = digits, format = "html",
                                caption = "Factor Loadings")
          table <- kableExtra::kable_styling(table)
          table <- kableExtra::add_header_above(table,
                                                c(" ", " ",
                                                  "Unstandardized" = 7,
                                                  "Standardized" = 1))
        }
      } else {
        table <- ""
      }
    }

    if (standardized == FALSE){
      table <- dplyr::select(table, 'Latent Factor' = lhs, Indicator = rhs,
                             Loadings = est, 'sig' = stars, p = pvalue,
                             Lower.CI = ci.lower, Upper.CI = ci.upper,
                             SE = se, z)
      if (nrow(table) > 0) {
        if (print == TRUE){
          table <- knitr::kable(table, digits = digits, format = "html",
                                caption = "Factor Loadings")
          table <- kableExtra::kable_styling(table)
          table <- kableExtra::add_header_above(table,
                                                c(" ", " ",
                                                  "Unstandardized" = 7))
        }
      } else {
        table <- ""
      }
    }
  }
  if (print == FALSE){
    if (nrow(table) > 0) table <- as.data.frame(table)
  }
  return(table)
}

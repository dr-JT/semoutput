#' SEM regression paths
#'
#' This function will display a table of SEM regression paths
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @param ci display standardized or unstandardized confidence intervals? (default = "standardized"). Not needed if standardized=FALSE
#' @param ci.level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem_paths(x)

sem_paths <- function(x, standardized = TRUE, ci = "standardized", ci.level = 0.95, digits = 3, print = TRUE){
  if (standardized==FALSE){
    ci <- "unstandardized"
  }
  if (ci=="standardized"){
    table <- lavaan::standardizedSolution(x, level = ci.level)
    table <- dplyr::filter(table, op=="~" | op==":=")
    table <- dplyr::mutate(table, stars = ifelse(pvalue < .001, "***",
                                                 ifelse(pvalue < .01, "**",
                                                        ifelse(pvalue < .05, "\\*", ""))))
    table <- dplyr::select(table, DV=lhs, Predictor=rhs,
                           Loadings=est.std, SE=se, z, 'sig'=stars,
                           Lower.CI = ci.lower, Upper.CI = ci.upper)

    if (print==TRUE){
      table <- knitr::kable(table, digits=digits, format="html", caption="Regression Paths")
      table <- kableExtra::kable_styling(table)
      table <- kableExtra::add_header_above(table, c(" ", " ", "Standardized" = 6))
    }
  }

  if (ci=="unstandardized"){
    table <- lavaan::parameterEstimates(x, standardized = standardized)
    table <- dplyr::filter(table, op=="~" | op==":=")
    table <- dplyr::mutate(table, stars = ifelse(pvalue < .001, "***",
                                                 ifelse(pvalue < .01, "**",
                                                        ifelse(pvalue < .05, "*", ""))))
    if (standardized==TRUE){
      table <- dplyr::select(table, DV=lhs, Predictor=rhs,
                             Loadings=est, SE=se, z, 'sig'=stars,
                             Lower.CI = ci.lower, Upper.CI = ci.upper, Loadings.std=std.all)
      if (print==TRUE){
        table <- knitr::kable(table, digits=digits, format="html", caption="Regression Paths")
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table, c(" ", " ", "Unstandardized" = 6, "Standardized" = 1))
      }
    }

    if (standardized==FALSE){
      table <- dplyr::select(table, DV=lhs, Predictor=rhs,
                             Loadings=est, SE=se, z, 'sig'=stars,
                             Lower.CI = ci.lower, Upper.CI = ci.upper)
      if (print==TRUE){
        table <- knitr::kable(table, digits=digits, format="html", caption="Regression Paths")
        table <- kableExtra::kable_styling(table)
        table <- kableExtra::add_header_above(table, c(" ", " ", "Unstandardized" = 6))
      }
    }
  }
  if (print==FALSE){
    table <- as.data.frame(table)
  }
  return(table)
}

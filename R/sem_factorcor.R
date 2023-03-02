#' Latent factor correlations
#'
#' This function will display a table of Latent factor correlations
#' @param x a cfa() or sem() lavaan model
#' @param factors depricated.
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_factorcor <- function(x, factors = c(), print = TRUE){
  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::standardizedSolution(x, level = 0.95)
  table <- dplyr::filter(table,
                         op == "~~",
                         lhs %in% factors,
                         !is.na(pvalue),
                         lhs != rhs)
  table <- dplyr::mutate(table, stars = ifelse(pvalue < .001, "***",
                                       ifelse(pvalue < .01, "**",
                                              ifelse(pvalue < .05, "*", ""))))
  table <- dplyr::select(table, 'Factor 1' = lhs, 'Factor 2' = rhs,
                         r = est.std, sig = stars, p = pvalue,
                         Lower.CI = ci.lower, Upper.CI = ci.upper,
                         SE = se)

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Latent Factor Correlations",
                            row.names = FALSE)
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

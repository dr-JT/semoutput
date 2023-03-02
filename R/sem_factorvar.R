#' Latent factor variances
#'
#' This function will display a table of Latent factor variances
#' @param x a cfa() or sem() lavaan model
#' @param factors depricated.
#' @param standardized logical. include standardized loadings? (default = TRUE)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_factorvar <- function(x, factors = c(), standardized = TRUE, print = TRUE){
  factors <- x@pta$vnames$lv[[1]]
  table <- lavaan::parameterEstimates(x, standardized = standardized)
  table <- dplyr::filter(table,
                         op == "~~",
                         lhs %in% factors,
                         !is.na(pvalue),
                         lhs == rhs)
  table <- dplyr::mutate(table,
                         stars = ifelse(pvalue < .001, "***",
                                        ifelse(pvalue < .01, "**",
                                               ifelse(pvalue < .05, "*", ""))))
  table <- dplyr::select(table, 'Factor 1' = lhs, 'Factor 2' = rhs,
                         var = est, var.std = std.all, sig = stars, p = pvalue)

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Latent Factor Variance/Residual Variance",
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

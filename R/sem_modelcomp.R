#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m1 A nested lavaan model. Null hypothesis
#' @param m2 A nested lavaan model. Alternative hypothesis
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_modelcomp <- function(m1, m2, print = TRUE){

  stats <- lavaan::anova(m1, m2)
  bic1 <- BIC(m1)
  bic2 <- BIC(m2)
  bf <- exp((bic2 - bic1) / 2)

  table <- suppressWarnings(broom::tidy(stats))
  table <- dplyr::rename(Model = term,
                         `Chi Square` = statistic)
  table$Model <- c(1, 2)
  table <- dplyr::mutate(table,
                         BICnull = dplyr::first(BIC),
                         `Bayes Factor` = ifelse(Model == 1, NA,
                                                 exp((BIC - BICnull) / 2)))
  table <- dplyr::select(Model, df, AIC, BIC, `Bayes Factor`,
                         `Chi Square`, Chisq.diff, df.diff = Df.diff,
                         p = p.value)

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Comparison", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

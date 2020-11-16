#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m0 A nested lavaan model. Null hypothesis
#' @param m1 A nested lavaan model. Alternative hypothesis
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_modelcomp <- function(m0, m1, print = TRUE){
  # Get names of model objects to use in output
  m0_name <- as.character(substitute(m0))
  m1_name <- as.character(substitute(m1))

  # Get values to be used in the table
  stats <- lavaan::anova(m0, m1)
  bic1 <- BIC(m0)
  bic2 <- BIC(m1)
  bf <- exp((bic2 - bic1) / 2)

  # Create table
  table <- suppressWarnings(broom::tidy(stats))
  table <- dplyr::rename(table, Model = term,
                         `Chi Square` = statistic)
  table <- dplyr::mutate(table,
                         Model = ifelse(Model == "m0",
                                        0, 1))
  table <- dplyr::arrange(table, Model)
  table <- dplyr::mutate(table,
                         Chisq.diff = ifelse(Model == 1,
                                             dplyr::first(Chisq.diff), NA),
                         df.diff = ifelse(Model == 1,
                                          dplyr::first(Df.diff), NA),
                         p = ifelse(Model == 1,
                                    dplyr::first(p.value), NA),
                         BF =
                           ifelse(Model == 0,
                                  exp((dplyr::last(BIC) - dplyr::first(BIC)) / 2),
                                  exp((dplyr::first(BIC) - dplyr::last(BIC)) / 2)),
                         `P(Model|Data)` = BF / (BF + 1),
                         Model = ifelse(Model == 0, m0_name, m1_name))
  table <- dplyr::select(table, Model, df, AIC, BIC, BF, `P(Model|Data)`,
                         `Chi Square`, `Chi Square Diff` = Chisq.diff,
                         `df Diff` = df.diff, p)

  # Print table
  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Comparison", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

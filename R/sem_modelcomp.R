#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m0 A nested lavaan model. Null hypothesis
#' @param m1 A nested lavaan model. Alternative hypothesis
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

sem_modelcomp <- function(m0, m1, print = TRUE){

  stats <- lavaan::anova(m0, m1)
  bic1 <- BIC(m0)
  bic2 <- BIC(m1)
  bf <- exp((bic2 - bic1) / 2)

  table <- suppressWarnings(broom::tidy(stats))
  table <- dplyr::rename(table, Model = term,
                         `Chi Square` = statistic)
  table <- dplyr::mutate(table,
                         Model = ifelse(Model == "m0",
                                        "0 (Null)", "1 (Alternative)"))
  table <- dplyr::arrange(table, Model)
  table <- dplyr::mutate(table,
                         Chisq.diff = ifelse(Model == 2,
                                             dplyr::first(Chisq.diff), NA),
                         df.diff = ifelse(Model ==2,
                                          dplyr::first(Df.diff), NA),
                         p = ifelse(Model == 2,
                                    dplyr::first(p.value), NA),
                         BICnull = dplyr::first(BIC),
                         BF.10 = ifelse(Model == 1, NA,
                                                 exp((BICnull - BIC) / 2)),
                         `P(Model|Data)` = last(BF.10) / (last(BF.10) + 1),
                         `P(Model|Data)` = ifelse(Model == 1, `P(Model|Data)`,
                                                  1 - `P(Model|Data)`))
  table <- dplyr::select(table, Model, df, AIC, BIC, BF.10, `P(Model|Data)`,
                         `Chi Square`, `Chi Square Diff` = Chisq.diff,
                         `df Diff` = df.diff, p)

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Comparison", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

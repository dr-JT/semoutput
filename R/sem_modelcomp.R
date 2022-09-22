#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m0 A lavaan model.
#' @param m1 A lavaan model.
#' @param print Create a knitr table for displaying as html table.
#' @export
#'

sem_modelcomp <- function(m0, m1, print = TRUE){
  # Get names of model objects to use in output
  m0_name <- as.character(substitute(m0))
  m1_name <- as.character(substitute(m1))

  # Get values to be used in the table
  stats <- lavaan::anova(m0, m1)

  # Create table
  table <- suppressWarnings(broom::tidy(stats))
  table <- dplyr::rename(table, Model = term,
                         `Chi Square` = statistic)
  table <- dplyr::mutate(table, Model = ifelse(Model == "m0",
                                               m0_name, m1_name))
  table <- dplyr::arrange(table, dplyr::desc(df))
  table <- dplyr::mutate(table,
                         row = dplyr::row_number(),
                         Chisq.diff = ifelse(row == 2,
                                             dplyr::first(Chisq.diff), NA),
                         df.diff = ifelse(row == 2,
                                          dplyr::first(Df.diff), NA),
                         p = ifelse(row == 2,
                                    dplyr::first(p.value), NA),
                         BF =
                           ifelse(row == 1,
                                  exp((dplyr::last(BIC) - dplyr::first(BIC)) / 2),
                                  exp((dplyr::first(BIC) - dplyr::last(BIC)) / 2)),
                         `P(Model|Data)` = BF / (BF + 1))
  table <- dplyr::select(table, Model, df, AIC, BIC, BF, `P(Model|Data)`,
                         `Chi Square`, `Chi Square Diff` = Chisq.diff,
                         `df Diff` = df.diff, p)

  # Print table
  if (print == TRUE) {
    table <- knitr::kable(table, digits = 3,
                          caption = "Model Comparison", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

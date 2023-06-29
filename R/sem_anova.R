#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m1 A nested lavaan model
#' @param m2 A nested lavaan model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @keywords internal
#' @export
#'

sem_anova <- function(m1, m2, print = TRUE){
  stats <- lavaan::anova(m1, m2)
  table <- suppressWarnings(broom::tidy(stats))
  table <- dplyr::arrange(table, desc(df))
  table$term <- c(1, 2)

  if (nrow(table) > 0) {
    if (print == TRUE) {
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Model Comparison", row.names = FALSE,
                            table.attr = 'data-quarto-disable-processing="true"')
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
    }
  } else {
    table <- ""
  }

  return(table)
}

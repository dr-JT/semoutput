#' EFA Total Variance Explained
#'
#' This function will display a table of total variance explained in an EFA
#' @param fit an efa model created by psych::fa()
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

efa_var <- function(fit, print = TRUE){
  table <- as.data.frame(fit$Vaccounted)
  table <- tibble::rownames_to_column(table)
  table <- tidyr::pivot_longer(table, -rowname)
  table <- tidyr::pivot_wider(table, names_from = rowname)
  table <- dplyr::select(table, Factor = name, Eigenvalue = `SS loadings`, `Proportion Var`, `Cumulative Var`)
  table <- dplyr::mutate(table, Factor = stringr::str_replace(Factor, "PA", ""))

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Total Variance Explained", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

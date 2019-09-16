#' EFA Factor Rotation Matrix
#'
#' This function will display a factor rotation matrix for an EFA
#' @param fit an efa model created by psych::fa()
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

efa_rotmatrix <- function(fit, print = TRUE){
  table <- as.data.frame(fit$rot.mat)
  colnames(table) <- stringr::str_replace(colnames(table), "V", "")
  table <- tibble::rownames_to_column(table)
  table <- dplyr::rename(table, Factor = rowname)

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Factor Rotation Matrix", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

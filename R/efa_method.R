#' EFA Extraction Method
#'
#' This function will display a table of extraction method for an EFA
#' @param fit an efa model created by psych::fa()
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

efa_method <- function(fit, print = TRUE){
  sample.n <- fit$n.obs
  factor.n <- fit$Call$nfactors
  method = fit$Call$fm
  rotation = fit$Call$rotate

  if (method == "pa") method <- "Principal Axis Factoring"
  if (rotation == "varimax") rotation <- "Varimax"

  table <- data.frame(`Sample.Size` = sample.n, Method = method,
                      `Factors Extracted` = factor.n, Rotation = rotation)

  if (print == TRUE) {
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Extraction Method", row.names = FALSE,
                          table.attr = 'data-quarto-disable-processing="true"')
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

#' EFA Loadings and Communalities
#'
#' This function will display a table of factor loadings and communalities for an EFA
#' @param fit an efa model created by psych::fa()
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#'

efa_loadings <- function(fit, print = TRUE){
  factors.n <- fit$factors
  table <- as.data.frame(fit$loadings[,1:factors.n])
  colnames(table) <- stringr::str_replace(colnames(table), "PA", "F")
  table <- tibble::rownames_to_column(table)
  table <- dplyr::mutate(table, id = dplyr::row_number())
  table <- dplyr::rename(table, Variable = rowname)

  comm <- as.data.frame(fit$communality)
  comm <- tibble::rownames_to_column(comm)
  comm <- dplyr::rename(comm, Variable = rowname, `h2` = `fit$communality`)

  table <- merge(table, comm, by = "Variable", all = TRUE)
  table <- dplyr::arrange(table, id)
  table <- dplyr::select(table, -id)

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Factor Loadings", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

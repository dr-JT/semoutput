#' Chi-square significance test
#'
#' This function will display a table of Model significance tests
#' @param x results from a cfa() or sem() lavaan model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @export
#' @examples
#' sem.sig(x)

sem.sig <- function(x, print = TRUE){
  stats <- lavaan::fitMeasures(x, c("ntotal", "chisq", "pvalue", "df"))
  table <- data.frame('Sample Size'=stats[["ntotal"]], 'Chi-Square'=stats[["chisq"]], df=stats[["df"]], 'p-value'=stats[["pvalue"]])

  if (print==TRUE){
    table <- knitr::kable(table, digits=3, format="html", caption="Model Significance", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width=FALSE, position = "left")
  } else if (print==FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}

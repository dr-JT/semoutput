#' Model comparison
#'
#' This function will display a table of Model significance tests
#' @param m1 A nested lavaan model
#' @param m2 A nested lavaan model
#' @param print Create a knitr table for displaying as html table (default = TRUE)
#' @param method Model comparison method. "chi-square" or "BIC"
#' @export
#'

sem_modelcomp <- function(m1, m2, print = TRUE, method = "chi-square"){
  if (method = "chi-square") {
    stats <- lavaan::anova(m1, m2)
    table <- suppressWarnings(broom::tidy(stats))
    table <- dplyr::arrange(table, desc(df))
    table$term <- c(1, 2)
  }

  if (method = "BIC") {
    df1 <- lavaan::fitMeasures(m1, c("df"))
    df2 <- lavaan::fitMeasures(m2, c("df"))
    bic1 <- BIC(m1)
    bic2 <- BIC(m2)
    bf <- exp((bic1 - bic2)/2)

    table <- data.frame(Model = c(1, 2), df = c(df1, df2),
                        BIC = c(bic1, bic2), BF = c(NA, bf))
  }

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Comparison", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
  }

  return(table)
}

#' SEM Tables
#'
#' Display all tables relevant for SEM models
#'
#' @param x a lavaan model
#' @param standardized logical. Include standardized loadings? (default = TRUE)
#' @param ci logical. display standardized or unstandardized confidence
#'     intervals? (default = "standardized"). Not needed if standardized=FALSE
#' @param ci.level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_tables <- function(x, standardize = TRUE, ci = "standardized",
                       ci.level = 0.95, digits = 3, print = TRUE){
  sem_sig(x, print = print)
  sem_fitmeasures(x, print = print)
  sem_factorloadings(x, standardized = standardize, ci = ci,
                     ci.level = ci.level, digits = digits, print = print)
  sem_paths(x, standardized = standardized, ci = ci,
            ci.level = ci.level, digits = digits, print = print)
  sem_factorcor(x, print = print)
  sem_factorvar(x, standardized = standardized, print = print)
  sem_rsquared(x, print = print)
}

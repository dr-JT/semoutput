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

sem_tables <- function(x, standardized = TRUE, ci = "standardized",
                       ci.level = 0.95, digits = 3, print = TRUE){
  sig_table <- sem_sig(x, print = print)
  fit_table <- sem_fitmeasures(x, print = print)
  loadings_table <- sem_factorloadings(x, standardized = standardized, ci = ci,
                                       ci.level = ci.level, digits = digits,
                                       print = print)
  paths_table <- sem_paths(x, standardized = standardized, ci = ci,
                           ci.level = ci.level, digits = digits,
                           print = print)
  corr_table <- sem_factorcor(x, print = print)
  var_table <- sem_factorvar(x, standardized = standardized, print = print)
  rsquared_table <- sem_rsquared(x, print = print)

  return(sig_table, fit_table, loadings_table, paths_table,
         corr_table, var_table, rsquared_table)
}

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
#' @export
#'

sem_tables <- function(x, standardized = TRUE, ci = "standardized",
                       ci.level = 0.95, digits = 3){
  sig_table <- sem_sig(x, print = TRUE)
  fit_table <- sem_fitmeasures(x, print = TRUE)
  loadings_table <- sem_factorloadings(x, standardized = standardized, ci = ci,
                                       ci.level = ci.level, digits = digits,
                                       print = TRUE)
  paths_table <- sem_paths(x, standardized = standardized, ci = ci,
                           ci.level = ci.level, digits = digits,
                           print = TRUE)
  corr_table <- sem_factorcor(x, print = TRUE)
  var_table <- sem_factorvar(x, standardized = standardized, print = TRUE)
  rsquared_table <- sem_rsquared(x, print = TRUE)

  if (sig_table != "") print(sig_table)
  if (fit_table != "") print(fit_table)
  if (loadings_table != "") print(loadings_table)
  if (paths_table != "") print(paths_table)
  if (corr_table != "") print(corr_table)
  if (var_table != "") print(var_table)
  if (rsquared_table != "") print(rsquared_table)
}

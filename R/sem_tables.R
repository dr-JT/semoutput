#' SEM Tables
#'
#' Display all tables relevant for SEM models. Make sure results='asis' is
#' specified in the R code chunk setting.
#'
#' @param x a lavaan model
#' @param standardized Logical, indicating whether or not to print standardized
#'      estimates. Standardized estimates are based on "refit" of the model
#'      on standardized data but it will not standardize categorical predictors.
#'      Defualt is TRUE.
#' @param unstandardized Logical, indicating whether or not to print
#'      unstandardized estimates. Default is TRUE.
#' @param robust logical. Display robust fit statistics? (default: FALSE)
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param rmsea_ci_level What level of confidence interval to use for
#'     RMSE? (default = .90)
#' @param digits How many digits to display? (default = 3)
#' @export
#'

sem_tables <- function(x, standardized = TRUE, unstandardized = FALSE,
                       robust = FALSE, ci_level = 0.95,
                       rmsea_ci_level = .90, digits = 3) {

  sig_table <- sem_sig(x, digits = digits)
  table <- gt::gt_group(sig_table)

  fit_table <- sem_fitmeasures(x, robust = robust,
                               ci_level = rmsea_ci_level, digits = digits)

  if (robust == TRUE) {
    robust_table <- gt::grp_pull(fit_table, 2)
    fit_table <- gt::grp_pull(fit_table, 1)
  }

  if (is.list(fit_table)) table <- gt::grp_add(table, fit_table)
  if (robust == TRUE) table <- gt::grp_add(table, robust_table)

  loadings_table <- sem_factorloadings(x, standardized = standardized,
                                       unstandardized = unstandardized,
                                       ci_level = ci_level, digits = digits)
  if (is.list(loadings_table)) table <- gt::grp_add(table, loadings_table)

  paths_table <- sem_paths(x, standardized = standardized,
                           unstandardized = unstandardized,
                           ci_level = ci_level, digits = digits)
  if (is.list(paths_table)) table <- gt::grp_add(table, paths_table)

  corr_table <- sem_factorcor(x, ci_level = ci_level, digits = digits)
  if (is.list(corr_table)) table <- gt::grp_add(table, corr_table)

  var_table <- sem_factorvar(x, digits = digits)
  if (is.list(var_table)) table <- gt::grp_add(table, var_table)

  rsquared_table <- sem_rsquared(x, digits = digits)
  if (is.list(rsquared_table)) table <- gt::grp_add(table, rsquared_table)

  return(table)
}

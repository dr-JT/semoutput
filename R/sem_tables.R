#' SEM Tables
#'
#' Display all tables relevant for SEM models. Make sure results='asis' is
#' specified in the R code chunk setting.
#'
#' @param x a lavaan model
#' @param standardized logical. Include standardized loadings? (default = TRUE)
#' @param ci logical. display standardized or unstandardized confidence
#'     intervals? (default = "standardized"). Not needed if standardized=FALSE
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @export
#'

sem_tables <- function(x, standardized = TRUE, ci = "standardized",
                       ci_level = 0.95, digits = 3){
  # Function to make sure table prints whether running code in R code chunk,
  # with knit button or with rmarkdown::render()
  print_table <- function(x) {
    if (isTRUE(getOption('knitr.in.progress'))) {
      writeLines(x)
    } else {
      print(x)
    }
  }

  sig_table <- sem_sig(x, print = TRUE)
  fit_table <- sem_fitmeasures(x, print = TRUE)
  loadings_table <- sem_factorloadings(x, standardized = standardized, ci = ci,
                                       ci_level = ci_level, digits = digits,
                                       print = TRUE)
  paths_table <- sem_paths(x, standardized = standardized, ci = ci,
                           ci_level = ci_level, digits = digits,
                           print = TRUE)
  corr_table <- sem_factorcor(x, print = TRUE)
  var_table <- sem_factorvar(x, print = TRUE)
  rsquared_table <- sem_rsquared(x, print = TRUE)

  if (sig_table != "") print_table(sig_table)
  if (fit_table != "") print_table(fit_table)
  if (loadings_table != "") print_table(loadings_table)
  if (paths_table != "") print_table(paths_table)
  if (corr_table != "") print_table(corr_table)
  if (var_table != "") print_table(var_table)
  if (rsquared_table != "") print_table(rsquared_table)
}

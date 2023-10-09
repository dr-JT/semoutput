#' SEM regression paths
#'
#' This function will display a table of SEM regression paths
#' @param x a cfa() or sem() lavaan model
#' @param standardized Logical, indicating whether or not to print standardized
#'      estimates. Standardized estimates are based on "refit" of the model
#'      on standardized data but it will not standardize categorical predictors.
#'      Defualt is TRUE.
#' @param unstandardized Logical, indicating whether or not to print
#'      unstandardized estimates. Default is TRUE.
#' @param ci_level What level of confidence interval to use (default = 0.95)
#' @param digits How many digits to display? (default = 3)
#' @param print Create a knitr table for displaying as html table? (default = TRUE)
#' @export
#'

sem_paths <- function(x, standardized = TRUE, unstandardized = FALSE,
                      ci_level = 0.95, digits = 3, print = TRUE) {

  ci_col_label <- paste(round(ci_level*100, 0), "% ", "CI", sep = "")

  if (!is.null(x@call$se)) {
    if (x@call$se == "boot" | x@call$se == "bootstrap") {
      message("SEs, p-values, and confidence intervals may not be accuracte ",
              "for bootstrapped indirect effects. If using bootstrapping for ",
              "indirect effects it is advised to set se = \"standard\" and to ",
              "use semTools::monteCarloCI() instead. This will calculate ",
              "bias-corrected percentile confidence intervals.")
    }
  }

  fit_standardized <- lavaan::standardizedSolution(x, level = ci_level) |>
    dplyr::filter(op == "~" | op == ":=") |>
    format_ci(digits = digits) |>
    format_stars() |>
    dplyr::rename(CI_std = CI)

  fit_unstandardized <- lavaan::parameterEstimates(x, level = ci_level) |>
    dplyr::filter(op == "~" | op == ":=") |>
    format_ci(digits = digits) |>
    format_stars() |>
    dplyr::select(lhs, rhs, est, CI_unstd = CI, stars_unstd = stars,
                  se_unstd = se, z_unstd = z, pvalue_unstd = pvalue)

  table <- merge(fit_unstandardized, fit_standardized, by = c("lhs", "rhs")) |>
    dplyr::select(rhs, lhs, est, CI_unstd, stars_unstd, se_unstd,
                  z_unstd, pvalue_unstd,
                  est.std, CI_std, stars, se, z, pvalue) |>
    dplyr::arrange(lhs)

  if (nrow(table) > 0) {
    if (print == TRUE) {

      table_title <- "Regression Paths"

      table <- gt::gt(table) |>
        table_styling() |>
        gt::tab_header(title = table_title) |>
        gt::cols_label(lhs = "DV", rhs = "Predictor",
                       est = "b", CI_unstd = ci_col_label,
                       stars_unstd = "sig", se_unstd = "SE", z_unstd = "z",
                       pvalue_unstd = "p",
                       est.std = "β",
                       CI_std = ci_col_label,
                       stars = "sig",
                       se = "SE",
                       pvalue = "p") |>
        gt::cols_align(align = "left", columns = c(lhs, rhs)) |>
        gt::sub_small_vals(columns = pvalue, threshold = .001) |>
        gt::fmt_number(decimals = digits)

      if (standardized == TRUE & unstandardized == TRUE) {
        table <- table |>
          gt::cols_hide(c(stars_unstd, se_unstd, z_unstd, pvalue_unstd)) |>
          gt::tab_spanner(label = "Unstandardized",
                          columns = c(est, CI_unstd)) |>
          gt::tab_spanner(label = "Standardized",
                          columns = c(est.std, CI_std, stars, se, z, pvalue))
      }

      if (standardized == TRUE & unstandardized == FALSE) {
        table <- table |>
          gt::cols_hide(c(est, CI_unstd, stars_unstd, se_unstd,
                          z_unstd, pvalue_unstd)) |>
          gt::tab_spanner(label = "Standardized",
                          columns = c(est.std, CI_std, stars, se, z, pvalue))
      }

      if (standardized == FALSE & unstandardized == TRUE) {
        table <- table |>
          gt::cols_hide(c(est.std, CI_std, stars, se, z, pvalue)) |>
          gt::tab_spanner(label = "Unstandardized",
                          columns = c(est, CI_unstd, stars_unstd, se_unstd,
                                      z_unstd, pvalue_unstd))
      }
    }
    if (print == FALSE) {
      table <- as.data.frame(table)
    }
  } else {
    table <- ""
  }
  return(table)
}

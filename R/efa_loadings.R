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

  if (print == TRUE) {
    table <- gt::gt(table) |>
      table_styling() |>
      gt::tab_header(title = "Factor Loadings") |>
      gt::cols_label(lhs = "Latent Factor", rhs = "Indicator",
                     est = "Loading", ci.lower_unstd = ci_col_label,
                     stars_unstd = "sig", se_unstd = "SE", z_unstd = "z",
                     pvalue_unstd = "p",
                     est.std = "Loading",
                     ci.lower_std = ci_col_label,
                     stars = "sig",
                     se = "SE",
                     pvalue = "p") |>
      gt::cols_align(align = "left", columns = c(lhs, rhs)) |>
      gt::sub_small_vals(columns = pvalue, threshold = .001) |>
      gt::fmt_number(decimals = digits) |>
      gt::tab_footnote("* p < .05; ** p < .01; *** p < .001")

    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Factor Loadings", row.names = FALSE,
                          table.attr = 'data-quarto-disable-processing="true"')
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
    table <- kableExtra::add_header_above(table,
                                          c(" ", "Factors" = factors.n, " "))
    table <- kableExtra::column_spec(table, 1 + factors.n, border_right = TRUE)
  }

  return(table)
}

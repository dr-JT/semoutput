#' Table style
#'
#' @param x a table
#' @export
#'

table_styling <- function(x) {
  x <- x |>
    gt::tab_options(
      table.align = "left",
      table.border.top.width = 0,
      table.border.bottom.width = 0,
      heading.align = "left",
      heading.padding = 10,
      heading.border.bottom.width = 0,
      heading.title.font.size = 18,
      heading.title.font.weight = "bolder",
      column_labels.border.top.width = 0,
      column_labels.font.weight = "bold",
      column_labels.padding = 6,
      footnotes.font.size = 14,
      quarto.disable_processing = TRUE
    ) |>
    gt::tab_style(style = gt::cell_text(align = 'center'),
                  locations = gt::cells_column_labels()) |>
    gt::opt_horizontal_padding(scale = 3) |>
    gt::cols_align_decimal(columns = dplyr::everything()) |>
    gt::sub_missing(missing_text = "") |>
    gt::fmt_number(use_seps = FALSE)
  return(x)
}

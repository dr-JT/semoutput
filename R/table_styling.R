#' Table style
#'
#' @param x a table
#' @export
#'

table_styling <- function(x) {
  x <- x |>
    gt::tab_options(
      table.align = "left",
      heading.border.bottom.color = "black",
      heading.title.font.size = 12,
      heading.align = "center",
      table.border.top.color = "black",
      heading.title.font.weight = "bolder",
      column_labels.font.weight = "bold",
      footnotes.font.size = 12,
      table.border.top.width = 0,
      table.border.bottom.width = 0,
      column_labels.border.top.width = 2,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 0,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table_body.border.top.color = "black"
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom")
        ),
        gt::cell_text(
          align = "center"
        )
      ),
      locations = gt::cells_column_labels(
        columns = -dplyr::any_of(c("Model", "Term"))
      )
    ) |>
    gt::opt_horizontal_padding(scale = 2)
  return(x)
}

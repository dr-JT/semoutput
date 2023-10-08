#' Table style
#'
#' @param x a table
#' @export
#'

table_styling <- function(x) {
  x <- x |>
    gt::tab_options(
      table.align = "left",
      heading.border.bottom.width = 0,
      heading.border.bottom.color = "gray",
      heading.title.font.size = 18,
      heading.align = "left",
      table.border.top.color = "gray",
      heading.title.font.weight = "bolder",
      column_labels.font.weight = "bold",
      footnotes.font.size = 12,
      table.border.top.width = 0,
      table.border.bottom.width = 0,
      column_labels.border.top.width = 1.25,
      column_labels.border.top.color = "gray",
      column_labels.border.bottom.width = 1.5,
      column_labels.border.bottom.color = "gray",
      table_body.border.bottom.color = "gray",
      table_body.border.top.color = "gray",
      column_labels.padding = 3
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
        columns = -dplyr::any_of(c("lhs", "rhs"))
      )
    ) |>
    gt::opt_horizontal_padding(scale = 3) |>
    gt::sub_missing(missing_text = "")
  return(x)
}

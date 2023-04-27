#' Insert EFA Section
#'
#' @export
#'

add_efa <- function() {
  efa_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/efa.qmd"

  # Make a GET request to retrieve the raw content of the file
  efa_text <- httr::content(httr::GET(efa_url), as = "text")
  rstudioapi::insertText(efa_text)
}

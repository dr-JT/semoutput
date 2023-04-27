#' Insert CFA Section
#'
#' @export
#'

add_cfa <- function() {
  cfa_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/cfa.qmd"

  # Make a GET request to retrieve the raw content of the file
  cfa_text <- httr::content(httr::GET(cfa_url), as = "text")
  rstudioapi::insertText(cfa_text)
}

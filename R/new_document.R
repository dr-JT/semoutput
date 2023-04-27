#' Create New Quarto Document
#'
#' @export
#'

new_document <- function() {
  yaml_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/yaml.qmd"
  setup_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/setup.qmd"
  data_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/data.qmd"
  foot_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/footer.qmd"

  # Make a GET request to retrieve the raw content of the file
  yaml_text <- httr::content(httr::GET(yaml_url), as = "text")
  setup_text <- httr::content(httr::GET(setup_url), as = "text")
  data_text <- httr::content(httr::GET(data_url), as = "text")
  footer_text <- httr::content(httr::GET(foot_url), as = "text")

  rstudioapi::documentNew(c(yaml_text, setup_text, data_text, footer_text),
                          type = "rmarkdown")
}

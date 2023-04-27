#' Insert SEM Section
#'
#' @export
#'

add_sem <- function() {
  sem_url <- "https://raw.githubusercontent.com/dr-JT/semoutput/main/inst/extdata/_extensions/sem.qmd"

  # Make a GET request to retrieve the raw content of the file
  sem_text <- httr::content(httr::GET(sem_url), as = "text")
  rstudioapi::insertText(sem_text)
}

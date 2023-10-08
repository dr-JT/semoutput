#' Format confidence intervals for table display
#'
#' @param x
#' @param digits How many digits to display? (default = 3)
#' @export
#'

format_ci <- function(x, digits = digits) {
    x <- as.data.frame(x) |>
      dplyr::mutate(ci.lower = round(ci.lower, digits),
                    ci.upper = round(ci.upper, digits)) |>
      tidyr::unite(CI, ci.lower, ci.upper, sep = " - ")
    return(x)
  }

#' Validate date format
#'
#' Checks whether a date string is in `"dd/mm/yyyy"` format and can be
#' successfully parsed using `lubridate::dmy()`.
#'
#' @param date Character date string.
#'
#' @return Logical. `TRUE` if the date can be parsed as `"dd/mm/yyyy"`,
#' otherwise `FALSE`.
#'
#' @export
#'
#' @examples
#' check_date("01/01/2025")
#' check_date("2025-01-01")
check_date <- function(date) {

  if (length(date) != 1) {
    stop("`date` must be a single value.", call. = FALSE)
  }

  !is.na(lubridate::dmy(date, quiet = TRUE))
}

#' Validate date format strictly
#'
#' Checks whether a date string or vector of date strings strictly matches the
#' `"dd/mm/yyyy"` format and can be successfully parsed into a valid calendar date.
#'
#' @param date Character. A single date string or a vector of date strings.
#'
#' @return Logical vector of the same length as `date`. `TRUE` if the date
#'   strictly matches `"dd/mm/yyyy"` and is a real date, otherwise `FALSE`.
#'
#' @export
#'
#' @examples
#' check_date_format("01/01/2025") # TRUE
#' check_date_format("2025-01-01") # FALSE (Wrong format)
#' check_date_format("32/01/2025") # FALSE (Invalid calendar date)
check_date_format <- function(date) {
  if (length(date) == 0) return(logical(0))

  # Ensure pattern matches strict digit/digit/digit formatting
  has_correct_pattern <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date)

  # Ensure it is a real calendar date (e.g., catching Feb 30th)
  is_valid_date <- !is.na(lubridate::dmy(date, quiet = TRUE))

  has_correct_pattern & is_valid_date
}

#' Validate date chronology
#'
#' Checks whether a vector of start dates occurs chronologically before or on a
#' corresponding vector of end dates.
#'
#' @param from_date Character vector. Start date(s) in `"dd/mm/yyyy"` format.
#' @param to_date Character vector. End date(s) in `"dd/mm/yyyy"` format.
#'
#' @return Logical vector of the same length as the input vectors. `TRUE` if
#'   `from_date` is chronologically less than or equal to `to_date`, otherwise `FALSE`.
#' @export
#'
#' @examples
#' check_date_chronology("01/01/2025", "05/01/2025") # TRUE
#' check_date_chronology("10/01/2025", "01/01/2025") # FALSE
check_date_chronology <- function(from_date, to_date) {
  # Assumes length match and format checks were run upstream
  start_parsed <- lubridate::dmy(from_date, quiet = TRUE)
  end_parsed   <- lubridate::dmy(to_date, quiet = TRUE)

  start_parsed <= end_parsed
}

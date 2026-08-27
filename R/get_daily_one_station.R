#' Retrieve daily air quality data for a single station
#'
#' Downloads daily averaged air quality data from the
#' Environment Canterbury data portal for one monitoring site.
#'
#' @param site_id Numeric or character station identifier.
#' @param from_date Character date in `"dd/mm/yyyy"` format.
#' @param to_date Character date in `"dd/mm/yyyy"` format.
#'
#' @return A tibble in long format containing:
#' \describe{
#'   \item{DateTime}{Date of observation}
#'   \item{StationName}{Station name}
#'   \item{name}{Pollutant name}
#'   \item{value}{Daily value}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' get_daily_one_station(101, "01/01/2025", "31/01/2025")
#' }

get_daily_one_station <- function(site_id, from_date, to_date) {
  if (length(site_id) != 1 || is.na(site_id) || !nzchar(as.character(site_id))) {
    stop("`site_id` must be a single non-empty value.", call. = FALSE)
  }

  if (!check_date(from_date) || !check_date(to_date)) {
    stop("Dates must be in 'dd/mm/yyyy' format.", call. = FALSE)
  }

  if (lubridate::dmy(from_date) > lubridate::dmy(to_date)) {
    stop("from_date must be earlier than to_date.", call. = FALSE)
  }

  base_url <- "https://data.ecan.govt.nz:443/data/98/Air/Air%20quality%20data%20for%20a%20monitored%20site%20(daily)/CSV"

  response <- httr::GET(
    base_url,
    query = list(
      SiteID = site_id,
      StartDate = from_date,
      EndDate = to_date
    )
  )

  ecan_stop_for_status(
    response,
    paste0("Daily data request for station ", site_id)
  )

  dat_raw <- httr::content(response, encoding = "UTF-8", as = "text")

  dat <- readr::read_csv(
    I(dat_raw),
    show_col_types = FALSE
  )

  names(dat) <- stringr::str_replace_all(names(dat), "\\.", "")

  required_columns <- c("DateTime", "StationName")
  missing_columns <- setdiff(required_columns, names(dat))
  if (length(missing_columns) > 0) {
    stop(
      "Daily data response is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  measurement_columns <- setdiff(names(dat), required_columns)
  if (length(measurement_columns) == 0) {
    stop(
      "Daily data response contains no measurement columns.",
      call. = FALSE
    )
  }

  parsed_dates <- lubridate::ymd(dat$DateTime, quiet = TRUE)
  supplied_dates <- !is.na(dat$DateTime) &
    nzchar(trimws(as.character(dat$DateTime)))
  if (any(supplied_dates & is.na(parsed_dates))) {
    stop(
      "Daily data response contains invalid DateTime values.",
      call. = FALSE
    )
  }

  if (!all(vapply(dat[measurement_columns], is.numeric, logical(1)))) {
    stop(
      "Daily data response contains non-numeric measurement columns.",
      call. = FALSE
    )
  }

  dat |>
    tidyr::pivot_longer(
      cols = -c(DateTime, StationName),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      DateTime = lubridate::ymd(DateTime, quiet = TRUE),
      value = janitor::round_half_up(value, 1)
    )
}

#' Retrieve daily air quality data for a single station
#'
#' Downloads daily averaged air quality data from the
#' Environment Canterbury data portal for one monitoring site.
#'
#' @param site_no Numeric or character station identifier. Default is `2` (Riccarton Road)
#' @param from_date Character date in `"dd/mm/yyyy"` format. Default is `1/01/2026`
#' @param to_date Character date in `"dd/mm/yyyy"` format. Default is `31/01/2026`
#' @param request_timeout Numeric. Maximum time in seconds to wait for the server
#'   response before aborting. Default is `10`.
#'
#' @return A tibble in long format containing:
#' \describe{
#'   \item{date}{Date of observation}
#'   \item{station}{Station name}
#'   \item{parameter}{Pollutant name}
#'   \item{value}{Daily value}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' get_daily_one_station(2, "1/01/2026", "31/01/2026")
#' }

get_daily_one_station <- function(site_no = 2, from_date = "1/01/2026", to_date = "31/01/2026", request_timeout = 10) {

  if (length(site_no) != 1 || is.na(site_no) || !nzchar(as.character(site_no))) {
    stop("`site_no` must be a single non-empty value.", call. = FALSE)
  }

  # Integrated format check (fixed to stop when invalid)
  if (any(!check_date_format(c(from_date, to_date)))) {
    stop("Dates must be in 'dd/mm/yyyy' format.", call. = FALSE)
  }

  # Integrated chronology check
  if (!check_date_chronology(from_date, to_date)) {
    stop("from_date must be earlier than to_date.", call. = FALSE)
  }

  url <- "https://data.ecan.govt.nz:443/data/98/Air/Air%20quality%20data%20for%20a%20monitored%20site%20(daily)/CSV"

  response <- httr::GET(
    url,
    query = list(
      SiteID = site_no,
      StartDate = from_date,
      EndDate = to_date
    ),
    httr::timeout(request_timeout)
  )

  httr::stop_for_status(response)

  dat_raw <- httr::content(response, as = "text", encoding = "UTF-8")

  dat <- readr::read_csv(
    I(dat_raw),
    show_col_types = FALSE,
    lazy = FALSE
  )

  if (nrow(dat) == 0) {
    return(tibble::tibble(date = as.Date(character()), station = character(), parameter = character(), value = numeric()))
  }

  dat |>
    tidyr::pivot_longer(
      cols = -c(DateTime, StationName),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      DateTime = lubridate::ymd(.data$DateTime),
      value = janitor::round_half_up(.data$value, 1)
    ) |>
    janitor::clean_names() |>
    dplyr::rename(
      date = .data$date_time,
      station = .data$station_name,
      parameter = .data$name
    )

}

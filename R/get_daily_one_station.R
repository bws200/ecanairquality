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

get_daily_one_station <- function(site_id, from_date, to_date) {
  DateTime <- NULL
  StationName <- NULL
  value <- NULL

  base_url <- "https://data.ecan.govt.nz:443/data/98/Air/Air%20quality%20data%20for%20a%20monitored%20site%20(daily)/CSV"

  if (!check_date(from_date) || !check_date(to_date)) {
    return(tibble::tibble())
  }

  response <- httr::GET(
    base_url,
    query = list(
      SiteID = site_id,
      StartDate = from_date,
      EndDate = to_date
    )
  )

  if (httr::http_error(response)) {
    return(tibble::tibble())
  }

  dat_raw <- httr::content(response, encoding = "UTF-8", as = "text")

  dat <- readr::read_csv(
    dat_raw,
    show_col_types = FALSE
  )

  names(dat) <- stringr::str_replace_all(names(dat), "\\.", "")

  dat |>
    tidyr::pivot_longer(
      cols = -c(DateTime, StationName),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      DateTime = lubridate::ymd(DateTime),
      value = janitor::round_half_up(value, 1)
    )
}

#' Retrieve daily air quality data from the ECan website
#'
#' Retrieves daily averaged air quality data for all monitoring sites
#' available via the Environment Canterbury (ECan) data portal.
#'
#' @param from_date Character. Start date in `"dd/mm/yyyy"` format.
#' @param to_date Character. End date in `"dd/mm/yyyy"` format.
#' @param station_ids Vector of station IDs to retrieve.
#'
#' @return A data frame containing daily air quality measurements in
#' long format.
#'
#' @export
get_daily_all_stations <- function(from_date, to_date, station_ids = station_id_list$SiteNo) {

  from <- lubridate::dmy(from_date)
  to <- lubridate::dmy(to_date)

  if (is.na(from) || is.na(to)) {
    stop("Dates must be in 'dd/mm/yyyy' format.", call. = FALSE)
  }

  if (from > to) {
    stop("from_date must be earlier than to_date.", call. = FALSE)
  }

  time_interval <- lubridate::interval(from, to)

  message(
    "Time interval: ",
    round(lubridate::time_length(time_interval, "days")),
    " days"
  )

  datalist <- purrr::map(
    station_ids,
    get_daily_one_station,
    from_date = from_date,
    to_date = to_date
  )

  dplyr::bind_rows(datalist)
}

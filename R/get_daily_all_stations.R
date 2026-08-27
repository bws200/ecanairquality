#' Retrieve daily air quality data from the ECan website
#'
#' Retrieves daily averaged air quality data for all monitoring sites
#' available via the Environment Canterbury (ECan) data portal.
#'
#' @param from_date Character. Start date in `"dd/mm/yyyy"` format.
#' @param to_date Character. End date in `"dd/mm/yyyy"` format.
#' @param on_error How to handle a station request failure: `"warn"` (the
#'   default) continues with other stations and warns, `"stop"` aborts the
#'   request, or `"ignore"` continues silently.
#'
#' @return A data frame containing daily air quality measurements in
#' long format.
#'
#' @export
get_daily_all_stations <- function(from_date, to_date,
                                   on_error = c("warn", "stop", "ignore")) {

  if (!check_date(from_date) || !check_date(to_date)) {
    stop("Dates must be in 'dd/mm/yyyy' format.", call. = FALSE)
  }

  on_error <- match.arg(on_error)
  from <- lubridate::dmy(from_date)
  to <- lubridate::dmy(to_date)

  if (from > to) {
    stop("from_date must be earlier than to_date.", call. = FALSE)
  }

  time_interval <- lubridate::interval(from, to)

  message(
    "Time interval: ",
    round(lubridate::time_length(time_interval, "days")),
    " days"
  )

  station_id_list <- get_stations()

  station_ids <- station_id_list$SiteNo

  fetch_station <- function(site_id) {
    tryCatch(
      get_daily_one_station(
        site_id = site_id,
        from_date = from_date,
        to_date = to_date
      ),
      error = function(error) {
        if (on_error == "stop") {
          stop(error)
        }

        if (on_error == "warn") {
          warning(conditionMessage(error), call. = FALSE)
        }

        tibble::tibble()
      }
    )
  }

  datalist <- purrr::map(station_ids, fetch_station)

  dplyr::bind_rows(datalist)
}

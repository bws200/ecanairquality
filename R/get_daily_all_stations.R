#' Retrieve daily air quality data from the ECan website
#'
#' Retrieves daily averaged air quality data for all monitoring sites
#' available via the Environment Canterbury (ECan) data portal.
#'
#' @param from_date Character. Start date in `"dd/mm/yyyy"` format.
#' @param to_date Character. End date in `"dd/mm/yyyy"` format.
#' @param on_error How to handle a station request failure: `"ignore"` (the default)
#'   continues silently`,"warn"` continues with other stations and displays a warning
#'   message, `"stop"` aborts the request
#'
#' @return A data frame containing daily air quality measurements in
#' long format.
#'
#' @export
get_daily_all_stations <- function(
    from_date = "1/01/2026", to_date = "31/01/2026", on_error = c("ignore","stop","warn")
    ) {

  on_error <- match.arg(on_error)

  # Integrated format check (fixed to stop when invalid)
  if (any(!check_date_format(c(from_date, to_date)))) {
    stop("Dates must be in 'dd/mm/yyyy' format.", call. = FALSE)
  }

  # Integrated chronology check
  if (!check_date_chronology(from_date, to_date)) {
    stop("from_date must be earlier than to_date.", call. = FALSE)
  }

  station_id_list <- get_stations()

  station_ids <- station_id_list$site_no

  fetch_station <- function(site_no) {
    tryCatch(
      get_daily_one_station(
        site_no = site_no,
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

#' Retrieve air quality station list from Environment Canterbury
#'
#' Downloads a table of air quality monitoring stations from the
#' Environment Canterbury data portal, including the latest observation date.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{SiteNo}{Station ID}
#'   \item{StationName}{Station name}
#'   \item{LatestDateTime}{Most recent observation timestamp (POSIXct)}
#' }
#' @export
#' @examples
#' \dontrun{
#' get_stations()
#' }
get_stations <- function() {
  LatestDateTime <- NULL
  response_stations <- httr::GET(
    "https://data.ecan.govt.nz/data/23/Air/Air%20quality%20sites%20monitored/CSV"
  )

  ecan_stop_for_status(response_stations, "Station metadata request")

  station_data <- httr::content(response_stations, encoding = "UTF-8") |>
    tibble::as_tibble()

  required_columns <- c("SiteNo", "StationName", "LatestDateTime")
  missing_columns <- setdiff(required_columns, names(station_data))

  if (length(missing_columns) > 0) {
    stop(
      "Station metadata response is missing columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  parsed_dates <- lubridate::dmy_hms(station_data$LatestDateTime, quiet = TRUE)
  supplied_dates <- !is.na(station_data$LatestDateTime) &
    nzchar(trimws(as.character(station_data$LatestDateTime)))

  if (any(supplied_dates & is.na(parsed_dates))) {
    stop(
      "Station metadata response contains invalid LatestDateTime values.",
      call. = FALSE
    )
  }

  station_data |>
    dplyr::mutate(LatestDateTime = parsed_dates)
}

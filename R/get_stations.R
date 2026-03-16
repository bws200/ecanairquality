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
    "http://data.ecan.govt.nz/data/23/Air/Air%20quality%20sites%20monitored/CSV"
  )

  httr::stop_for_status(response_stations)

  httr::content(response_stations, encoding = "UTF-8") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      LatestDateTime = lubridate::dmy_hms(LatestDateTime)
    )
}

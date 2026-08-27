#' Retrieve air quality station list from Environment Canterbury
#'
#' Downloads a table of air quality monitoring stations from the
#' Environment Canterbury data portal, including the latest observation date.
#'
#' @param request_timeout Numeric. Maximum time in seconds to wait for the server
#'   response before aborting. Default is `10`.
#'
#' @return A tibble with the following columns:
#'  \describe{
#'   \item{site_no}{Numeric. Unique station identifier}
#'   \item{site_name}{Short name of the station}
#'   \item{long_name}{Full name of the station}
#'   \item{city}{The city or town where the station is located}
#'   \item{air_shed}{The regional airshed classification}
#'   \item{latest_date_time}{The date and time of the most recent recorded observation}
#'  }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_stations()
#' }
get_stations <- function(request_timeout = 10) {

  url  <- "https://data.ecan.govt.nz/data/23/Air/Air%20quality%20sites%20monitored/CSV"

  response <- httr::GET(
    url,
    httr::timeout(request_timeout)
    )

  httr::stop_for_status(response)

  dat_raw <- httr::content(response, as = "text", encoding = "UTF-8")

  dat <- readr::read_csv(
    I(dat_raw),
    show_col_types = FALSE,
    lazy = FALSE
  ) |>
    tibble::as_tibble()

  dat |>
    dplyr::mutate(
      LatestDateTime = lubridate::parse_date_time(
        LatestDateTime,
        orders = "dmy_IMS p",
        tz = "Etc/GMT-12"
        )
    ) |>
    janitor::clean_names()
}

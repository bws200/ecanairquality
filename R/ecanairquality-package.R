#' Environment Canterbury Air Quality Data Package
#'
#' This package provides functions to retrieve air quality data from the
#' Environment Canterbury (ECan) data portal. It includes functions to:
#'
#' * List all monitoring stations (`get_stations()`)
#' * Retrieve daily measurements for a single station (`get_daily_one_station()`)
#' * Retrieve daily measurements for all stations (`get_daily_all_stations()`)
#'
#' The package handles date validation, tidy data formatting (long format),
#' and returns data as tibbles suitable for analysis with tidyverse tools.
#'
#' @docType package
#' @name ecanair
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom httr GET content stop_for_status
#' @importFrom lubridate dmy dmy_hms ymd interval time_length today
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace_all
#' @importFrom janitor round_half_up
#' @importFrom purrr map map_dfr
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
## usethis namespace: end
NULL

test_that("get_stations parses station metadata and timestamps", {
  response <- structure(list(), class = "response")
  station_data <- data.frame(
    site_no = c(101, 202),
    station_name = c("Alpha", "Beta"),
    latest_date_time = c("27/08/2026 10:30:00", "26/08/2026 09:15:00")
  )

  result <- testthat::with_mocked_bindings(
    {
      get_stations()
    },
    GET = function(...) response,
    stop_for_status = function(...) invisible(NULL),
    content = function(...) station_data,
    .package = "httr"
  )

  expect_equal(result$site_no, c(101, 202))
  expect_equal(result$station_name, c("Alpha", "Beta"))
  expect_s3_class(result$latest_date_time, "POSIXct")
  expect_equal(
    result$latest_date_time,
    as.POSIXct(
      c("2026-08-27 10:30:00", "2026-08-26 09:15:00"),
      tz = "UTC"
    )
  )
})

test_that("get_stations normalizes the ECan site_name column", {
  response <- structure(list(), class = "response")
  station_data <- data.frame(
    site_no = 101,
    site_name = "Alpha",
    latest_date_time = "27/08/2026 10:30:00"
  )

  result <- testthat::with_mocked_bindings(
    {
      get_stations()
    },
    GET = function(...) response,
    stop_for_status = function(...) invisible(NULL),
    content = function(...) station_data,
    .package = "httr"
  )

  expect_equal(result$station_name, "Alpha")
  expect_false("site_name" %in% names(result))
})

test_that("get_daily_one_station reshapes and rounds CSV data", {
  response <- structure(list(), class = "response")
  csv_data <- paste(
    "DateTime,Station.Name,PM10,Temperature.2m..DegC.",
    "2026-08-27,Alpha,12.35,8.86",
    "2026-08-28,Alpha,4.44,9.04",
    sep = "\n"
  )

  result <- testthat::with_mocked_bindings(
    {
      get_daily_one_station(
        site_no = 101,
        from_date = "27/08/2026",
        to_date = "28/08/2026"
      )
    },
    GET = function(...) response,
    stop_for_status = function(...) invisible(NULL),
    content = function(...) csv_data,
    .package = "httr"
  )

  expect_equal(nrow(result), 4)
  expect_equal(
    result[, c("DateTime", "station_name", "name", "value")],
    tibble::tibble(
      DateTime = as.Date(c("2026-08-27", "2026-08-27", "2026-08-28", "2026-08-28")),
      station_name = rep("Alpha", 4),
      name = rep(c("PM10", "Temperature2mDegC"), 2),
      value = c(12.4, 8.9, 4.4, 9.0)
    )
  )
})

test_that("get_daily_all_stations combines each station result", {
  station_data <- tibble::tibble(site_no = c(101, 202))
  station_result <- function(site_no, from_date, to_date) {
    tibble::tibble(
      site_no = site_no,
      DateTime = as.Date("2026-08-27"),
      value = site_no / 10
    )
  }

  result <- testthat::with_mocked_bindings(
    {
      get_daily_all_stations(
        from_date = "27/08/2026",
        to_date = "27/08/2026"
      )
    },
    get_stations = function() station_data,
    get_daily_one_station = station_result,
    .package = "ecanairquality"
  )

  expect_equal(result$site_no, c(101, 202))
  expect_equal(result$value, c(10.1, 20.2))
})

test_that("get_stations parses station metadata and timestamps", {
  response <- structure(list(), class = "response")
  station_data <- data.frame(
    SiteNo = c(101, 202),
    StationName = c("Alpha", "Beta"),
    LatestDateTime = c("27/08/2026 10:30:00", "26/08/2026 09:15:00")
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

  expect_equal(result$SiteNo, c(101, 202))
  expect_equal(result$StationName, c("Alpha", "Beta"))
  expect_s3_class(result$LatestDateTime, "POSIXct")
  expect_equal(
    result$LatestDateTime,
    as.POSIXct(
      c("2026-08-27 10:30:00", "2026-08-26 09:15:00"),
      tz = "UTC"
    )
  )
})

test_that("get_stations normalizes the ECan SiteName column", {
  response <- structure(list(), class = "response")
  station_data <- data.frame(
    SiteNo = 101,
    SiteName = "Alpha",
    LatestDateTime = "27/08/2026 10:30:00"
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

  expect_equal(result$StationName, "Alpha")
  expect_false("SiteName" %in% names(result))
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
        site_id = 101,
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
    result[, c("DateTime", "StationName", "name", "value")],
    tibble::tibble(
      DateTime = as.Date(c("2026-08-27", "2026-08-27", "2026-08-28", "2026-08-28")),
      StationName = rep("Alpha", 4),
      name = rep(c("PM10", "Temperature2mDegC"), 2),
      value = c(12.4, 8.9, 4.4, 9.0)
    )
  )
})

test_that("get_daily_all_stations combines each station result", {
  station_data <- tibble::tibble(SiteNo = c(101, 202))
  station_result <- function(site_id, from_date, to_date) {
    tibble::tibble(
      SiteNo = site_id,
      DateTime = as.Date("2026-08-27"),
      value = site_id / 10
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

  expect_equal(result$SiteNo, c(101, 202))
  expect_equal(result$value, c(10.1, 20.2))
})

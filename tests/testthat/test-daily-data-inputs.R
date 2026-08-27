test_that("get_daily_one_station rejects invalid dates", {
  expect_error(
    get_daily_one_station(
      site_id = 1,
      from_date = "2025-01-01",
      to_date = "02/01/2025"
    ),
    "dd/mm/yyyy"
  )
})

test_that("get_daily_all_stations rejects a reversed date range", {
  expect_error(
    get_daily_all_stations(
      from_date = "02/01/2025",
      to_date = "01/01/2025"
    ),
    "from_date must be earlier than to_date"
  )
})

test_that("get_daily_one_station rejects a reversed date range", {
  expect_error(
    get_daily_one_station(
      site_id = 1,
      from_date = "02/01/2025",
      to_date = "01/01/2025"
    ),
    "from_date must be earlier than to_date"
  )
})

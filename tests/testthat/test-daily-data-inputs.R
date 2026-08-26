test_that("get_daily_one_station returns an empty tibble for invalid dates", {
  result <- get_daily_one_station(
    site_id = 1,
    from_date = "2025-01-01",
    to_date = "02/01/2025"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
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

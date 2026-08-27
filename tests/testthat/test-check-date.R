test_that("check_date_format accepts valid dd/mm/yyyy dates", {
  expect_true(check_date_format("01/01/2025"))
  expect_true(check_date_format("29/02/2024"))
})

test_that("check_date_format rejects invalid dates and formats", {
  expect_false(check_date_format("31/02/2025"))
  expect_false(check_date_format("2025-01-01"))
  expect_false(check_date_format(NA_character_))
  expect_false(check_date_format(20250101))
})
1
test_that("check_date_chronology compares date ranges", {
  expect_true(check_date_chronology("01/01/2025", "01/01/2025"))
  expect_true(check_date_chronology("01/01/2025", "05/01/2025"))
  expect_false(check_date_chronology("10/01/2025", "01/01/2025"))
})

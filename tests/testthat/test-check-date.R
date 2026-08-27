test_that("check_date accepts valid dd/mm/yyyy dates", {
  expect_true(check_date("01/01/2025"))
  expect_true(check_date("29/02/2024"))
})

test_that("check_date rejects invalid dates and formats", {
  expect_false(check_date("31/02/2025"))
  expect_false(check_date("2025-01-01"))
  expect_false(check_date(NA_character_))
  expect_false(check_date(20250101))
})

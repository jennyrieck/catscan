test_that("expected FY format returned", {
  expect_equal(gc_fiscal_year("2018-01-15", "short"), "17-18")
  expect_equal(gc_fiscal_year("1987-08-05", "short"), "87-88")
  expect_equal(gc_fiscal_year("2020-03-31", "long"), "2019-2020")
  expect_equal(gc_fiscal_year("2025-04-01", "long"), "2025-2026")
})

test_that("multiple dates can be processed at once", {
  expect_equal(gc_fiscal_year(c("2026-01-01", "2026-04-20"),"short"),
    c("25-26", "26-27"))
})

test_that("invalid date throws an error", {
  expect_error(gc_fiscal_year("2026-01-99"))
  expect_error(gc_fiscal_year(NA))
})

test_that("invalid fy format throws an error", {
  expect_error(gc_fiscal_year("2026-01-01", "short_format"))
})

test_that("output is character", {
  expect_type(gc_fiscal_year("2026-01-01"), "character")
})

test_that("output length matches input length", {
  dates <- as.Date(c("2025-01-01","2025-06-01", "2026-01-01"))
  expect_length(gc_fiscal_year(dates),  length(dates) )
})

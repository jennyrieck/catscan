test_that("wb_style returns a list", {
  expect_type(wb_style(), "list")
})

test_that("all expected style objects are present", {
  expect_named(
    wb_style(),
    c("Title", "Subtitle",  "CentreTable", "WrapContent",
      "WrapContentCentre","TimeStampCentre","DateCentre") )
})

test_that("no styles are NULL", {  styles <-
  expect_false(any(vapply(wb_style(), is.null, logical(1))) )
})

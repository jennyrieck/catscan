test_that("summary tab can be created", {

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"Search Parameters")

  params <- list(
    title = "Test Report",
    start_date = "2026-01-01",
    end_date = "2026-12-31",
    search_terms = data.frame(
      Search_Terms = "cat"
    ),
    search_fields = data.frame(Search_Fields = "note"))

  expect_no_error(
    wb_search_params(
      wb,
      wb_style(),
      params,
      1))
})

test_that("missing parameters throw error", {

  wb <- openxlsx::createWorkbook()

  params <- list(title = "Test")

  expect_error(
    wb_search_params(
      wb,
      wb_style(),
      params,
      1
    ),
    "Missing required params")
})

test_that("invalid dates throw error", {

  wb <- openxlsx::createWorkbook()

  params <- list(
    title = "Test",
    start_date = "2026-12-31",
    end_date = "today",
    search_terms = data.frame(Search_Terms = "cat"),
    search_fields = data.frame(Search_Fields = "note")
  )

  expect_error(
    wb_search_params(
      wb,
      wb_style(),
      params,
      1
    ),
    "valid dates"
  )
})


test_that("report title is written correctly", {

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"Search Parameters")

  params <- list(
    title = "My Cat Report",
    start_date = "2026-01-01",
    end_date = "2026-12-31",
    search_terms = data.frame(
      Search_Terms = "cat"
    ),
    search_fields = data.frame(
      Search_Fields = "note"
    )
  )

  wb_search_params(
    wb,
    wb_style(),
    params,
    1
  )

  tf <- tempfile(fileext = ".xlsx")

  openxlsx::saveWorkbook(wb, tf, overwrite = TRUE)
  check <- openxlsx::read.xlsx(tf, sheet = 1, colNames = FALSE)
  expect_equal(check[1,1],"My Cat Report")
})

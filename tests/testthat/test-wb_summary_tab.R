test_that("summary tab can be created", {

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"Results")

  expect_no_error(
    wb_summary_tab(
      wb = wb,
      wb_style = wb_style(),
      summary_tab = mtcars,
      sheet_num = 1,
      cols_to_wrap = character(0),
      cols_to_widen = character(0)
    )
  )
})

test_that("summary_tab must be dataframe", {

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"Results")

  expect_error(
    wb_summary_tab(
      wb,
      wb_style(),
      summary_tab = "not a dataframe",
      sheet_num = 1,
      cols_to_wrap = character(0),
      cols_to_widen = character(0)
    ),
    "data frame"
  )
})

test_that("empty dataframes are handled", {
  df <- data.frame(id = integer(),note = character())
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet( wb, "Results" )

  expect_no_error(
    wb_summary_tab(
      wb,
      wb_style(),
      df,
      sheet_num = 1,
      cols_to_wrap = "note",
      cols_to_widen = "note"
    )
  )
})

test_that("Date columns are handled", {

  df <- data.frame(
    id = 1:3,
    received_date = as.Date(
      c(
        "2026-01-01",
        "2026-01-02",
        "2026-01-03"
      )
    )
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet( wb, "Results" )

  expect_no_error(
    wb_summary_tab(
      wb,
      wb_style(),
      df,
      sheet_num = 1,
      cols_to_wrap = character(0),
      cols_to_widen = character(0)
    )
  )
})

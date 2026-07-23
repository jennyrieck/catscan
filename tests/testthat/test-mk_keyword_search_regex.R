test_that("returns dataframe with expected columns", {
  result <- mk_keyword_search_regex("Pixel")
  expect_s3_class(result, "data.frame")
  expect_named( result, c("KEYWORD", "KEYCODE") )
})

test_that("KEYWORDS must be character", {
  expect_error(mk_keyword_search_regex(1:5), "character vector")
  expect_error(mk_keyword_search_regex(c("Pixel", NA)), "cannot contain NA")
})

test_that("empty keyword vector throws an error", {
  expect_error(mk_keyword_search_regex(character(0)), "at least one keyword")
})

test_that("logical arguments are logical", {
  expect_error( mk_keyword_search_regex("Pixel", STANDALONE = "yes") )
  expect_error( mk_keyword_search_regex("Pixel", IGNORE_CASE = 1) )
  expect_error( mk_keyword_search_regex("Pixel", MAKE_PLURAL = "TRUE") )

})

test_that("regex returned as expected", {
  result_1 <- mk_keyword_search_regex("Pixel")
  expect_equal(result_1$KEYCODE, "(?i)\\bPixel\\b" )
  result_2 <- mk_keyword_search_regex("Pixel", STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = TRUE)
  expect_equal(result_2$KEYCODE, "(?i)\\bPixels?\\b" )
  result_3 <- mk_keyword_search_regex("Pixel", STANDALONE = FALSE, IGNORE_CASE = FALSE, MAKE_PLURAL = FALSE)
  expect_equal(result_3$KEYCODE, "Pixel" )

})

test_that("multiple keywords are handled", {
  result <- mk_keyword_search_regex(c("Pixel", "Oreo", "Mr Jones"))
  expect_equal(nrow(result), 3)
  expect_equal(result$KEYWORD, c("Pixel", "Oreo", "Mr Jones"))
})

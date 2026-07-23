# dummy data ----
test_data <- data.frame(
  id = 1:4, note = c(
    "Pixel was seen in the yard",
    "Mr Jones was sleeping",
    "The cats meowed loudly",
    "No animals observed"
  )
)

test_keywords <- data.frame(
  KEYWORD = c("Pixel", "Mr Jones", "meow"),
  KEYCODE = c("(?i)\\bPixel\\b",
              "(?i)\\bMr Jones\\b",
              "(?i)meows?|meowed")
)

# tests ----
test_that("returns a dataframe", {
  result <- keyword_search( test_keywords,
                            test_data,
                            "note")

  expect_s3_class(result, "data.frame")
})

test_that("finds matching records", {
  result <- keyword_search(
    test_keywords,
    test_data,
    "note"
  )
  expect_true(all(c("Pixel", "Mr Jones", "meow") %in% result$MATCH))
})

test_that("adds MATCH column", {
  result <- keyword_search(
    test_keywords,
    test_data,
    "note"
  )

  expect_true("MATCH" %in% names(result))
})

test_that("returns zero rows when no matches found", {
  result <- keyword_search(
    data.frame(KEYWORD = "Dog", KEYCODE = "(?i)dog" ),
    test_data,
    "note")

  expect_equal(nrow(result),  0)
})

test_that("records appear multiple times when multiple keywords match", {

  dat <- data.frame(
    note = "Pixel meowed"
  )

  kws <- data.frame(
    KEYWORD = c("Pixel", "meow"),
    KEYCODE = c(
      "(?i)Pixel",
      "(?i)meow"
    )
  )

  result <- keyword_search(
    data.frame( KEYWORD = c("Pixel", "meow"),
                KEYCODE = c(
                  "(?i)Pixel",
                  "(?i)meow") ),
    data.frame(note = "Pixel meowed"),
    "note")

  expect_equal( nrow(result),  2)
})

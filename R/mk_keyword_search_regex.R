#' Turn search terms into regex
#'
#' Takes a character vector and converts it to a dataframe containing
#' regex patterns suitable for use with keyword searches.
#'
#' Creates a dataframe compatible for use with [keyword_search()]
#'
#' @param KEYWORDS Character vector of search terms.
#' @param STANDALONE Logical. Should word boundaries be added? default = TRUE
#' @param IGNORE_CASE Logical. Should matching be case-insensitive? default = TRUE
#' @param MAKE_PLURAL Logical. Should an optional trailing "s" be added? default = FALSE
#' @examples
#' KEYWORDS_DF <- mk_keyword_search_regex(KEYWORDS = c("Pixel", "Beach(es)?"),
#'                                 STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = FALSE)
#'
#' @returns A data frame with columns KEYWORD and KEYCODE (regex)
#' @export
#'

mk_keyword_search_regex <- function(KEYWORDS, STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = FALSE) {

  # Input validation ----
  if (!is.character(KEYWORDS)) {
    stop("`KEYWORDS` must be a character vector.", call. = FALSE)
  }
  if (length(KEYWORDS) == 0) {
    stop("`KEYWORDS` must contain at least one keyword.", call. = FALSE)
  }
  if (any(is.na(KEYWORDS))) {
    stop("`KEYWORDS` cannot contain NA values.", call. = FALSE)
  }
  if (!all(c(is.logical(STANDALONE), is.logical(IGNORE_CASE), is.logical(MAKE_PLURAL)))) {
    stop("`STANDALONE`, `IGNORE_CASE`, and `MAKE_PLURAL` must be logical.", call. = FALSE)
  }
  if (length(STANDALONE) != 1 ||
      length(IGNORE_CASE) != 1 ||
      length(MAKE_PLURAL) != 1) {
    stop("`STANDALONE`, `IGNORE_CASE`, and `MAKE_PLURAL` must be length 1.", call. = FALSE)
  }

  # Build regex ----
  keycode <- KEYWORDS

  if (MAKE_PLURAL) {
    keycode <- paste0(keycode, "s?")
  }
  if (STANDALONE) {
    keycode <- paste0("\\b", keycode, "\\b")
  }
  if (IGNORE_CASE) {
    keycode <- paste0("(?i)", keycode)
  }
  KEYWORDS_DF <- data.frame(
    KEYWORD = KEYWORDS,
    KEYCODE = keycode
  )
  return(KEYWORDS_DF)
}

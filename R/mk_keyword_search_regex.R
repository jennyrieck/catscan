#' Turn search terms into regex
#'
#' Takes a string vector and converts it to a dataframe with regex formatting
#' to handle word boundaries, case sensitivity, and basic pluralization.
#' Creates a dataframe compatible for use with [keyword_search()]
#'
#' @param KEYWORDS a string vector of search terms
#' @param STANDALONE boolean; standalone word/word boundaries? default = TRUE
#' @param IGNORE_CASE boolean; ignore upper/lower case? default = TRUE
#' @param MAKE_PLURAL boolean; make keyword plural by adding "s" to the end? default = FALSE
#' @examples
#' KEYWORDS_DF <- mk_keyword_search_regex(c("Pixel", "Beach(es)?"),
#'                                 STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = FALSE)
#'
#' @returns dataframe of keywords and the corresponding regex keycodes
#' @export
mk_keyword_search_regex <- function(KEYWORDS, STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = FALSE){

  # Initialize an empty df to store the regex patterns
  KEYWORDS_DF <- data.frame(KEYWORD = KEYWORDS, KEYCODE = NA_character_)

  # Loop through each keyword and generate the regex pattern
  for (i in 1:dim(KEYWORDS_DF)[1]) {

    keycode <- KEYWORDS[i]

    # Make keyword plural by adding an "s" to the end
    if (MAKE_PLURAL) {
      keycode <- paste0(keycode, "s?")
    }

    # Add word boundaries if STANDALONE is TRUE
    if (STANDALONE) {
      keycode <- paste0("\\b", keycode, "\\b")
    }

    # Add case-insensitive flag if IGNORE_CASE is TRUE
    if (IGNORE_CASE) {
      keycode <- paste0("(?i)", keycode)
    }

    # Store the generated regex pattern
    KEYWORDS_DF$KEYCODE[i] <- keycode
  }

  return(KEYWORDS_DF)
}


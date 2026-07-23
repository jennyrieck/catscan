#' Keycode search and filter
#'
#' Conduct a keycode search of a particular column of an input dataframe and
#' return filters that dataframe filtered for keycode hits.
#' Expects KEYCODE to be regex formatted strings
#'
#' @param KEYWORDS_DF dataframe with columns KEYWORD and KEYCODE, generated from [mk_keyword_search_regex()]
#' @param DATA dataframe that includes a column with content to search
#' @param SEARCH_COLUMN string; name of DATA column in which to conduct the search
#' @examples
#' data(cat_observations)
#' Search_Terms <- data.frame(
#'     KEYWORD = c("Mr Jones", "meow", "yowl"),
#'     KEYCODE = c("(?i)\\b(Mister|Mr\\.?) Jones?\\b", "(?i)meow", "(?i)yowl")
#' )
#' search_results <- keyword_search(
#'   KEYWORDS_DF = Search_Terms,
#'   DATA = cat_observations,
#'   SEARCH_COLUMN = "observation_note")
#'
#' @return returns DATA dataframe filtered for only those rows with keycode hits
#' @importFrom stringr str_detect
#' @importFrom purrr pmap_dfr
#' @importFrom dplyr filter mutate sym
#' @export

keyword_search <- function(KEYWORDS_DF, DATA, SEARCH_COLUMN) {

  # Validate KEYWORDS_DF ----
  if (!is.data.frame(KEYWORDS_DF)) {
    stop("`KEYWORDS_DF` must be a data frame.", call. = FALSE)
  }
  required_cols <- c("KEYWORD", "KEYCODE")
  if (!all(required_cols %in% names(KEYWORDS_DF))) {
    stop("`KEYWORDS_DF` must contain columns KEYWORD and KEYCODE.",  call. = FALSE)
  }
  if (any(is.na(KEYWORDS_DF$KEYWORD))) {
    stop("`KEYWORD` contains missing values.", call. = FALSE)
  }
  if (any(is.na(KEYWORDS_DF$KEYCODE))) {
    stop("`KEYCODE` contains missing values.", call. = FALSE)
  }

  # Validate DATA ----
  if (!is.data.frame(DATA)) {
    stop("`DATA` must be a data frame.", call. = FALSE)
  }

  # Validate SEARCH_COLUMN ----
  if (!is.character(SEARCH_COLUMN) ||
      length(SEARCH_COLUMN) != 1) {
    stop("`SEARCH_COLUMN` must be a single character string.",  call. = FALSE)
  }
  if (!SEARCH_COLUMN %in% names(DATA)) {
    stop(paste0("`", SEARCH_COLUMN, "` not found in DATA." ), call. = FALSE)
  }

  pmap_dfr(KEYWORDS_DF, function(KEYWORD, KEYCODE) {
    DATA |>
      filter(str_detect(!!sym(SEARCH_COLUMN), KEYCODE)) |>
      mutate(MATCH = KEYWORD)
  })
}

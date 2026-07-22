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
#'     KEYCODE = c("(?i)\\b(Mister|Mr\.?) Jones?\\b", "(?i)meows?", "(?i)yowls?")
#' )
#' RADAR_search_results <- keyword_search(
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

  pmap_dfr(KEYWORDS_DF, function(KEYWORD, KEYCODE) {
    DATA |>
      filter(str_detect(!!sym(SEARCH_COLUMN), KEYCODE)) |>
      mutate(MATCH = KEYWORD)
  })
}

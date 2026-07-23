library(catscan)
library(dplyr)
library(tidyr)
data("cat_observations")

Search_Terms <- data.frame(
  KEYWORD = c("Mr Jones", "meow", "yowl"),
  KEYCODE = c("(?i)\\b(Mister|Mr\\.?) Jones?\\b", "(?i)meow", "(?i)yowl")
)

Search_Terms_2 <-mk_keyword_search_regex(KEYWORDS = c("Pixel", "Beach(es)?"),
                                         STANDALONE = TRUE, IGNORE_CASE = TRUE, MAKE_PLURAL = FALSE)

search_results <- keyword_search(
  KEYWORDS_DF = Search_Terms,
  DATA = cat_observations,
  SEARCH_COLUMN = "observation_note")

search_results_aggregated <- search_results |>
  group_by(note_id) |>
  mutate(ALL_HITS = paste0(unique(MATCH), collapse = " | ")) |>
  ungroup() |>
  select(-MATCH) |>
  distinct() |>
  mutate(fy_received = gc_fiscal_year(date_received)) |>
  arrange(incident_id)


search_params <- list(
  title = "Cat Search - Mr Jones",
  start_date = "2026-01-01",
  end_date = Sys.Date(),
  search_terms = data.frame(Search_Terms = Search_Terms$KEYWORD),
  search_fields = data.frame(Search_Fields = "observation_note")
)

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Search Parameters", gridLines = FALSE)
addWorksheet(wb, sheetName = "Search Results", gridLines = FALSE)
wb_search_params(wb = wb, wb_style = wb_style(), params = search_params, sheet_num = 1)

to_wrap <- c("observation_note")
to_widen <- c("cat_name", "observation_note", "ALL_HITS")

wb_summary_tab(wb = wb,
               wb_style = wb_style(),
               summary_tab = search_results_aggregated,
               sheet_num = 2,
               cols_to_wrap = to_wrap,
               cols_to_widen = to_widen)

saveWorkbook(wb, paste0(search_params$title, "_" , Sys.Date(), ".xlsx"), overwrite = TRUE)

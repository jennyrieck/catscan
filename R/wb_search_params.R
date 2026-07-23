#' Create a search parameter tab in an openxlsx workbook
#'
#' Function to create a standardized search parameter tab that outlines
#' time range, fields, and search terms used in report. Use with [wb_style()]
#' and [wb_summary_tab()]
#'
#' @param wb openxlsx wb object
#' @param wb_style openxlsx wb style list generated from [wb_style()]
#' @param params list of search parameters including: title, start_date,
#' end_date, search_terms, and search_fields (see example)
#' @param sheet_num the sheet number to output the search parameters
#' @returns openxlsx wb object with a search parameter sheet added
#' @examples
#'  \dontrun{
#' search_params <- list(
#'   title = "Cat Search - Mr Jones",
#'   start_date = "2026-01-01",
#'   end_date = Sys.Date(),
#'   search_terms = data.frame(Search_Terms = c("Mr Jones", "meow", "yowl")),
#'   search_fields = data.frame(Search_Fields = "observation_note")
#' )
#'
#' library(openxlsx)
#' wb <- createWorkbook()
#' addWorksheet(wb, sheetName = "Search Parameters", gridLines = FALSE)
#' wb_search_params(wb = wb, wb_style = wb_style(), params = search_params, sheet_num = 1)
#' saveWorkbook(wb, paste0(search_params$title, "_" , Sys.Date(), ".xlsx"), overwrite = TRUE)
#' }
#' @importFrom openxlsx writeData writeDataTable addStyle setColWidths
#' @export

wb_search_params <- function(wb, wb_style, params, sheet_num = 1){

  # Validate input ----
  if(!inherits(wb, "Workbook")){
    stop( "`wb` must be class Workbook generated with `openxlsx::createWorkbook()`", call. = FALSE)
  }
  required_params <- c("title","start_date","end_date", "search_terms", "search_fields")
  missing_params <- setdiff(required_params, names(params))
  if(length(missing_params) > 0){
    stop( "Missing required params: ",paste(missing_params, collapse = ", "), call. = FALSE)
  }

  # Validate dates ----
  start_date <- tryCatch(
    as.Date(params$start_date),
    error = function(e) NA
  )
  end_date <- tryCatch(
    as.Date(params$end_date),
    error = function(e) NA
  )

  if (is.na(start_date) || is.na(end_date)) {
    stop("`start_date` and `end_date` must be valid dates.", call. = FALSE)
  }

  # Write the parameter tab ----
  openxlsx::writeData(wb, sheet_num, x = params$title, startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_num, x = "PROTECTED B", startCol = 8, startRow = 1)
  openxlsx::writeData(wb, sheet_num, x = "Search Parameters", startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sheet_num, style = wb_style$Title, rows = 1, cols = c(1,8))
  openxlsx::addStyle(wb, sheet_num, style = wb_style$Subtitle, rows = 2, cols = 1)
  openxlsx::writeData(wb, sheet_num, x = paste("Data as of:", format(Sys.time(), "%A, %B %d, %Y %I:%M%p")), startCol = 3, startRow = 1)
  openxlsx::writeData(wb, sheet_num, x = paste("Received From:", format(as.Date(params$start_date), "%d-%b-%y"), "To:", format(as.Date(params$end_date), "%d-%b-%y")), startCol = 3, startRow = 2)
  openxlsx::writeDataTable(wb, sheet_num, params$search_terms, startCol = 1, startRow = 4, tableStyle = "TableStyleMedium2")
  openxlsx::writeDataTable(wb, sheet_num, params$search_fields, startCol = 3, startRow = 4, tableStyle = "TableStyleMedium2")
  openxlsx::setColWidths(wb, sheet_num, cols = 1:8, widths = c("auto", 14, "auto", "auto", "auto", "auto", "auto", 80))
}

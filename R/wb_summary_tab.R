#' Create a search results summary tab in an openxlsx workbook
#'
#' Function to create a standardized search results table. Use
#' with [wb_style()] and [wb_search_params()]
#'
#' @param wb openxlsx wb object
#' @param wb_style openxlsx wb style list generated from [wb_style()]
#' @param summary_tab dataframe of the search results
#' @param cols_to_wrap vector of column names to be wrapped
#' @param cols_to_widen vector of column names to be widened
#' @param sheet_num the sheet number to output the results table
#' @returns openxlsx wb object with a search summary sheet added
#' @examples
#'  \dontrun{
#' data(cat_observations)

#' cols_to_wrap = c("observation_note")
#' cols_to_widen = c("cat_name", "neighborhood", "observation_note")
#'
#' library(openxlsx)
#' wb <- createWorkbook()
#' addWorksheet(wb, sheetName = "Search Results", gridLines = FALSE)
#' wb_summary_tab(wb = wb, wb_style = wb_style(),
#'   summary_tab = cat_observations, sheet_num = 1,
#'   cols_to_wrap = cols_to_wrap,
#'   cols_to_widen = cols_to_widen)
#' saveWorkbook(wb, paste0("Cat Search_" ,Sys.Date(), ".xlsx"), overwrite = TRUE)
#' }
#' @importFrom openxlsx writeDataTable addStyle setColWidths
#' @export


wb_summary_tab <- function(wb, wb_style, summary_tab, sheet_num, cols_to_wrap, cols_to_widen){

  # Validate input ----
  if(class(wb) != "Workbook"){
    stop( "`wb` must be class Workbook generated with `openxlsx::createWorkbook()`", call. = FALSE)
  }
  if (!is.data.frame(summary_tab)) {
    stop("`summary_tab` must be a data frame.", call. = FALSE)
  }

  required_styles <- c("CentreTable", "WrapContent","DateCentre")
  missing_styles <- setdiff(required_styles, names(wb_style))
  if(length(missing_styles) > 0){
    stop("Missing styles: ",paste(missing_styles, collapse = ", "), call. = FALSE)
  }

  openxlsx::writeDataTable(wb, sheet_num, summary_tab, startCol = 1, startRow = 1, tableStyle = "TableStyleMedium2")

  date_cols <- which(
    vapply(
      summary_tab, function(x) inherits(x, c("Date", "POSIXct")),  logical(1)
    ))
  wrap_cols <- which(names(summary_tab) %in% cols_to_wrap,
                     useNames = FALSE)
  wider_cols <- sort(which(names(summary_tab) %in% cols_to_widen,
                           useNames = FALSE))
  reg_width_cols <- sort(which(!names(summary_tab) %in% cols_to_widen,
                               useNames = FALSE))

  openxlsx::addStyle(wb, sheet_num, style = wb_style$CentreTable, rows = 1:(nrow(summary_tab) + 1),
                     cols = 1:(ncol(summary_tab)), gridExpand = TRUE)
  if(length(wrap_cols) > 0 ){
    openxlsx::addStyle(wb, sheet_num, style = wb_style$WrapContent, rows = 1:(nrow(summary_tab) + 1),
                       cols = wrap_cols, gridExpand = TRUE)
  }
  if(length(date_cols) > 0 ){
    openxlsx::addStyle(wb, sheet_num, style = wb_style$DateCentre, rows = 1:(nrow(summary_tab) + 1),
                       cols = date_cols, gridExpand = TRUE)
  }
  if(length(wider_cols) > 0){
    openxlsx::setColWidths(wb, sheet_num, cols = wider_cols,
                           widths = rep(45, length(wider_cols)))
  }
  if(length(reg_width_cols) > 0){
    openxlsx::setColWidths(wb, sheet_num, cols = reg_width_cols,
                           widths = rep(20, length(reg_width_cols)))
  }
}

#' openxlsx workbook style helper
#'
#' Function to apply standard formatting to datatables created with openxlsx.
#' Use with [wb_search_params()] and [wb_summary_tab()]
#'
#' @returns list of openxlsx styling
#' @examples
#' wb_style()
#'
#' @importFrom openxlsx createStyle
#' @export

wb_style <- function(){

  Title <- openxlsx::createStyle(fontSize = 16, textDecoration = c("bold"), fontColour = "#0F243E")
  Subtitle <- openxlsx::createStyle(fontSize = 14, textDecoration = c("bold"), fontColour = "#0F243E")
  CentreTable <- openxlsx::createStyle(halign = "center", valign = "top")
  WrapContent <- openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE)
  WrapContentCentre <- openxlsx::createStyle(halign = "center", valign = "top", wrapText = TRUE)
  TimeStampCentre <- openxlsx::createStyle(halign = "center", valign = "top", numFmt = "yyyy-mm-dd hh:mm:ss")
  DateCentre <- openxlsx::createStyle(halign = "center", valign = "top", numFmt = "yyyy-mm-dd")

  return(list = c(Title = Title, Subtitle = Subtitle, CentreTable = CentreTable,
                  WrapContent = WrapContent, TimeStampCentre = TimeStampCentre, DateCentre = DateCentre))
}

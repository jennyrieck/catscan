#' Convert dates to Government of Canada fiscal year
#'
#' @description
#' Government of Canada fiscal years begin on April 1 and end on
#' March 31 of the following year.
#'
#' This function is meant to show best practices for writing an R function.
#' See [fy()] for a counter example.
#'
#' @param date A date vector coercible to Date.
#' @param fy_format long (YYYY-YYYY) or short (YY-YY) format?
#'
#' @return A character vector of fiscal years in the format
#'   "YYYY-YYYY" or "YY-YY"
#'
#' @examples
#' gc_fiscal_year("2026-03-31", fy_format = "long")
#' gc_fiscal_year(c("2026-01-01", "2026-04-20"), fy_format = "short")
#'
#' @importFrom lubridate year month
#'
#' @export
#'
gc_fiscal_year <- function(date, fy_format = c("long", "short")) {

  # Check format type
  fy_format <- match.arg(fy_format)

  # Check if input is proper date format
  date <- as.Date(date)
  if (any(is.na(date))) {
    stop("`date` contains invalid or missing values.", call. = FALSE)
  }

  # Extract Year and Month from date
  date_year <- lubridate::year(date)
  date_month <- lubridate::month(date)

  # Determine starting year based on month
  start_year <- ifelse(date_month >= 4, date_year, date_year - 1)
  end_year <- start_year + 1

  # Format fiscal year output
  if(fy_format == "short"){
    fy_formatted <- paste0(substr(start_year, 3, 4),
                           "-",
                           substr(end_year, 3, 4))
  }else{
    fy_formatted <- paste0(start_year, "-", end_year)
  }

  # Explicitly return value
  return(fy_formatted)
}

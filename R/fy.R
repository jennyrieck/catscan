#' Get the FY
#'
#' This function is meant to serve as an example of a R bad function.
#' It is missing documentation. The function, input parameters and variable names
#' are not descriptive. There is an input parameter named "format" which is
#' also a base R function. There is no validation of the input parameters.
#'
#' See [gc_fiscal_year()] for an improved version of this function
#'
#' @export
#'
fy <- function(date, format) {

  y <- as.integer(format(as.Date(date), "%Y"))
  m <- as.integer(format(as.Date(date), "%m"))

  start <- ifelse(m >= 4, y, y - 1)
  end <- start + 1

  if(format == "short"){
    paste0(substr(start, 3, 4),
           "-",
           substr(end, 3, 4))
  }else{
    paste0(start, "-", end)
  }

}

#' @title Subset method for 'eden' class
#'
#' @description Subset EDEN objects (lists with data and dates). Works only on SpatRaster data (raster data will be converted and returned as SpatRaster).
#' 
#' @param x  EDEN object
#' @param subset  integer, character (in \%Y-\%m-\%d format), or Date. Should indicate the layers (represented as integer or by the corresponding date)
#' 
#' @return a single `eden` object, which is a list with two elements: (1) the date(s) used, and (2) a spatial object with water levels (centimeters relative to soil surface) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' dat1 <- getEDEN(EDEN_date = "2022-01-01", quarterly = TRUE, returnType = 'terra')
#' 
#' ### subset by index
#' dat2 <- subset.eden(x = dat1, subset = 5)
#' 
#' ### subset by date
#' dat3 <- subset.eden(x = dat1, subset = "2022-01-15")
#' 
#' ### subset using date range
#' dateRange <- seq.Date(from = as.Date("2022-01-01"), 
#'                       to = as.Date("2022-01-15"), by = "day")
#' dat4 <- subset.eden(x = dat1, subset = dateRange)
#' 
#' dat1$date
#' dat2$date
#' dat3$date
#' dat4$date
#' 
#' }
#' 
#' @importFrom terra rast
#' @importFrom terra subset
#' 
#' @export


subset.eden <- function(x, subset, ...) {
  
  if (!any(grepl(x = tolower(class(x)), pattern = 'eden|list'))) {
    stop('arguments must be a list of eden objects\n')
  }
  
  if (any(is.numeric(subset))) {
    targetIndex <- subset
  } else {
    targetIndex <- grep(pattern = paste0(subset, collapse = "|"), x =  x$date)
  }
  
  date_new <- x$date[targetIndex]
  
  if (any(grepl(x = tolower(class(x$data)), pattern = '^rasterlayer$|^raster$'))) {
    x$data <- terra::rast(x$data*1)
  }
  if (any(grepl(x = tolower(class(x$data)), pattern = '^spatraster$'))) {
    data_new <- terra::subset(x = x$data*1, subset = targetIndex, ...)
  }
  EDEN_list <- list(date = date_new,
                    data = data_new)
  
  class(EDEN_list) <- c("eden", grep(x = class(EDEN_list), pattern = "eden", invert = TRUE, value = TRUE)) 
  return(EDEN_list)
}

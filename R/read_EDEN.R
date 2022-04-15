#' @title Read EDEN data saved to disk
#'
#' @description Load a saved EDEN object (list with data and dates). Returns an eden obeject with SpatRaster data.
#' 
#' @param x  EDEN object
#' @param filename  character. Input filename with or without a file extension; extension is ignored. two files are saved: (1) a geoTiff with the data, and (2) a text file with the dates. These are combined when loaded using `read.eden()`
#' 
#' @return none
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' dat1 <- getEDEN(EDEN_date = "2022-01-01", quarterly = TRUE, returnType = 'terra')
#' 
#' write.eden(x = dat1, filename = "test_dat1.tif")
#' newdat <- read.eden("test_dat1.tif")
#' 
#' newdat
#' 
#' }
#' 
#' @importFrom terra rast
#' 
#' @rdname eden-methods
#' @export


read.eden <- function(filename = "C:/RDATA/dataTest.tif") {
  ### requires two files in same directory with same name (different type)
  ### check if file extension is provided
  if(grepl(pattern = "\\.", x = filename)) {
    ### if period is present
    filename_base <- strsplit(x = filename, split = '[.]')[[1]][1]
  } else {
    ### if no period present, assume no file extension is provided
    filename_base <- filename_base 
  }
  filename_data <- paste0(filename_base, ".tif")
  filename_date <- paste0(filename_base, ".txt")
  
  eden_dates <- dget(file = filename_date)
  eden_data  <- terra::rast(filename_data)
  EDEN_list  <- list(date = eden_dates, data = eden_data)
  class(EDEN_list) <- c("eden", grep(x = class(EDEN_list), pattern = "eden", invert = TRUE, value = TRUE)) 
  invisible(EDEN_list)
}

a <- read.eden()

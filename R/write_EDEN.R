#' @title Write EDEN data to disk
#'
#' @description Write an EDEN object (list with data and dates) to a file. Works only on SpatRaster data (raster data will be converted to SpatRaster). Two files
#' 
#' @param x  EDEN object
#' @param filename  character. Output filename with or without a file extension; extension is ignored. two files are saved: (1) a geoTiff with the data, and (2) a text file with the dates. These are combined when loaded using `read.eden()`
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
#' @importFrom terra writeRaster
#' 
#' @export


write.eden <- function(x, 
                       filename) {
  ### need to save data and dates. filename will be used to save an tiff (data) and a .txt (dates) 
  ### with the same name
  if(grepl(pattern = "\\.", x = filename)) {
    ### if period is present
    filename_base <- strsplit(x = filename, split = '[.]')[[1]][1]
  } else {
    ### if no period present, assume no file extension is provided
    filename_base <- filename_base 
  }
  filename_data <- paste0(filename_base, ".tif")
  filename_date <- paste0(filename_base, ".txt")
  
  ### convert to SpatRaster if needed
  if (!any(grepl(x = tolower(class(x$data)), pattern = '^spatraster$'))) {
    x$data <- terra::rast(x$data*1)
  }
  
  ### save two files
  dput(x = x$date, file = filename_date)
  terra::writeRaster(x = x$data, filename = filename_data, overwrite = TRUE)
}

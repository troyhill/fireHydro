#' @title Extract values from an EDEN object
#'
#' @description Extract values from the SpatRaster portion of an EDEN object. This is a wrapper for terra::extract().
#' 
#' @param x  EDEN object
#' @param y  SpatVector locations to extract data for
#' @param fun function to apply when extracted data have multiple cells. Untested with FUN = NULL.
#' 
#' @return dataframe
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' dat1 <- getEDEN(EDEN_date = "2022-01-01", quarterly = TRUE, returnType = 'terra')
#' 
#' sfwmd <- terra::vect(st_as_sf(EvergladesEBM::sfwmd))
#' wca1  <- sfwmd[sfwmd$NAME == "CONSERVATION AREA 1",]
#' wca2  <- sfwmd[sfwmd$NAME == "CONSERVATION AREA 2A",]
#' wca3  <- sfwmd[sfwmd$NAME == "CONSERVATION AREA 3A",]
#' 
#' extract.eden(x = dat1, y = wca1 + wca2 + wca3, fun = mean, na.rm = TRUE)
#' 
#' }
#' 
#' @importFrom terra rast
#' @importFrom terra extract
#' 
#' 
#' @export


extract.eden <- function(x, y,
                       fun = NULL, ...) {
  ### convert to SpatRaster if needed
  if (!any(grepl(x = tolower(class(x$data)), pattern = '^spatraster$'))) {
    x$data <- terra::rast(x$data*1)
  }
  
  dat <- terra::extract(x = x$data, y = y, fun = fun, ...)
  invisible(dat)
}

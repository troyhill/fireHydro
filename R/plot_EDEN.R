#' @title Plot EDEN data
#'
#' @description Plot EDEN objects (lists with data and dates)
#' 
#' @param x  EDEN object
#' @param y  NULL; not used
#' 
#' @return  a plot
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' dat1 <- getEDEN(EDEN_date = Sys.Date(), returnType = 'terra')
#' 
#' plot.eden(dat1)
#' 
#' dat2 <- getEDEN(EDEN_date = Sys.Date(), returnType = 'sf')
#' plot.eden(dat2)
#' }
#' 
#' @importFrom terra plot
#' @importFrom terra  subset
#' 
#' @export


plot.eden <- function(x, y = NULL, ...) {
  
  sub <- 1
  if (!any(grepl(x = class(x), pattern = 'eden|list'))) {
    stop('x must be a list of eden objects\n')
  }
  
  
  if (any(grepl(x = tolower(class(x$data)), pattern = 'sf'))) {
    ### quarterly data aren't available as sf objects, so sub isn't used here
    plot(x$data, border = NA, ...) # sf::plot_sf not working?
  }
  if (any(!grepl(x = tolower(class(x$data)), pattern = 'spatraster'))) {
    x$data[[sub]] <- terra::rast(x$data[[sub]])
  }
  if (any(grepl(x = tolower(class(x$data)), pattern = 'spatraster'))) {
    terra::plot(x$data[[sub]], main = x$date[[sub]], axes = FALSE, ...)
  }
}

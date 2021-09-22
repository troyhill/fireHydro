#' Check for EDEN object
#'
#' Checks object to see if it is an EDEN object
#'
#' @param x object to check
#' @export
#' @return logical
#' @examples
#' edenList <- list(date = Sys.Date(), data = raster::raster())
#' is.eden(edenList)
is.eden <- function(x) {
  
  # Added this because there was a situation
  # where people were fiddling with the eList and removing critical bits 
  if(!all(c("date","data") %in% names(x))){
    stop("Missing ", c("date","data")[!(c("date","data") %in% names(x))])
  }
  return(all(c(all(c("date","data") %in% names(x)),
               inherits(x, "eden"))))
}
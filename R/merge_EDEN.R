#' @title Merge EDEN data
#'
#' @description Merge EDEN objects (lists with data and dates)
#' 
#' @param x  list of EDEN objects
#' 
#' @return a single `eden` object, which is a list with two elements: (1) the date(s) used, and (2) a spatial object with water levels (centimeters relative to soil surface) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' dat1 <- getEDEN(EDEN_date = Sys.Date(), quarterly = TRUE, returnType = 'terra')
#' dat2 <- getEDEN(EDEN_date = Sys.Date()-95, quarterly = TRUE, returnType = 'terra')
#' 
#' dat3 <- merge.eden(x = list(dat1, dat2))
#' 
#' dat1$date
#' dat2$date
#' dat3$date
#' 
#' }
#' 
#' @importFrom raster subset
#' @importFrom raster stack
#' @importFrom terra  subset
#' 
#' @export


merge.eden <- function(x) {
  
  if (!any(grepl(x = sapply(x, class), pattern = 'eden|list'))) {
    stop('x must be a list of eden objects\n')
  }
  
  dateVec  <- Reduce(c, sapply(x, FUN = '[', 1))
  ### will be sorting date and data elements by date
  newOrder <- order(dateVec)
  date_new <- dateVec[newOrder]
  
  if (grepl(x = tolower(class(x[[1]]$data)), pattern = 'raster')) {
    data_new <- Reduce(raster::stack, sapply(x, FUN = '[', 2))
    data_new <- raster::subset(x = data_new, subset = newOrder)
  }
  if (grepl(x = tolower(class(x[[1]]$data)), pattern = 'spatraster')) {
    # x2 <- list(list(date = eden2022$date[1:2], data = subset(eden2022$data, 1:2)),
    #            list(date = eden2022$date[3:4], data = subset(eden2022$data, 3:4))
    #            )
    # x2[[1]]$data <- terra::rast(x2[[1]]$data*1)
    # x2[[2]]$data <- terra::rast(x2[[2]]$data*1)
    # c(x2[[1]]$data, x2[[2]]$data )
    data_new <- Reduce(c, sapply(x, FUN = '[', 2))
    data_new <- terra::subset(x = data_new, subset = newOrder)
  }
  EDEN_list <- list(date = date_new,
                    data = data_new)
  class(EDEN_list) <- c("eden", class(EDEN_list)) 
  return(EDEN_list)
}

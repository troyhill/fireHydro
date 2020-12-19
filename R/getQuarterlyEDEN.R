#' @title Downloads quarterly EDEN data (from 1991 through present) 
#'
#' @description This a beta replacement for getOldEDEN, providing access to quarterly data covering the entire period of record. Downloads netCDF files from https://sflthredds.er.usgs.gov/thredds/catalog/eden/surfaces/catalog.html. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param YYYYMMDD EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product.
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded.
#' 
#' @return list \code{getQuarterlyEDEN} returns a list with two elements: (1) the name of the quarter (e.g., "1991_q2"), and (2) an sf object (if quarterly = FALSE) or a rasterStack object with water depths in the EDEN grid for all days in the quarter.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' edenDat <- getQuarterlyEDEN(YYYYMMDD = "19910101", quarterly = TRUE)
#' }
#' 
#' @importFrom raster brick
#' @importFrom raster stack
#' @importFrom zoo    as.yearqtr
#' @importFrom utils  unzip
#' @importFrom utils  download.file
#' @importFrom sf     st_as_sf
#' 
#' @export


getQuarterlyEDEN <- function(YYYYMMDD, 
                       DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro")),
                       quarterly = FALSE) {
  
  YYYYMMDD <- as.character(YYYYMMDD)
  ### create vector of possible urls
  
  ### identify quarterly netCDF to be downloaded
  qtr <- tolower(format(zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")), format = "%Y_Q%q"))

  currentQ <- FALSE  
  ### check if requested date is in current quarter (and thus on the front page of EDEN)
  if (zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")) == zoo::as.yearqtr(as.Date(Sys.Date(), format = "%Y%m%d"))) {
    currentQ <- TRUE  
  }
  
   
  ### assemble vector of URLS and identify the working url
  # url_prep  <- expand.grid(a = baseURL, b = qtr, c = "_", d = urlEnding, e = ".zip")
  # temp_urls <- do.call(paste0, c(url_prep))
  url <- paste0("https://sflthredds.er.usgs.gov/thredds/fileServer/eden/surfaces/", qtr, ".nc")
    
  tmpDir <- tempdir() 
  # temp   <- tempfile(tmpdir = tmpDir, fileext = ".nc")
  
  utils::download.file(url = url, destfile = file.path(tmpDir, paste0(qtr, ".nc")), mode = 'wb')
  # fileName <- utils::unzip(zipfile = temp, exdir = tmpDir, list = TRUE)$Name
  ras      <- raster::brick(x = file.path(tmpDir, paste0(qtr, ".nc")))
  
  if (quarterly == FALSE) {
    ### load raster for specified date 
    targetRas <- ras[[which(gsub(x = ras@z$Date, pattern = "-", replacement = "")  %in% YYYYMMDD)]]
    ### make sure projection matches DEM
    targetRas      <- projectRaster(targetRas, crs=crs(DEM))
    
    targetRas <- targetRas - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    names(targetRas) <- "WaterDepth"     # to match EDEN geoTiffs and getFireHydro hard-coded variables
    rasDate <- as(targetRas, "SpatialPolygonsDataFrame")
    rasDate <- sf::st_as_sf(rasDate)
    
    # rasDate <- sf::st_as_sf(rasDate)
    # a.ras  <- raster::raster(a)
    # a.ras <- a.ras - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    # a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
    #
    # names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
    returnDat <- list(date = YYYYMMDD, data = rasDate)
  } else if (quarterly == TRUE) {
    rasDate <- raster::stack(x = ras) #(x = file.path(tmpDir, paste0(qtr, ".nc")))
    ### make sure projection matches DEM
    rasDate      <- projectRaster(rasDate, crs=crs(DEM))
    
    ### need to subtract DEM*100, convert each layer to SPDF, and sf::st_as_sf
    rasDate  <- rasDate - (DEM*100)
    # rasDate <- as.list(rasDate)
    # polylist2 <- lapply(as.list(rasDate), as("SpatialPolygonsDataFrame"))
    # 
    # rasDate_2 <- sf::st_as_sf(as(rasDate[[1]], "SpatialPolygonsDataFrame"))
    # 
    # rasDate <- as(ras, "SpatialPolygonsDataFrame") # creates a spatialPolygonsDataFrame with a variable for each day
    # rasDate <- sf::st_as_sf(rasDate)
    returnDat <- list(date = qtr, data = rasDate)
  }

  # unlink(x = temp)     # deletes the zipped file
  unlink(x = file.path(tmpDir, paste0(qtr, ".nc"))) # deletes the unzipped file
  
  invisible(returnDat)
  
}

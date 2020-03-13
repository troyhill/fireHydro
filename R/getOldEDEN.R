#' @title Downloads and imports archived EDEN data (from 2000 through the penultimate quarter in the record) 
#'
#' @description Downloads netCDF files from https://sofia.usgs.gov/eden/models/watersurfacemod_download.php, unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param YYYYMMDD EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param baseURL beginning part of url
#' @param urlEnding ending part of url
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product.
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded.
#' 
#' @return list \code{getOldEDEN} returns a list with two elements: (1) the date used, and (2) an sf object with water levels in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' edenDat <- getOldEDEN(YYYYMMDD = "20180101")
#' }
#' 
#' @importFrom httr   GET
#' @importFrom httr   http_status
#' @importFrom raster brick
#' @importFrom raster stack
#' @importFrom zoo    as.yearqtr
#' @importFrom utils  unzip
#' @importFrom utils  download.file
#' @importFrom sf     st_as_sf
#' 
#' @export



getOldEDEN <- function(YYYYMMDD, 
                       baseURL = c("https://sofia.usgs.gov/eden/data/netcdf/v2/", "https://sofia.usgs.gov/eden/data/netcdf/v3/", "https://sofia.usgs.gov/eden/data/realtime2/"),
                       urlEnding = c("v2prov", "v2r1", "v2r2", "v2r3", "v2prov", "v3prov", "v3r1", "v3r2", "v3r3", "v3rt_nc", "v4rt_nc"), # weak - data end in many variants
                       DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro")),
                       quarterly = FALSE) {
  
  YYYYMMDD <- as.character(YYYYMMDD)
  ### create vector of possible urls
  if (as.numeric(substr(YYYYMMDD, 1, 3)) == 199) {
    urlEnding <- "v2prov_r2.zip" # override; all 1990s data appear to share a file ending (https://sofia.usgs.gov/eden/models/watersurfacemod_download.php)
  }
  
  ### identify quarterly netCDF to be downloaded
  qtr <- tolower(format(zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")), format = "%Y_Q%q"))

  currentQ <- FALSE  
  ### check if requested date is in current quarter (and thus on the front page of EDEN)
  if (zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")) == zoo::as.yearqtr(as.Date(Sys.Date(), format = "%Y%m%d"))) {
    currentQ <- TRUE  
  }
   
  ### assemble vector of URLS and identify the working url
  url_prep  <- expand.grid(a = baseURL, b = qtr, c = "_", d = urlEnding, e = ".zip")
  temp_urls <- do.call(paste0, c(url_prep))
    
  i <- NA
  for(i in 1:length(temp_urls)) {
    success <- FALSE
    outDat <- httr::http_status(httr::GET(temp_urls[i]))
    # cat(outDat$message)
    if ("Success: (200) OK" %in% outDat$message) {
      success <- TRUE
      break
    }
    if ((!success) && (i == length(temp_urls))) {
      stop("EDEN data url could not be identified")
    }
  }
  
  ### download quarterly netCDF
  temp_url <- temp_urls[i]


  tmpDir <- tempdir() # tempdir()
  temp   <- tempfile(tmpdir = tmpDir, fileext = ".zip")
  
  utils::download.file(url = temp_url, destfile = temp)
  fileName <- utils::unzip(zipfile = temp, exdir = tmpDir, list = TRUE)$Name
  ras      <- raster::brick(unzip(zipfile = temp, exdir = tmpDir))
  
  if (quarterly == FALSE) {
    ### load raster for specified date 
    targetRas <- ras[[which(gsub(x = ras@z$Date, pattern = "-", replacement = "")  %in% YYYYMMDD)]]
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
  } else if (quarterly == TRUE) {
    rasDate <- raster::stack(file.path(tmpDir, fileName))
    ### need to subtract DEM*100, convert each layer to SPDF, and sf::st_as_sf
    rasDate  <- rasDate - (DEM*100)
    # rasDate <- as.list(rasDate)
    # polylist2 <- lapply(as.list(rasDate), as("SpatialPolygonsDataFrame"))
    # 
    # rasDate_2 <- sf::st_as_sf(as(rasDate[[1]], "SpatialPolygonsDataFrame"))
    # 
    # rasDate <- as(ras, "SpatialPolygonsDataFrame") # creates a spatialPolygonsDataFrame with a variable for each day
    # rasDate <- sf::st_as_sf(rasDate)
    YYYYMMDD <- qtr
  }

  unlink(x = temp)     # deletes the zipped file
  unlink(x = file.path(tmpDir, fileName)) # deletes the unzipped file
  
  invisible(list(date = YYYYMMDD, data = rasDate))
  
}

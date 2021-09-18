#' @title Downloads quarterly EDEN data (from 1991 through present) 
#'
#' @description This a beta replacement for getOldEDEN, providing access to quarterly data covering the entire period of record. Downloads netCDF files from https://sflthredds.er.usgs.gov/thredds/catalog/eden/surfaces/catalog.html. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param YYYYMMDD EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product in meters NAVD88. If `DEM = NULL`, output will be water surface in centimeters NAVD88.
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded.
#' 
#' @return list \code{getQuarterlyEDEN} returns a list with two elements: (1) a vector of dates in the specified quarter, and (2) a rasterStack with a water depth layer for each date (units = cm w.r.t. soil surface, unless `DEM = NULL`).
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
#' @importFrom raster compareCRS
#' @importFrom raster projectRaster
#' @importFrom raster nlayers
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
  
  ### identify quarterly netCDF to be downloaded
  qtr <- tolower(format(zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")), format = "%Y_Q%q"))

  currentQ <- FALSE  
  ### check if requested date is in current quarter (and thus on the front page of EDEN)
  if (zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")) == zoo::as.yearqtr(as.Date(Sys.Date(), format = "%Y%m%d"))) {
    currentQ <- TRUE  
  }
  
  createDateVec <- function(year_qtr) {
    ### creates a vector of dates from a year_qtr object
    # YYMMDD <- "20210115"
    # year_qtr <- zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d"))
    
    yr <- substr(year_qtr, 1, 4)
    dd_start <- "01"
    mo_tmp <- substr(YYYYMMDD, 5, 6)
    if (any(grepl(x = mo_tmp, pattern = "01|02|03"))) {
      mo_start <- "01"
      mo_end <- "03"
      dd_end <- "31"
    }
    if (any(grepl(x = mo_tmp, pattern = "04|05|06"))) {
      mo_start <- "04"
      mo_end <- "06"
      dd_end <- "30"
    }
    if (any(grepl(x = mo_tmp, pattern = "07|08|09"))) {
      mo_start <- "07"
      mo_end <- "09"
      dd_end <- "30"
    }
    if (any(grepl(x = mo_tmp, pattern = "10|11|12"))) {
      mo_start <- "10"
      mo_end <- "12"
      dd_end <- "31"
    }
    return(seq.Date(from = as.Date(paste(yr, mo_start, dd_start, sep = "-")),
                    to = as.Date(paste(yr, mo_end, dd_end, sep = "-")), by = 1))
    
  }
   
  dateVec <- createDateVec(qtr)
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
    if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
      if (!raster::compareCRS(DEM, targetRas)) { 
        ### make sure projection matches DEM before subtracting to get water depth
        targetRas <- raster::projectRaster(from = targetRas, to = DEM) # crs=raster::crs(DEM))
      }
      targetRas <- targetRas - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    }
    
    names(targetRas) <- "WaterDepth"     # to match EDEN geoTiffs and getFireHydro hard-coded variables
    rasDate <- as(targetRas, "SpatialPolygonsDataFrame")
    rasDate <- sf::st_as_sf(rasDate)
    
    # rasDate <- sf::st_as_sf(rasDate)
    # a.ras  <- raster::raster(a)
    # a.ras <- a.ras - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    # a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
    #
    # names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
    returnDat <- list(date = as.Date(YYYYMMDD, format = "%Y%m%d"), data = rasDate)
  } else if (quarterly == TRUE) {
    rasDate <- raster::stack(x = ras) #(x = file.path(tmpDir, paste0(qtr, ".nc")))
    if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
      ### make sure projection matches DEM
      if (!raster::compareCRS(DEM, rasDate)) {
        rasDate      <- raster::projectRaster(from = rasDate, to = DEM) # crs=raster::crs(DEM))
      }
      ### need to subtract DEM*100, convert each layer to SPDF, and sf::st_as_sf
      rasDate  <- rasDate - (DEM*100)
    }
    
    returnDat <- list(date = dateVec[1:nlayers(rasDate)], # this accommodates partial current quarters (could be improved)
                      data = rasDate)
  }

  # unlink(x = temp)     # deletes the zipped file
  unlink(x = file.path(tmpDir, paste0(qtr, ".nc"))) # deletes the unzipped file
  
  invisible(returnDat)
  
}

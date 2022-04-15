#' @title Downloads quarterly EDEN data (from 1991 through present) 
#'
#' @description Provides access to quarterly data covering the entire period of record. Downloads netCDF files from https://sflthredds.er.usgs.gov/thredds/catalog/eden/surfaces/catalog.html. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param YYYYMMDD EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product in meters NAVD88. If `DEM = NULL`, output will be water surface in centimeters NAVD88.
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded.
#' @param download.method For use in debugging or troubleshooting. Method to be used for downloading files. See options in utils::download.file
#' 
#' @return eden \code{getQuarterlyEDEN} returns an `eden` object, which is a list with two elements: (1) a vector of dates in the specified quarter, and (2) a SpatRaster with a water depth layer for each date (units = cm w.r.t. soil surface, unless `DEM = NULL`). Geospatial data returned by this function is always a SpatRaster.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' edenDat <- getQuarterlyEDEN(YYYYMMDD = "19910101", quarterly = TRUE)
#' }
#' 
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra subset
#' @importFrom terra nlyr
#' @importFrom zoo    as.yearqtr
#' @importFrom utils  unzip
#' @importFrom utils  download.file
#' @importFrom sf     st_as_sf
#' 
#' @export


getQuarterlyEDEN <- function(YYYYMMDD, 
                       DEM = terra::rast(system.file("extdata/edenDEM.grd", package = "fireHydro")),
                       quarterly = FALSE, # why even have this argument? in case an older single day is desired.
                       download.method = 'libcurl') {
  
  if (class(YYYYMMDD) == "Date") {
    YYYYMMDD <- as.character(format(YYYYMMDD, format = "%Y%m%d"))
  }
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
  url <- paste0("https://sflthredds.er.usgs.gov/thredds/fileServer/eden/surfaces/", qtr, ".nc")
                # "https://sflthredds.er.usgs.gov/thredds/fileServer/eden/surfaces/1991_q1.nc"
  # url <- paste0("https://sflthredds.er.usgs.gov/thredds/catalog/eden/surfaces/catalog.html?dataset=EDEN/surfaces/", qtr, ".nc")
    
  dirTemp <- tempdir() 
  # temp   <- tempfile(tmpdir = dirTemp, fileext = ".nc")
  
  utils::download.file(url = url, destfile = file.path(dirTemp, paste0(qtr, ".nc")), 
                       mode = 'wb', method = download.method)
  # fileName <- utils::unzip(zipfile = temp, exdir = dirTemp, list = TRUE)$Name
  # ras      <- raster::brick(x = file.path(dirTemp, paste0(qtr, ".nc")))
  ras      <- terra::rast(x = file.path(dirTemp, paste0(qtr, ".nc")))
  dateVec <- dateVec[1:terra::nlyr(ras)]
  
  ### convert to water depths
  if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
    if (!identical(terra::crs(DEM, proj = TRUE), terra::crs(ras, proj = TRUE))) { 
      ### make sure projection matches DEM before subtracting to get water depth
      ras <- terra::project(x = ras, y = DEM) # crs=raster::crs(DEM))
    }
    ### DEM units must be meters
    ras <- ras - (DEM * 100) # apply DEM to convert water surfaces to depths in centimeters ## UNIX: "Error in .local(.Object, ...) : "
  }
  names(ras) <- gsub(x = dateVec, pattern = "-", replacement = "")
  
  if (quarterly == FALSE) {
    ### subset specified date 
    targetRas <- terra::subset(x = ras, subset = grep(pattern = YYYYMMDD, x = gsub(x = dateVec, pattern = "-", replacement = "")))
    names(targetRas) <- YYYYMMDD #"WaterDepth"     # to match EDEN geoTiffs and getFireHydro hard-coded variables
    returnDat <- list(date = as.Date(YYYYMMDD, format = "%Y%m%d"), data = targetRas)
  } else if (quarterly == TRUE) {
    returnDat <- list(date = dateVec, # this accommodates partial current quarters (could be improved)
                      data = ras)
  }
  
  # if (grepl(x = returnType, pattern = 'raster')) {
  #   returnDat$data <- raster::stack(returnDat$data)
  # }

  # unlink(x = temp)     # deletes the zipped file
  unlink(x = file.path(dirTemp, paste0(qtr, ".nc"))) # deletes the unzipped file
  class(returnDat) <- c("eden", class(returnDat)) 
  invisible(returnDat)
}

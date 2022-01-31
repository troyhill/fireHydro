#' @title Downloads and imports archived EDEN data (from 2000 through the penultimate quarter in the record) 
#'
#' @description Note: This function is deprecated. See \code{getQuarterlyEDEN} for a more generally useful function. Downloads netCDF files from https://sofia.usgs.gov/eden/models/watersurfacemod_download.php, unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param YYYYMMDD EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param returnType  character; class of object returned. Acceptable options: "sf", "raster"
#' @param baseURL beginning part of url
#' @param urlEnding ending part of url
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product. If `DEM = NULL`, output will be water surface in centimeters NAVD88.
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded and returned as a RasterStack.
#' 
#' @return eden \code{getOldEDEN} returns an `eden` object, which is a list with two elements: (1) the date used, and (2) a spatial object with water levels (centimeters relative to soil surface) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' edenDat <- getOldEDEN(YYYYMMDD = "20170809", returnType = "raster")
#' }
#' 
#' @importFrom httr   GET
#' @importFrom httr   http_status
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


getOldEDEN <- function(YYYYMMDD, 
                       returnType = "sf",
                       baseURL = c("https://sofia.usgs.gov/eden/data/netcdf/v2/", "https://sofia.usgs.gov/eden/data/netcdf/v3/", "https://sofia.usgs.gov/eden/data/realtime2/"),
                       urlEnding = c("v2prov", "v2prov_r2", "v2prov_r3", "v2r1", "v2r2", "v2r3",  "v2rt_nc", 
                                     "v3prov", "v3prov_r2", "v3prov_r3", "v3r1", "v3r2", "v3r3", "v3rt_nc", 
                                     "v4prov", "v4prov_r2", "v4prov_r3", "v4r1", "v4r2", "v4r3", "v4rt_nc"), # weak - data end in many variants
                       DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro")),
                       quarterly = FALSE) {
  
  if (class(YYYYMMDD) == "Date") {
    YYYYMMDD <- as.character(format(YYYYMMDD, format = "%Y%m%d"))
  }
  if(grepl(x = as.character(YYYYMMDD), pattern = "-")) {
    YYYYMMDD <- gsub(x = as.character(YYYYMMDD), pattern = "-", replacement = "")
    cat("Hyphens in EDEN_date are being removed. Date is interpreted as '%Y-%m-%d'\n")
    }
  
  YYYYMMDD <- as.character(YYYYMMDD)
  ### create vector of possible urls
  if (as.numeric(substr(YYYYMMDD, 1, 3)) == 199) {
    urlEnding <- "v2prov_r2" # override; all 1990s data appear to share a file ending (https://sofia.usgs.gov/eden/models/watersurfacemod_download.php)
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
  
  if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
    if (!raster::compareCRS(DEM, a.ras)) { 
      ### make sure projection matches DEM before subtracting to get water depth
      ras <- raster::projectRaster(from = ras, to = DEM) # crs=raster::crs(DEM))
    }
  }
  # ### make sure projection matches DEM
  # if (!raster::compareCRS(DEM, ras)) {
  #   ras      <- raster::projectRaster(from = ras, to = DEM) # crs = raster::projection(DEM))
  # }
  
  
  if (quarterly == FALSE) {
    ### load raster for specified date 
    targetRas <- ras[[which(gsub(x = names(ras), pattern = "X|-|\\.", replacement = "")  %in% YYYYMMDD)]]
    if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
      targetRas <- targetRas - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    }
    names(targetRas) <- "WaterDepth"     # to match EDEN geoTiffs and getFireHydro hard-coded variables
    
    if (returnType == "sf") {
      rasDate <- as(targetRas, "SpatialPolygonsDataFrame")
      rasDate <- sf::st_as_sf(rasDate)
      
    } else {
      rasDate <- targetRas
    }
    
    # rasDate <- sf::st_as_sf(rasDate)
    # a.ras  <- raster::raster(a)
    # a.ras <- a.ras - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
    # a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
    #
    # names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
  } else if (quarterly == TRUE) {
    rasDate <- raster::stack(file.path(tmpDir, fileName))
    if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
      ### make sure projection matches DEM
      if (!raster::compareCRS(DEM, rasDate)) {
        rasDate      <- raster::projectRaster(from = rasDate, to = DEM) # crs=raster::crs(DEM))
      }
      ### need to subtract DEM*100, convert each layer to SPDF, and sf::st_as_sf
      rasDate  <- rasDate - (DEM*100)
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
    
    YYYYMMDD <- createDateVec(qtr)[1:raster::nlayers(rasDate)]
  }

  unlink(x = temp)     # deletes the zipped file
  unlink(x = file.path(tmpDir, fileName)) # deletes the unzipped file
  
  EDEN_list <- list(date = as.Date(YYYYMMDD, format = "%Y%m%d"), data = rasDate)
  class(EDEN_list) <- "eden"
  invisible(EDEN_list)
}

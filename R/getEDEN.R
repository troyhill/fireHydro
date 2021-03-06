#' @title Downloads and imports EDEN data
#'
#' @description Downloads GeoTiff files from https://sofia.usgs.gov/eden/models/real-time.php , unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' @usage getEDEN(EDEN_date = gsub(Sys.Date(), pattern  = "-", replacement = ""), 
#'     exact = FALSE, 
#'     quarterly = FALSE,
#'     returnType  = "sf",
#'     DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro")))
#' 
#' @param EDEN_date EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018". By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param exact logical; if TRUE, output is only returned if the requested date is available. If exact = FALSE, the function responds to an invalid EDEN_date input by returning data from the most recent available date
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded and returned as a RasterStack.
#' @param returnType  character; class of object returned. Acceptable options: "sf", "raster"
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product.
#' 
#' @return list \code{getEDEN} returns a list with two elements: (1) the date used, and (2) a spatial object with water levels (centimeters relative to soil surface) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' ### default behavior is to return data from the most recent date
#' a <- getEDEN()
#' 
#' ### getEDEN output can be directly used in getFireHydro()
#' a.fire <- getFireHydro(EDEN_date = a$date, 
#'      EDEN_GIS_directory = a$data,
#'      fireSpreadExport = paste0("fireRisk_", a$date, ".png"), 
#'      waterLevelExport = paste0("waterLevels_", a$date, ".png"))
#'      
#' ### only the most recent data are available as individual geotiffs.
#' ### Use 'exact = TRUE' to return NULL if an exact date match isn't found
#' edenDat.false <- getEDEN(EDEN_date = "20180101", exact = FALSE)
#' edenDat.true  <- getEDEN(EDEN_date = "20180101", exact = TRUE)
#' 
#' edenDat.false
#' edenDat.true 
#' # reporting the date with the output is particularly useful if the requested date is unavailable
#' }
#' 
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom raster raster
#' @importFrom raster rasterToPolygons
#' @importFrom raster compareCRS
#' @importFrom raster projectRaster
#' @importFrom utils unzip
#' @importFrom utils tail
#' @importFrom sf st_as_sf
#' 
#' @export


getEDEN <- function(EDEN_date = gsub(Sys.Date(), pattern  = "-", replacement = ""), 
                exact = FALSE, 
                quarterly = FALSE,
                returnType  = "sf",
                DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro"))) {
  
  if (!grepl(x = EDEN_date, pattern = "^[0-9]{8}$")) {
    stop(paste0("\n", EDEN_date, " is not a valid date entry. Dates must be in format YYYYMMDD. \n"))
  }

  # on.exit(close(base_url))
  # on.exit(close(url))
  
  # if date isn't present on USGS site, find and use most recent date, inform user
  url  <- "https://sofia.usgs.gov/eden/models/real-time.php"
  html <- paste(readLines(url, warn = FALSE), collapse="\n") # warn = FALSE dodges a warning message about an incomplete final line in the url
  
  txt <- unlist(regmatches(x = html, gregexpr('[0-9]{8}_geotif', html)))
  txt <- gsub(pattern = "_geotif", replacement = "", x = txt)
  cont <- TRUE
  
  if(as.numeric(EDEN_date) > as.numeric(txt[1])) {
    cat(paste0("\n The date you provided, ", EDEN_date, ", is not yet available on EDEN.\n"))   
    if (exact == TRUE) {
      cont <- FALSE # don't continue if exact date is requested but unavailable
      cat(" Because 'exact' was set to TRUE, no data is being returned. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. \n")
    }
    if (exact == FALSE) {
      cat(paste0(" Because 'exact' was set to FALSE, the most recent data from ", txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. Older dates need to be downloaded manually. \n\n"))
    }
    EDEN_date <- txt[1]
  }
  
  if(cont) {
    # if((!EDEN_date %in% txt) && (exact == FALSE)) {
    #   cat(paste0("\n The date you provided, ", EDEN_date, ", is not yet available on EDEN. The most recent data from ", txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. Older dates need to be downloaded manually. \n\n"))
    #   EDEN_date <- txt[1]
    # }
    
    if (!quarterly){
      if(as.numeric(EDEN_date) > as.numeric(txt[1])) {
      cat(paste0("\n The date you provided, ", EDEN_date, ", is not yet available on EDEN. The most recent data from ", txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. \n\n"))
      EDEN_date <- txt[1]
    } else if(as.numeric(EDEN_date) %in% as.numeric(txt)) {
      
      # 1: identify zip file for EDEN_date or EDEN_date-1 (add option approving this)
      ### TODO: identify link to avoid v2/v3 issues
      base_url <- paste0("https://sofia.usgs.gov/eden/data/realtime2/", 
                                      unlist(regmatches(x = html, gregexpr(paste0(EDEN_date, '_geotif(.*?)zip'), html)))
      )
      ### get file name by replacing .zip with .tif
      dataName <- gsub(x = utils::tail(unlist(strsplit(x = base_url, split = "/")), 1), pattern = ".zip", replacement = ".tif")
      dataName <- gsub(x = dataName, pattern = "_geotif", replacement = "")
  
      geotiff_zip <- tempfile(fileext='.zip')
      httr::GET(base_url, httr::write_disk(path=geotiff_zip))
      
      # TODO: if no file returned (file size < 100 kb), find most recent date, inform user, and use most recent date
      
      
      # 2: download and unzip zip file
      utils::unzip(zipfile = geotiff_zip, overwrite = TRUE, exdir = tempdir())
      
      
      # 3: load geotiff as sf, set projection
      a <- paste0(tempdir(), "/s_", dataName) # "_v2rt.tif")
      
      a.ras  <- raster::raster(a)
      ### make sure projection matches DEM
      if (!raster::compareCRS(DEM, a.ras)) {
        a.ras <- raster::projectRaster(from = a.ras, to = DEM) # crs=raster::crs(DEM))
      }
      
      a.ras <- a.ras - (DEM * 100) # apply DEM to convert water surfaces to depths ## UNIX: "Error in .local(.Object, ...) : "
      
      if (returnType == "sf") {
        a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
        a.sf <- sf::st_as_sf(a.poly) # this is super slow. Recently became even slower (20200526)
        names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
        # plot(a.sf)
      } else {
        a.sf <- a.ras
      }
      
      ### cleanup
      # file.remove(c(geotiff_zip, a))
      unlink(c(geotiff_zip, a))
      
      invisible(list(date = EDEN_date, data = a.sf))
    } else if (as.numeric(EDEN_date) < as.numeric(txt[length(txt)])) {
      cat(paste0("\n The date you provided, ", EDEN_date, ", is not available on EDEN's main  website. Attempting to download archived data... \n\n"))
      invisible(getOldEDEN(YYYYMMDD = EDEN_date, quarterly = quarterly, returnType = returnType))
    }
    } else if (quarterly) {
      invisible(getQuarterlyEDEN(YYYYMMDD = EDEN_date, quarterly = quarterly))
    }
    }
}

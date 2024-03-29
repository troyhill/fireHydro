#' @title Downloads and imports EDEN data
#'
#' @description Downloads GeoTiff files from https://sofia.usgs.gov/eden/models/real-time.php , unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' @param EDEN_date The date(s) for which water levels are desired. Format should be a numeric or character string with the format "20181018" or "2018-10-18", or a Date object in any format. By default, today's date is used; if "exact = FALSE" this returns the most recent EDEN data available.
#' @param exact logical; if TRUE, output is only returned if the requested date is available. If exact = FALSE, the function responds to an invalid EDEN_date input by returning data from the most recent available date
#' @param quarterly logical; if set to TRUE, entire quarter is downloaded and returned as a SpatRaster.
#' @param returnType  character; class of object returned. Acceptable options: "sf" or "terra"
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths in centimeters relative to soil surface. The default DEM is a USGS/EDEN product in meters NAVD88. If an alternate DEM is used, units should be meters. The DEM is multiplied by 100 internally in `getEDEN()` before being subtracted from the water surface. If `DEM = NULL`, output will be water surface in centimeters NAVD88. If a DEM is supplies and projection differs from the water surface, water surface is re-projected to match the DEM.
#' @param download.method Method to be used for downloading files. See options in utils::download.file
#' 
#' @return eden \code{getEDEN} returns an `eden` object, which is a list with two elements: (1) the date(s) used, and (2) a spatial object with water levels (centimeters relative to soil surface) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' ### default behavior is to return data from the most recent date
#' a <- getEDEN(EDEN_date = Sys.Date(), returnType = 'terra')
#' 
#' ### only the most recent data are available as individual geotiffs.
#' ### Use 'exact = TRUE' to return NULL if an exact date match isn't found
#' edenDat.false <- getEDEN(EDEN_date = "20180101", exact = FALSE)
#' edenDat.true  <- getEDEN(EDEN_date = "20180101", exact = TRUE)
#' 
#' edenDat.false
#' edenDat.true 
#' }
#' 
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom terra rast
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra subset
#' @importFrom terra as.polygons
#' @importFrom utils unzip
#' @importFrom utils tail
#' @importFrom sf st_as_sf
#' 
#' @export


getEDEN <- function(EDEN_date = Sys.Date(), 
                exact = FALSE, 
                quarterly = FALSE,
                returnType  = "sf",
                DEM = terra::rast(system.file("extdata/edenDEM.grd", package = "fireHydro")),
                download.method = 'curl') {
  
  ### issues: 
  ### - DEM not properly being subtracted
  ### - getFireHydro breaks with a topology error
  
  ### handle requests for multiple dates
  ### TODO: efficient behavior: detect when dates are within a quarter and do fewer data pulls. subset/stitch
  if (length(EDEN_date) > 1) {
    EDEN_list_int  <- lapply(X = EDEN_date, FUN = getEDEN, exact = exact, quarterly = quarterly, returnType = returnType, DEM = DEM)
    class(EDEN_list_int) <- c("eden", grep(x = class(EDEN_list_int), pattern = "eden", invert = TRUE, value = TRUE)) 
    ### will only work with raster type
    # EDEN_list      <- list(date = as.Date(do.call(rbind, lapply(EDEN_list_int, function(x) {as.character(x[[1]])}))),
    #                        data = do.call(raster::stack, lapply(EDEN_list_int, function(x) {x[[2]]})))
    EDEN_list <- merge.eden(x = EDEN_list_int, y = NULL)
    class(EDEN_list) <- c("eden", grep(x = class(EDEN_list), pattern = "eden", invert = TRUE, value = TRUE)) 
    return(EDEN_list)
  } 
  if (length(EDEN_date) == 1) {
    ### accommodate hyphenated dates but notes
    if (any(class(EDEN_date) == "Date")) {
      EDEN_date <- as.character(format(EDEN_date, format = "%Y%m%d"))
    }
    if(grepl(x = as.character(EDEN_date), pattern = "-")) {
      EDEN_date <- gsub(x = as.character(EDEN_date), pattern = "-", replacement = "")
      cat("Date is interpreted as", as.character(format(EDEN_date, format = "%d %b %Y")), "\n")
    }
    
    if (!grepl(x = EDEN_date, pattern = "^[0-9]{8}$")) {
      stop(paste0("\n", EDEN_date, " is not a valid date entry. Dates must be in format YYYYMMDD. \n"))
    }
    
    # on.exit(close(base_url))
    # on.exit(close(url))
    
    # if date isn't present on USGS site, find and use most recent date, inform user
    theurl  <- "https://sofia.usgs.gov/eden/models/real-time.php"
    
   txt <- tryCatch( {
      html <- paste(readLines(theurl, warn = FALSE), collapse="\n") # warn = FALSE dodges a warning message about an incomplete final line in the url
      txt <- unlist(regmatches(x = html, gregexpr('[0-9]{8}_geotif', html)))
      txt <- gsub(pattern = "_geotif", replacement = "", x = txt)
      prep <- NULL
      txt <- list(txt = txt, prep = prep)
      invisible(txt)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", theurl))
      message("Here's the original error message:")
      message(cond)
      message("\n\nfireHydro is attempting to download most recent quarterly data...\n")
      # Do stuff
      prep <- getQuarterlyEDEN(YYYYMMDD = Sys.Date(), quarterly = TRUE, download.method = download.method)
      txt  <- rev(gsub(x = prep$date, pattern = "-", replacement = ""))
      txt <- list(txt = txt, prep = prep)
      invisible(txt)
      # invisible(list(txt = txt, prep = prep))
    }
    )
    if (is.null(txt$prep)) {
      errorVal <- 0
    } else if (!is.null(txt$prep)) {
      ### flag for ability to use already downloaded data
      errorVal <- 1
    }
    
    if(as.numeric(EDEN_date) > as.numeric(txt$txt[1])) {
      cat(paste0("\n The date you provided, ", EDEN_date, ", is not yet available on EDEN.\n"))   
      if (exact == TRUE) {
        cont <- FALSE # don't continue if exact date is requested but unavailable
        cat(" Because 'exact' was set to TRUE, no data is being returned. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. \n")
      }
      if (exact == FALSE) {
        cat(paste0(" Because 'exact' was set to FALSE, the most recent data from ", txt$txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. \n\n"))
      }
      EDEN_date <- txt$txt[1]
    }
    
      if (!quarterly){
        if(as.numeric(EDEN_date) > as.numeric(txt$txt[1])) {
        cat(paste0("\n The date you provided, ", EDEN_date, ", is not yet available on EDEN. The most recent data from ", txt$txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. \n\n"))
        EDEN_date <- txt$txt[1]
      } else if(as.numeric(EDEN_date) %in% as.numeric(txt$txt)) {
        
        # 1: identify zip file for EDEN_date or EDEN_date-1 (add option approving this)
        ### TODO: identify link to avoid v2/v3 issues
        if (errorVal == 0) { # if site accessible
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
          a      <- file.path(tempdir(), utils::unzip(zipfile = geotiff_zip, overwrite = TRUE, exdir = tempdir(), list = TRUE)$Name)
          a.ras  <- terra::rast(a)
          
          ### set projection to package-provided DEM to start, in case DEM argument is null
          a.ras <- terra::project(x = a.ras, y = terra::rast(system.file("extdata/edenDEM.grd", package = "fireHydro")))
          
          if (!is.null(DEM)) { # if DEM == NULL, water surface in cm NAVD88 is returned
            if (!identical(terra::crs(DEM, proj = TRUE), terra::crs(a.ras, proj = TRUE))) { 
              ### make sure projection matches DEM before subtracting to get water depth
              # a.ras <- raster::projectRaster(from = a.ras, to = DEM) # crs=raster::crs(DEM))
              a.ras <- terra::project(x = a.ras, y = DEM) # crs=raster::crs(DEM))
            }
            ### DEM units must be meters
            a.ras <- a.ras - (DEM * 100) # apply DEM to convert water surfaces to depths in centimeters ## UNIX: "Error in .local(.Object, ...) : "
          }
        } else if (errorVal == 1) { 
          ### if quarterly file was used, subset it to get target date:
          ### DEM is already applied
          if (!any(grepl(x = class(txt$prep$data), pattern = 'SpatRaster'))) {
            ### convert to SpatRaster if not in that form
            txt$prep$data <- terra::rast(txt$prep$data*1) # dangerous to load into memory? Only a quarter's worth of data, so ~300 mb
          }
          a.ras  <- terra::subset(x = txt$prep$data, subset = grep(x = gsub(x = txt$prep$date, pattern = "-", replacement = ""), pattern = EDEN_date))
          # a.ras  <- raster::subset(x = txt$prep$data, subset = grep(x = gsub(x = txt$prep$date, pattern = "-", replacement = ""), pattern = EDEN_date))
        }
        
        
        if (returnType == "sf") {
          # a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
          a.poly <- terra::as.polygons(x=a.ras, dissolve = FALSE)
          a.sf <- sf::st_as_sf(a.poly) # this is super slow. Recently became even slower (20200526)
          # plot(a.sf)
        } else if (returnType == 'terra') {
          a.sf <- a.ras
        } else if (returnType == 'raster') {
          #a.sf <- raster::raster(a.ras)
          a.sf <- a.ras
          message('raster returnType not supported. Returning SpatRaster.\n')
        } 
        ### change column name
        names(a.sf)[grepl(x = names(a.sf), pattern = paste0("layer|", EDEN_date))] <- EDEN_date
        
        ### cleanup
        # file.remove(c(geotiff_zip, a))
        if (is.null(txt$prep)) { # if site accessible
          unlink(c(geotiff_zip, a))
        }
        
        EDEN_list <- list(date = as.Date(EDEN_date, format = "%Y%m%d"), data = a.sf)
        class(EDEN_list) <- c("eden", grep(x = class(EDEN_list), pattern = "eden", invert = TRUE, value = TRUE)) 
        return(EDEN_list)
      } else if (as.numeric(EDEN_date) < as.numeric(txt$txt[length(txt$txt)])) {
        stop(paste0("\n The date you provided, ", EDEN_date, ", is not available on EDEN's real-time website.\n\n"))
        ### this will return a single date. Should subset here and remove quarterly argument from getQuarterlyEDEN
        EDEN_list <- getQuarterlyEDEN(YYYYMMDD = EDEN_date, quarterly = quarterly)
        if (returnType == "sf") {
          a.poly <- terra::as.polygons(x=EDEN_list$data, dissolve = FALSE)
          EDEN_list$data <- sf::st_as_sf(a.poly) # this is super slow. Recently became even slower (20200526)
          # plot(a.sf)
        } else if (returnType == 'terra') {
          EDEN_list$data <- EDEN_list$data
        } else if (returnType == 'raster') {
          # EDEN_list$data <- raster::raster(EDEN_list$data)
          message('raster returnType not supported. Returning SpatRaster.\n')
        } 
        ### change column name
        names(EDEN_list$data)[grepl(x = names(EDEN_list$data), pattern = paste0("layer|", EDEN_date))] <- EDEN_date #"WaterDepth"
        
        # class(EDEN_list) <- c("eden", class(EDEN_list))
        return(EDEN_list)
      }
      } else if (quarterly) {
        EDEN_list <- getQuarterlyEDEN(YYYYMMDD = EDEN_date, quarterly = quarterly, download.method = download.method)
        # class(EDEN_list) <- c("eden", class(EDEN_list)) 
        return(EDEN_list)
      }
    }
  return(EDEN_list)
}

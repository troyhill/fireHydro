#' @title Downloads and imports EDEN data
#'
#' @description Downloads GeoTiff files from https://sofia.usgs.gov/eden/models/real-time.php , unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' @usage getEDEN(EDEN_date)
#' 
#' @param EDEN_date eight-digit character string identifying the date to be used for water levels (e.g., "20181018"). 
#' 
#' @return sf \code{getEDEN} returns an sf object. Units reported are water depth in centimeters relative to the ground surface.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' EDEN_date_target <- "20190606"
#' a <- getEDEN(EDEN_date = EDEN_date_target)
#' 
#' ### getEDEN output can then be used in getFireHydro
#' a.fire <- getFireHydro(EDEN_date = EDEN_date_target, 
#'      EDEN_GIS_directory = "a",
#'      output_shapefile = NULL,
#'      fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png", burnHist = TRUE)
#' }
#' 
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom raster raster
#' @importFrom raster rasterToPolygons
#' @importFrom utils unzip
#' @importFrom sf st_as_sf
#' 
#' @export


getEDEN <- function(EDEN_date) {
  
  if (!grepl(x = EDEN_date, pattern = "^[0-9]{8}$")) {
    stop(paste0("\n", EDEN_date, " is not a valid date entry. Dates must be in format YYYYMMDD. \n"))
  }
  
  # if date isn't present on USGS site, find and use most recent date, inform user
  url  <- "https://sofia.usgs.gov/eden/models/real-time.php"
  html <- paste(readLines(url), collapse="\n")
  
  txt <- unlist(regmatches(x = html, gregexpr('[0-9]{8}_geotif', html)))
  txt <- gsub(pattern = "_geotif", replacement = "", x = txt)
  
  if(!EDEN_date %in% txt) {
    cat(paste0("\n The date you provided, ", EDEN_date, ", is not available on EDEN. The most recent data from ", txt[1], " is being used instead. Check here for a list of available recent dates: https://sofia.usgs.gov/eden/models/real-time.php. Older dates need to be downloaded manually. \n\n"))
    EDEN_date <- txt[1]
  }
  

  # 1: identify zip file for EDEN_date or EDEN_date-1 (add option approving this)
  base_url <- paste0("https://sofia.usgs.gov/eden/data/realtime2/", EDEN_date ,"_geotif_v2rt.zip")
  
  geotiff_file <- tempfile(fileext='.tif')
  httr::GET(base_url, httr::write_disk(path=geotiff_file)) ### this is a zip file so can't be loaded directly

  # 2: download and unzip zip file
  tempDirContents <- list.files(tempdir(), full.names = TRUE) # record contents of temp folder
  utils::unzip(geotiff_file, overwrite = TRUE, exdir = tempdir())

  # 3: load geotiff as sf, set projection
  a <- paste0(tempdir(), "/s_", EDEN_date, "_v2rt.tif")
  
  a.poly <- raster::rasterToPolygons(raster::raster(a) - (fireHydro::edenDEM * 100), # apply DEM to convert water surfaces to depths in cm 
                                     dissolve = TRUE) #dissolve option requires rgeos
  a.sf <- sf::st_as_sf(a.poly)
  names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
  # plot(a.sf)
  
  ### cleanup
  file.remove(c(geotiff_file, a, list.files(tempdir(), full.names = TRUE)[!list.files(tempdir(), full.names = TRUE) %in% tempDirContents]))
  
  invisible(a.sf)
  }
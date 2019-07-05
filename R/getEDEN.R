#' @title Downloads and imports EDEN data
#'
#' @description Downloads GeoTiff files from https://sofia.usgs.gov/eden/models/real-time.php , unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' @usage getEDEN(EDEN_date)
#' 
#' @param EDEN_date EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018"
#' 
#' @return sf \code{getEDEN} returns an sf object.
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
  # 1: identify zip file for EDEN_date or EDEN_date-1 (add option approving this)
  base_url <- paste0("https://sofia.usgs.gov/eden/data/realtime2/", EDEN_date ,"_geotif_v2rt.zip")
  
  geotiff_file <- tempfile(fileext='.tif')
  shp_file <- tempfile(fileext='.tif')
  httr::GET(base_url, httr::write_disk(path=geotiff_file))
  
  # TODO: if no file returned (file size < 100 kb), find most recent date, inform user, and use most recent date
  
  
  # 2: download and unzip zip file
  utils::unzip(geotiff_file, overwrite = TRUE, exdir = tempdir())
  
  
  # 3: load geotiff as sf, set projection
  a <- paste0(tempdir(), "/s_", EDEN_date, "_v2rt.tif")
  
  a.ras  <- raster::raster(a)
  a.ras <- a.ras - (fireHydro::edenDEM * 100) # apply DEM to convert water surfaces to depths
  a.poly <- raster::rasterToPolygons(a.ras, dissolve = TRUE) #dissolve option requires rgeos
  a.sf <- sf::st_as_sf(a.poly)
  names(a.sf)[names(a.sf) %in% "layer"] <- "WaterDepth"
  # plot(a.sf)
  
  ### cleanup
  file.remove(c(geotiff_file, shp_file, a))
  
  invisible(a.sf)
  }
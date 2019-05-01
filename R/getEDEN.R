#' @title Downloads and imports EDEN data
#'
#' @description Downloads GeoTiff files from https://sofia.usgs.gov/eden/models/real-time.php , unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers.
#' 
#' @usage getEDEN(EDEN_date)
#' 
#' @param EDEN_date EDEN date to be used for water levels. Should be an 8-digit numeric or character stirng, e.g., "20181018"
#' 
#' @return sf \code{getEDEN} produces an sf shapefile.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' a <- getEDEN(EDEN_date = "20181018")
#' 
#' ### getEDEN output can then be used in getFireHydro
#' getFireHydro(EDEN_date = "20181018", 
#'      EDEN_GIS_directory = a,
#'      output_shapefile = NULL,
#'      fireSpreadExport = "fireRisk.pdf", waterLevelExport = "waterLevels.pdf")
#' }
#' 
#' @importFrom httr GET
#' @importFrom raster writeRaster
#' @importFrom utils write.csv
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_intersection
#' @importFrom sf st_write
#' @importFrom sf st_set_geometry
#' @importFrom utils unzip
#' @importFrom rgdal setCPLConfigOption
#' @importFrom rgdal writeGDAL
#' @importFrom graphics plot
#' 
#' @export


getEDEN <- function(EDEN_date) {
  # 1: identify zip file for EDEN_date or EDEN_date-1 (add option approving this)
  base_url <- paste0("https://sofia.usgs.gov/eden/data/realtime2/", EDEN_date ,"_geotif_v2rt.zip")
  
  geotiff_file <- tempfile(fileext='.tif')
  shp_file <- tempfile(fileext='.tif')
  httr::GET(base_url, httr::write_disk(path=geotiff_file))
  
  # if no file returned, find most recent date, inform user, and use most recent date
  
  
  # 2: download and unzip zip file
  utils::unzip(geotiff_file, overwrite = TRUE, exdir = tempdir())
  
  
  # 3: load geotiff as sf, set projection
  a <- paste0(tempdir(), "/s_", EDEN_date, "_v2rt.tif")
  
  a.ras <- raster::raster(a)
  # output raster as .shp
  raster::writeRaster(a.ras, shp_file, format = "GTiff")
  
  a.ras2 <- sf::st_read(shp_file)
  g <- as(a.ras, 'SpatialGridDataFrame')
  summary(g)
  graphics::plot(g)
  
  }
#' Data: A raster DEM used to calculate EDEN water depths from water surfaces
#'
#' @description A raster digital elevation map for the greater Everglades. Units = meters NAVD88.
#'
#' @format A raster layer. Downloaded from https://sofia.usgs.gov/eden/models/groundelevmod.php on 20190705. Metadata available here: http://sofia.usgs.gov/metadata/sflwww/eden_em_oc11.html
#' @docType data
#' @keywords digital elevation model DEM
#' @name edenDEM
#' @examples 
#' \dontrun{
#' ### code used to generate object
#' edenDEM <- raster::raster("C:/Users/tdh/Downloads/eden_em_oc11/eden_em_oc11.e00")
#' save(edenDEM, file = "C:/RDATA/fireHydro/data/edenDEM.RData")
#'  
#'}
#' 
"edenDEM"
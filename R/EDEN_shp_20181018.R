#' Data: A shapefile of EDEN water level data for 18 October 2018
#'
#' @description A shapefile of EDEN water level and stage data
#'
#' @format A shapefile of vegetation in Big Cypress National Preserve and Everglades National Park
#' \describe{
#'   \item{row}{tbd}
#'   \item{col}{tbd}
#'   \item{WaterDepth}{Water depth (ft above surface)}
#'   \item{Stage}{Water depth (ft NGVD29?)}
#'   \item{geometry}{tbd}
#'}
#' @examples 
#' summary(EDEN_shp_20181018)
#' 
#' \dontrun{
#' ### code used to generate object
#' EDEN_shp_20181018          <- sf::st_read("/opt/physical/gis/eden/2018/eden_epa20181018.shp")
#'  
#'}
#' 
"EDEN_shp_20181018"
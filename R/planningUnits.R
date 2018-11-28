#' Data: Big Cypress and Everglades planning units - an sfc_POLYGON object
#'
#'
#'
#' @format An sfc_POLYGON object of planning unit boundaries for Big Cypress National Preserve and Everglades National Park
#' \describe{
#' \item{none}{none}
#'}
#' @docType data
#' @keywords planning units
#' @name planningUnits
#' @usage planningUnits
#' @examples 
#' plot(planningUnits)
#' 
#' \dontrun{
#' ### code used to generate object using shapefile provided by NCSU
#'  planningUnits <- sf::st_union(sf::st_read("BICY_EVER_PlanningUnits_EDEN_py.shp"))
#' }
#' 
NULL
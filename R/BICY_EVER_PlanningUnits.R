#' Data: Big Cypress and Everglades planning units
#'
#' @description A shapefile of planning units
#'
#' @format A shapefile of planning unit boundaries for Big Cypress National Preserve and Everglades National Park
#' \describe{
#' \item{Park_Code}{tbd}
#' \item{PlanningUn}{tbd}
#' \item{FMU_Name}{tbd}
#' \item{FMU_Code}{tbd}
#' \item{Shape_Leng}{tbd}
#' \item{Shape_Area}{tbd}
#' \item{geometry}{tbd}
#'}
#' @docType data
#' @keywords planning units
#' @name BICY_EVER_PlanningUnits
#' @usage BICY_EVER_PlanningUnits
#' @examples 
#' plot(BICY_EVER_PlanningUnits)
#' 
#' \dontrun{
#' ### code used to generate object using shapefile provided by NCSU
#' BICY_EVER_PlanningUnits <- sf::st_read("BICY_EVER_PlanningUnits_EDEN_py.shp") 
#' }
#' 
NULL
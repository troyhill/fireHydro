#' Data: Vegetation shapefile for Big Cypress National Preserve and Everglades National Park
#'
#'
#'
#' @format A shapefile of vegetation in Big Cypress National Preserve and Everglades National Park
#' \describe{
#' \item{NAME_SITE}{tbd}
#' \item{NAME_STATE}{tbd}
#' \item{Veg_Cat}{vegetation types (e.g., Coastal Forest, Brazilian Pepper/HID, Shrub etc.)}
#' \item{Notes}{tbd}
#' \item{Shape_Leng}{tbd}
#' \item{Shape_Area}{tbd}
#' \item{geometry}{tbd}
#' \item{FuelType}{Ranges between 1 and 5, where 1 represents low fire and 5 represents high fire risk}
#'}
#' @docType data
#' @keywords vegetation, fuel
#' @name vegetation
#' @usage vegetation
#' @examples 
#' plot(vegetation)
#' 
#' \dontrun{
#' ### code used to generate object using shapefile provided by NCSU
#' # vegetation          <- sf::st_read("CLC_Veg_exotic_Final_Updated_EDEN_py.shp") # "CLC_Veg_Exotic_Final_Updated.shp" not present in provided .zip; changed to "CLC_Veg_exotic_Final_Updated_EDEN_py.shp"
#' vegetation$FuelType <- revalue(vegetation$Veg_Cat, c("NA"=1,
#' "Coastal Forest"=2,
#' "Brazilian Pepper/HID" = 2,
#' "Shrub"=2,
#' "Hammock/Tree Island"=3,
#' "Short Sparse Grass"=3,
#' "Pine Savannah"=4,
#' "Short Continuous Grass"=4,
#' "Pine Forest"=5,
#'                                                     "Tall Continuous Grass"=5))                               # Reclassify vegetation/fuel classes into five fule categories
#' vegetation_reclass <- vegetation[, c("Veg_Cat", "FuelType")]                                              # Select needed parameters (e.g., vegetation class, fuel ranking etc.)
#' sf::st_write(vegetation_reclass, "analysis/outcomes/vegReclass.shp", delete_layer = TRUE)   
#'} }
#' 
NULL
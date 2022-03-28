#' Data: 2022 fire history shapefile
#'
#' @description A shapefile of fires in calendar year 2022 (through 25 March 2022) occurring in Big Cypress National Preserve and Everglades National Park
#'
#' @format A shapefile of fires in 2022 in Big Cypress National Preserve and Everglades National Park
#' @docType data
#' @keywords fire, burn history, fuel
#' @name fire_2022_cy
#' @usage fire_2022_cy
#' @examples 
#' 
#' \dontrun{
#' fireShps <- grep(x = list.files("C:/ESRIDATA/EVER_fireHistory/20220328_fireHistory", 
#' recursive = TRUE, full.names = TRUE), 
#' pattern = "shp$", value = TRUE)
#' fire_2022_cy <- sf::st_read(grep(x = fireShps, pattern = "_2022", value = TRUE))
#' 
#' fire_2022_cy <- st_transform(fire_2022_cy, crs = sf::st_crs(fireHydro::edenDEM))
#' save(fire_2022_cy, file = "data/fire_2022.RData")
#'}
#' 
"fire_2022_cy"
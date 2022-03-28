#' Data: 2020 fire history shapefile
#'
#' @description A shapefile of fires in calendar year 2020 occurring in Big Cypress National Preserve and Everglades National Park
#'
#' @format A shapefile of fires in 2020 in Big Cypress National Preserve and Everglades National Park
#' @docType data
#' @keywords fire, burn history, fuel
#' @name fire_2020_cy
#' @usage fire_2020_cy
#' @examples 
#' 
#' \dontrun{
#' fireShps <- grep(x = list.files("C:/ESRIDATA/EVER_fireHistory/20220328_fireHistory", 
#' recursive = TRUE, full.names = TRUE), 
#' pattern = "shp$", value = TRUE)
#' fire_2020_cy <- sf::st_read(grep(x = fireShps, pattern = "_2020", value = TRUE))
#' 
#' fire_2020_cy <- st_transform(fire_2020_cy, crs = sf::st_crs(fireHydro::edenDEM))
#' save(fire_2020_cy, file = "data/fire_2020.RData")
#'}
#' 
"fire_2020_cy"
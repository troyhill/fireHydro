#' Data: 2019 fire history shapefile
#'
#' @description A shapefile of fires in 2019 occurring in Big Cypress National Preserve and Everglades National Park
#'
#' @format A shapefile of fires in 2019 in Big Cypress National Preserve and Everglades National Park
#' @docType data
#' @keywords fire, burn history, fuel
#' @name fire192
#' @usage fire192
#' @examples 
#' 
#' \dontrun{
#' summary(fire192)
#' 
#' ### to create:
#' fireShps <- grep(x = list.files("C:/ESRIDATA/EVER_fireHistory", 
#'     full.names = TRUE), pattern = "shp$", value = TRUE)
#' fire17 <- st_read(fireShps[1])
#' fire18 <- st_read(fireShps[2])
#' fire19 <- st_read(fireShps[3])
#' 
#' st_transform(fire17, st_crs(a)$epsg)
#' st_transform(fire18, st_crs(a)$epsg)
#' st_transform(fire19, st_crs(a)$epsg)
#' 
#' fire17 <- fire17[, "OBJECTID"]
#' fire18 <- fire18[, "OBJECTID"] 
#' fire19 <- fire19[, "OBJECTID"]
#' fire17$burnYear <- "2017" 
#' fire18$burnYear <- "2018" 
#' fire19$burnYear <- "2019"
#' 
#' st_crs(fire17) <- st_crs(fire18) <- st_crs(fire19) <- st_crs(a)$epsg
#' 
#' fire172 <- sf::st_buffer(fire17, dist = 0)
#' fire182 <- sf::st_buffer(fire18, dist = 0)
#' fire192 <- sf::st_buffer(fire19, dist = 0)
#' 
#' save("fire172", "fire182", "fire192", file = "data/burnHistory.RData")
#' 
#'}
#' 
"fire192"
#' Data: 2018 fire history shapefile
#'
#' @description A shapefile of fires in 2018 occurring in Big Cypress National Preserve and Everglades National Park
#'
#' @format A shapefile of fires in 2018 in Big Cypress National Preserve and Everglades National Park
#' @docType data
#' @keywords fire, burn history, fuel
#' @name fire182
#' @usage fire182
#' @examples 
#' 
#' \dontrun{
#' summary(fire182)
#' 
#' ### if crs check is desirable
#' # edenDat       <- getEDEN() 
#' # fireDat <- getFireHydro(EDEN_date         = edenDat$date, 
#' #                         EDEN_GIS_directory = edenDat$data)
#' 
#' 
#' ### to create:
#' fireShps <- grep(x = list.files("C:/ESRIDATA/fireHydro/fire_history_recd_2019", 
#'                   recursive = TRUE, full.names = TRUE), 
#'                   pattern = "shp$", value = TRUE)
#' layers_2017 <- grep(x = fireShps, pattern = "_2017|CY_17_ALL", value = TRUE)
#' layers_2018 <- grep(x = fireShps, pattern = "_2018|CY_18", value = TRUE)
#' layers_2019 <- grep(x = fireShps, pattern = "/2019/|/2019_2/", value = TRUE)
#' fire17 <- lapply(layers_2017, st_read, stringsAsFactors = FALSE)
#' fire18 <- lapply(layers_2018, st_read, stringsAsFactors = FALSE)
#' fire19 <- lapply(layers_2019, st_read, stringsAsFactors = FALSE)
#' 
#' lapply(fire17, sf::st_transform, "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs") 
#' lapply(fire18, sf::st_transform, "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs") 
#' # works as long as crs of fire19[[1]] == "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"
#' fire19[[2]] <- sf::st_transform(x = fire19[[2]], crs = sf::st_crs(fire19[[1]])) 
#' 
#' names17 <- names(fire17[[1]])[names(fire17[[1]]) %in% names(fire17[[2]])]
#' union17 <- rbind(fire17[[1]][, names17], fire17[[2]][, names17])
#' 
#' names18 <- names(fire18[[1]])[names(fire18[[1]]) %in% names(fire18[[2]])]
#' union18 <- rbind(fire18[[1]][, names18], fire18[[2]][, names18])
#' 
#' names19 <- names(fire19[[1]])[names(fire19[[1]]) %in% names(fire19[[2]])]
#' union19 <- rbind(fire19[[1]][, names19], fire19[[2]][, names19])
#' 
#' fire172 <- sf::st_buffer(fire17, dist = 0)
#' fire182 <- sf::st_buffer(fire18, dist = 0)
#' fire192 <- sf::st_buffer(fire19, dist = 0)
#' 
#' fire172 <- sf::st_transform(fire172, sf::st_crs(fireHydro::edenDEM))
#' fire182 <- sf::st_transform(fire182, sf::st_crs(fireHydro::edenDEM))
#' fire192 <- sf::st_transform(fire192, sf::st_crs(fireHydro::edenDEM))
#' 
#' save("fire172", "fire182", "fire192", file = "data/burnHistory.RData")
#' 
#'}
#' 
"fire182"
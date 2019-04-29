library(fireHydro)
### fails
a2 <- getFireHydro(EDEN_date = "20181018", EDEN_GIS_directory = "EDEN_shp_20181018",
                   output_shapefile = "deleteMe.shp", returnShp = TRUE,
                   fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png")
###

a <- getFireHydro(EDEN_date = "20181018", EDEN_GIS_directory = "EDEN_shp_20181018",
                  output_shapefile = NULL, returnShp = TRUE,
                  fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png")

sf::st_write(obj = a, "deleteMe2.shp",
             delete_layer = TRUE, driver = "ESRI Shapefile")
a <- shp <- st_read("deleteMe2.shp")
ggplot() + geom_sf(data = shp, aes(fill = WL_des, col = WL_des), alpha = 1, lwd = 0, show.legend = FALSE) + theme_classic()


fireShps <- grep(x = list.files("C:/ESRIDATA/EVER_fireHistory", full.names = TRUE), pattern = "shp$", value = TRUE)

fire17 <- st_read(fireShps[1])
fire18 <- st_read(fireShps[2])
fire19 <- st_read(fireShps[3])

st_transform(fire17, st_crs(a)$epsg)
st_transform(fire18, st_crs(a)$epsg)
st_transform(fire19, st_crs(a)$epsg)


dataToPlot <- "WF_Use"
legendLabel   <- paste0("Fire Spread Risk \n", "18 Oct 2018")
legendPalette <- "Reds"
group.colors  <- c(`High Fire Spread Risk` = "indianred4", `Low Fire Spread Risk` = "ivory2")
dataLabels    <- names(group.colors)

### first version: simple overlay (requires a bit of squinting and eye-balling)
### TODO: make legend
ggplot() + geom_sf(data = a, aes(fill = as.character(get(dataToPlot)), col = as.character(get(dataToPlot))), alpha = 1, lwd = 0) + theme_bw(base_size = 12) + 
  ggplot2::geom_sf(data = fire17, alpha = 0.7, fill = "gray", 
                 lwd = 0.0, show.legend = FALSE) +
  ggplot2::geom_sf(data = fire18, alpha = 0.7, fill = "gray", 
                   lwd = 0.0, show.legend = FALSE) + 
  ggplot2::geom_sf(data = fire19, alpha = 0.7, fill = "gray", 
                   lwd = 0.0, show.legend = FALSE) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits, alpha = 0, col = "black", 
                   lwd = 0.05, show.legend = FALSE) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits[!BICY_EVER_PlanningUnits$FMU_Name %in% "Pinelands",], alpha = 0, col = "black", 
                   lwd = 0.25, show.legend = FALSE)+
  ggplot2::labs(fill = legendLabel) + 
  ggplot2::scale_fill_manual(values=group.colors, labels = dataLabels, drop = FALSE)  + 
  ggplot2::scale_colour_manual(values=group.colors, labels = dataLabels, guide = FALSE) 

ggplot2::ggsave(file = "fireSpread_burnAreas.png", width = 6, height = 4.5, units = "in")

### 2nd version: five categories of fire spread risk.
fire17 <- fire17[, "OBJECTID"]
fire18 <- fire18[, "OBJECTID"]
fire19 <- fire19[, "OBJECTID"]
fire17$burnYear <- "2017"
fire18$burnYear <- "2018"
fire19$burnYear <- "2019"

fireHist <- rbind(fire17, fire18, fire19)




### split high fire spread risk areas into categories based on burn year
group.colors  <- c(`High` = "indianred4", 
                   `Moderately High` = "indianred3",
                   `Moderate`        = "indianred2", 
                   `Moderately Low`  = "indianred1", 
                   `Low` = "ivory2")
dataLabels    <- names(group.colors)

st_crs(fire17) <- st_crs(fire18) <- st_crs(fire19) <- st_crs(a)$epsg

a2 <- sf::st_buffer(a, dist = 0)
fire172 <- sf::st_buffer(fire17, dist = 0)
fire182 <- sf::st_buffer(fire18, dist = 0)
fire192 <- sf::st_buffer(fire19, dist = 0)

# save("fire172", "fire182", "fire192", file = "data/burnHistory.RData")


highs <- a[a$WF_Use %in% "High Fire Spread Risk", ]




# # high17 <- high17[high17$WF_Use == "High Fire Spread Risk", ]
# # high17 <- st_intersection(fire172, highs)
# high18 <- st_intersection(fire182, highs)
# high19 <- st_intersection(fire192, highs)

# levels(a$WF_Use) <- c("High Fire Spread Risk", "Low Fire Spread Risk", "Moderately High")
# high17$WF_Use <- as.character(high17$WF_Use)
# high17[high17$WF_Use == "High Fire Spread Risk", "WF_Use"] <- "Moderately High"

# # high17$WF_Use <- factor(high17$WF_Use)
# # high17 <- high17[high17$WF_Use == "High Fire Spread Risk", ]
# high18 <- st_intersection(fire182, highs)
# high19 <- st_intersection(fire192, highs)

 

# test <- st_join(high17[, "OBJECTID"], a, join = st_intersects)
# names(test)
# table(test$WF_Use)

high17 <- st_intersection(a, fire172)
levels(high17$WF_Use) <- c("Moderately High", "Low")

high18 <- st_intersection(a2, fire182)
levels(high18$WF_Use) <- c("Moderate", "Low")

high19 <- st_intersection(a, fire192)
levels(high19$WF_Use) <- c("Moderately Low", "Low")


test <- rbind(high17, high18, high19)
table(test$WF_Use)



ggplot() + geom_sf(data = high17, aes(fill = get(dataToPlot), col = get(dataToPlot)), alpha = 1, lwd = 0) + theme_bw(base_size = 12) +
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits, alpha = 0, col = "black", 
                   lwd = 0.05, show.legend = FALSE) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits[!BICY_EVER_PlanningUnits$FMU_Name %in% "Pinelands",], alpha = 0, col = "black", 
                   lwd = 0.25, show.legend = FALSE) # +
  # ggplot2::labs(fill = legendLabel) +
  # ggplot2::scale_fill_manual(values=group.colors, labels = dataLabels, drop = FALSE)  +
  # ggplot2::scale_colour_manual(values=group.colors, labels = dataLabels, guide = FALSE)
# ggplot2::ggsave(file = "fireSpread_version2.png", width = 6, height = 4.5, units = "in")

levels(a$WF_Use) <- c("High", "Low")

levels(a$WF_Use) <- c(levels(a$WF_Use), dataLabels[!dataLabels %in% levels(a$WF_Use)]) 
a$WF_Use <- factor(a$WF_Use, levels = levels(a$WF_Use)[c(1, 3:5, 2)])
table(a$WF_Use)


ggplot() + geom_sf(data = a, aes(fill = get(dataToPlot), col = get(dataToPlot)), alpha = 1, lwd = 0) + theme_bw(base_size = 12)  +
  ggplot2::geom_sf(data = high17, alpha = 1,
                   aes(fill = get(dataToPlot), col = get(dataToPlot)),
                   lwd = 0.0, show.legend = FALSE)  +
  ggplot2::geom_sf(data = high18, alpha = 1,
                   aes(fill = get(dataToPlot), col = get(dataToPlot)),
                   lwd = 0.0, show.legend = FALSE)  +
  ggplot2::geom_sf(data = high19, alpha = 1,
                   aes(fill = get(dataToPlot), col = get(dataToPlot)),
                   lwd = 0.0, show.legend = FALSE) +
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits, alpha = 0, col = "black", 
                   lwd = 0.05, show.legend = FALSE) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits[!BICY_EVER_PlanningUnits$FMU_Name %in% "Pinelands",], alpha = 0, col = "black", 
                   lwd = 0.25, show.legend = FALSE) +
  ggplot2::labs(fill = legendLabel) +
  ggplot2::scale_fill_manual(values=group.colors, labels = dataLabels, drop = FALSE)  +
  ggplot2::scale_colour_manual(values=group.colors, labels = dataLabels, guide = FALSE)
# ggplot2::ggsave(file = "fireSpread_version3.png", width = 6, height = 4.5, units = "in")





sh1  <- 9134 
p1   <- 10.69
tot1 <- sh1 * p1

tot2 <- 15000
p2   <- 2.5
sh2  <- tot2 / p2

(tot2 + tot1) / (sh1 + sh2)

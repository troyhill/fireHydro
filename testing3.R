devtools::install_github("troyhill/fireHydro")

library(fireHydro)


getFireHydro(EDEN_date = "20190429", 
                  output_shapefile = NULL, returnShp = FALSE, burnHist = TRUE,
                  fireSpreadExport = "fireRisk_20190429.png", waterLevelExport = "waterLevels_20190429.png")


a <- getFireHydro(EDEN_date = "20181018", EDEN_GIS_directory = "EDEN_shp_20181018",
                  output_shapefile = NULL, returnShp = TRUE,
                  fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png")
beepr::beep(2)
a.bkp <- a

dataLabels    <- riskNames <- c("High", "Moderately High", "Moderate", "Moderately Low", "Low")


# `High` = "#FFC5D0", 
# `Moderately High` = "#FFE6EA",
# `Moderate`        = "#FFDB99", 
# `Moderately Low`  = "#BBDEB1", 
# `Low` = "#9BE0E4"


# `High` = "firebrick",
# `Moderately High` = "darkorange1",
# `Moderate`        = "yellow3",
# `Moderately Low`  = "deepskyblue2",
# `Low` = "forestgreen"

dataToPlot <- "WF_Use"
legendLabel   <- paste0("Fire Spread Risk \n", "18 Oct 2018")


a2 <- sf::st_buffer(a, dist = 0)

high17 <- st_intersection(a, fire172)
high17$WF_Use <- factor(high17$WF_Use)
levels(high17$WF_Use) <- c(riskNames[2], riskNames[length(riskNames)])

high18 <- st_intersection(a2, fire182)
high18$WF_Use <- factor(high18$WF_Use)
levels(high18$WF_Use) <- c(riskNames[3], riskNames[length(riskNames)])

high19 <- st_intersection(a, fire192)
high19$WF_Use <- factor(high19$WF_Use)
levels(high19$WF_Use) <- c(riskNames[4], riskNames[length(riskNames)])

a$WF_Use <- factor(a$WF_Use)
levels(a$WF_Use) <- c(riskNames[1], riskNames[length(riskNames)])
levels(a$WF_Use) <- c(levels(a$WF_Use), dataLabels[!dataLabels %in% levels(a$WF_Use)]) 
a$WF_Use <- factor(a$WF_Use, levels = riskNames)
table(a$WF_Use)

group.colors  <- c(
  `High` = "#FFC5D0",
  `Moderately High` = "#FFE6EA",
  `Moderate`        = "#FFDB99",
  `Moderately Low`  = "#BBDEB1",
  `Low` = "#9BE0E4"
)
group.colors  <- c(
  `High` = "brown4",
  `Moderately High` = "darkorange1",
  `Moderate`        = "yellow3",
  `Moderately Low`  = "deepskyblue2",
  `Low` = "chartreuse4"
)


# ## display a palettes simultanoeusly
# display.brewer.all(n=10, exact.n=FALSE)

group.colors  <- c(
  `High` = RColorBrewer::brewer.pal(9, "Reds")[4],
  `Moderately High` = RColorBrewer::brewer.pal(9, "Oranges")[4],
  `Moderate`        = RColorBrewer::brewer.pal(9, "YlOrBr")[2],
  `Moderately Low`  = RColorBrewer::brewer.pal(9, "Blues")[4],
  `Low` = RColorBrewer::brewer.pal(9, "Greens")[4]
)

dataLabels    <- names(group.colors)

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
# ggplot2::ggsave(file = "/opt/physical/troy/RDATA/fireSpread_version5.png", width = 6, height = 4.5, units = "in")



dataLabels    <- unique(a$WL_des_colors)[order(as.numeric(unique(a$WaterLevel)))]

waterLevelLabels <- c("Above Surface: >4 ft"         = RColorBrewer::brewer.pal(9, "Blues")[4],         
                      "Above Surface: 3-4 ft"        = RColorBrewer::brewer.pal(9, "Greens")[4],   
                      "Above Surface: 1.6-3 ft"      = "green4",    
                      "Above Surface: 0.6-1.6 ft"    = RColorBrewer::brewer.pal(9, "YlOrBr")[2],    
                      "Above Surface: 0-0.6 ft"      = RColorBrewer::brewer.pal(9, "YlOrBr")[4], 
                      "Below Surface: -0.6-0 ft"     = RColorBrewer::brewer.pal(9, "Oranges")[4], 
                      "Below Surface: -1 to -0.6 ft" = RColorBrewer::brewer.pal(9, "Oranges")[6],
                      "Below Surface: < -1 ft"       = RColorBrewer::brewer.pal(9, "Reds")[4])


# group.colors  <- c("7" = "firebrick",
#                    "6" = "orangered3",
#                    "5" = "orange",
#                    "4" = "yellow3",  # new category introduced 20190222
#                    "3" = "yellow1",
#                    "2" = "green4",
#                    "1" = "lightseagreen",
#                    "0" = "cornflowerblue")

group.colors  <- c("7" = RColorBrewer::brewer.pal(9, "Reds")[4],
                   "6" = RColorBrewer::brewer.pal(9, "Oranges")[6],
                   "5" = RColorBrewer::brewer.pal(9, "Oranges")[4],
                   "4" = RColorBrewer::brewer.pal(9, "YlOrBr")[4],  # new category introduced 20190222
                   "3" = RColorBrewer::brewer.pal(9, "YlOrBr")[2],
                   "2" = "green4",
                   "1" = RColorBrewer::brewer.pal(9, "Greens")[4],
                   "0" = RColorBrewer::brewer.pal(9, "Blues")[4])

dataLabels    <- unique(a$WL_des_colors)[order(as.numeric(unique(a$WaterLevel)))]

# group.colors$WaterLevel <- factor(eden_epaNveg_planningUnits$WaterLevel, levels=unique(eden_epaNveg_planningUnits$WaterLevel[order(eden_epaNveg_planningUnits$WaterLevel)]), ordered=TRUE)
# group.colors$WaterLevel <- unique(eden_epaNveg_planningUnits$WL_des)[order(as.numeric(unique(eden_epaNveg_planningUnits$WaterLevel)))]

ggplot2::ggplot() + ggplot2::geom_sf(data = a, ggplot2::aes(fill = WL_des, col = WL_des), lwd = 0, alpha = 1) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits, alpha = 0, col = "black", 
                   lwd = 0.05, show.legend = FALSE) + 
  ggplot2::geom_sf(data = BICY_EVER_PlanningUnits[!BICY_EVER_PlanningUnits$FMU_Name %in% "Pinelands",], alpha = 0, col = "black", 
                   lwd = 0.25, show.legend = FALSE) + 
  ggplot2::theme_bw(base_size = 12) + ggplot2::labs(fill = "water levels") + 
  ggplot2::scale_fill_manual(values = waterLevelLabels, labels = dataLabels, drop = FALSE)  + 
  ggplot2::scale_colour_manual(values = waterLevelLabels, labels = dataLabels, guide = FALSE) 




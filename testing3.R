devtools::install_github("troyhill/fireHydro")

library(fireHydro)


a <- getFireHydro(EDEN_date = "20181018", EDEN_GIS_directory = "EDEN_shp_20181018",
                  output_shapefile = NULL, returnShp = TRUE,
                  fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png")
beepr::beep(2)

riskNames <- c("High", "Moderately High", "Moderate", "Moderately Low", "Low")

group.colors  <- c(`High` = "indianred4", 
                   `Moderately High` = "indianred3",
                   `Moderate`        = "indianred2", 
                   `Moderately Low`  = "indianred1", 
                   `Low` = "ivory2")
dataLabels    <- names(group.colors)
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
# ggplot2::ggsave(file = "fireSpread_version4.png", width = 6, height = 4.5, units = "in")


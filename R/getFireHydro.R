#' @title Generate fire-hydro shapefile
#'
#' @description Generates shapefile showing fire potential. Presently only works on the SFNRC network (for EDEN data access)
#' 
#' @usage getFireHydro(EDEN_date, 
#'     output_shapefile = paste0(tempdir(), "/output_", EDEN_date, ".shp"), 
#'     waterLevelExport = NULL,
#'     fireSpreadExport = NULL,
#'     csvExport = NULL,
#'     EDEN_GIS_directory = "detect",
#'     vegetation_shp = fireHydro::vegetation,
#'     BICY_EVER_PlanningUnits_shp = fireHydro::BICY_EVER_PlanningUnits,
#'     returnShp = FALSE,
#'     figureWidth = 6,
#'     figureHeight = 4.5,
#'     ggBaseSize = 20)
#' 
#' @param EDEN_date EDEN date to be used for water levels. Should be a character stirng, e.g., "20181018"
#' @param output_shapefile file address for shapefile output
#' @param waterLevelExport NULL or a character vector specifying the file address/name used for exporting an image file of water level categories (e.g., /home/waterLevels.pdf).
#' @param fireSpreadExport NULL or a character vector specifying the file address/name used for exporting an image file of fire spread risk (e.g., /home/fireSpreadRisk.pdf).
#' @param csvExport If an exported .csv file of the output is desired, include a file addess/name here (e.g., "fireHydroOutput.csv")
#' @param EDEN_GIS_directory The source for EDEN data. For users with access to the SFNRC's physical drive, the default value (\code{"detect"}) will identify the parent directory where EDEN water level data are located ("/opt/physical/gis/eden/" on linux; "Y:/gis/eden/" on Windows). This can alternative be the specific address of a shapefile of EDEN data. This can also be a character string naming an object in the working environment. 
#' @param vegetation_shp shapefile of vegetation data in Big Cypress and Everglades
#' @param BICY_EVER_PlanningUnits_shp shapefile of polygons representing Big Cypress and Everglades planning units
#' @param returnShp TRUE/FALSE determinant of whether output is returned to the working environment
#' @param figureWidth width of output figure, in inches
#' @param figureHeight height of output figure, in inches 
#' @param ggBaseSize base_size argument passed to ggplot theme. 
#' @return dataframe \code{getFireHydro} produces a shapefile.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' getFireHydro(EDEN_date = "20181018", 
#'      output_shapefile = NULL,
#'      fireSpreadExport = "fireRisk.png", waterLevelExport = "waterLevels.png")
#' }
#' 
#' @importFrom utils write.csv
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_intersection
#' @importFrom sf st_write
#' @importFrom sf st_set_geometry
#' @importFrom plyr revalue
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom rgdal setCPLConfigOption
#' @importFrom rgdal writeGDAL
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_brewer
#' @importFrom ggplot2 scale_colour_brewer
#' 
#' @export


getFireHydro <- function(EDEN_date, 
                         output_shapefile = paste0(tempdir(), "/output_", EDEN_date, ".shp"), 
                         waterLevelExport = NULL,
                         fireSpreadExport = NULL,
                         csvExport = NULL, 
                         EDEN_GIS_directory = "detect",
                         vegetation_shp = fireHydro::vegetation,
                         BICY_EVER_PlanningUnits_shp = fireHydro::BICY_EVER_PlanningUnits,
                         returnShp = FALSE, figureWidth = 6, figureHeight = 4, 
                         ggBaseSize = 12) {
  ### TODO:
  ### supply example EDEN data for testing
  ### avoid warnings from st_intersect http://r-sig-geo.2731867.n2.nabble.com/Warning-in-st-intersection-td7591290.html https://github.com/r-spatial/sf/issues/406
  ### un-pack piped statements
  ### user specifies what's displayed in the output?
  ### what's up with the shapefiles that used to be exported - are they useful? should they have export options? 
  
  planningUnits_shp <- sf::st_union(BICY_EVER_PlanningUnits_shp)
  
  ### argument to auto-generate output 
  # output_shapefile <- paste0("analysis/outcomes/fireRisk_area_", EDEN_date, ".csv")
  # outputCsv  <- paste0("analysis/outcomes/fireRisk_area_", EDEN_date, ".csv")
  
  
  
  ### adjust EDEN directory for operating system
  if (EDEN_GIS_directory == "detect") {
    switch(Sys.info()[['sysname']],
           Windows= {EDEN_GIS_directory <- "Y:/gis/eden/"},
           Linux  = {EDEN_GIS_directory <- "/opt/physical/gis/eden/"},
           Darwin = {stop("EDEN data parent directory address is not automatically identified for Mac OS.")})
    eden_epa               <- sf::st_read(paste0(EDEN_GIS_directory, substr(EDEN_date, 1, 4), "/eden_epa", EDEN_date, ".shp"))
  } else if (grepl(x = EDEN_GIS_directory, pattern = "shp$")) {
    eden_epa               <- sf::st_read(EDEN_GIS_directory)
  } else if (exists(EDEN_GIS_directory)) {
    if (any(class(get(EDEN_GIS_directory)) %in% "sf")) { # if EDEN data are already a SIMPLE FEATURE object in workspace
      eden_epa               <- get(EDEN_GIS_directory)
    }
  } else {
   stop("EDEN_GIS_DIRECTORY argument appears to be invalid. It is not an sf object in current working environment")
  }
  

  ### Read EDEN EPA hydro data                    
  # ye olde version (pre-20190222): eden_epa$WaterLevel    <- c(6, 5, 4, 3, 2, 1, 0)[findInterval(eden_epa$WaterDepth, c(-Inf, -30.48, -18.288, 0, 48.768, 91.44, 121.92, Inf))]   # Rank water depth
  eden_epa$WaterLevel    <- c(7, 6, 5, 4, 3, 2, 1, 0)[findInterval(eden_epa$WaterDepth, c(-Inf, -30.48, -18.288, 0, 18.288, 48.768, 91.44, 121.92, Inf))]   # Rank water depth
  eden_epaGroup          <- eden_epa %>% dplyr::group_by(WaterLevel) %>% dplyr::summarize(sum=sum(WaterDepth))                         # Dissovle grid to minize the file size
  eden_epaGroupPrj       <- sf::st_transform(eden_epaGroup, sf::st_crs(planningUnits_shp))                                   # Reproject dissolved grid to park boundary
  eden_epa_reclass       <- eden_epaGroupPrj[,"WaterLevel"]
  
  # warning message: attribute variables are assumed to be spatially constant throughout all geometries 
  withCallingHandlers( # takes a long time
    eden_epa_reclass <- sf::st_intersection(eden_epa_reclass, planningUnits_shp), warning = fireHydro::intersectionWarningHandler)                                 # Clip the EDEN EPA hydro using the park boundary
  
  ### Combine EDEN EPA hydro and fuel types
  vegetation_reclass <- vegetation_shp[, c("Veg_Cat", "FuelType")]     
  withCallingHandlers(
    eden_epaNveg        <- sf::st_intersection(st_buffer(vegetation_reclass,0), eden_epa_reclass), warning = fireHydro::intersectionWarningHandler)
  eden_epaNveg$WF_Use <-ifelse(eden_epaNveg$FuelType == 5 & eden_epaNveg$WaterLevel >= 0, "High Fire Spread Risk",
                               ifelse(eden_epaNveg$FuelType == 4 & eden_epaNveg$WaterLevel >= 1, "High Fire Spread Risk",
                                      ifelse(eden_epaNveg$FuelType == 3 & eden_epaNveg$WaterLevel >= 5, "High Fire Spread Risk", # changed  waterLevel threshold from 4 to 5 on 20190222
                                             ifelse(eden_epaNveg$FuelType == 2 & eden_epaNveg$WaterLevel > 5, "High Fire Spread Risk", "Low Fire Spread Risk")))) # changed  waterLevel threshold from 4 to 5 on 20190222
  
  eden_epaNveg$RX_Use <-ifelse(eden_epaNveg$WF_Use == "High Fire Spread Risk ", "High Fuel Availability", "Low Fuel Availability")
  
  
  ### Combine fireRisk data with planning units
  # st_intersection warning: attribute variables are assumed to be spatially constant throughout all geometries
  withCallingHandlers( # takes a long time
    eden_epaNveg_planningUnits         <- sf::st_intersection(eden_epaNveg, BICY_EVER_PlanningUnits_shp[, c("PlanningUn", "FMU_Name")]), warning = fireHydro::intersectionWarningHandler)
  eden_epaNveg_planningUnits$WL_des         <- plyr::revalue(as.factor(eden_epaNveg_planningUnits$WaterLevel), c("0" = "Above Surface: >4 ft",      "1" = "Above Surface: 3-4 ft",          "2" = "Above Surface: 1.6-3 ft",    "3" = "Above Surface: 0.6-1.6 ft",    "4" = "Above Surface: 0-0.6 ft", "5" = "Below Surface: -0.6-0 ft", "6" = "Below Surface: -1 to -0.6 ft", "7" = "Below Surface: < -1 ft" ))
  eden_epaNveg_planningUnits$WL_des_colors  <- plyr::revalue(as.factor(eden_epaNveg_planningUnits$WaterLevel), c("0" = "cornflowerblue", "1" = "lightseagreen", "2" = "green4", "3" = "yellow1", "4" = "yellow3",  "5" = "orange",                   "6" = "orangered3",                  "7" = "firebrickred" ))
  eden_epaNveg_planningUnits$area    <- sf::st_area(eden_epaNveg_planningUnits) * 0.000247105
  
  
  ### Create a summary table of fire risk area for each planning unit        
  keyVars_df <- eden_epaNveg_planningUnits %>% sf::st_set_geometry(NULL)                                                # Drop geometry for summing each column for total values
  planFMUs   <- keyVars_df %>% dplyr::group_by(PlanningUn, FMU_Name, WF_Use) %>% dplyr::summarize(area_acres=sum(area))                 # Summarize data (mean) by planning units
  is.num     <- sapply(planFMUs, is.numeric)                                                                        
  planFMUs[is.num] <- lapply(planFMUs[is.num], round, 2)
  
  
  ### export as shapefile
  if (!is.null(output_shapefile)) { # nocov start
    # sf::st_write(obj = eden_epaNveg_planningUnits, output_shapefile, delete_layer = TRUE, driver="ESRI Shapefile") 
    sf::st_write(obj = eden_epaNveg_planningUnits, dsn = output_shapefile, delete_layer = TRUE, update = FALSE, delete_dsn = TRUE)
    # rgdal::writeOGR(eden_epaNveg_planningUnits, output_shapefile, driver="ESRI Shapefile")
    # rgdal::writeOGR(eden_epaNveg_planningUnits, output_shapefile, driver="GPKG")
  }
  ### export as csv
  if (!is.null(csvExport)) { # nocov start
    utils::write.csv(planFMUs, file = csvExport, row.names = FALSE)       
  }
  ### export as image
   if (!is.null(waterLevelExport)) {
      dataToPlot <- "WL_des"
      group.colors  <- as.character(eden_epaNveg_planningUnits$WaterLevel)
      dataToPlot    <- "WaterLevel"
      dataLabels    <- unique(eden_epaNveg_planningUnits$WL_des)[order(as.numeric(unique(eden_epaNveg_planningUnits$WaterLevel)))]
      legendLabel   <- paste0("Water Levels\n", EDEN_date)
      legendPalette <- "Blues"
      group.colors  <- c("7" = "firebrick",
                         "6" = "orangered3",
                         "5" = "orange",
                         "4" = "yellow3",  # new category introduced 20190222
                         "3" = "yellow1",
                         "2" = "green4",
                         "1" = "lightseagreen",
                         "0" = "cornflowerblue")
      # group.colors$WaterLevel <- factor(eden_epaNveg_planningUnits$WaterLevel, levels=unique(eden_epaNveg_planningUnits$WaterLevel[order(eden_epaNveg_planningUnits$WaterLevel)]), ordered=TRUE)
      # group.colors$WaterLevel <- unique(eden_epaNveg_planningUnits$WL_des)[order(as.numeric(unique(eden_epaNveg_planningUnits$WaterLevel)))]
      
      ggplot2::ggplot() + ggplot2::geom_sf(data = eden_epaNveg_planningUnits, ggplot2::aes(fill = as.character(get(dataToPlot)), col = as.character(get(dataToPlot))), lwd = 0, alpha = 1) + 
        ggplot2::geom_sf(data = BICY_EVER_PlanningUnits_shp, alpha = 0, col = "black", lwd = 0.5, show.legend = FALSE) + 
        ggplot2::theme_bw(base_size = ggBaseSize) + ggplot2::labs(fill = legendLabel) + 
        ggplot2::scale_fill_manual(values=group.colors, labels = dataLabels, drop = FALSE)  + 
        ggplot2::scale_colour_manual(values=group.colors, labels = dataLabels, guide = FALSE) 
      
      # ggplot2::scale_fill_brewer(palette = legendPalette, direction=-1) +  ggplot2::scale_colour_brewer(palette= legendPalette, direction = -1, guide = "none")
      ggplot2::ggsave(file = waterLevelExport, width = figureWidth, height = figureHeight, units = "in")
      
    } 
    
      
    if (!is.null(fireSpreadExport)) {
      dataToPlot <- "WF_Use"
      legendLabel   <- paste0("Fire Spread Risk \n", EDEN_date)
      legendPalette <- "Reds"
      group.colors  <- c(`High Fire Spread Risk` = "firebrick", `Low Fire Spread Risk` = "ivory3")
      dataLabels    <- names(group.colors)
      
      ggplot2::ggplot() + ggplot2::geom_sf(data = eden_epaNveg_planningUnits, ggplot2::aes(fill = as.character(get(dataToPlot)), col = as.character(get(dataToPlot))), lwd = 0, alpha = 1) + 
        ggplot2::geom_sf(data = BICY_EVER_PlanningUnits_shp, alpha = 0, col = "black", lwd = 0.5, show.legend = FALSE) + 
        ggplot2::theme_bw(base_size = ggBaseSize) + ggplot2::labs(fill = legendLabel) + 
        ggplot2::scale_fill_manual(values=group.colors, labels = dataLabels, drop = FALSE)  + 
        ggplot2::scale_colour_manual(values=group.colors, labels = dataLabels, guide = FALSE) 
      
      # ggplot2::scale_fill_brewer(palette = legendPalette, direction=-1) +  ggplot2::scale_colour_brewer(palette= legendPalette, direction = -1, guide = "none")
      ggplot2::ggsave(file = fireSpreadExport, width = figureWidth, height = figureHeight, units = "in")
      
    }
      
  # nocov end
  if (returnShp) {
    invisible(eden_epaNveg_planningUnits)
  }
}

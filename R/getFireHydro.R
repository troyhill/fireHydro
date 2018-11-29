#' @title Generate fire-hydro shapefile
#'
#' @description Generates shapefile showing fire potential. Presently only works on the SFNRC network (for EDEN data access)
#' 
#' @usage getFireHydro(EDEN_date, 
#' output_shapefile = paste0(tempdir(), "/output_", EDEN_date, ".shp"), 
#' imageExport = NULL, csv = NULL,
#' EDEN_GIS_directory = "detect",
#'                          vegetation_shp = vegetation,
#'                          BICY_EVER_PlanningUnits_shp = BICY_EVER_PlanningUnits)
#' 
#' @param EDEN_date EDEN date to be used for water levels. Should be a character stirng, e.g., "20181018"
#' @param output_shapefile file address for shapefile output
#' @param imageExport If an image output is desired, include a file addess/name here (e.g., "fireHydroOutput.png" or "fireHydroOutput.pdf").
#' @param csv If a .csv table of the output is desired, include a file addess/name here (e.g., "fireHydroOutput.csv")
#' @param EDEN_GIS_directory The source for EDEN data. For users with access to the SFNRC's physical drive, the default value (\code{"detect"}) will identify the parent directory where EDEN water level data are located ("/opt/physical/gis/eden/" on linux; "Y:/gis/eden/" on Windows). This can alternative be the specific address of a shapefile of EDEN data.
#' @param vegetation_shp shapefile of vegetation data in Big Cypress and Everglades
#' @param BICY_EVER_PlanningUnits_shp shapefile of polygons representing Big Cypress and Everglades planning units
#' 
#' @return dataframe \code{getFireHydro} produces a shapefile.
#' 
#' 
#' @examples
#' 
#' \dontrun{
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


getFireHydro <- function(EDEN_date, output_shapefile = paste0(tempdir(), "/output_", EDEN_date, ".shp"), 
                         imageExport = NULL, csv = NULL, EDEN_GIS_directory = "detect",
                         vegetation_shp = vegetation,
                         BICY_EVER_PlanningUnits_shp = BICY_EVER_PlanningUnits) {
  ### TODO:
  ### supply example EDEN data for testing
  ### un-pack piped statements
  ### user specifies what's displayed in the output?
  ### what's up with the shapefiles that used to be exported - are they useful? should they have export options? 
  
  planningUnits_shp <- sf::st_union(BICY_EVER_PlanningUnits_shp)
  
  ### argument to auto-generate output 
  # output_shapefile <- paste0("analysis/outcomes/fireRisk_area_", EDEN_date, ".csv")
  # outputCsv  <- paste0("analysis/outcomes/fireRisk_area_", EDEN_date, ".csv")
  
  if (grepl(x = EDEN_GIS_directory, pattern = "shp$")) {
    eden_epa               <- sf::st_read(EDEN_GIS_directory)
  }
  
  ### adjust EDEN directory for operating system
  if (EDEN_GIS_directory == "detect") {
    switch(Sys.info()[['sysname']],
           Windows= {EDEN_GIS_directory <- "Y:/gis/eden/"},
           Linux  = {EDEN_GIS_directory <- "/opt/physical/gis/eden/"},
           Darwin = {stop("EDEN data parent directory address is not automatically identified for Mac OS.")})
    eden_epa               <- sf::st_read(paste0(EDEN_GIS_directory, substr(EDEN_date, 1, 4), "/eden_epa", EDEN_date, ".shp"))
  }

  ### Read EDEN EPA hydro data                    
  # eden_epa <-st_read("eden_epa20181018.shp") # file not provided. Verify that no processing of EDEN data is necessary. 
  # edenReclassFileName  <- paste0("analysis/outcomes/eden_epa", EDEN_date, "Reclass.shp")
  eden_epa$WaterLevel    <- c(5, 4, 3, 2, 1, 0)[findInterval(eden_epa$WaterDepth, c(-Inf, -30.48, 0, 48.768, 91.44, 121.92, Inf))]   # Rank water depth
  eden_epaGroup          <- eden_epa %>% dplyr::group_by(WaterLevel) %>% dplyr::summarize(sum=sum(WaterDepth))                         # Dissovle grid to minize the file size
  eden_epaGroupPrj       <- sf::st_transform(eden_epaGroup, sf::st_crs(planningUnits_shp))                                   # Reproject dissolved grid to park boundary
  eden_epa_reclass       <- eden_epaGroupPrj[,"WaterLevel"]
  
  # warning message: attribute variables are assumed to be spatially constant throughout all geometries 
  eden_epa_reclass <- sf::st_intersection(eden_epa_reclass, planningUnits_shp)                                 # Clip the EDEN EPA hydro using the park boundary
  # sf::st_write(eden_epa_reclass, edenReclassFileName, delete_layer = TRUE)
  
  
  ### Combine EDEN EPA hydro and fuel types
  # edenVegFileName <- paste0("analysis/outcomes/fireRisk_fuelAvailability_", EDEN_date, ".shp")
  vegetation_reclass <- vegetation_shp[, c("Veg_Cat", "FuelType")]     
  eden_epaNveg        <- sf::st_intersection(st_buffer(vegetation_reclass,0), eden_epa_reclass)
  # sf::st_write(eden_epaNveg, paste0("analysis/outcomes/eden_epa", EDEN_date, "_vegReclass.shp"), delete_layer = TRUE)
  eden_epaNveg$WF_Use <-ifelse(eden_epaNveg$FuelType == 5 & eden_epaNveg$WaterLevel >= 0, "High Fire Spread Risk ",
                               ifelse(eden_epaNveg$FuelType == 4 & eden_epaNveg$WaterLevel >= 1, "High Fire Spread Risk ",
                                      ifelse(eden_epaNveg$FuelType == 3 & eden_epaNveg$WaterLevel >= 4, "High Fire Spread Risk ",
                                             ifelse(eden_epaNveg$FuelType == 2 & eden_epaNveg$WaterLevel > 4, "High Fire Spread Risk ", "Low Fire Spread Risk"))))
  
  eden_epaNveg$RX_Use <-ifelse(eden_epaNveg$WF_Use == "High Fire Spread Risk ", "High Fuel Availability", "Low Fuel Availability")
  # sf::st_write(eden_epaNveg, edenVegFileName, delete_layer = TRUE)
  
  
  ### Combine fireRisk data with planning units
  # st_intersection warning: attribute variables are assumed to be spatially constant throughout all geometries
  eden_epaNveg_planningUnits         <- sf::st_intersection(eden_epaNveg, BICY_EVER_PlanningUnits_shp[, c("PlanningUn", "FMU_Name")])
  eden_epaNveg_planningUnits$WL_des  <- plyr::revalue(as.factor(eden_epaNveg_planningUnits$WaterLevel), c("0" = "Very high", "1" = "High", "2" = "Low", "3" = "Very low", "4" = "Just below surface ", "5" = "Well below surface" ))
  eden_epaNveg_planningUnits$area    <- sf::st_area(eden_epaNveg_planningUnits) * 0.000247105
  
  
  ### Create a summary table of fire risk area for each planning unit        
  keyVars_df <- eden_epaNveg_planningUnits %>% sf::st_set_geometry(NULL)                                                # Drop geometry for summing each column for total values
  planFMUs   <- keyVars_df %>% dplyr::group_by(PlanningUn, FMU_Name, WF_Use) %>% dplyr::summarize(area_acres=sum(area))                 # Summarize data (mean) by planning units
  is.num     <- sapply(planFMUs, is.numeric)                                                                        
  planFMUs[is.num] <- lapply(planFMUs[is.num], round, 2)
  
  
  ### export shapefile
  sf::st_write(obj = eden_epaNveg_planningUnits, output_shapefile, delete_layer = TRUE, driver="ESRI Shapefile")
  # rgdal::writeOGR(eden_epaNveg_planningUnits, output_shapefile, driver="ESRI Shapefile")
  
  if (!is.null(csv)) {
    utils::write.csv(planFMUs, file = csv, row.names = FALSE)       
  }
 
  if (!is.null(imageExport)) {
  ### output as png using rgdal:
  ### https://stackoverflow.com/questions/44547626/create-png-using-writegdal-without-georeference-aux-xml
    ggplot2::ggplot() + ggplot2::geom_sf(data = eden_epaNveg_planningUnits, ggplot2::aes(fill = WL_des, colour = WL_des), lwd = 0 ,alpha = 0.8) + 
      ggplot2::theme_bw() + ggplot2::labs(fill = "Water level category") + 
      ggplot2::scale_fill_brewer(palette="Blues", direction=-1) +  ggplot2::scale_colour_brewer(palette="Blues", direction = -1, guide = "none")
      # ggplot2::scale_fill_manual(values = rev(rainbow(5))) + ggplot2::scale_colour_manual(values = rev(rainbow(5)), guide = "none")
    ggplot2::ggsave(file = imageExport)
    # sf::st_write(obj = eden_epaNveg_planningUnits, imageExport, delete_layer = TRUE, driver="PDF")
    # rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
    # rgdal::writeGDAL(as(eden_epaNveg_planningUnits, 'SpatialPixelsDataFrame')[, 'WL_des'], imageExport, drivername = 'PNG', type = 'Byte', mvFlag = 0, colorTables = list(colorRampPalette(c('black', 'white'))(11)))
    # png::writePNG(eden_epaNveg_planningUnits[, 'WL_des'], imageExport) # alternative approach
    # base_family = "Roboto Condensed"
    

  }
}

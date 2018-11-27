### -----------------------------------------
### NPS Everglades fire and hydro analysis
### -----------------------------------------


### Install required R libraries if necessary
if(!require(sf)){
  install.packages("sf", repos='http://cran.us.r-project.org')
}
if(!require(plyr)){
  install.packages("plyr", repos='http://cran.us.r-project.org')
}
if(!require(dplyr)){
  install.packages("dplyr", repos='http://cran.us.r-project.org')
}
if(!require(here)){
  install.packages("here", repos='http://cran.us.r-project.org')
}
library(sf)                # Vector and raster analysis 
library(plyr)              # For the 'revalue' function
library(dplyr)             # For the 'group_by' function
library(here)


### set target date
targetDate <- "20181018"

### Locate all the require data
# setwd(here())
dir.create(here("analysis/outcomes"), showWarnings = FALSE, recursive = TRUE)

### Read vegetation types data
vegetation <- sf::st_read("CLC_Veg_exotic_Final_Updated_EDEN_py.shp") # "CLC_Veg_Exotic_Final_Updated.shp" not present in provided .zip; changed to "CLC_Veg_exotic_Final_Updated_EDEN_py.shp"
  vegetation$FuelType <- revalue(vegetation$Veg_Cat, c("NA"=1,                                                   
                                                       "Coastal Forest"=2,
                                                       "Brazilian Pepper/HID" = 2,      
                                                       "Shrub"=2,
                                                       "Hammock/Tree Island"=3,
                                                       "Short Sparse Grass"=3,
                                                       "Pine Savannah"=4,
                                                       "Short Continuous Grass"=4,
                                                       "Pine Forest"=5,
                                                       "Tall Continuous Grass"=5))                               # Reclassify vegetation/fuel classes into five fule categories
        vegetation_reclass <- vegetation[, c("Veg_Cat", "FuelType")]                                              # Select needed parameters (e.g., vegetation class, fuel ranking etc.)
          st_write(vegetation_reclass, "analysis/outcomes/vegReclass.shp", delete_layer = TRUE)   


### Read EDEN EPA hydro data                    
# eden_epa <-st_read("eden_epa20181018.shp") # file not provided. Verify that no processing of EDEN data is necessary. 
edenReclassFileName        <- paste0("analysis/outcomes/eden_epa", targetDate, "Reclass.shp")
eden_epa                   <- sf::st_read(paste0("/opt/physical/gis/eden/", substr(targetDate, 1, 4), "/eden_epa", targetDate, ".shp"))
  eden_epa$WaterLevel      <- c(5, 4, 3, 2, 1, 0)[findInterval(eden_epa$WaterDepth, c(-Inf, -30.48, 0, 48.768, 91.44, 121.92, Inf))]   # Rank water depth
    eden_epaGroup          <- eden_epa %>% group_by(WaterLevel) %>% summarize(sum=sum(WaterDepth))                         # Dissovle grid to minize the file size
      planningUnits        <- sf::st_union(sf::st_read("BICY_EVER_PlanningUnits_EDEN_py.shp")) # "BICY_EVER_PlanningUnits_EDEN.shp" is not provided. changed to "BICY_EVER_PlanningUnits_EDEN_py.shp"
        eden_epaGroupPrj   <- sf::st_transform(eden_epaGroup, sf::st_crs(planningUnits))                                   # Reproject dissolved grid to park boundary
          eden_epa_reclass <- eden_epaGroupPrj[,"WaterLevel"]
          
          # warning message: attribute variables are assumed to be spatially constant throughout all geometries 
            eden_epa_reclass <- sf::st_intersection(eden_epa_reclass, planningUnits)                                 # Clip the EDEN EPA hydro using the park boundary
            sf::st_write(eden_epa_reclass, edenReclassFileName, delete_layer = TRUE)


### Combine EDEN EPA hydro and fuel types
edenVegFileName <- paste0("analysis/outcomes/fireRisk_fuelAvailability_", targetDate, ".shp")
eden_epaNveg    <- sf::st_intersection(st_buffer(vegetation_reclass,0), eden_epa_reclass)
sf::st_write(eden_epaNveg, paste0("analysis/outcomes/eden_epa", targetDate, "_vegReclass.shp"), delete_layer = TRUE)
    eden_epaNveg$WF_Use <-ifelse(eden_epaNveg$FuelType == 5 & eden_epaNveg$WaterLevel >= 0, "High Fire Spread Risk ",
                                 ifelse(eden_epaNveg$FuelType == 4 & eden_epaNveg$WaterLevel >= 1, "High Fire Spread Risk ",
                                        ifelse(eden_epaNveg$FuelType == 3 & eden_epaNveg$WaterLevel >= 4, "High Fire Spread Risk ",
                                               ifelse(eden_epaNveg$FuelType == 2 & eden_epaNveg$WaterLevel > 4, "High Fire Spread Risk ", "Low Fire Spread Risk"))))
      
      eden_epaNveg$RX_Use <-ifelse(eden_epaNveg$WF_Use == "High Fire Spread Risk ", "High Fuel Availability", "Low Fuel Availability")
      sf::st_write(eden_epaNveg, edenVegFileName, delete_layer = TRUE)


### Combine fireRisk data with planning units
combinedFileName <-  paste0("analysis/outcomes/fireRisk_fuelAvailability_planningFMUs_", targetDate, ".shp")
BICY_EVER_PlanningUnits                <- sf::st_read("BICY_EVER_PlanningUnits_EDEN_py.shp") # verify that "_py" version is what they used
  # st_intersection warning: attribute variables are assumed to be spatially constant throughout all geometries
  eden_epaNveg_planningUnits           <- sf::st_intersection(eden_epaNveg, BICY_EVER_PlanningUnits[, c("PlanningUn", "FMU_Name")])
    eden_epaNveg_planningUnits$WL_des  <- revalue(as.factor(eden_epaNveg_planningUnits$WaterLevel), c("0" = "Very high", "1" = "High", "2" = "Low", "3" = "Very low", "4" = "Just below surface ", "5" = "Well below surface" ))
      eden_epaNveg_planningUnits$area  <- sf::st_area(eden_epaNveg_planningUnits)*0.000247105
      sf::st_write(eden_epaNveg_planningUnits, combinedFileName, delete_layer = TRUE)
        

### Create a summary table of fire risk area for each planning unit        
outputCsv  <- paste0("analysis/outcomes/fireRisk_area_", targetDate, ".csv")
keyVars_df <- eden_epaNveg_planningUnits %>% st_set_geometry(NULL)                                                # Drop geometry for summing each column for total values
  planFMUs <- keyVars_df %>% group_by(PlanningUn, FMU_Name, WF_Use) %>% summarize(areaHA=sum(area))                 # Summarize data (mean) by planning units
    is.num <- sapply(planFMUs, is.numeric)                                                                        
      planFMUs[is.num] <- lapply(planFMUs[is.num], round, 2)
        write.csv(planFMUs, file = outputCsv)        

                
###        
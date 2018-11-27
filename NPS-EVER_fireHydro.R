### -----------------------------------------
### NPS Everglades fire and hydro analysis
### -----------------------------------------


### Required R libraries

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
library(sf)                # Vector and raster anlaysis 
library(plyr)              # For the 'revalue' function
library(dplyr)             # For the 'group_by' fucntion
library(here)


### Locate all the require data
# setwd (dir="E:/npsHydro/EVERoutcomes")


### Read vegetation types data
vegetation <- st_read("CLC_Veg_Exotic_Final_Updated.shp")
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
        vegetation_reclass <- vegetation[,c("Veg_Cat", "FuelType")]                                              # Select needed parameters (e.g., vegetation class, fuel ranking etc.)
          st_write(vegetation_reclass, "analysis/outcomes/vegReclass.shp", delete_layer = TRUE)   


### Read EDEN EPA hydro data                    
eden_epa <-st_read("analysis/eden_epa20181018.shp")
  eden_epa$WaterLevel <-c(5, 4, 3, 2, 1, 0)[findInterval(eden_epa$WaterDepth, c(-Inf, -30.48, 0, 48.768, 91.44, 121.92, Inf))]   # Rank water depth
    eden_epaGroup <-eden_epa %>% group_by(WaterLevel) %>% summarize(sum=sum(WaterDepth))                         # Dissovle grid to minize the file size
      planningUnits <- st_union(st_read("analysis/BICY_EVER_PlanningUnits_EDEN.shp"))
        eden_epaGroupPrj <- st_transform(eden_epaGroup, st_crs(planningUnits))                                   # Reproject dissolved grid to park boundary
          eden_epa_reclass <- eden_epaGroupPrj[,"WaterLevel"]
            eden_epa_reclass <- st_intersection(eden_epa_reclass, planningUnits)                                 # Clip the EDEN EPA hydro using the park boundary
              st_write(eden_epa_reclass, "analysis/outcomes/eden_epa20181018Relcass.shp", delete_layer = TRUE)


### Combine EDEN EPA hydro and fuel types
eden_epaNveg <- st_intersection(st_buffer(vegetation_reclass,0), eden_epa_reclass)
  st_write(eden_epaNveg, "analysis/outcomes/eden_epa20181018_vegReclass.shp", delete_layer = TRUE)
    eden_epaNveg$WF_Use <-ifelse(eden_epaNveg$FuelType == 5 & eden_epaNveg$WaterLevel >= 0, "High Fire Spread Risk ",
                                 ifelse(eden_epaNveg$FuelType == 4 & eden_epaNveg$WaterLevel >= 1, "High Fire Spread Risk ",
                                        ifelse(eden_epaNveg$FuelType == 3 & eden_epaNveg$WaterLevel >= 4, "High Fire Spread Risk ",
                                               ifelse(eden_epaNveg$FuelType == 2 & eden_epaNveg$WaterLevel > 4, "High Fire Spread Risk ", "Low Fire Spread Risk"))))
      
      eden_epaNveg$RX_Use <-ifelse(eden_epaNveg$WF_Use == "High Fire Spread Risk ", "High Fuel Availability", "Low Fuel Availability")
        st_write(eden_epaNveg, "analysis/outcomes/fireRisk_fuelAvailability_20181018.shp", delete_layer = TRUE)


### Combine fireRisk data with planning units
BICY_EVER_PlanningUnits <-st_read("analysis/BICY_EVER_PlanningUnits_EDEN.shp")
  eden_epaNveg_planningUnits <- st_intersection(eden_epaNveg, BICY_EVER_PlanningUnits[,c("PlanningUn", "FMU_Name")])
    eden_epaNveg_planningUnits$WL_des  <-revalue(as.factor(eden_epaNveg_planningUnits$WaterLevel), c("0" = "Very high", "1" = "High", "2" = "Low", "3" = "Very low", "4" = "Just below surface ", "5" = "Well below surface" ))
      eden_epaNveg_planningUnits$area<-st_area(eden_epaNveg_planningUnits)*0.000247105
        st_write(eden_epaNveg_planningUnits, "analysis/outcomes/fireRisk_fuelAvailability_planningFMUs_20181018.shp", delete_layer = TRUE)
        
        
### Create a summary table of fire risk area for each planning unit        
keyVars_df <- eden_epaNveg_planningUnits %>% st_set_geometry(NULL)                                                # Drop geometry for summing each column for total values
  planFMUs<-keyVars_df %>% group_by(PlanningUn, FMU_Name, WF_Use) %>% summarize(areaHA=sum(area))                 # Summarize data (mean) by planning units
    is.num <- sapply(planFMUs, is.numeric)                                                                        
      planFMUs[is.num] <- lapply(planFMUs[is.num], round, 2)
        write.csv(planFMUs, file = "analysis/outcomes/fireRisk_area_20181018.csv")        

                
###        
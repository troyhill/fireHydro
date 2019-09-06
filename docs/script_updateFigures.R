### script to generate most recent fireHydro maps

if (!"devtools" %in% installed.packages()) install.packages("devtools")
  
if (!"fireHydro" %in% installed.packages()) devtools::install_github("troyhill/fireHydro")


library(fireHydro)

### create directory, if it doesn't exist
figureDir <- paste0(getwd(), "/docs/figures/")
dir.create(figureDir, recursive = TRUE)

edenDat       <- getEDEN()
fireDat <- getFireHydro(EDEN_date         = edenDat$date, 
                       EDEN_GIS_directory = edenDat$data,
                       fireSpreadExport   = paste0(figureDir, "fireRisk.png"), 
                       waterLevelExport   = paste0(figureDir, "waterLevels.png"))

### export output as a shapefile
# st_write(fireDat, paste0(tempdir(), "/fireDat.shp"),
#          driver = "ESRI Shapefile")

# fireHydro

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/fireHydro?branch=master&svg=true)](https://ci.appveyor.com/project/troyhill/fireHydro) [![Build Status](https://travis-ci.org/troyhill/fireHydro.svg?branch=master)](https://travis-ci.org/troyhill/fireHydro) [![codecov.io](https://codecov.io/github/troyhill/fireHydro/coverage.svg?branch=master)](https://codecov.io/github/troyhill/fireHydro?branch=master)


## fireHydro installation

```
install.packages("devtools")
devtools::install_github("troyhill/fireHydro")
```


## fireHydro usage

fireHydro can be used to create shapefiles and/or images providing an integrative measure of fire risk in Big Cypress National Preserve and Everglades National Park.

```
library(fireHydro)

shp <- getFireHydro(EDEN_date = "20181018", 
     output_shapefile = NULL) # this can be a file address if shapefile output is desired
          
getFireHydro(EDEN_date = "20181018", 
  # EDEN data already in the working environment can be used
  EDEN_GIS_directory = "EDEN_shp_20181018", 
  dataToPlot = "WL_des",
  output_shapefile = NULL, 
  # simultaneously export as pdf and png
  fireSpreadExport = c("fireRisk.png", "fireRisk.pdf"), waterLevelExport = c("waterLevels.png", "waterLevels.pdf")) 
```

<img src="https://github.com/troyhill/images/blob/master/WaterLevels_20190320.png" width="600" height="450" />
Figure 1. Example output: water level categories  

      

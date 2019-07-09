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

EDEN_date_target <- "20190606"
edenDat <- getEDEN(EDEN_date = EDEN_date_target)
 
### getEDEN output can then be used in getFireHydro
fireDat <- getFireHydro(EDEN_date = edenDat$date, 
     EDEN_GIS_directory = edenDat$data,
     fireSpreadExport = c("fireRisk.png", "fireRisk.pdf"), # multiple outputs can be created simultaneously
     waterLevelExport = c("waterLevels.png", "waterLevels.pdf"))



```

<img src="https://github.com/troyhill/images/blob/master/waterLevels.png" width="650" height="425" />
Figure 1. Water level categories  

<img src="https://github.com/troyhill/images/blob/master/fireRisk.png" width="650" height="425" />
Figure 2. Fire spread risk categories. Risk estimates include water levels, vegetation type, and the burn history over the preceding three years.

      

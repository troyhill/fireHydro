# fireHydro

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/fireHydro?branch=master&svg=true)](https://ci.appveyor.com/project/troyhill/fireHydro) [![Build Status](https://travis-ci.org/troyhill/fireHydro.svg?branch=master)](https://travis-ci.org/troyhill/fireHydro) [![codecov.io](https://codecov.io/github/troyhill/fireHydro/coverage.svg?branch=master)](https://codecov.io/github/troyhill/fireHydro?branch=master)


## What you have here

`fireHydro` is an R package that estimates fire spread risk in south Florida by integrating diverse datasets:
1. water level data sourced from USGS/EDEN
2. vegetation cover maps classified by University of Georgia and categorized into litter classes using the professional judgement of Everglades National Park's Fire Cache group
3. the spatial extent of historical burns over the preceding three years



## fireHydro installation

```
install.packages("devtools")
devtools::install_github("troyhill/fireHydro")
```


## fireHydro usage

fireHydro can be used to create shapefiles and/or images providing an integrative measure of fire risk in Big Cypress National Preserve and Everglades National Park.

```
library(fireHydro)

### the EDEN web scraper defaults to downloading the most recent EDEN water depth data
edenDat <- getEDEN()
 
### getEDEN() output can then be used directly in getFireHydro()
fireDat <- getFireHydro(EDEN_date = edenDat$date, 
     EDEN_GIS_directory = edenDat$data,
     fireSpreadExport = c("fireRisk.png", "fireRisk.pdf"), # multiple outputs can be created simultaneously
     waterLevelExport = c("waterLevels.png", "waterLevels.pdf"))



```

<img src="https://github.com/troyhill/fireHydro/blob/master/docs/figures/waterLevels.png" width="650" height="425" />
Figure 1. Water level categories  

<img src="https://github.com/troyhill/fireHydro/blob/master/docs/figures/fireRisk.png" width="650" height="425" />
Figure 2. Fire spread risk categories. Risk estimates include water levels, vegetation type, and the burn history over the preceding three years.

      

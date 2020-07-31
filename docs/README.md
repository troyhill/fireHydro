# Current water levels and fire spread risk

<img src="{{site.url}}figures/waterLevels.png" width="600">


Figure 1. Water levels in south Florida. This map is based on the most recent EDEN water surface data. Download this figure as a [.png](https://github.com/troyhill/fireHydro/blob/master/docs/figures/waterLevels.png).

&nbsp;

<img src="{{site.url}}figures/fireRisk.png" width="600">

Figure 2. Fire spread risk categories, based on the most recent EDEN data. Risk estimates integrate water levels, vegetation type, and burn history over the past three years. More detail on fire spread risk categories is [below](#fire-spread-risk-categories). Download this figure as a [.png](https://github.com/troyhill/fireHydro/blob/master/docs/figures/fireRisk.png).

&nbsp;

&nbsp;



## About fireHydro

`fireHydro` is an R package with the primary purpose of estimating fire spread risk in south Florida by integrating diverse datasets:
1. water level data sourced from the [USGS's Everglades Depth Estimation Network](https://sofia.usgs.gov/eden/models/real-time.php)
2. vegetation cover maps classified by University of Georgia and categorized into litter classes using the professional judgement of Everglades National Park's Fire Cache group
3. the spatial extent of historical burns over the preceding three years (source: Everglades National Park Fire Cache)


`fireHydro` is a product of the South Florida Natural Resources Center (SFNRC), but does not rely on the SFNRC network. It can be used anywhere with an internet connection, although it is limited to processing EDEN water level data available on the [USGS's real-time water surface web page](https://sofia.usgs.gov/eden/models/real-time.php). In general, this means it can produce fire risk maps for dates from the most recent quarter or two - see [workflow example below](#firehydro-usage). Fire risk maps from earlier dates can be easily produced by users on the SFNRC's internal network; external users seeking older shapefiles or maps should email Troy_Hill at nps.gov.

&nbsp;

&nbsp;


### Fire spread risk categories

Fire spread risk is estimated primarily based on water levels and vegetation cover, with burn history introducing a gradient of fire spread risk. The categories in Figure 2 are defined as follows:


* **High risk** areas meet criteria for water depth defined for vegetation types based on the amount of litter produced (categorization and criteria were based on the professional judgement of Everglades National Park's Fire Cache). The table below shows water depth thresholds for each fuel category. Areas where water depth is below the threshold would experience high risk of fire spread.

| Vegetation cover | Fuel category  | Water depth threshold (ft)  | 
| :--- | :---: |:---:|
| Tall continuous grass  | 5      | 3.6 |
| Pine forest            | 5      | 2.6 |
| Beach dune             | NA     | 2.6 |
| Pine savannah          | 4      | 1.6 |
| Short continuous grass | 4      | 0 |
| Short sparse grass     | 3      | 0 |
| Shrub                  | 2      | -0.6 |
| Hammock/tree island    | 3      | -0.6 |
| Coastal forest         | 2      | -0.6 |
| Brazilian Pepper       | 2      | -1 |

* **Moderately high risk** areas meet the criteria for high risk but burned two calendar years ago.

* **Moderate risk** areas meet the criteria for high risk but burned in the previous calendar year.

* **Moderately low risk** areas meet the criteria for high risk but burned within the current calendar year.

* **Low risk** areas are characterized by inundation above the ground surface and/or vegetation cover that does not produce large amounts of litter.

&nbsp;


### Broader relevance

&nbsp;

The `fireHydro` R package provides utilities that are useful beyond this specific application to fire risk estimation in South Florida. For example, the package provides console-level access to the spatially explicit water level surfaces developed by USGS and hosted on EDEN. These spatially-explicit water level data are made readily available by `fireHydro`'s `getEDEN()` function, and were used to create this .gif, showing water level change over the past year:

&nbsp;

<img src="{{site.url}}figures/eden_pastYear.gif" width="600">

Figure 3. Changes in water level from July 2019 - May 2020. Blue indicates water levels exceeding the soil surface elevation, whereas water levels below the surface are shown in shades of red. Note the rapid drawdown in water levels beginning in March 2020. Download this figure as a [.gif](https://github.com/troyhill/fireHydro/blob/master/docs/figures/eden_pastYear.gif).

&nbsp;




## fireHydro installation


[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/fireHydro?branch=master&svg=true)](https://ci.appveyor.com/project/troyhill/fireHydro) [![Build Status](https://travis-ci.org/troyhill/fireHydro.svg?branch=master)](https://travis-ci.org/troyhill/fireHydro) [![codecov.io](https://codecov.io/github/troyhill/fireHydro/coverage.svg?branch=master)](https://codecov.io/github/troyhill/fireHydro?branch=master)

fireHydro is a package of functions for use in R. The R statistical software is open source, and freely available from the [Comprehensive R Archive Network](https://cran.r-project.org/). The fireHydro package has been tested on Windows and Linux operating systems.

fireHydro can be installed using the following commands in R:

```
install.packages("devtools")
devtools::install_github("troyhill/fireHydro")
```

&nbsp;


## fireHydro usage

A simple fireHydro usage example is below. This code generates maps of water levels and fire spread risk using the most recent EDEN water level data.

```
library(fireHydro)

### Step 1: Generate a map of water depths using getEDEN()
### By default, the most recent EDEN water surface data are used
edenDat <- getEDEN()
 
### Step 2: Generate fire spread risk maps using getFireHydro()
fireDat <- getFireHydro(EDEN_date = edenDat$date, 
     EDEN_GIS_directory = edenDat$data,
     fireSpreadExport =  "fireRisk.png", # to see location: getwd()
     waterLevelExport =  "waterLevels.png")
     
### Step 3: If desired, export output as a shapefile
sf::st_write(obj = fireDat, driver = "ESRI Shapefile",
     layer = paste0("fireDat_", edenDat$date, ".shp"))

```


&nbsp;

      

## What's coming

Updates planned for fireHydro include the following:

* Treatment of burn histories will become more accurate and flexible. Burn histories currently use full calendar/water years rather than expressing time relative to the queried date, leading to somewhat inaccurate categorization. To more accurately incorporate burn history, the burn history inputs may transition to a single, dated burn history file that queries based on burn date.

If you're interested in contributing to these or other improvements, email Troy_Hill at nps.gov.

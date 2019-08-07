# Current water levels and fire spread risk

<img src="{{site.url}}figures/waterLevels.png" width="600">


Figure 1. Water levels in south Florida. [Download this figure here.](https://github.com/troyhill/fireHydro/blob/master/docs/figures/waterLevels.png)

&nbsp;

<img src="{{site.url}}figures/fireRisk.png" width="600">

Figure 2. Fire spread risk categories. Risk estimates integrate water levels, vegetation type, and burn history over the past three years. More detail on fire spread risk categories is [below](#fire-spread-risk-categories). [Download this figure here.](https://github.com/troyhill/fireHydro/blob/master/docs/figures/fireRisk.png)

&nbsp;

&nbsp;



## About fireHydro

`fireHydro` is an R package that estimates fire spread risk in south Florida by integrating diverse datasets:
1. water level data sourced from the [USGS's Everglades Depth Estimation Network](https://sofia.usgs.gov/eden/models/real-time.php)
2. vegetation cover maps classified by University of Georgia and categorized into litter classes using the professional judgement of Everglades National Park's Fire Cache group
3. the spatial extent of historical burns over the preceding three years (source: Everglades National Park Fire Cache)


fireHydro is a product of the South Florida Natural Resources Center (SFNRC), but does not rely on the SFNRC network. It can be used anywhere with an internet connection, although it is limited to processing EDEN water level data available on the [USGS's real-time water surface web page](https://sofia.usgs.gov/eden/models/real-time.php). In general, this means it can produce fire risk maps for dates from the most recent quarter or two - see [workflow example below](#firehydro-usage). Fire risk maps from earlier dates can be easily produced by users on the SFNRC's internal network; external users seeking older shapefiles or maps should email Troy_Hill at nps.gov.

&nbsp;

&nbsp;


### Fire spread risk categories

Fire spread risk is estimated primarily based on water levels and vegetation cover, with burn history introducing a gradient of fire spread risk. The categories in Figure 2 are defined as follows:


* **High risk** areas meet criteria for water depth defined for vegetation types based on the amount of litter produced (categorization and criteria were based on the professional judgement of Everglades National Park's Fire Cache). The table below shows water depth thresholds for each fuel category. Areas where water depth is below the threshold would experience high risk of fire spread.

| Vegetation cover | Fuel category  | Water depth threshold (ft)  | 
| :--- | :---: |:---:|
| Tall continuous grass, pine forest | 5      | Inf (always high risk) |
| Short continuous grass, pine savannah | 4      | 4 |
| Short sparse grass, hammock/tree island | 3      | 0 |
| Coastal forest, Brazilian Pepper, shrub | 2      | -0.6 |

* **Moderately high risk** areas meet the criteria for high risk but burned two calendar years ago.

* **Moderate risk** areas meet the criteria for high risk but burned in the previous calendar year.

* **Moderately low risk** areas meet the criteria for high risk but burned within the current calendar year.

* **Low risk** areas are characterized by inundation above the ground surface and/or vegetation cover that does not produce large amounts of litter.

&nbsp;

## fireHydro installation

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
     fireSpreadExport =  paste0(tempdir(), "/fireRisk.png"),
     waterLevelExport =  paste0(tempdir(), "/waterLevels.png"))
     
### Step 3: If desired, export output as a shapefile
sf::st_write(obj = fireDat, driver = "ESRI Shapefile",
     layer = paste0(tempdir(), "/fireDat_", edenDat$date, ".shp"))

```


&nbsp;

      

## What's coming

Updates planned for fireHydro include the following:

* Access historic water surface data archived [here](https://sofia.usgs.gov/eden/models/watersurfacemod_download.php) and [here](https://sofia.usgs.gov/eden/models/watersurfacemod_download_1990s.php). These data are presently accessible only to fireHydro users on the SFNRC's internal network (or to external users who manually download the data). This feature would provide equal access to data for all users.

* Treatment of burn histories will become more accurate and flexible. Burn histories are currently hard-coded and manually updated. Additionally, full calendar years are used rather than years relative to the queried date, leading to inaccurate categorization. This will transition to a single, dated burn history file that queries based on burn date.

If you're interested in contributing to these or other improvements, email Troy_Hill at nps.gov.

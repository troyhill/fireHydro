# Most recent fire risk maps



<img src="{{site.url}}/docs/figures/waterLevels.png" style="display: block; margin: auto;" />

<img src="{{site.url}}/figures/waterLevels.png" style="display: block; margin: auto;" />

<img src="https://github.com/troyhill/fireHydro/blob/master/docs/figures/waterLevels.png" style="display: block; margin: auto;" />

![current water levels]({{ BASE_PATH }}/docs/figures/waterLevels.png)

&nbsp;

Figure 1. Water levels in south Florida. [Download figure here](../docs/figures/waterLevel.png)


&nbsp;

&nbsp;


<img src="{{site.url}}/docs/figures/fireRisk.png" style="display: block; margin: auto;" />

<img src="https://github.com/troyhill/fireHydro/blob/master/docs/figures/fireRisk.png" style="display: block; margin: auto;" />

![current fire risk]({{ BASE_PATH }}/docs/figures/fireRisk.png)

&nbsp;

Figure 2. Fire spread risk categories. Risk estimates integrate water levels, vegetation type, and burn history over the preceding three years. [Download figure here](../docs/figures/fireRisk.png)

&nbsp;

&nbsp;



## What you have here

`fireHydro` is an R package that estimates fire spread risk in south Florida by integrating diverse datasets:
1. water level data sourced from USGS/EDEN
2. vegetation cover maps classified by University of Georgia and categorized into litter classes using the professional judgement of Everglades National Park's Fire Cache group
3. the spatial extent of historical burns over the preceding three years


fireHydro is a product of the South Florida Natural Resources Center (SFNRC), but does not rely on the SFNRC network. It can be used anywhere with an internet connection, although it is limited to processing EDEN water level data available on the [USGS's real-time water surface web page](https://sofia.usgs.gov/eden/models/real-time.php). In general, this means it can produce fire risk maps for dates from the most recent quarter or two. Fire risk maps from earlier dates can be easily produced by users on the SFNRC's internal network; external users seeking these shapefiles or maps should email Troy_Hill at nps.gov.


## fireHydro installation

fireHydro is a package of functions for use in R. The R statistical software is open source, and freely available from the [Comprehensive R Archive Network](https://cran.r-project.org/). The fireHydro package has been tested on Windows and Linux operating systems.

fireHydro can be installed using the following commands in R:

```
install.packages("devtools")
devtools::install_github("troyhill/fireHydro")
```


## fireHydro usage

A simple fireHydro usage example is below. This code generates maps of water levels and fire spread risk using the most recent EDEN water level data.

```
library(fireHydro)

### By default, the most recent EDEN water surface data are used
edenDat <- getEDEN()
 
### getEDEN() output can then be used directly in getFireHydro()
fireDat <- getFireHydro(EDEN_date = edenDat$date, 
     EDEN_GIS_directory = edenDat$data,
     fireSpreadExport = "fireRisk.png",
     waterLevelExport = "waterLevels.png")



```


      

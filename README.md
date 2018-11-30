# fireHydro

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/troyhill/fireHydro?branch=master&svg=true)](https://ci.appveyor.com/project/troyhill/fireHydro) [![Build Status](https://travis-ci.org/troyhill/fireHydro.svg?branch=master)](https://travis-ci.org/troyhill/fireHydro) [![codecov.io](https://codecov.io/github/troyhill/fireHydro/coverage.svg?branch=master)](https://codecov.io/github/troyhill/fireHydro?branch=master)


## fireHydro installation

```
install.packages("devtools")
devtools::install_github("troyhill/fireHydro")
```


## fireHydro usage

fireHydro can be used to create shapefiles and/or images providing an integrative measure of fire risk.

```
library(fireHydro)

getFireHydro(EDEN_date = "20181018", 
  dataToPlot = "WL_des",
  output_shapefile = NULL, # this can be a file address if shapefile output is desired
  imageExport = "output.png")
```

<img src="https://github.com/troyhill/images/blob/master/WL_des_20181018.png" width="400" height="500" />
Figure 1. Example output 

      

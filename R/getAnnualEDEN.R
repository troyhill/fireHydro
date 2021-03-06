#' @title Downloads and imports archived EDEN data at annual or multi-annual time steps 
#'
#' @description Downloads and combines netCDF files from https://sofia.usgs.gov/eden/models/watersurfacemod_download.php, unzips and loads them into the workspace. Zip files are deleted after loading. This function makes `fireHydro` able to operate with complete independence from Department of Interior servers. This code generates a water depth map using the USGS water surface data and the USGS EDEN digital elevation map (present in this R package as raster layer "edenDEM").
#' 
#' 
#' @param years a vector specifying which years of EDEN data should be downloaded. Please consider RAM limitations - a year of data can be ~ 300 Mb of RAM. If this is an issue, consider using getAnnualEDEN() and do analysis/extraction incrementally in a loop.
#' @param DEM raster digital elevation model for south Florida. Used to subtract land elevations from water surface to get water depths. The default DEM is a USGS/EDEN product.
#' 
#' @return list \code{getAnnualEDEN} returns a list with two versions of the same data: (1) the dates used, and (2) a raster stack object with a layer for each day, containing water level data for the EDEN grid (units = cm rel. to soil surface).
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' edenDat <- getAnnualEDEN(years = 2019:2020)
#' }
#' 
#' @importFrom raster stack
#' @importFrom raster nlayers
#' 
#' @export



getAnnualEDEN <- function(years,
                          DEM = raster(system.file("extdata/edenDEM.grd", package = "fireHydro"))
) {
  
  
  ### check if current year is included in years. If it is, behave more complexly (handle errors 
  ### from bad urls, or check dates and only request valid data)
  # 'https://sofia.usgs.gov/eden/data/netcdf/v2/1999_q1_v2prov_r2.zip'
  for (i in 1:length(years)) {
    yrSelect   <- years[i]
    
    # ### Check whether year is current year and treat accordingly
    # qtr <- tolower(format(zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")), format = "%Y_Q%q"))
    # 
    # currentQ <- FALSE  
    # ### check if requested date is in current quarter (and thus on the front page of EDEN)
    # if (zoo::as.yearqtr(as.Date(YYYYMMDD, format = "%Y%m%d")) == zoo::as.yearqtr(as.Date(Sys.Date(), format = "%Y%m%d"))) {
    #   currentQ <- TRUE  
    # }
    
    
    EDEN_qtr_1 <- fireHydro::getEDEN(EDEN_date = paste0(yrSelect, "0105"), quarterly = TRUE, exact = TRUE, DEM = DEM) # this behavior is fragile, but seems to work. setting exact == TRUE will cause there to be no download (and no object) if this date is not available in EDEN. If the date is available, the entire quarter is downloaded (not a single date)
    EDEN_qtr_2 <- fireHydro::getEDEN(EDEN_date = paste0(yrSelect, "0505"), quarterly = TRUE, exact = TRUE, DEM = DEM)
    EDEN_qtr_3 <- fireHydro::getEDEN(EDEN_date = paste0(yrSelect, "0801"), quarterly = TRUE, exact = TRUE, DEM = DEM)
    EDEN_qtr_4 <- fireHydro::getEDEN(EDEN_date = paste0(yrSelect, "1101"), quarterly = TRUE, exact = TRUE, DEM = DEM)
    
    
    ### identify and ignore objects that don't exist (if a quarter doesn't exist yet)
    # https://stackoverflow.com/questions/25317362/rbind-multiple-dataframes-within-a-function
    # quarterNames <- c("EDEN_qtr_1", "EDEN_qtr_2",
    #                   "EDEN_qtr_3", "EDEN_qtr_4",
    #                   "EDEN_qtr_5")
    # quarterNames <- quarterNames[sapply(X = quarterNames, function(x) exists(x))]
    # message(paste0("\n", length(quarterNames), " quarterNames exist", "\n"))
    # quarterNames <- paste0(quarterNames[sapply(X = quarterNames, function(x) exists(x))], "$date" )
    # get(quarterNames[1])["date"]
    # dat <- rbind(dat, if(exists("old_dat")) old_dat)
    # tst <-  raster::stack(get(quarterNames))
    
    # if (length(quarterNames) == 4) {
    #   tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data,
    #                        EDEN_qtr_3$data, EDEN_qtr_4$data)
    # } else if (length(quarterNames) == 3) {
    #   tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data,
    #                        EDEN_qtr_3$data)
    # } else if (length(quarterNames) == 2) {
    #   tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data)
    # } else if (length(quarterNames) == 1) {
    #   tst <- raster::stack(EDEN_qtr_1$data)
    # }
    
    if (exists("EDEN_qtr_4")) {
      # message("qtr4 data exists!")
      tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data,
                           EDEN_qtr_3$data, EDEN_qtr_4$data)
    } else if (exists("EDEN_qtr_3")) {
      # message("qtr3 data exists!")
      tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data, EDEN_qtr_3$data)
    } else if (exists("EDEN_qtr_2")) {
      # message("qtr2 data exists!")
      tst <- raster::stack(EDEN_qtr_1$data, EDEN_qtr_2$data)
    } else if (exists("EDEN_qtr_1")) {
      # message("only qtr1 data exists")
      tst <- raster::stack(EDEN_qtr_1$data)
    }
    
    ###
    tst.dates <- seq.Date(from = as.Date(paste0(yrSelect, "-01-01"), format = "%Y-%m-%d"), 
                          to   = as.Date(paste0(yrSelect, "-12-31"), format = "%Y-%m-%d"),
                          by   = 1)
    names(tst) <- tst.dates[1:raster::nlayers(tst)] # might want to convert to character: as.character(tst.dates)
    
    if (i == 1) {
      outList <- list(date = tst.dates[1:raster::nlayers(tst)], 
                      data = tst)
    } else {
      outList <- list(
        date = c(outList$date, tst.dates[1:raster::nlayers(tst)]), 
        data = raster::stack(outList$data, tst))
    }
    
    rm(tst)
    rm(tst.dates)
    rm(EDEN_qtr_1)
    rm(EDEN_qtr_2)
    rm(EDEN_qtr_3)
    rm(EDEN_qtr_4)
    
  }
  invisible(outList)
}


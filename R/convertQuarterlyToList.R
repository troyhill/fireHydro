#' @title Re-formats quarterly EDEN data to a list-of-lists
#'
#' @description Takes quarterly EDEN data and produces output identical to lapply(dateVector, getEDEN), much more rapidly than downloading individual days of data.
#' 
#' @usage convertQuarterlyToList(quarterlyEDEN_data)
#' 
#' @param quarterlyEDEN_data quarterly EDEN water level data, as produced by, e.g., getOldEDEN(YYYYMMDD = gsub(x = Sys.Date(), pattern = "-", replacement = ""), quarterly = TRUE)
#' 
#' @return list \code{convertQuarterlyToList} returns a list with an element for each day in the quarter. Each sub-list contains two elements: (1) the date (character vector), and (2) an sf multipolygon object with water depths (not stages) in the EDEN grid.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' ### Example of use in context of other functions in RSM and fireHydro
#' library(RSM)
#' sim_means <- polyDat$traceDataLong[!is.na(polyDat$traceDataLong$value), ]
#' ### add grouping variable
#' sim_means$cluster <- sim_clusters[match(sim_means$simulation, sim_clusters$simulation), 1]
#' IR_pattern <- "WCA-1"
#' 
#' # pull  EDEN data for quarter
#' EDEN_qtr <- getOldEDEN(YYYYMMDD = "20200312", quarterly = TRUE)
#' 
#' # convert quarterly data to list-of-lists (match output from lapply(dates, getEDEN))
#' inputTest <- convertQuarterlyToList(EDEN_qtr)
#' 
#' ### convoluted transformation from list of Dates to character vector
#' dateVec <- sapply(inputTest, "[[", 1)
#' 
#' EDEN_by_IR <- RSM::getEDENbyROI(targetLocations = ROIs,
#'                            targetLocationNames = targetLocationNames,
#'                            dateRange = dateVec, 
#'                            needEDEN = inputTest)
#'                            
#' RSM::tracePlot(namesMatching = IR_pattern, simMeansObject = sim_means) + 
#'     geom_line(data = EDEN_by_IR, 
#'     mapping = aes(x = date, y = ave), size = 1.5)
#' }
#' 
#' @importFrom zoo     as.yearqtr
#' @importFrom sf      st_as_sf
#' 
#' @export


convertQuarterlyToList <- function(quarterlyEDEN_data # getoldEDEN output: list with quarter and raster stack stored as EDEN_data$data when getoldEDEN(quarterly = TRUE)
) {
  ### prep dates
  daysInDataset <- length(names(quarterlyEDEN_data$data))
  quarter       <- zoo::as.yearqtr(toupper(quarterlyEDEN_data$date), format = "%Y_Q%q")
  quarter_begin <- as.Date(quarter)
  quarterDates  <- seq.Date(from = quarter_begin, to = quarter_begin + daysInDataset, by = "day")
  
  ### prep data (convert raster stack to sf)
  ### Note: next line is source of sp dependency. How to make the call to sp explicit?
  rasData <- sf::st_as_sf(as(quarterlyEDEN_data$data, "SpatialPolygonsDataFrame"))
  
  ### combine into a list of lists (match output from lapply(dateVector, getEDEN))
  ### very important to set variable name to "WaterDepth" in data objects
  outDat <- list()
  for (i in 1:daysInDataset) {
    newData <- rasData[, c(i, daysInDataset+1)]
    names(newData)[1] <- "WaterDepth"
    outDat[[i]] <- list(date = gsub(x = as.character(quarterDates[i]), pattern = "-", replacement = ""), data = newData)
  }
  invisible(outDat)
}

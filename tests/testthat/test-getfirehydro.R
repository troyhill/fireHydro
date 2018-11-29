context("test-getfirehydro")

test_that("getFireHydro works", {
  suppressWarnings(outputDat <- getFireHydro(EDEN_date = "20181018", 
               EDEN_GIS_directory = "EDEN_shp_20181018",
               returnShp = TRUE))
                     
  expect_equal(typeof(outputDat), "list")
  expect_equal(nrow(outputDat), 1533)
})

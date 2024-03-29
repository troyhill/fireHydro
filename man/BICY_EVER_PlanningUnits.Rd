% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BICY_EVER_PlanningUnits.R
\docType{data}
\name{BICY_EVER_PlanningUnits}
\alias{BICY_EVER_PlanningUnits}
\title{Data: Big Cypress and Everglades planning units}
\format{
A shapefile of planning unit boundaries for Big Cypress National Preserve and Everglades National Park
\describe{
\item{Park_Code}{tbd}
\item{PlanningUn}{tbd}
\item{FMU_Name}{tbd}
\item{FMU_Code}{tbd}
\item{Shape_Leng}{tbd}
\item{Shape_Area}{tbd}
\item{geometry}{tbd}
}
}
\usage{
BICY_EVER_PlanningUnits
}
\description{
A shapefile of planning units
}
\examples{
names(BICY_EVER_PlanningUnits)

\dontrun{
### code used to generate object using shapefile provided by NCSU
BICY_EVER_PlanningUnits <- sf::st_read("BICY_EVER_PlanningUnits_EDEN_py.shp")
BICY_EVER_PlanningUnits <- sf::st_transform(BICY_EVER_PlanningUnits, sf::st_crs(fireHydro::edenDEM))
save(BICY_EVER_PlanningUnits, file = "data/BICY_EVER_PlanningUnits.RData")
}

}
\keyword{planning}
\keyword{units}

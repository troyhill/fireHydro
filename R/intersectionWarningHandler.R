

#' Warning handler for sf::st_intersection
#'
#' @param warningMessage the warning message produced by \code{sf::st_intersection}
#'
#' @return nothing is returned.
#' @export
#'
#' @examples
#'#   dat <- data.frame(dat = c("5", "10", "-", "helloWorld"))
#'#   withCallingHandlers(
#'#     as.numeric(as.character(dat$dat)), warning = intersectionWarningHandler)

intersectionWarningHandler <- function(warningMessage) {
  if( any( grepl( "attribute variables are assumed to be spatially constant throughout all geometries", warningMessage) ) ) invokeRestart( "muffleWarning" )
}
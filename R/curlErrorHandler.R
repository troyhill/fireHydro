

#' Modified version of googledrive::drive_upload with error handling
#'
#' @description A curl error occasionally appears when using googledrive::drive_upload, but appears to be overcome by repeating the command.
#' 
#' @param mediaInput media argument sent to \code{googledrive::drive_upload}
#' @param pathInput path argument sent to \code{googledrive::drive_upload}
#' @param NumberOfAttempts maximum number of attempts
#'
#' @return nothing is returned.
#' @export
#' 
#' @importFrom googledrive drive_upload
#'
#' @examples
#' ### error message:
#' # Error in curl::curl_fetch_memory(url, handle = handle) : 
#' #   Error in the HTTP2 framing layer
#' # drive_upload_mod(
#' #  mediaInput = waterLevelPdf,
#' #  pathInput = "FireHydro output/pdf/")
#'
#'
drive_upload_mod <- function(mediaInput, pathInput, NumberOfAttempts = 5) {
  r <- NULL
  attempt <- 0
  while( is.null(r) && (attempt <= NumberOfAttempts) ) {
    attempt <- attempt + 1
    try(
      r <- googledrive::drive_upload(
        media = mediaInput,
        path = pathInput)
    )
  } 
}

#' isTrue Function
#'
#' This function checks whether TRUE is TRUE.
#' @param arg Is value TRUE? Defaults to TRUE.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @format 
#' \describe{
#' \item{type}{factor; type of.}
#' \item{long}{numeric; longitude.}
#' \item{lat}{numeric; latitude.}
#' }
#' @examples
#' isTrue()

isTrue <- function(arg = TRUE){
  if(arg == TRUE){
    result <- TRUE
  } else {
    result <- FALSE
  }
  
  return(result)
}
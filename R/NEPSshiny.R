#' Shiny interface for NEPSscaling
#'
#' This function calls a shiny interface for NEPSscaling.
#'
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export
NEPSshiny <- function(launch.browser=TRUE){
  shiny::runApp(system.file('NEPSshiny', package = 'NEPSscaling'),
                launch.browser = launch.browser)
}

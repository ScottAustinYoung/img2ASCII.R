# R/launch_app.R

#' Launch the img2ASCII Shiny Application
#'
#' This function launches the Shiny application contained within the img2ASCII package.
#'
#' @param ... Arguments passed to `shiny::runApp` (e.g., `launch.browser = TRUE`).
#'
#' @return Does not return a value; launches the Shiny application.
#' @export
launch_img2ascii_app <- function(...) {
  appDir <- base::system.file("shinyapp", package = "img2ASCII")
  if (appDir == "") {
    stop("Could not find the shinyapp directory in package img2ASCII. ",
         "Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", ...)
}

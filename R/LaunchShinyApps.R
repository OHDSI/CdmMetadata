
.checkShinyDeps <- function() {
  dependencies <- c("shiny",
                    "DT",
                    "shinydashboard",
                    "magrittr",
                    "tidyr")
  
  for (d in dependencies) {
    if (!requireNamespace(d, quietly = TRUE)) {
      message <- sprintf(
        "You must install %1s first. You may install it using devtools with the following code: 
        \n    install.packages('%2s')
        \n\nAlternately, you might want to install ALL suggested packages using:
        \n    devtools::install_github('OHDSI/Achilles', dependencies = TRUE)", d, d)
      stop(message, call. = FALSE)
    }
  }
}


#' Launches the Shiny Metadata App
#'  
#' @export
launchMetadataApp <- function() {

  .checkShinyDeps()

  appDir <- system.file("shinyApps", package = "CdmMetadata")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}

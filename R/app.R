#'
#'Run the animaltracker 'shiny' app by calling this function.
#'Depending on the size of input files, it may be advisable to increase the maximum request size.
#'
#'@param browser logical, whether to launch the app in your default browser (defaults to TRUE)
#'@param showcase logical, whether to launch the app in 'showcase' mode (defaults to FALSE)
#'@return None
#'@export
run_shiny_animaltracker <- function( browser = TRUE, showcase=FALSE) {
  requireNamespace("shiny")
  requireNamespace("leaflet")
  requireNamespace("dplyr")
  requireNamespace("ggplot2")
  requireNamespace("shinyWidgets")
  requireNamespace("shinycssloaders")
  requireNamespace("leaflet.extras")
  requireNamespace("shinythemes")
  requireNamespace("sp")
  requireNamespace("raster")
  requireNamespace("shinyjs")
  requireNamespace("V8")
  requireNamespace("shinyBS")
 
  # Run the application (see associated functions, defined separately)
  if(browser | showcase){
    runApp(shinyApp(ui = app_ui, server = app_server), 
          launch.browser=browser, display.mode = ifelse(showcase, "showcase", "normal"))
  }else{
    shinyApp(ui = app_ui, server = app_server)
  }
}

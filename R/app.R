#'
#'You can run the animaltracker 'shiny' app by calling this function.
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
  requireNamespace("shinyBS")
  
  options(shiny.maxRequestSize=30*1024^2) 
 
  # Run the application (see associated functions, defined separately)
  if(browser | showcase){
    runApp(shinyApp(ui = app_ui, server = app_server), 
          launch.browser=browser, display.mode = ifelse(showcase, "showcase", "normal"))
  }else{
    shinyApp(ui = app_ui, server = app_server)
  }
}

#'
#'You can run the animaltracker Shiny app by calling this function.
#'@param launch.browser logical, whether to launch the app in your default browser (defaults to FALSE)
#'@export
run_shiny_animaltracker <- function( launch.browser = TRUE) {
  require("shiny")
  require("leaflet")
  require("dplyr")
  require("ggplot2")
  require("shinyWidgets")
  require("shinycssloaders")
  require("leaflet.extras")
  require("shinythemes")
  require("sp")
  
  options(shiny.maxRequestSize=30*1024^2) 
 
  # Run the application (see associated functions, defined separately)
  runApp(shinyApp(ui = app_ui, server = app_server), 
         launch.browser=launch.browser)
}

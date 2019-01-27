#'
#'You can run the animaltracker Shiny app by calling this function.
#'
#'@export
run_shiny_animaltracker <- function() {
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
         launch.browser=TRUE)
}

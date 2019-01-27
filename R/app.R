#'
#'You can run the animaltracker Shiny app by calling this function.
#'
#'@export
run_shiny_animaltracker <- function() {
  require("shiny")
  require("leaflet")
  require("ggplot2")
  require("shinyWidgets")
  require("shinycssloaders")
  require("leaflet.extras")
  require("shinythemes")
  
  options(shiny.maxRequestSize=30*1024^2) 
 
  # Run the application (see associated functions, defined separately)
  shinyApp(ui = app_ui, server = app_server)
}

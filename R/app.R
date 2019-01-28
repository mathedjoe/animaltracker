#'
#'You can run the animaltracker Shiny app by calling this function.
#'@param browser logical, whether to launch the app in your default browser (defaults to TRUE)
#'@param showcase logical, whether to launch the app in 'showcase' mode (defaults to FALSE)
#'@export
run_shiny_animaltracker <- function( browser = TRUE, showcase=FALSE) {
  require("shiny")
  require("leaflet")
  require("dplyr")
  require("ggplot2")
  require("shinyWidgets")
  require("shinycssloaders")
  require("leaflet.extras")
  require("shinythemes")
  require("sp")
  require("raster")
  
  options(shiny.maxRequestSize=30*1024^2) 
 
  # Run the application (see associated functions, defined separately)
  if(browser | showcase){
    runApp(shinyApp(ui = app_ui, server = app_server), 
          launch.browser=browser, display.mode = ifelse(showcase, "showcase", "normal"))
  }else{
    shinyApp(ui = app_ui, server = app_server)
  }
}

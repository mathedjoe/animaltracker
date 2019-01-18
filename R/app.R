#'
#'You can run the animaltracker Shiny app by calling this function.
#'
#'@param rds_path Path of Animal data file to input
#'@export
run_shiny_animaltracker <- function() {
  require("shiny")
  require("leaflet")
  require("ggplot2")
  require("shinyWidgets")
  require("shinycssloaders")
  
  options(shiny.maxRequestSize=30*1024^2) 
  
  
  # Define UI for application 
  ui <- fluidPage(
    titlePanel("Animal Tracker App"),
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Upload Data",
                             fileInput("zipInput", "Compressed Folder", accept=c(".zip", ".7z"))
                             ),
                    tabPanel("Choose Data", 
                             uiOutput("choose_site") %>% withSpinner(),
                             uiOutput("choose_data") %>% withSpinner(),
                             uiOutput("choose_dates") %>% withSpinner(),
                             uiOutput("choose_times") %>% withSpinner()
                    )
        )#sidebarPanel
      ),#sidebarLayout
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plots",  fluidRow(
                      column(8, 
                             leafletOutput("mainmap", height = 640),
                             plotOutput("plot1"),
                             plotOutput("plot2")
                      )
                    )),
                    tabPanel("Statistics", tableOutput("stats"))
        ) 
      ) #mainPanel
    ) #sidebarLayout
  ) #fluidPage
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    meta <- reactive({ 
      if(is.null(input$zipInput)) {
        return()
      }
      clean_batch(input$zipInput) 
    }) 
    
    
    # Drop-down selection box for which sites
    output$choose_site <- renderUI({
      if(nrow(meta()) == 0) {
        return()
      }
      meta <- meta()
      pickerInput("selected_site", "Filter by Site",
                  choices = as.list(as.character(unique(meta$site))),
                  multiple = TRUE,
                  inline = FALSE, options = list(`actions-box` = TRUE)
      ) 
    }) 
    
    
    # Drop-down selection box for which animals
    output$choose_data <- renderUI({
      if(is.null(input$selected_site)) {
        return()
      }
      meta <- meta() %>%
        dplyr::filter(site %in% input$selected_site) 
    
      pickerInput("selected_ani", "Filter by Animal ID",
                  choices = as.list(as.character(unique(meta$ani_id))),
                  multiple = TRUE, 
                  inline = FALSE, options = list(`actions-box` = TRUE)
      )
    })
    
    
    # date range
    output$choose_dates <- renderUI({
      
      # If missing input, return to avoid error later in function
      if(is.null(input$selected_ani))
        return()
      
      # Get the data set with the appropriate name
      
      meta <- meta() %>%
        dplyr::filter(ani_id %in% input$selected_ani)
      
      max_dates <- meta$max_date
      min_dates <- meta$min_date
      
      sliderInput("dates", "Date Range", min = min(min_dates),
                  max = max(max_dates), value = c(min(min_dates), max(max_dates)), step = 1,
                  animate = animationOptions(loop = FALSE, interval = 1000))
    })
    
    output$choose_times <- renderUI({
      
      # If missing input, return to avoid error later in function
      if(is.null(input$selected_ani))
        return()
      
      dat_no_time <- dat_no_time()
      
      min_times <- min(dat_no_time$DateTime)
      max_times <- max(dat_no_time$DateTime)
        
      sliderInput("times", "Time Range", min = min_times,
                  max = max_times, value = c(min_times, max_times), step = 1,
                  animate = animationOptions(loop = FALSE, interval = 1000))
    })
    
    
    dat_no_time <- reactive({
      if(is.null(input$selected_ani) || is.null(input$dates))
        return()
      meta <- meta() %>%
        dplyr::filter(ani_id %in% input$selected_ani)
    
      current_df <- get_data_from_meta(meta, input$dates[1], input$dates[2])
      
    })
    
    dat <- reactive({
      if(is.null(input$times)) {
        return()
      }
      current_df <- dat_no_time() %>%
        dplyr::filter(DateTime >= input$times[1],
                      DateTime <= input$times[2])
        
    })
    
    points <- reactive({
      # If missing input, return to avoid error later in function
      if(is.null(input$selected_ani) || is.null(input$dates)) 
         return()
         
         SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], data = dat(),
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    })
      
      
      output$mainmap <- renderLeaflet({
        
        
        
        if(is.null(input$selected_ani) || is.null(input$dates) )
          return()
        
        factpal <- colorFactor(hue_pal()(length(input$selected_ani)), input$selected_ani)
        
        leaflet() %>%  # Add tiles
          addTiles() %>%
          # addProviderTiles("OpenTopoMap") %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
          # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
          
          addCircleMarkers(data = points(), radius=6, fillOpacity = .6, stroke=F,
                           color = ~ factpal(Animal), 
                           popup = ~ paste(paste("<h4>",paste("Animal ID:", points()$Animal), "</h4>"),
                                           paste("Date/Time:", points()$DateTime),
                                           paste("Altitude:", points()$Altitude),
                                           paste("Elevation:", points()$Elevation),
                                           paste("Lat/Lon:", paste(points()$Latitude, points()$Longitude, sep=", ")),
                                           sep="<br/>")
          )
        
        # leaflet() %>%
        #   addMarkers(data = points(),popup=as.character(points()$a))
      })
      
      output$plot1 <- renderPlot({
        if(is.null(input$selected_ani) || is.null(input$dates))
          return()
        
        # hist(dat()$TimeDiffMin [dat()$TimeDiffMin < 100], main = "Distribution of Time Between GPS Measurements" )
        ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
          labs( title = "Elevation (meters) during Data Collection")+
          ylim(1000,2000)+geom_line() + 
          geom_point() + 
          theme_minimal()
        
        
      })
      
      output$plot2 <- renderPlot({
        if(is.null(input$selected_ani) || is.null(input$dates))
          return()
        
        ggplot(dat(), aes(x=TimeDiffMins, fill=Animal))+
          geom_histogram(  col="White", breaks = seq(0,40, 2)) +
          facet_wrap(~Animal, ncol=2)+
          labs( title = "Distribution of Time between GPS Readings, by GPS Unit" )+ 
          theme_minimal()
        
        
      })
      
      stats <- reactive({
        if(is.null(input$selected_ani) || is.null(input$dates)) 
          return()
        
        summary <- dat() %>% 
          dplyr::group_by(Animal) %>%
          dplyr::summarize(meanAlt = mean(Altitude),
                     sdAlt = sd(Altitude),
                     minAlt = min(Altitude),
                     maxAlt = max(Altitude),
                     meanSpeed = mean(Speed),
                     sdSpeed = sd(Speed),
                     minSpeed = min(Speed),
                     maxSpeed = max(Speed))
  
      })
      
      output$stats <- renderTable(stats())
      
      session$onSessionEnded(stopApp)
    
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
}

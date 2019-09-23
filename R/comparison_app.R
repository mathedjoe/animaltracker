
#'
#'Run the Shiny validation app
#'
#'@export
run_validation_app <- function() {
  options(shiny.maxRequestSize=100*1024^2)
  
  
  ui <- fluidPage(
    titlePanel("Animal Data Validation"),
    sidebarLayout(
      sidebarPanel(
        h4("Upload Data"),
        helpText("Please select a reference and candidate file from your computer."),
        fileInput("correctInput", "Upload reference data (.csv)", accept=c(".csv")),
        fileInput("candidateInput", "Upload candidate data (.csv)", accept=c(".csv")),
        dateInput("date", "Date", value="2018-03-05",
                  min="2018-03-05", max="2018-05-26"),
        radioButtons("var", "Variable", 
                     choices=c("Cumulative Distance (+Flags)", "Rate (+Flags)", "Course (+Flags)",
                               "Elevation", "Slope"),
                     selected="Cumulative Distance (+Flags)")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot", height="1000px")),
          tabPanel("Summary", 
                   h4("Total Flags by GPS"),
                   tableOutput("gps_totals"),
                   h4("Summary of Distance Flags by GPS"),
                   tableOutput("gps_distance"),
                   h4("Summary of Distance Flag Time Differences by GPS"),
                   tableOutput("gps_distance_time")
                   )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    comparison <- reactive({
      req(input$correctInput, input$candidateInput)
      compare_flags(read.csv(input$correctInput$datapath, skipNul = TRUE), read.csv(input$candidateInput$datapath, skipNul = TRUE))
    })
    
    current_data <- reactive({
      get_column(comparison(), input$var, input$date)
    })
    
    gps_totals <- reactive({ 
      current_data() %>% 
      dplyr::group_by(GPS) %>% 
      dplyr::summarise(
        RateFlags = sum(RateFlag, na.rm=TRUE),
        CourseFlags = sum(CourseFlag, na.rm=TRUE),
        DistFlags = sum(DistanceFlag, na.rm=TRUE),
        TotalFlags = sum(TotalFlags, na.rm=TRUE),
        Dropped = sum(Dropped, na.rm=TRUE)
      )
    })
    
    
    summarise_flag <- function(df, col, flag) {
      flag <- dplyr::enquo(flag)
      col <- dplyr::enquo(col)
      df %>%
        dplyr::filter(!!flag > 0) %>% 
        dplyr::group_by(GPS) %>% 
        dplyr::summarise(
          Min.y = min(!!col),
          Q1.y = quantile(!!col, 0.25, na.rm=TRUE),
          Median.y = median(!!col),
          Mean.y = mean(!!col),
          Q3.y = quantile(!!col, 0.75, na.rm=TRUE),
          Max.y = max(!!col)
        )
    }
    
    output$plot <- renderPlot(
      ggplot(data=current_data(), aes(x=DateTime, y=VAR, group=Source, color=Source)) +
        geom_line(aes(size = Source)) +
        scale_color_discrete(guide = guide_legend(reverse = T)) +
        scale_size_manual(values=c(2, 1))+
        facet_wrap(vars(GPS), ncol=3) +
        theme_minimal()
    )
    
    get_column <- function(df, choice, date) {
      df <- df %>% dplyr::ungroup() %>% dplyr::filter(Date == as.Date(date))
      if(choice == "Cumulative Distance (+Flags)") {
        x <- df %>% dplyr::select(cumDist.x) %>% dplyr::mutate(VAR = cumDist.x)
        y <- df %>% dplyr::select(cumDist.y) %>% dplyr::mutate(VAR = cumDist.y)
      }
      else if(choice == "Rate (+Flags)") {
        x <- df %>% dplyr::select(Rate.x) %>% dplyr::mutate(VAR = Rate.x)
        y <- df %>% dplyr::select(Rate.y) %>% dplyr::mutate(VAR = Rate.y)
      }
      else if(choice == "Course (+Flags)") {
        x <- df %>% dplyr::select(Course.x) %>% dplyr::mutate(VAR = Course.x)
        y <- df %>% dplyr::select(Course.y) %>% dplyr::mutate(VAR = Course.y)
      }
      else if(choice == "Elevation") {
        x <- df %>% dplyr::select(Elevation.x) %>% dplyr::mutate(VAR = Elevation.x)
        y <- df %>% dplyr::select(Elevation.y) %>% dplyr::mutate(VAR = Elevation.y)
      }
      else {
        x <- df %>% dplyr::select(Slope.x) %>% dplyr::mutate(VAR = Slope.x)
        y <- df %>% dplyr::select(Slope.y) %>% dplyr::mutate(VAR = Slope.y)
      }
      date_gps <- df %>% dplyr::select(GPS, DateTime)
      x <- x %>% dplyr::mutate(Source = "Correct") %>% dplyr::bind_cols(date_gps)
      y <- y %>% dplyr::mutate(Source = "Candidate") %>% dplyr::bind_cols(date_gps)
      return(dplyr::bind_rows(x, y))
    }
    
    output$gps_totals <- renderTable(gps_totals())
    output$gps_distance <- renderTable(summarise_flag(current_data(), Distance.y, DistanceFlag))
    output$gps_distance_time <- renderTable(summarise_flag(current_data(), TimeDiffMins, DistanceFlag))
    
    session$onSessionEnded(stopApp)
  }
  
  shinyApp(ui = ui, server = server)
}

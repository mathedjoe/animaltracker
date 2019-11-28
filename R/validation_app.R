if(getRversion() >= '2.5.1') {
  globalVariables(c('demo_comparison', 'RateSignal', 'cumDistSignal', 'Dropped.x',
                    'Dropped.y', 'VAR', 'Source', 'Signal', 'Flag', 'Rate.y',
                    'Course.y', 'Distance.y', 'cumDist.x', 'cumDistLower',
                    'cumDistUpper', 'RateLower', 'RateUpper'))
}
#'
#'Run the Shiny validation app
#'
#'@export
run_validation_app <- function() {
  options(shiny.maxRequestSize=100*1024^2)
  
  
  ui <- fluidPage(theme = shinythemes::shinytheme("yeti"),
    titlePanel("Animal Data Validation"),
    sidebarLayout(
      sidebarPanel(
        h4("Upload Data"),
        helpText("Please select a correct (reference) and candidate file from your computer."),
        fileInput("correctInput", "Upload correct data (.csv)", accept=c(".csv")),
        fileInput("candidateInput", "Upload candidate data (.csv)", accept=c(".csv")),
        h4("Data Options"),
        radioButtons("outliers", "Extreme Value Detection", choices=c("None", "Modified Z-Score"), selected="None"),
        uiOutput("choose_lag"),
        uiOutput("choose_max_score"),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", div(style="display: inline-block;vertical-align:top;",
                               uiOutput("choose_date")),
                   div(style="display: inline-block;vertical-align:top;",
                       shinyWidgets::pickerInput("var", "Variable", 
                                choices=c("Cumulative Distance (+Flags)", "Rate (+Flags)", "Course (+Flags)",
                                          "Elevation", "Slope"),
                                selected="Cumulative Distance (+Flags)", multiple=FALSE)),
                   plotOutput("plot", height="1000px") %>% shinycssloaders::withSpinner()),
          tabPanel("Summary", 
                   h4("Total Flags by GPS"),
                   tableOutput("gps_totals"),
                   h4("Summary of Rate Flags by GPS"),
                   tableOutput("gps_rate"),
                   h4("Summary of Course Flags by GPS"),
                   tableOutput("gps_course"),
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
    
    ### UPDATE AND SELECT DATA
    comparison <- reactive({
      if(is.null(input$correctInput) | is.null(input$candidateInput)) {
        demo_comparison
      }
      else {
        compare_flags(read.csv(input$correctInput$datapath, skipNul = TRUE), 
                      read.csv(input$candidateInput$datapath, skipNul = TRUE))
      }
    })
    
    comparison_peaks <- reactive({
      req(input$lag, input$max_score)
      if(input$outliers == "Modified Z-Score") {
        comparison() %>% detect_peak_modz(input$lag, input$max_score)
      }
    })
    
    current_data <- reactive({
      req(input$date)
      if(input$outliers == "Modified Z-Score") {
        get_column(comparison_peaks(), input$var, input$date)
      }
      else {
        get_column(comparison(), input$var, input$date)
      }
    })
    
    gps_totals <- reactive({ 
      if(input$outliers == "Modified Z-Score") {
        comparison_peaks() %>% 
          dplyr::group_by(GPS) %>% 
          dplyr::summarise(
            RateFlags = sum(RateFlag, na.rm=TRUE),
            RateSignals = sum(RateSignal, na.rm=TRUE),
            CourseFlags = sum(CourseFlag, na.rm=TRUE),
            DistFlags = sum(DistanceFlag, na.rm=TRUE),
            cumDistSignals = sum(cumDistSignal, na.rm=TRUE),
            TotalFlags = sum(TotalFlags, na.rm=TRUE),
            Dropped.x = sum(Dropped.x, na.rm=TRUE),
            Dropped.y = sum(Dropped.y, na.rm=TRUE)
          )
      }
      else {
        comparison() %>% 
        dplyr::group_by(GPS) %>% 
        dplyr::summarise(
          RateFlags = sum(RateFlag, na.rm=TRUE),
          CourseFlags = sum(CourseFlag, na.rm=TRUE),
          DistFlags = sum(DistanceFlag, na.rm=TRUE),
          TotalFlags = sum(TotalFlags, na.rm=TRUE),
          Dropped.x = sum(Dropped.x, na.rm=TRUE),
          Dropped.y = sum(Dropped.y, na.rm=TRUE)
        )
      }
    })
    
    output$choose_date <- renderUI({
      dateInput("date", "Date", value=min(comparison()$Date, na.rm=TRUE), min=min(comparison()$Date, na.rm=TRUE), max= max(comparison()$Date, na.rm=TRUE))
    })
    
    output$choose_lag <- renderUI({
      if(input$outliers == "Modified Z-Score") {
        numericInput("lag", "Lag (# observations for rolling median/MAD, must be odd)", value=5)
      }
    })
    
    output$choose_max_score <- renderUI({
      if(input$outliers == "Modified Z-Score") {
        numericInput("max_score", "Modified Z-Score Cutoff (to classify observations as outliers)", value=3.5)
      }
    })
    
    
    summarise_flag <- function(df, col, flag) {
      flag <- dplyr::enquo(flag)
      col <- dplyr::enquo(col)
      df %>%
        dplyr::filter(!!flag > 0) %>% 
        dplyr::group_by(GPS) %>% 
        dplyr::summarise(
          Min.y = min(!!col),
          Q1.y = stats::quantile(!!col, 0.25, na.rm=TRUE),
          Median.y = stats::median(!!col),
          Mean.y = mean(!!col),
          Q3.y = stats::quantile(!!col, 0.75, na.rm=TRUE),
          Max.y = max(!!col)
        )
    }
    
    get_column <- function(df, choice, date) {
      df <- df %>% dplyr::ungroup() %>% dplyr::filter(Date == as.Date(date))
      if(choice == "Cumulative Distance (+Flags)") {
        x <- df %>% dplyr::select(cumDist.x) %>% dplyr::mutate(VAR = cumDist.x)
        if(input$outliers == "Modified Z-Score") {
          y <- df %>% dplyr::select(cumDist.y, DistanceFlag, cumDistLower, cumDistUpper, cumDistSignal) %>% 
            dplyr::mutate(VAR = cumDist.y, 
                          Flag = DistanceFlag, 
                          Lower = cumDistLower, 
                          Upper = cumDistUpper,
                          Signal = cumDistSignal)
        }
        else {
          y <- df %>% dplyr::select(cumDist.y, DistanceFlag) %>% 
            dplyr::mutate(VAR = cumDist.y, 
                          Flag = DistanceFlag)
        }
      }
      else if(choice == "Rate (+Flags)") {
        x <- df %>% dplyr::select(Rate.x) %>% dplyr::mutate(VAR = Rate.x)
        if(input$outliers == "Modified Z-Score") {
          y <- df %>% dplyr::select(Rate.y, RateFlag, RateLower, RateUpper, RateSignal) %>% 
            dplyr::mutate(VAR = Rate.y, 
                          Flag = RateFlag,
                          Lower = RateLower,
                          Upper = RateUpper,
                          Signal = RateSignal)
        }
        else {
          y <- df %>% dplyr::select(Rate.y, RateFlag) %>% 
            dplyr::mutate(VAR = Rate.y, 
                          Flag = RateFlag)
        }
      }
      else if(choice == "Course (+Flags)") {
        x <- df %>% dplyr::select(Course.x) %>% dplyr::mutate(VAR = Course.x)
        y <- df %>% dplyr::select(Course.y, CourseFlag) %>% dplyr::mutate(VAR = Course.y, Flag = CourseFlag)
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
    
    ### UPDATE PLOTS AND SUMMARY TABLES
    output$plot <- renderPlot(
      if(input$var == "Cumulative Distance (+Flags)" | input$var == "Rate (+Flags)" | input$var == "Course (+Flags)") {
        if((input$var == "Cumulative Distance (+Flags)" | input$var == "Rate (+Flags)") & input$outliers == "Modified Z-Score") {
            ggplot(data=current_data(), aes(x=DateTime, y=VAR, group=Source, color=Source)) +
              geom_line(aes(size = Source)) +
              geom_point(data=current_data() %>% dplyr::mutate(Signal = ifelse(is.na(Signal), 0, Signal)) %>% dplyr::filter(Signal==1), aes(x=DateTime, y=VAR), color="black") +
              #(data=current_data() %>% dplyr::filter(Source == "Candidate"), aes(x=DateTime, y=Upper), color="black") +
              #geom_line(data=current_data() %>% dplyr::filter(Source == "Candidate"), aes(x=DateTime, y=Lower), color="black") +
              scale_color_discrete(guide = guide_legend(reverse = T)) +
              scale_size_manual(values=c(2, 1))+
              facet_wrap(vars(GPS), ncol=3) +
              theme_minimal()
        }
        else {
          ggplot(data=current_data(), aes(x=DateTime, y=VAR, group=Source, color=Source)) +
            geom_line(aes(size = Source)) +
            geom_point(data=current_data() %>% dplyr::mutate(Flag = ifelse(is.na(Flag), 0, Flag)) %>% dplyr::filter(Flag==1), aes(x=DateTime, y=VAR), color="black") +
            scale_color_discrete(guide = guide_legend(reverse = T)) +
            scale_size_manual(values=c(2, 1))+
            facet_wrap(vars(GPS), ncol=3) +
            theme_minimal()
        }
      }
      else {
        ggplot(data=current_data(), aes(x=DateTime, y=VAR, group=Source, color=Source)) +
          geom_line(aes(size = Source)) +
          scale_color_discrete(guide = guide_legend(reverse = T)) +
          scale_size_manual(values=c(2, 1))+
          facet_wrap(vars(GPS), ncol=3) +
          theme_minimal()
      }
    )
    
    output$gps_totals <- renderTable(gps_totals())
    output$gps_rate <- renderTable(summarise_flag(comparison(), Rate.y, RateFlag))
    output$gps_course <- renderTable(summarise_flag(comparison(), Course.y, CourseFlag))
    output$gps_distance <- renderTable(summarise_flag(comparison(), Distance.y, DistanceFlag))
    output$gps_distance_time <- renderTable(summarise_flag(comparison(), TimeDiffMins, DistanceFlag))
    
    session$onSessionEnded(stopApp)
    }
  shinyApp(ui = ui, server = server)
}

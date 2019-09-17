library(shiny)
library(dplyr)
library(ggplot2)

comparison <- read.csv("inst/extdata/comparison.csv") %>% 
  tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
  tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
  tibble::add_column(cumDist.x=NA, .after="Distance.x") %>% 
  tibble::add_column(cumDist.y=NA, .after="Distance.y") %>% 
  dplyr::mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S")) %>% 
  dplyr::mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>% 
  dplyr::group_by(GPS, Date) %>% 
  dplyr::arrange(DateTime, .by_group = TRUE) %>% 
  dplyr::mutate(Distance.y = dplyr::lag(Distance.y,1), 
                cumDist.x = cumsum(Distance.x),
                cumDist.y = cumsum(Distance.y),
                TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > 100), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), 
                TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
                Dropped = ifelse((TotalFlags < 2 & !DistanceFlag), 0, 1))
  
gps_totals <- comparison %>% 
  dplyr::group_by(GPS) %>% 
  dplyr::summarise(
    RateFlags = sum(RateFlag, na.rm=TRUE),
    CourseFlags = sum(CourseFlag, na.rm=TRUE),
    DistFlags = sum(DistanceFlag, na.rm=TRUE),
    TotalFlags = sum(TotalFlags, na.rm=TRUE),
    Dropped = sum(Dropped, na.rm=TRUE)
  )

gps_distance <- comparison %>%
  dplyr::filter(DistanceFlag > 0) %>% 
  dplyr::group_by(GPS) %>% 
  dplyr::summarise(
    Min.y = min(Distance.y),
    Q1.y = quantile(Distance.y, 0.25),
    Median.y = median(Distance.y),
    Mean.y = mean(Distance.y),
    Q3.y = quantile(Distance.y, 0.75),
    Max.y = max(Distance.y)
  )

gps_distance_time <- comparison %>%
  dplyr::filter(DistanceFlag > 0) %>% 
  dplyr::group_by(GPS) %>% 
  dplyr::summarise(
    Min = min(TimeDiffMins),
    Q1 = quantile(TimeDiffMins, 0.25),
    Median = median(TimeDiffMins),
    Mean.y = mean(TimeDiffMins), 
    Q3 = quantile(TimeDiffMins, 0.75),
    Max = max(TimeDiffMins)
  )
  
  
  
  
  
  
get_column <- function(df, choice, date) {
  df <- df %>% dplyr::ungroup() %>% dplyr::filter(Date == as.Date(date))
  if(choice == "Cumulative Distance (+Flags)") {
    x <- df %>% dplyr::select(cumDist.x) %>% dplyr::mutate(cumDist = cumDist.x)
    y <- df %>% dplyr::select(cumDist.y) %>% dplyr::mutate(cumDist = cumDist.y)
  }
  else if(choice == "Rate (+Flags)") {
    x <- df %>% dplyr::select(Rate.x) %>% dplyr::mutate(Rate = Rate.x)
    y <- df %>% dplyr::select(Rate.y) %>% dplyr::mutate(Rate = Rate.y)
  }
  else if(choice == "Course (+Flags)") {
    x <- df %>% dplyr::select(Course.x) %>% dplyr::mutate(Course = Course.x)
    y <- df %>% dplyr::select(Course.y) %>% dplyr::mutate(Course = Course.y)
  }
  else if(choice == "Elevation") {
    x <- df %>% dplyr::select(Elevation.x) %>% dplyr::mutate(Elevation = Elevation.x)
    y <- df %>% dplyr::select(Elevation.y) %>% dplyr::mutate(Elevation = Elevation.y)
  }
  else {
    x <- df %>% dplyr::select(Slope.x) %>% dplyr::mutate(Slope = Slope.x)
    y <- df %>% dplyr::select(Slope.y) %>% dplyr::mutate(Slope = Slope.y)
  }
  date_gps <- df %>% dplyr::select(GPS, DateTime)
  x <- x %>% dplyr::mutate(Source = "Correct") %>% dplyr::bind_cols(date_gps)
  y <- y %>% dplyr::mutate(Source = "Candidate") %>% dplyr::bind_cols(date_gps)
  return(dplyr::bind_rows(x, y))
}

run_arizona_app <- function() {
  ui <- fluidPage(
    titlePanel("Arizona Data Comparison w/ Flags"),
    sidebarLayout(
      sidebarPanel(
        dateInput("date", "Date", value="2018-03-05",
                  min="2018-03-05", max="2018-05-26"),
        radioButtons("var", "Variable", 
                     choices=c("Cumulative Distance (+Flags)", "Rate (+Flags)", "Course (+Flags)",
                               "Elevation", "Slope"),
                     selected="Cumulative Distance (+Flags)")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
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
    
    current_data <- reactive({
      print(get_column(comparison, input$var, input$date))
    })
    
    output$plot <- renderPlot(
      ggplot(data=current_data(), aes(x=DateTime, y=cumDist, group=Source, color=Source)) +
        geom_line() +
        scale_color_discrete(guide = guide_legend(reverse = T)) +
        facet_wrap(vars(GPS))
    )
    
    output$gps_totals <- renderTable(gps_totals)
    output$gps_distance <- renderTable(gps_distance)
    output$gps_distance_time <- renderTable(gps_distance_time)
    
    session$onSessionEnded(stopApp)
  }
  
  shinyApp(ui = ui, server = server)
}
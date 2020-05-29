#'
#''shiny' module UI output for the animaltracker app's plots tab.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' plotOutput object
#'
reactivePlotOutput <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's summary statistics tables.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param plot_type plot type to generate
#'@param dat animal data frame 
#'@return 'shiny' renderPlot object
#'
reactivePlot <- function(input, output, session, plot_type, dat) {
  req(dat)
    if(plot_type == "line") {
      output$plot_out <- renderPlot({
        ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
          labs( title = "Elevation Time Series, by Animal",
                x = "Date",
                y = "Elevation (meters)") +
          ylim(1000,2000) + 
          geom_line(na.rm = TRUE) + 
          geom_point(na.rm = TRUE) + 
          theme_minimal()
      })
    }
    else if(plot_type == "hist") {
      output$plot_out <- renderPlot({
        ggplot(dat(), aes(x=TimeDiffMins, fill=Animal)) +
          geom_histogram(  col="White", breaks = seq(0,40, 2)) +
          facet_wrap(~Animal, ncol=2)+
          labs( title = "Sample Rate, by GPS Unit" ,
                x = "Time between GPS Readings (minutes)", 
                y = "Frequency") + 
          theme_minimal()
      })
    }
    else if(plot_type == "violin") {
      output$plot_out <- renderPlot({
        ggplot(dat() %>% dplyr::filter(Rate < 50), aes(x=Animal, y= Rate, fill=Animal))+
          geom_violin() + 
          geom_boxplot(width=.2, outlier.color = NA) +
          theme_minimal()+
          labs( title = "Rate of Travel, by GPS Unit" ,
                x = "Animal", 
                y = "Rate of Travel (meters/minute)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    else if(plot_type == "heatmap") {
      mybreaks <- reactive({
        list(x = round( seq(min(dat()$Longitude), max(dat()$Longitude), length.out = 10 ),3),
            y = round( seq(min(dat()$Latitude), max(dat()$Latitude), length.out = 10 ),3))
      })
      output$plot_out <- renderPlot({
        ggplot(dat() %>% 
               dplyr::mutate( LongBin = cut_number(Longitude, 100, 
                                                   labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
               ),
               LatBin = cut_number(Latitude, 100, 
                                   labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
               )) %>%
               group_by(LongBin, LatBin, Animal) %>%
               summarize(Duration = sum(TimeDiffMins, na.rm = TRUE)/60), 
             aes (x = LongBin,  y = LatBin, fill = Duration))+
        geom_tile() +
        facet_wrap(~Animal, ncol=2)+
        labs( title = "Total Time Spent per Location (hours)" ,
              x = "Longitude", 
              y = "Latitude")+
        scale_fill_gradientn(colors = c("white", "green", "red")) +
        scale_x_discrete( breaks = mybreaks()$x) +
        scale_y_discrete( breaks = mybreaks()$y) +
        coord_equal() +
        theme_minimal()
      }, height = 1200)
    }
}
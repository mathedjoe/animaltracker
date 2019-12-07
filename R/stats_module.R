#'
#'Shiny Module UI output for the animaltracker app's summary statistics labels.
#'
#'@param id chosen ID of UI output
#'
statsLabelOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("label"))
}


#'
#'Shiny Module server-side UI generator for the animaltracker app's summary statistics labels.
#'
#'@param input Shiny server input, automatically populated
#'@param output Shiny server output, automatically populated
#'@param session Shiny server session, automatically populated
#'@param selected_cols selected columns from animaltracker app input
#'@param selected_stats selected summary statistics from animaltracker app input
#'@param col_name column name to compute summary statistics
#'@param text text of summary statistics label
#'
statsLabel <- function(input, output, session, selected_cols, selected_stats, col_name, text) {
  output$label <- renderUI({
    if(is.null(selected_stats()) | is.null(selected_cols()) | !(col_name %in% selected_cols())) 
      return()
    h4(text)
  })
}

#'
#'Shiny Module UI output for the animaltracker app's summary statistics tables.
#'
#'@param id chosen ID of UI output
#'
statsOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("stats_table"))
}

#'
#'Shiny Module server-side UI generator for the animaltracker app's summary statistics tables.
#'
#'@param input Shiny server input, automatically populated
#'@param output Shiny server output, automatically populated
#'@param session Shiny server session, automatically populated
#'@param selected_cols selected columns from animaltracker app input
#'@param selected_stats selected summary statistics from animaltracker app input
#'@param col_name column name to compute summary statistics
#'@param col column to compute summary statistics
#'@param dat animal data frame containing col
#'
stats <- function(input, output, session, selected_cols, selected_stats, col_name, col, dat) {
  generate_table <- reactive({
    if(!(col_name %in% selected_cols()) | is.null(selected_stats())) 
      return()
    summary <- summarise_col(dat(), TimeDiffMins)
    renderTable(subset(summary, select=c("Animal", selected_stats())))
  })
  observe ({
    output$stats_table <- generate_table()
  })
}

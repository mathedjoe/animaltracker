#'
#'Shiny Module UI output for the animaltracker app's basic dropdown selections.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' uiOutput object for dropdown selection
#'
staticPickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("static_picker_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's basic dropdown selections.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param selected_ani selected animals from animaltracker app input
#'@param text title for picker
#'@param choices vector of possible choices for picker
#'@param min_selected index of lowest selected value in choices
#'@param max_selected index of highest selected value in choices
#'@return 'shiny' renderUI object for dropdown selection
#'
staticPicker <- function(input, output, session, selected_ani, text, choices, min_selected, max_selected) {
  ns <- session$ns
  output$static_picker_out <- renderUI({ 
    req(selected_ani)
    shinyWidgets::pickerInput(ns("static_picker"), text,
                              choices = choices,
                              selected = choices[min_selected:max_selected],
                              multiple = TRUE,
                              inline = FALSE, options = list(`actions-box` = TRUE))
  })
  return(reactive({input$static_picker}))
}

#'
#''shiny' module UI output for the animaltracker app's dynamic dropdown selections.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' uiOutput object for dropdown selection
#'
reactivePickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("reactive_picker_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's dynamic dropdown selections.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param type purpose of picker - currently supported types are "site", "ani", and "recent"
#'@param req_list list of reactive statements required to display picker
#'@param text title for picker
#'@param min_selected index of lowest selected value in possible choices, should be null if type is "recent"
#'@param max_selected index of highest selected value in possible choices should be null if type is "recent"
#'@param multiple logical, whether to allow selecting multiple values
#'@param options options for shinyWidgets pickerInput
#'@return 'shiny' renderUI object for dropdown selection
#'
reactivePicker <- function(input, output, session, type, req_list, text, min_selected = NULL, max_selected = NULL, multiple, options = NULL) {
  ns <- session$ns
  output$reactive_picker_out <- renderUI({
    
    if(type == "site" | type == "ani") {
      if(is.null(req_list$meta())) {
        return()
      }
      meta <- req_list$meta()
      
      if(type == "site") {
        choices <- as.list(as.character(unique(meta$site)))
      }
      else {
        
        if(is.null(req_list$selected_site())) {
          return()
        }
        
        selected_site <- req_list$selected_site()
        if(nrow(meta %>% dplyr::filter(site %in% selected_site)) > 0) {
          meta <- meta %>% dplyr::filter(site %in% selected_site) 
        }
        choices <- as.list(as.character(unique(meta$ani_id)))
      }
      
      selected <- choices[min_selected:max_selected]
    }
    else if(type == "recent") {
      if(is.null(req_list$selected_ani()) || is.null(req_list$cache()) || 
         is.null(req_list$dates()) || is.null(req_list$min_time()) ||
         is.null(req_list$max_time())) {
        
        return()
      }
      
      ani_names <- paste(req_list$selected_ani(), collapse = ", ")
      choices <- names(req_list$cache())
      selected <- paste0(ani_names, ", ", req_list$dates()[1], " ", req_list$min_time(), "-", req_list$dates()[2], " ", req_list$max_time())
      if(paste(selected, "(processed)") %in% choices) {
        selected <- paste(selected, "(processed)")
      }
    
    }
    shinyWidgets::pickerInput(ns("reactive_picker"), text,
                              choices = choices,
                              selected = selected,
                              multiple = multiple, 
                              inline = FALSE, options = options)
  })
  return(reactive({input$reactive_picker}))
}

#'
#''shiny' module UI output for the animaltracker app's date picker.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' uiOutput for date picker
#'
datePickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("date_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's date picker.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param req_list list of reactive statements required to display picker
#'@param text title for picker
#'@return 'shiny' renderUI object for date picker
#'
datePicker <- function(input, output, session, req_list, text) {
  ns <- session$ns
  output$date_out <- renderUI({
    
    if(is.null(req_list$meta()) || is.null(req_list$selected_ani())) {
      return()
    }
    
    meta <- req_list$meta()
    
    max_date <- max(as.Date(meta$max_date), na.rm=TRUE)
    min_date <- min(as.Date(meta$min_date), na.rm=TRUE)
    
    dateRangeInput(ns("dates"), "Date Range", start = min_date, end = max_date, min = min_date,
                max = max_date)
  })
  
  return(reactive({input$dates}))
}

#'
#''shiny' module UI output for the animaltracker app's coordinate range input.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' uiOutput for coordinate range input
#'
reactiveRangeOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("range_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's coordinate range input.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param type latitude or longitude
#'@param dat animal data frame 
#'@return 'shiny' renderUI object for coordinate range input
#'
reactiveRange <- function(input, output, session, type, dat) {
  ns <- session$ns
  output$range_out <- renderUI({
    
    if(type == "latitude") {
      shinyWidgets::numericRangeInput(ns("range"), "Latitude Range:", value = c(dat()$min_lat, dat()$max_lat))
    }
    else if(type == "longitude") {
      shinyWidgets::numericRangeInput(ns("range"), "Longitude Range:", value = c(dat()$min_long, dat()$max_long))
    }
  })
  
  return(reactive({input$range}))
}

#'
#''shiny' module UI output for the animaltracker app's time input
#'
#'@param id chosen ID of UI output
#'@return 'shiny' uiOutput for time input
#'
timeOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("time_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's time input.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param type min or max
#'@param meta animal metadata from app, must be non-empty for time input to display
#'@param selected_ani selected animals from app, must be non-empty for time to display
#'@return 'shiny' renderUI object for time input 
#'
time <- function(input, output, session, type, meta, selected_ani) {
  ns <- session$ns
  output$time_out <- renderUI({
    
    if(any(is.null(c(meta(), selected_ani())))) {
      return()
    }
    
    meta <- meta()
    
    if(type == "min") {
      textInput(ns("selected_time"), "Min Time", value = strftime(min(meta$min_date), format="%H:%M:%S", tz="UTC"), placeholder = "HH:MM:SS")
    }
    else if(type == "max") {
      textInput(ns("selected_time"), "Max Time", value = strftime(max(meta$max_date), format="%H:%M:%S", tz="UTC"), placeholder = "HH:MM:SS")
    }
  })
  
  return(reactive({input$selected_time}))
}

#'
#'Shiny Module UI output for the animaltracker app's basic dropdown selections.
#'
#'@param id chosen ID of UI output
#'
staticPickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("static_picker_out"))
}

#'
#'Shiny Module server-side UI generator for the animaltracker app's basic dropdown selections.
#'
#'@param input Shiny server input, automatically populated
#'@param output Shiny server output, automatically populated
#'@param session Shiny server session, automatically populated
#'@param selected_ani selected animals from animaltracker app input
#'@param text title for picker
#'@param choices vector of possible choices for picker
#'@param min_selected index of lowest selected value in choices
#'@param max_selected index of highest selected value in choices
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
#'Shiny Module UI output for the animaltracker app's dynamic dropdown selections.
#'
#'@param id chosen ID of UI output
#'
reactivePickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("reactive_picker_out"))
}

#'
#'Shiny Module server-side UI generator for the animaltracker app's dynamic dropdown selections.
#'
#'@param input Shiny server input, automatically populated
#'@param output Shiny server output, automatically populated
#'@param session Shiny server session, automatically populated
#'@param type purpose of picker - currently supported types are "site", "ani", and "recent"
#'@param req_list list of reactive statements required to display picker
#'@param text title for picker
#'@param min_selected index of lowest selected value in possible choices, should be null if type is "recent"
#'@param max_selected index of highest selected value in possible choices should be null if type is "recent"
#'@param multiple logical, whether to allow selecting multiple values
#'@param options options for shinyWidgets pickerInput
#'
reactivePicker <- function(input, output, session, type, req_list, text, min_selected = NULL, max_selected = NULL, multiple, options = NULL) {
  ns <- session$ns
  output$reactive_picker_out <- renderUI({
    lapply(req_list, req)
    
    if(type == "site" | type == "ani") {
      meta <- req_list$meta()
      
      if(type == "site") {
        choices <- as.list(as.character(unique(meta$site)))
      }
      else {
        selected_site <- req_list$selected_site()
        if(nrow(meta %>% dplyr::filter(site %in% selected_site)) > 0) {
          meta <- meta %>% dplyr::filter(site %in% selected_site) 
        }
        choices <- as.list(as.character(unique(meta$ani_id)))
      }
      
      selected <- choices[min_selected:max_selected]
    }
    else if(type == "recent") {
      ani_names <- paste(req_list$selected_ani(), collapse = ", ")
      selected <- paste0(ani_names, ", ", req_list$dates()[1], " ", req_list$min_time(), "-", req_list$dates()[2], " ", req_list$max_time())
      choices <- names(req_list$cache())
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
#'Shiny Module UI output for the animaltracker app's date picker.
#'
#'@param id chosen ID of UI output
#'
datePickerOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("date_out"))
}

#'
#'Shiny Module server-side UI generator for the animaltracker app's date picker.
#'
#'@param input Shiny server input, automatically populated
#'@param output Shiny server output, automatically populated
#'@param session Shiny server session, automatically populated
#'@param req_list list of reactive statements required to display slider
#'@param text title for slider
#'
datePicker <- function(input, output, session, req_list, text) {
  ns <- session$ns
  output$date_out <- renderUI({
    lapply(req_list, req)
    
    # Get the data set with the appropriate name
    
    meta <- req_list$meta()
    
    if(nrow(meta %>% dplyr::filter(ani_id %in% req_list$selected_ani())) > 0) {
      meta <- meta %>% dplyr::filter(ani_id %in% req_list$selected_ani())
    }
    
    max_date <- max(as.Date(meta$max_date), na.rm=TRUE)
    min_date <- min(as.Date(meta$min_date), na.rm=TRUE)
    
    dateRangeInput(ns("dates"), "Date Range", start = min_date, end = max_date, min = min_date,
                max = max_date)
  })
  
  return(reactive({input$dates}))
}

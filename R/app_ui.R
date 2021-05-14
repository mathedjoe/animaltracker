### UI the Shiny App

#'
#'Defines a user interface for the 'shiny' app
#'
#'
#'@return ui function for use in a 'shiny' app
#'@export
#'

app_ui <- function(){
  jsCode <- 'shinyjs.removePolygon = function() {
    var event = document.createEvent("Event");
    event.initEvent("click", true, true);
    var trashButton = document.getElementsByClassName("leaflet-draw-edit-remove");
    !trashButton[0].dispatchEvent(event);
    var clearButton = document.querySelector(\'[title="Clear all layers"]\');
    !clearButton.dispatchEvent(event);
  }'
    navbarPage(theme = shinythemes::shinytheme("yeti"),
             header = div(shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(text = jsCode, functions = "removePolygon")),
             title = "Animal Tracker",
             
             ## DATA PANEL
             tabPanel("Data", 
                      sidebarLayout(
                        sidebarPanel( 
                          h4("1. Upload Data"),
                          helpText("Select a zip folder on your computer containing .csv files. Please upload data from one
                                   area at a time."),

                          fileInput("zipInput", "Upload zip file", accept=c(".zip")),
                          textOutput("numUploaded"),
                          hr(),
                          
                          h4("2. Select Data"),
                          reactivePickerOutput("choose_site") %>% shinycssloaders::withSpinner(),
                          reactivePickerOutput("choose_ani"),
                          datePickerOutput("choose_dates"),
                          timeOutput("min_time"),
                          timeOutput("max_time"),
                          
                          hr(),
                          
                          h4("3. Data Processing"),
                          shinyBS::bsCollapse(id = "uploadOptions", open = "Elevation Options",
                                     shinyBS::bsCollapsePanel("Cleaning Options",
                                                     checkboxInput("filterBox", label = "Filter bad data points", value = TRUE),
                                                     shinyBS::bsCollapsePanel("Filter Configuration Options",
                                                     	uiOutput("max_rate"),
                                                     	uiOutput("max_course"),
                                                     	uiOutput("max_dist"),
                                                     	uiOutput("max_time")
						     ),

                                                     checkboxInput("kalman_enable", label = "Cluster data with Kalman filtering", value = FALSE),
                                                     shinyBS::bsCollapsePanel("Kalman Configuration Options",
                                                       uiOutput("kalman_max_timestep")
                                                     )
                                     ),
                                     shinyBS::bsCollapsePanel("Elevation Options",
                                                     reactiveRangeOutput("lat_bounds"),
                                                     reactiveRangeOutput("long_bounds"),
                                                     uiOutput("zoom"),
                                                     checkboxInput("slopeBox", label = "Include slope", value = TRUE),
                                                     checkboxInput("aspectBox", label = "Include aspect", value = TRUE)
                                     )
                          ),
                          actionButton("processButton", "Process All"),
                          actionButton("processSelectedButton", "Process Selected"),
                          
                          hr(),
                          
                          h4("4. Download Data"),
                          helpText("Save data to a (potentially large) .csv file."),
                          # Button
                          shinyBS::bsCollapse(id = "downloadOptionsPanel",
                                     shinyBS::bsCollapsePanel("Download Options",
                                                     radioButtons("downloadOptions", NULL,
                                                                  c("Processed (unfiltered) data", "Processed (filtered) data", "Currently selected data"),
                                                                  selected = "Currently selected data"))
                                     ),
                          downloadButton("downloadData", "Download")
                          
                        ),#sidebarPanel
                        mainPanel(
                          
                          leafletOutput("mainmap", height = 640) %>% shinycssloaders::withSpinner(),
                          htmlOutput("mapinfo"),
                          actionButton("generateGif", "Create movement animation"),
                          br(),
                          br(),
                          h4("Display Fencing"),
                          fileInput("kmzInput", "Upload kmz file", accept=c(".kmz")),
                          h4("Recent Data"),
                          reactivePickerOutput("choose_recent"),
                          textOutput("nrow_recent"),
                          br(),
                          tableOutput("head_recent")
                          
                        ) #mainPanel
                      ) #sidebarLayout
             ), #end data panel
             
             ## PLOTS PANEL
             tabPanel("Plots",
                      reactivePlotOutput("plot_elevation_line"),
                      reactivePlotOutput("plot_samplerate_hist"),
                      reactivePlotOutput("plot_rate_violin"),
                      reactivePlotOutput("plot_time_heatmap")
                        
                         
             ),# end plots panel
             ## ANALYSIS PANEL
             tabPanel("Analysis",
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Variables"),
                          staticPickerOutput("choose_cols"),
                          hr(),
                          
                          h4("Summary Statistics"),
                          staticPickerOutput("choose_stats")
                        ),
                        
                        mainPanel(
                          h2("Statistical Summary of Selected Data"),
                          helpText("To change the sample, switch to the Data panel."),
                         
                          statsLabelOutput("elevation_title"),
                          statsOutput("elevation"),
                          statsLabelOutput("speed_title"),
                          statsOutput("speed"),
                          statsLabelOutput("timediff_title"),
                          statsOutput("timediff"),
                          statsLabelOutput("course_title"),
                          statsOutput("course"),
                          statsLabelOutput("coursediff_title"),
                          statsOutput("coursediff"),
                          statsLabelOutput("distance_title"),
                          statsOutput("distance"),
                          statsLabelOutput("rate_title"),
                          statsOutput("rate"),
                          statsLabelOutput("slope_title"),
                          statsOutput("slope"),
                          statsLabelOutput("aspect_title"),
                          statsOutput("aspect")
                        )
                      )
                      
             ),# end analysis panel
             
             ## Secondary navigation "More" Panel
             navbarMenu("More",
                        ## ABOUT PANEL
                        tabPanel("About",
                                 p("This app is part of a larger project to integrate GPS tracking data for animals (e.g., cattle) with data management and visualization tools. If you're interesting in joining our efforts, please reach out to one of the collaborators."),
                                 h2("Get the Code"),
                                 p("The code for this Shiny App and related functions is stored on github. "),
                                 a(h4("View Code", class = "btn btn-default action-button" , 
                                             style = "fontweight:600"), target = "_blank",
                                          href = 'https://github.com/mathedjoe/animaltracker'),
                                h2("Usage"),
                                p("The following R code will install the package and run this app in your browser. (For more advanced usage, please consult the documentation and codebase, or reach out to the contributors.)"),
                                code("install.packages('devtools')  # (if not already installed)", tags$br(),
                                     "devtools::install_github('mathedjoe/animaltracker') # installs the package", tags$br(),
                                     "library(animaltracker) # loads the package", tags$br(),
                                     "run_shiny_animaltracker() # runs the app"),
                                h2("Contributors"),
                                tags$ul(
                                  tags$li("Sergio Arispe (lead researcher), Oregon State University,", a("sergio.arispe@oregonstate.edu", href='mailto:Sergio.Arispe@oregonstate.edu'), 
                                     HTML(",&nbsp;"), a("website",href = 'https://extension.oregonstate.edu/people/sergio-arispe', target="_blank" )),
                                  tags$li("Joe Champion (lead developer), Boise State University,", a("joechampion@boisestate.edu", href='mailto:joechampion@boisestate.edu'), 
                                     HTML(",&nbsp;"), a("website",href = 'https://math.boisestate.edu/jchampion/', target="_blank" )),
                                  tags$li("Thea Sukianto (student assistant), Boise State University,", a("TheophiliaSukian@u.boisestate.edu", href='mailto:TheophiliaSukian@u.boisestate.edu')),
                                  tags$li("Chithkala Dhulipati (student assistant), Boise State University,", a("chithkaladhulipa@u.boisestate.edu ", href='mailto:chithkaladhulipa@u.boisestate.edu ')),
                                  tags$li("Dylan Mikesell (researcher), Boise State University,", a("dylanmikesell@boisestate.edu", href='mailto:dylanmikesell@boisestate.edu'))
                                  
                                )
                        ))
  )
}

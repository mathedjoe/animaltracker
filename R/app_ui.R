### UI the Shiny App

#'
#'Defines a user interface for the shiny app
#'
#'
#'@return ui function for use in a shiny app
#'@export
#'

app_ui <- function(){
    require(shiny)
    require(shinyjs)
    require(V8)
    require(shinyBS)
    navbarPage(theme = shinytheme("yeti"),
             # shinythemes::themeSelector(),  # <--- run this to choose a style theme
             header = div(useShinyjs(),
                          extendShinyjs(script = "js/removepolygon.js")),
             title = "Animal Tracker",
             
             ## DATA PANEL
             tabPanel("Data", 
                      sidebarLayout(
                        sidebarPanel( 
                          h4("Upload Data"),
                          helpText("Select a zip folder on your computer containing .csv files. Please upload data from one
                                   area at a time."),

                          fileInput("zipInput", "Upload zip file", accept=c(".zip")),
                          textOutput("numUploaded"),
                          hr(),
                          
                          h4("Data Processing"),
                          bsCollapse(id = "uploadOptions",
                                     bsCollapsePanel("Cleaning Options",
                                                     checkboxInput("autocleanBox", label = "Autoclean lat/long/altitude/distance", value = FALSE),
                                                     checkboxInput("filterBox", label = "Filter bad data points", value = TRUE)
                                     ),
                                     bsCollapsePanel("Elevation Options",
                                                     uiOutput("lat_bounds"),
                                                     uiOutput("long_bounds"),
                                                     checkboxInput("slopeBox", label = "Include slope", value = TRUE),
                                                     checkboxInput("aspectBox", label = "Include aspect", value = TRUE)
                                     )
                          ),
                          actionButton("processButton", "Process Data"),
                          
                          hr(),
                          
                          h4("Select Data"),
                          uiOutput("choose_site") %>% withSpinner(),
                          uiOutput("choose_ani"),
                          uiOutput("choose_dates"),
                          # uiOutput("choose_times") %>% withSpinner(),
                          
                          hr(),
                          
                          h4("Download Data"),
                          helpText("Save data to a (potentially large) .csv file."),
                          # Button
                          bsCollapse(id = "downloadOptionsPanel",
                                     bsCollapsePanel("Download Options",
                                                     radioButtons("downloadOptions", NULL,
                                                                  c("Processed (unfiltered) data", "Processed (filtered) data", "Currently selected data")))
                                     ),
                          downloadButton("downloadData", "Download")
                          
                        ),#sidebarPanel
                        mainPanel(
                          
                          leafletOutput("mainmap", height = 640) %>% withSpinner(),
                          htmlOutput("mapinfo"),
                          h4("Recent Data"),
                          uiOutput("choose_recent")
                          
                          
                        ) #mainPanel
                      ) #sidebarLayout
             ), #end data panel
             
             ## PLOTS PANEL
             tabPanel("Plots",
                      plotOutput("plot_elevation_line"),
                      plotOutput("plot_samplerate_hist"),
                      plotOutput("plot_rate_violin"),
                      plotOutput("plot_time_heatmap", height = 1200)
                        
                         
             ),# end plots panel
             ## ANALYSIS PANEL
             tabPanel("Analysis",
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Variables"),
                          uiOutput("choose_cols"),
                          hr(),
                          
                          h4("Summary Statistics"),
                          uiOutput("choose_stats")
                        ),
                        
                        mainPanel(
                          h2("Statistical Summary of Selected Data"),
                          helpText("To change the sample, switch to the Data panel."),
                         
                          uiOutput("elevation_title"),
                          uiOutput("elevation"),
                          uiOutput("speed_title"),
                          uiOutput("speed"),
                          uiOutput("timediff_title"),
                          uiOutput("timediff"),
                          uiOutput("course_title"),
                          uiOutput("course"),
                          uiOutput("coursediff_title"),
                          uiOutput("coursediff"),
                          uiOutput("distance_title"),
                          uiOutput("distance"),
                          uiOutput("rate_title"),
                          uiOutput("rate"),
                          uiOutput("slope_title"),
                          uiOutput("slope"),
                          uiOutput("aspect_title"),
                          uiOutput("aspect")
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
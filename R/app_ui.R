### UI the Shiny App

#'
#'Defines a user interface for the shiny app
#'
#'
#'@return ui function for use in a shiny app
#'@export
#'

app_ui <- navbarPage(theme = shinytheme("yeti"),
           # shinythemes::themeSelector(),  # <--- run this to choose a style theme
           
           title = "Animal Tracker",
           
           ## DATA PANEL
           tabPanel("Data", 
                    sidebarLayout(
                      sidebarPanel( 
                        h4("Select Data"),
                        uiOutput("choose_site") %>% withSpinner(),
                        uiOutput("choose_ani"),
                        uiOutput("choose_dates"),
                        uiOutput("choose_times") %>% withSpinner(),
                        
                        hr(),
                        
                        h4("Upload Data"),
                        helpText("Select a zip folder on your computer containing .csv files."),
                        fileInput("zipInput", "Upload Archived Folder", accept=c(".zip", ".7z"))
                        
                      ),#sidebarPanel
                      mainPanel(
                        leafletOutput("mainmap", height = 640) %>% withSpinner()
                        
                      ) #mainPanel
                    ) #sidebarLayout
           ), #end data panel
           
           ## PLOTS PANEL
           tabPanel("Plots",
                    sidebarLayout(
                      
                      sidebarPanel(
                        h4("Select Plots")
                      ),
                      
                      mainPanel(
                        plotOutput("plot1"),
                        plotOutput("plot2")
                        
                      )
                    )         
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
                        uiOutput("timediff_title"),
                        uiOutput("timediff"),
                        uiOutput("altitude_title"),
                        uiOutput("altitude"),
                        uiOutput("speed_title"),
                        uiOutput("speed"),
                        uiOutput("course_title"),
                        uiOutput("course"),
                        uiOutput("coursediff_title"),
                        uiOutput("coursediff"),
                        uiOutput("distance_title"),
                        uiOutput("distance"),
                        uiOutput("rate_title"),
                        uiOutput("rate")
                        
                      )
                    )
                    
           ),# end analysis panel
           
           ## Secondary navigation "More" Panel
           navbarMenu("More",
                      ## ABOUT PANEL
                      tabPanel("About",
                               p("This app is part of a larger project to integrate GPS tracking data for animals (e.g., cattle) with data management and visualization tools. If you're interesting in joining our efforts, please reach out to one of the collaborators."),
                               h4("Get the Code"),
                               p("The code for this Shiny App and related functions is stored on github. "),
                               a(h4("View Code", class = "btn btn-default action-button" , 
                                           style = "fontweight:600"), target = "_blank",
                                        href = 'https://github.com/mathedjoe/animaltracker')
                               ))
)
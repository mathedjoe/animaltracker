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
           tabPanel("Data", 
                    sidebarLayout(
                      sidebarPanel(
                        tabsetPanel(type="tabs",
                                    
                                    tabPanel("Filter Data", 
                                             helpText("This app comes with demo data; use the Choose Data tab to upload your own data."),
                                             uiOutput("choose_site") %>% withSpinner(),
                                             uiOutput("choose_ani"),
                                             uiOutput("choose_dates"),
                                             uiOutput("choose_times") %>% withSpinner()
                                    ),
                                    tabPanel("Choose Data",
                                             fileInput("zipInput", "Upload Archived Folder", accept=c(".zip", ".7z"))
                                    )
                                    
                        )#tabsetPanel
                      ),#sidebarPanel
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Plots",  fluidRow(
                                      column(8, 
                                             leafletOutput("mainmap", height = 640) %>% withSpinner(),
                                             plotOutput("plot1"),
                                             plotOutput("plot2")
                                      )
                                    )),
                                    tabPanel("Statistics",
                                             fluidRow(
                                               column(3, uiOutput("choose_cols")),
                                               column(3, uiOutput("choose_stats"))
                                             ),
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
                        ) #tabsetPanel
                      ) #mainPanel
                    ) #sidebarLayout
           ),
           tabPanel("Map"),
           tabPanel("Plots"),
           navbarMenu("More",
                      tabPanel("About"))
)
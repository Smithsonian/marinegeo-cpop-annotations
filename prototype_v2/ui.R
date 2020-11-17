# shiny app demo showing SERC and STRI water quality, water level, and met data
# UI dashboard uses shinydashboard package 

header <- dashboardHeader(
  title = "MarineGEO CPOP Quality Control Dashboard",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Load data", tabName = "load_data", icon = icon("database")),
    menuItem("Annotate data", tabName = "annotate_data", icon = icon("chart-line")),
    menuItem("Review tabular data", tabName = "tabular_data", icon = icon("table")),
    menuItem("Review annotations", tabName = "review", icon = icon("clipboard-check")),
    
    uiOutput("data_info_box")
  )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem("load_data",
            
            fluidRow(
              valueBoxOutput("site_info_box", width = 3),
              valueBoxOutput("date_range_box", width = 6)
              #valueBoxOutput("annotation_progress_box", width = 3)
            ),
            
            fluidRow(
              column(width = 3,
                     box(width = NULL, 
                         status = "primary",
                         title = "Search available data",
                         
                         # Dropdown selector for sites
                         selectInput("site", "Select a site",
                                     unique(key$Site), multiple = TRUE),

                         # select a time range
                         selectInput("date_range", "Select a time range",
                                     unique(key$Date), multiple = TRUE),
                         
                         selectInput("file", "Select a file",
                                     unique(key$Filename), multiple = TRUE)
                     )
              ),
              column(width = 9,
                     box(width = NULL,
                         status = "primary",
                         title = "Select and load data",
                         
                         div(
                           tags$b("Click a row in the table and then \"Load Data\" to load the data into the application")
                         ),
                         
                         tags$br(), tags$br(),
                         
                         dataTableOutput("key"),
                         
                         tags$br(), 
                         
                         tags$head(
                           tags$style(HTML('#loadData{color:white}'))
                         ),
                         
                         # Read in data selected in table
                         actionButton("loadData", "Load data", class = "btn-primary")
                     )
              )
            )
    ),
    tabItem("annotate_data",
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         title = NULL,
                         status = "warning",
                         
                         # Accept, reject, and/or revise flags
                         uiOutput("quality_control_box")
                     )
              )
            ),
            
            fluidRow(
              column(width = 12,
                     #align = "center",
                     
                     box(width = NULL,
                         title = NULL,
                         status = "primary",

                         splitLayout(
                           style = "border: 1px solid silver;",
                           cellWidths = "25%",
                           cellArgs = list(style = "padding: 20px;"),
                           selectInput("sensor_qc", "Select a sensor",
                                       c("", names(sensor_vector_l1)),
                                       multiple = FALSE),
                           
                           # conditional dropdown for sensor parameters at selected site and sensor
                           uiOutput("parameter_qc"),
                           
                           radioButtons("label_mode", "Plot labels",
                                        choices = c("Flags",
                                                    "Codes")),
                           
                           selectInput("view_mode", "Show:",
                                       choices = c("All points",
                                                   "Flags that require review",
                                                   "Only accepted flags",
                                                   "Only rejected flags",
                                                   "Points that require codes",
                                                   "Highest level annotations"))
                         ),
                         tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                         
                         plotlyOutput("plot_qc")
                         
                     )
              )
            )
    ),
    tabItem("tabular_data",
            fluidRow(
              box(width = 12,
                  status = "primary",
                  title = "View Tabular Data",
                  
                  dataTableOutput("table_selected_points")
            ))),
    tabItem("review",
            fluidRow(
                     box(width = 12, 
                         status = "primary",
                         title = "Review Quality Control",
                         # Plot summarizes qc flags by parameter - qc flag
                         dataTableOutput("table_summary_qc"),
                         uiOutput("remove_codes"),
                         actionButton("confirm_removal", "Confirm QC code removal", class = "btn-warning"),
                         
                         div(tags$br()),
                         
                         textInput("tech_id", "Enter your technician code", "default_code"),
                         actionButton("submit_codes", "Confirm QC code selections", class = "btn-primary"))

                     
              )
            )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
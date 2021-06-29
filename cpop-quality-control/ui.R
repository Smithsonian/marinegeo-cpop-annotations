# shiny app demo showing SERC and STRI water quality, water level, and met data
# UI dashboard uses shinydashboard package 

header <- dashboardHeader(
  title = "MarineGEO CPOP Quality Control Dashboard",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  includeCSS("./www/style.css"),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Load data", tabName = "load_data", icon = icon("database")),
    menuItem("Annotate data", tabName = "annotate_data", icon = icon("chart-line")),
    menuItem("Review annotations", tabName = "review", icon = icon("clipboard-check")),
    
    uiOutput("data_info_box")
  )
)

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jscode_box_collapse, functions = c("expandBox", "collapseBox")),
  tabItems(

    ## Load data ####
    tabItem("load_data",
            
            fluidRow(
              column(width = 4,
                     box(width = NULL, 
                         status = "primary",
                         title = "Load markup data",
                         
                         # Dropdown selector for sites
                         selectInput("site_selection", "Select a site",
                                     unique(key$site_code), multiple = FALSE),
                         
                         uiOutput("year_selection"),
                         
                         uiOutput("month_selection"),
                         
                         #uiOutput("additional_months_selection"),
                         
                         # Read in data selected in table
                         actionButton("loadData", "Load data", class = "btn-primary"), tags$br(), tags$br(),
                         
                         "Once the data has loaded (the sidebar to the left will update), ",
                         "click the \"Annotate Data\" button to visualize the data and begin markup or ", 
                         "select a range of reference data below."

                     ),
                     
                     box(width = NULL, 
                         title = "Load reference data",
                         
                         "Reference data allows you to view data outside of the range you've selected for quality control. ",
                         "Add 1, 3, or 6 months of reference data to both bounds of the markup data (if available).",
                         
                         tags$br(), tags$br(),
                         selectInput("selectReferenceRange", "Choose interval",
                                     choices = c("1 month" = 1, "3 months" = 3, "6 months" = 6), multiple = FALSE),
                         
                         actionButton("loadReferenceData", "Load data"))
              ),
              column(width = 8,
                     box(width = NULL,
                         status = "primary",
                         #title = "Select and load data",
                         
                         div(
                           #tags$b("Click a row in the table and then \"Load data\" to load the data into the application")
                         ),
                         
                         tags$br(), tags$br(),
                         
                         dataTableOutput("key"),
                         
                         tags$br()

                     )
              )
            )
    ),
    
    ## Annotate data ####
    tabItem("annotate_data",
            fluidRow(
              column(width = 12,
                     box(width = NULL, title = "Plot Options", collapsible = T, id = "plot_controls_box",
                         
                         splitLayout(
                           id = "plot_controls_split_layout",
                           
                           div(class = "plot_controls",

                               annotation_controls_UI("control_plot_1")
                               #selectInput("sensor_plot_1", "Plot 1: Select a sensor",
                               #            c("", names(sensor_vector_l1)), multiple = FALSE),
                               
                               # conditional dropdown for sensor parameters at selected site and sensor
                               #uiOutput("parameter_plot_1")
                           ),
                           
                           div(class = "plot_controls",
                               annotation_controls_UI("control_plot_2")
                               
                               # selectInput("sensor_plot_2", "Plot 2: Select a sensor",
                               #             c("", names(sensor_vector_l1)), multiple = FALSE),
                               # 
                               # uiOutput("parameter_plot_2")

                           ),
                           
                           div(class = "plot_controls",
                               radioButtons("label_mode", "Plot labels",
                                            choices = c("Flags" = "flag",
                                                        "Codes" = "code"))
                           ),
                           # div(class = "plot_controls",
                           #     selectInput("view_mode", "Show:",
                           #                 choices = c("All points",
                           #                             "Flags that require review",
                           #                             "Points that require codes"))
                           # ),
                           div(class = "plot_controls",
                               uiOutput("start_date"),
                               selectInput("date_interval", label = "Select a date interval", 
                                           choices = c("All data", "1 day", "1 week", "1 month"))
                           )
                         )
                         
                     )
              )
            ),
            
            fluidRow(
              column(width = 12,
                     #align = "center",
                     
                     box(width = NULL,
                         title = NULL,
                         status = "primary",
                         
                         # Accept, reject, and/or revise flags
                         uiOutput("quality_control_box"),
                         hr(),
                         annotation_plot_UI("plot")
                         #renderUI("plot_window")
                    )
              )
            )
    ),
    
    ## Review ####
    tabItem("review",
            fluidRow(
                     box(width = 12, 
                         status = "primary",
                         title = "Review Quality Control",
                         # Plot summarizes qc flags by parameter - qc flag
                         #dataTableOutput("table_summary_qc"),
                         #uiOutput("remove_codes"),
                         #actionButton("confirm_removal", "Confirm QC code removal", class = "btn-warning"),
                         
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
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
    menuItem("Review annotations", tabName = "review", icon = icon("clipboard-check"))
  )
)

body <- dashboardBody(
  fluidRow(
    valueBoxOutput("site_info_box", width = 3),
    valueBoxOutput("date_range_box", width = 6),
    valueBoxOutput("annotation_progress_box", width = 3)
  ),
  tabItems(
    tabItem("load_data",
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
                         
                         # Read in data selected in table
                         actionButton("loadData", "Load data", class = "primary")
                     )
              )
            )
    ),
    tabItem("annotate_data",
            fluidRow(
              column(width = 3,
                     box(width = NULL,
                         title = "Select Parameters to QC",
                         status = "primary",
                         collapsible = TRUE,

                         selectInput("sensor_qc", "Select a sensor",
                                     c("", names(named_sensor_vector)),
                                     multiple = FALSE),
                         
                         # conditional dropdown for sensor parameters at selected site and sensor
                         uiOutput("parameter_qc"),
                         
                         # selectInput("parameter_qc", "Select a parameter to QC",
                         #             sensor_parameters(), multiple = FALSE),
                         uiOutput("filter_flag"),
                         
                         selectInput("parameter_reference", "Select a reference parameter",
                                     parameters, multiple = FALSE)
                         
                         # Select either zooming or selecting points
                         # radioButtons("figure_functionality", "Choose to either zoom in on data or select points for QC",
                         #              c("Zoom", "Select")),
                     ),
                     box(width = NULL,
                         title = "Select QC Tags",
                         status = "primary",
                         collapsible = TRUE,
                         
                         # select QC flags
                         selectInput("wq_qc_flags", "Water Quality QC flags",
                                     choices = wq_qc_flags$select_inputs,
                                     multiple = TRUE),
                         
                         selectInput("met_qc_flags", "MET QC flags",
                                     choices = met_qc_flags$select_inputs,
                                     multiple = TRUE),
                         
                         actionButton("apply_qc", "Apply QC flag to selected points")
                     )
                     
              ),
              column(width = 9,
                     box(width = NULL,
                         title = "Instructions",
                         collapsible = TRUE,
                         collapsed = TRUE,
                         div(
                           tags$b("To Zoom: "), "Click and drag to create a bounding box over the region of interest. ",
                           "Double-click within that box to zoom in. Double-click again to zoom back out.", tags$br(),
                           tags$b("To Select Points: "), "Create the bounding box over the points you'd like to annotate. ",
                           "Select the relevant quality control tags within the \"Select QC Flags\" box to the left of the plots, ",
                           "and then click the \"Apply QC flag to selected points\" button.", 
                           "Click once anywhere outside of the bounding box to cancel the selection.", tags$br(),
                           tags$b("To Subset Plot: "), "You can filter the plot by specifying which level 1 QC flagged-points ",
                           "you'd like to appear in the plot using the \"Plot Initial QC Flags\" input. By default, all level 1 flags are selected at start. ",
                           "Remove the flags you do not want to be plotted.", tags$br(),
                           tags$b("To Remove a Quality Control Flag: "), "You can remove a particular QC flag by navigating to the \"Review Annotations\" page, and ",
                           "selecting the flag you'd like to remove. You cannot remove individual points from an annotation."
                         )),
                     box(width = NULL,
                         title = "Quality Control Plot",
                         plotOutput("plot_qc",
                                    dblclick = "plot_dblclick",
                                    brush = brushOpts(
                                      id = "plot_brush",
                                      #direction = "x", # brush will only move horizontally
                                      resetOnNew = T # brush will be reset when the plot is updated
                                    ))
                         
                         # tabPanel(title = "Facet Plot",
                         #          # select any number of parameters to create a facet plot
                         #          selectInput("facet_parameters", "Create a facet plot by picking any number of parameters",
                         #                      choices = parameters,
                         #                      multiple = TRUE),
                         #          plotOutput("plot_facet")),
                     ),
                     box(width = NULL,
                         title = "Reference Plot",
                         collapsible = TRUE,
                         collapsed = TRUE,
                         plotOutput("plot_reference"))
                     
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
                         uiOutput("remove_flags"),
                         actionButton("confirm_removal", "Confirm QC flag removal", class = "btn-warning"),
                         
                         div(tags$br()),
                         
                         textInput("tech_id", "Enter your technician code", "default_code"),
                         actionButton("confirm_flags", "Confirm QC flag selections", class = "btn-primary"))

                     
              )
            )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
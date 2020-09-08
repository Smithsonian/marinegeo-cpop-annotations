# shiny app demo showing SERC and STRI water quality, water level, and met data
# UI dashboard uses shinydashboard package 

header <- dashboardHeader(
  title = "MarineGEO Sensor Dashboard Prototype",
  titleWidth = 450
)

body <- dashboardBody(
  fluidRow(
    box(status = "primary",
        solidHeader = F,
        collapsible = T,
        width = 12,
        fluidRow(
          column(width = 2, align = "center",
                 div(img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250))),
          column(width = 8, offset = 2,  align = "center", "Beta version: Please contact marinegeo@si.edu with any questions or issues."))),
    
    column(width = 3,
           box(width = NULL, 
               status = "primary",
               
               # # Dropdown selector for sites
               # selectInput("site", "Select a site",
               #             c("", "USA-IRL", "PAN-BDT"), multiple = FALSE),
               # 
               # # select a time range
               # selectInput("date_range", "Select a time range",
               #                key$full_timestamp, multiple = FALSE),
               
               selectInput("file", "Select a file",
                           bundled_directory, multiple = FALSE),
               
               tags$br(),
               
               # Read in data selected above
               actionButton("loadData", "Load data"),
               
               div(hr()),
               
               selectInput("sensor_qc", "Select a sensor",
                           c("", names(named_sensor_vector)),
                           multiple = FALSE),
                           
               # conditional dropdown for sensor parameters at selected site and sensor
               uiOutput("parameter_qc"),
               
               # selectInput("parameter_qc", "Select a parameter to QC",
               #             sensor_parameters(), multiple = FALSE),
               
               selectInput("parameter_reference", "Select a reference parameter",
                           parameters, multiple = FALSE),
               
               # Select either zooming or selecting points
               radioButtons("figure_functionality", "Choose to either zoom in on data or select points for QC",
                            c("Zoom", "Select")),
               
               div(hr()), 
               
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
           tabBox(width = NULL,
             tabPanel(title = "Main Panel",
                      #tags$script(jscode),
                      div("Quality Control Plot"),
                      plotOutput("plot_qc",
                                 dblclick = "plot_dblclick",
                                 brush = brushOpts(
                                   id = "plot_brush",
                                   #direction = "x", # brush will only move horizontally
                                   resetOnNew = T # brush will be reset when the plot is updated
                                 )),
                      div("Reference Plot"),
                      plotOutput("plot_reference")
             ),
             tabPanel(title = "Tabular View",
                      dataTableOutput("table_selected_points")),
             tabPanel(title = "Facet Plot",
                      # select any number of parameters to create a facet plot
                      selectInput("facet_parameters", "Create a facet plot by picking any number of parameters",
                                  choices = parameters,
                                  multiple = TRUE),
                      plotOutput("plot_facet")),
             tabPanel(title = "Review Quality Control",
                      # Plot summarizes qc flags by parameter - qc flag
                      dataTableOutput("table_summary_qc"),
                      uiOutput("remove_flags"),
                      actionButton("confirm_removal", "Confirm QC flag removal", class = "btn-warning"),
                      
                      div(tags$br()),
                  
                      textInput("tech_id", "Enter your technician code", "default_code"),
                      actionButton("confirm_flags", "Confirm QC flag selections", class = "btn-primary"))
           ),
           
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
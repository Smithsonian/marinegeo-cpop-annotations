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
    #menuItem("About", tabName = "about", icon = icon("bullhorn")),
    #menuItem("Submit data", tabName = "submit_data", icon = icon("file-upload")),
    menuItem("Load data", tabName = "load_data", icon = icon("database")),
    menuItem("Annotate data", tabName = "annotate_data", icon = icon("chart-line")),
    #menuItem("View tabular data", tabName = "tabular_data", icon = icon("table")),
    menuItem("Review annotations", tabName = "review", icon = icon("clipboard-check")),
    
    uiOutput("data_info_box")
  )
)

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs(text = jscode_box_collapse, functions = c("expandBox", "collapseBox")),
  tabItems(
    tabItem("about",
            
            ## Introduction - About page ####
            fluidRow(
              box(width = 12,
      
                div(id = "about_block",
                  tags$h3("MarineGEO CPOP Annotation Portal"),
                  
                  introduction_text, tags$br(), tags$br(),
                  
                  "To begin, either submit new data or load previously submitted data. Do not re-submit data.", 
                  tags$br(), tags$br(),
                  
                  splitLayout(
                    actionButton("initiate_submission", 
                                 div(div(id = "icon", icon("angle-double-up")),
                                     tags$h3("Submit data"))), 
                    actionButton("initiate_load", 
                                 div(div(id = "icon", icon("truck-loading")),
                                     tags$h3("Load data"))), 
                    cellArgs = list(style = "text-align:center")
                  )
                )
              )
            )
    ),
    
    ## Submit data ####
    tabItem("submit_data",
                
                box(width = 12,
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "ordered_list_instructions.css"),
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                    ),
                    
                    tags$h3("Submission Instructions"), tags$br(),
                    tags$ol(
                      tags$li("Provide deployment information."),
                      tags$li("Upload the instrument's calibration file and Korexo table."), 
                      tags$li("Review the submission summary."), 
                      tags$li("You can begin annotating the data immediately after submission.")
                    )),
            
                tabBox(id = "submission_box", width = 12,
                  tabPanel(title = "Deployment Metadata", value = "deployment_metadata",
                           div(id = "deployment_metadata_box", 
                               tags$style(HTML('#deployment_metadata_box{width:700px; margin:auto}')),
                               
                               selectInput("submission_site", "Select MarineGEO site", 
                                           choices = c("", "PAN-BDT", "USA-MDA", "USA-IRL")),
                               
                               selectInput("submission_data_type", "Select the data type",
                                           choices = c("Water Quality", "Water Level", "Meteorological")),
                               
                               textInput("deployment_timestamp", "Enter the deployment date and time"),
                               
                               textInput("retrieval_timestamp", "Enter the retrieval date and time"),
                               
                               selectInput("upload_action", "Select the instrument action", 
                                           choices = c("Sonde swap", "Troubleshooting", "Cleaning", "Calibration",
                                                       "other (specify below)"), multiple = TRUE),
                               
                               textAreaInput("submission_notes", "Enter any additional notes about this submission")

                           )),
                  
                  tabPanel(title = "Upload Files",
                           fileInput("fileKorexo", "Korexo file",
                                     multiple = FALSE),
                           fileInput("fileCalibration", "Calibration file", 
                                     multiple = FALSE)
                  ),
                  
                  tabPanel(title = "Review and Submit",
                           actionButton("confirm_submission", "Submit Data"))
                )
                
            ),
    
    ## Load data ####
    tabItem("load_data",
            
            # fluidRow(
            #   valueBoxOutput("site_info_box", width = 3),
            #   valueBoxOutput("date_range_box", width = 6)
            #   #valueBoxOutput("annotation_progress_box", width = 3)
            # ),
            
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
                         title = "Select and load data",
                         
                         div(
                           tags$b("Click a row in the table and then \"Load data\" to load the data into the application")
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
                                            choices = c("Flags",
                                                        "Codes"))
                           ),
                           div(class = "plot_controls",
                               selectInput("view_mode", "Show:",
                                           choices = c("All points",
                                                       "Flags that require review",
                                                       "Points that require codes"))
                           ),
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
    
    # ## Tabular Data ####
    # tabItem("tabular_data",
    #         fluidRow(
    #           box(width = 12,
    #               status = "primary",
    #               title = "View Tabular Data",
    #               
    #               dataTableOutput("table_selected_points")
    #         ))),
    
    ## Review ####
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
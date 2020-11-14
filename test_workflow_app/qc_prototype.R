# sensor_flag() -> "Conductivity_Temp"

library(shinydashboard)
library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(rdrop2)
library(DT)
library(plotly)
library(shinyjs)

sensor_vector_l1 <- c("Turbidity" = "Turbidity",
                      "Conductivity" = "Conductivity_Temp",
                      "Optical Dissolved Oxygen" = "Optical_DO",
                      "Depth" = "Depth",
                      "Fluorescent Dissolved Organic Matter" = "fDOM",
                      "Wiper" = "Wiper",
                      "pH" = "pH",
                      "EXO2 Sonde" = "EXO2_Sonde",
                      "Total Algae" = "Total_Algae_BGA_PE")

qc_flags <- c("-5" = "Outside high range",
              "-4" = "Outside low range",
              "-3" = "Data rejected due to QAQC",
              "-2" = "Missing Data",
              "-1" = "Optional parameter, not collected",
              "0" = "Passed L1 QC",
              "1" = "Suspect Data")

df_data <- read.csv("./data/PAN-BDT_bundle_2017_L1-data.csv") %>%
  select(-timestamp) %>%
  rename(timestamp = timestamp3) %>%
  mutate(timestamp = ymd_hms(timestamp)) 

df_flags <- read.csv("./data/PAN-BDT_bundle_2017_L1-flags.csv") %>%
  mutate_all(as.character) %>%
  rename(flag = code_num) %>%
  select(timestamp, ID, sensor, flag) %>%
  mutate(timestamp = ymd_hms(timestamp),
         status = "Not evaluated", # Whether L1 flag has been accepted, rejected, or needs to be evaluated
         flag = case_when(
           flag == "-5" ~ "Outside high range",
           flag == "-4" ~ "Outside low range",
           flag == "-3" ~ "Data rejected due to QAQC",
           flag == "-2" ~ "Missing Data",
           flag == "-1" ~ "Optional parameter, not collected",
           flag == "0" ~ "Passed L1 QC",
           flag == "1" ~ "Suspect Data",
           flag == "2" ~ "Reserved for Future Use",
           T ~ "Other Flags"
         ))


header <- dashboardHeader(
  title = "MarineGEO CPOP Quality Control Dashboard",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Annotate data", tabName = "annotate_data", icon = icon("chart-line"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
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
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output, session) {
  
  current_data <- reactiveValues(df=df_data)
  
  # Empty dataframes will hold points selected for level 1 QC flags and level 2 QC flags and codes
  qc_output <- reactiveValues(flags = df_flags,
                              codes = tibble(ID = as.character(NA),
                                             timestamp = as.POSIXct(NA), 
                                             sensor = as.character(NA),
                                             code = as.character(NA),
                                             .rows = 0))
  
  sensor_parameters <- reactiveVal("") # Holds parameters available for currently selected sensor
  parameter_mean <- reactiveVal(NA) # Mean of currently selected parameter to fill in for missing data
  current_file <- reactiveVal(NA) # Filename for data currently loaded
  current_site <- reactiveVal(NA) # Site for data currently loaded
  current_date_range <- reactiveVal(NA) # Date range for data currently loaded
  in_progress_qc <- reactiveValues() # Holds decision and outcomes of qc process until user cancels or confirms all decisions
  sensor_flag <- reactive(unname(sensor_vector_l1[input$sensor_qc])) # Name of sensor in QC columns 
  reset_plot_status <- reactiveVal(0)
  
  # Reactives for plotting ####
  
  # Return dataframe containing timestamps and codes 
  # Codes are collapsed to a single row for plotting 
  code_timestamps <- reactive({
    # Determine if observations for current sensor have been annotated
    qc_output$codes %>%
      filter(sensor == "Conductivity_Temp") %>%
      group_by(timestamp) %>%
      summarize(code = paste(code, collapse = ", "))
  })
  
  # Return dataframe containing timestamps and flags 
  # There should only be one flag per timestamp 
  flag_timestamps <- reactive({
    qc_output$flags %>%
      filter(sensor == "Conductivity_Temp") %>%
      select(-ID, -sensor)
  })
  
  subset_data <- reactive({
    
    current_data$df %>%
      select(timestamp, Cond_microS_cm) %>%
      merge(flag_timestamps(), by="timestamp", all.x=TRUE) %>%
      merge(code_timestamps(), by="timestamp", all.x=TRUE) %>%
      # Provide values to missing data
      mutate(Cond_microS_cm = case_when(
        flag == "Missing Data" ~ 40,
        T ~ Cond_microS_cm
      )) %>%
      mutate(code = case_when(
        is.na(code) ~ "No code applied",
        T ~ code
      ))
  })
  
  # Reformats label_mode input for plotly color argument (ex: Codes to code)
  label_type <- reactive({
    gsub("s", "", tolower(input$label_mode))
  })
  
  ## Plots ####
  
  # Generate a plot of the data 
  output$plot_qc <- renderPlotly({
    
    reset_plot_status()
    
    plot_ly(subset_data(), x = ~timestamp, y = ~Cond_microS_cm, 
            color = ~get(label_type()), # Format Codes or Flags to code or flag, respectively 
            key=~timestamp, type = "scatter") %>%
      rangeslider(type = "date") %>%
      layout(legend = list(orientation = 'h', # https://plotly.com/python/reference/layout/#layout-legend
                           y = -.6),
             yaxis = list(title = "Cond_microS_cm"),
             xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
                          rangeselector = list(
                            buttons = list(
                              list(count = 24, label = "1 day", step = "hour", stepmode = "todate"),
                              list(count = 3, label = "3 days", step = "day", stepmode = "todate"),
                              list(count = 7, label = "1 wk", step = "day", stepmode = "todate"),
                              list(count = 1, label = "1 mo", step = "month", stepmode = "todate")
                            )
                          )),
             showlegend = TRUE) %>%
      toWebGL()  # Conversion from SVG drastically improves performance
  })
  
  ## Apply QC logic ####
  
  # Reactive used to determine which points are in selection
  getPlotlySelection <- reactive({
    req(event_data("plotly_selected"))
    
    dat <- subset_data()
    
    brush_subset <- event_data("plotly_selected") %>%
      mutate(key = ymd_hms(key))
    
    # Return subset of data within selection
    dat %>%
      filter(timestamp %in% brush_subset$key)
  })
  
  ## ... QC UI box ####
  # UI options will depend on status of QC and is hierarchical
  # If no points selected, show instructions. Once selected:
  # ... First: any flags not yet approved/rejected
  # ... Second: provide option to add code(s)
  # ... Third: revise flags and/or code(s)
  
  quality_control_stage <- reactiveVal(NA)
  n_flags_unapproved <- reactiveVal(NA)
  n_codes_unassigned <- reactiveVal(NA)
  total_points_in_selection <- reactiveVal(NA)
  selection <- reactiveValues(df = tibble())
  
  getQualityControlUI <- reactive({
    if(is.null(event_data("plotly_selected"))){
      div(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "ordered_list_instructions.css")
        ),
        tags$h3("Quality Control Instructions"), tags$br(),
        tags$ol(
          tags$li("Select a sensor and parameter below to plot data"),
          tags$li("Select points using the \"box selection\" or \"lasso selection\" tools on the plot toolbar. Click and drag the selection tool over the points to review and annotate."), 
          tags$li("Flags have been algorithmically assigned to each point and must be either accepted or rejected. If they are rejected, you must provide an updated flag."), 
          tags$li("Assign quality control codes that provide additional context to the flag. Apply either a general or sensor-specific code. You can also tag points with one or more comment codes that provide additional context.")
        )
      )
    } else if(quality_control_stage() == "accept flags"){
      div(id = "accept_flag_div",
          tags$h3("Approve or Revise Initial Flags"), 
          tags$b(paste(n_flags_unapproved(), "of", total_points_in_selection(), "selected observations have pending flags. ", sep = " ")),
          "Approve or reassign flags for these points. Once you select \"reassign\", you will select an updated flag.",
          tags$br(), tags$br(),
          
          splitLayout(
            cellWidths = "50%",
            div(
              splitLayout(
                #cellWidths = "25%",
                #cellArgs = list(style = "padding: 30px;"),
                div(id = "initial_flag_decision",
                    actionButton("accept_flags", "Accept flags", class = "btn-primary"), tags$br(), tags$br(),
                    actionButton("reject_flags", "Reassign flags", class = "btn-danger")
                ),
                div(id = "flag_revision_options")
              )),
            div()),
          
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                    #reject_flags{color:white} #accept_flags{color:white}")))
          
      )
    } else if (quality_control_stage() == "revise flags"){
      div(
        selectInput("revise_flags", "Select updated flag", 
                    choices = unname(qc_flags)),
        actionButton("confirm_revisions", "Confirm revised flags")
      )
        
    } else if(quality_control_stage() == "revise codes"){
      div(id = "revise_codes_div",
          tags$h3("Add or Revise Quality Control Codes"),
          tags$b(paste(n_codes_unassigned(), "of", total_points_in_selection(), "selected observations have not been assigned a code. ", sep = " ")),
          "Assign a general or sensor code for ", tags$b("all"), " selected points. You may also select one or more comment codes.",
          tags$br(), tags$br(),
          
          splitLayout(
            div(splitLayout(
              div(
                selectInput("sensor_code_selection", "Select a general or sensor code",
                            choices = c("", sensor_codes)),
                actionButton("confirm_codes", "Confirm code selections", class = "btn-primary")),
              selectInput("comment_code_selection", "Select one or more comment codes",
                          choices = comment_codes, multiple = TRUE)
            )),
            div(actionButton("skip_revising_codes", "Do not assign a code", class = "btn-primary"))),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                                #confirm_codes{color:white}")))
      )
    } else if(quality_control_stage() == "update annotations"){
      div(id = "update_annotations_div",
          tags$h3("Update Quality Control Flags and/or Codes"), 
          "All selected observations have confirmed flags and been assigned quality control codes. You may revise either. ", 
          "If you revise flags, you will be given the option to revise the codes.",
          tags$br(), tags$br(),
          
          div(id = "all_revision_options",
              actionButton("revise_flags_button", "Revise flags"),
              actionButton("revise_codes_button", "Revise codes"),
              style = "padding:20px"
          ))
    }
  })
  
  # Triggers start of QC workflow when a selection event occurs
  observeEvent(!is.null(event_data("plotly_selected")), {
    selection$df <- getPlotlySelection()
    n_flags_unapproved(length(selection$df$status[selection$df$status == "Not evaluated"]))
    n_codes_unassigned(length(selection$df$code[selection$df$code == "No code applied"]))
    total_points_in_selection(nrow(selection$df))
    
    if("Not evaluated" %in% selection$df$status){
      quality_control_stage("accept flags")
    } else if("No code applied" %in% selection$df$code){
      quality_control_stage("revise codes")
    } else{
      quality_control_stage("update annotations")
    }  
  })
  
  ## ... Accept flags ####
  # If a user accepts flags, status column in flag dataframe is updated
  observeEvent(input$accept_flags,{
    
    in_progress_qc$flags <- qc_output$flags %>%
      mutate(status = case_when(
        timestamp %in% selection$df$timestamp &
          sensor == "Conductivity_Temp" &
          status == "Not evaluated" ~ "Approved",
        T ~ status
      ))
    
    quality_control_stage("revise codes")
    
  })
  
  ## ... Reject and revise flags ####
  # If a user accepts rejects flags, change UI to allow for new selection
  observeEvent(input$reject_flags, {
    quality_control_stage("revise flags")
    
  })

  observeEvent(input$confirm_revisions,{
    in_progress_qc$flags <- qc_output$flags %>%
      mutate(status = case_when(
        timestamp %in% selection$df$timestamp &
          sensor == "Conductivity_Temp" &
          status == "Not evaluated" ~ "Revised",
        T ~ status
      ),
      flag = case_when(
        timestamp %in% selection$df$timestamp &
          sensor == "Conductivity_Temp" ~ input$revise_flags,
        T ~ flag
      ))

    quality_control_stage("revise codes")
    
  })
  
  ## ... Confirm Codes ####
  observeEvent(input$confirm_codes, {

    qc_output$flags <- in_progress_qc$flags
    
    selected_codes <- c(input$sensor_code_selection, input$comment_code_selection)
    
    revised_codes <- data.frame()
    for(selected_code in selected_codes){
      revised_codes <- selection$df %>%
        select(timestamp) %>%
        mutate(sensor = "Conductivity_Temp",
               code = selected_code) %>%
        bind_rows(revised_codes)
    }
    
    qc_output$codes <- qc_output$codes %>%
      filter(!(timestamp %in% selection$df$timestamp & sensor == "Conductivity_Temp")) %>%
      bind_rows(revised_codes)
    
    runjs("Shiny.setInputValue('plotly_selected-A', null);")    
  })
  
  observeEvent(input$skip_revising_codes, {
    
    qc_output$flags <- in_progress_qc$flags
    runjs("Shiny.setInputValue('plotly_selected-A', null);")  
    
    reset_plot_status(
      reset_plot_status() + 1
    )
    
  })
  
  ## ... revise annotations ####
  observeEvent(input$revise_flags_button, {
    quality_control_stage("revise flags")
    
  })
  
  observeEvent(input$revise_codes_button,{
    
    in_progress_qc$flags <- qc_output$flags 
    
    quality_control_stage("revise codes")
    
  })
  
  
  output$quality_control_box <- renderUI({
    getQualityControlUI()
  })
  
}

shinyApp(ui, server)
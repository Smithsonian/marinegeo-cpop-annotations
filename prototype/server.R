
function(input, output, session) {
  ## Reactive Objects ####
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  
  # Empty dataframes will hold points selected for level 1 QC flags and level 2 QC flags and codes
  qc_output <- reactiveValues(flags = tibble(ID = as.character(NA),
                                             timestamp = as.POSIXct(NA),
                                             sensor = as.character(NA),
                                             status = as.character(NA),
                                             flag = as.character(NA),
                                             .rows = 0),
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
  ## Track QC progress info box ####
  current_qc_progress <- reactive({
    "NA"
    # num_rows <- nrow(current_data$df)
    # 
    # if(num_rows > 0){
    #   sum_qc_rows <- sum(current_data$df$l1_tag_present)
    #   paste0(round(sum_qc_rows / num_rows * 100), "%")
    #   
    # } else {
    #   "No Data Loaded"
    # }
  })
  
  ## Subset data inventory ####
  data_inventory <- reactive({
    subset_key <- key
    
    if(length(input$site) > 0){
      subset_key <- filter(subset_key,
                           Site %in% input$site)
    }
    if(length(input$date_range) > 0){
      subset_key <- filter(subset_key,
                           Date %in% input$date_range)
    }
    if(length(input$file) > 0){
      subset_key <- filter(subset_key,
                           Filename %in% input$file)
    }
    
    subset_key
  })
  
  ## Infoboxes for data information ####
  output$data_info_box <- renderUI({
    
    if(!is.na(current_site())){
      div(
        tags$hr(),
        
        paste0("Site: ", current_site()), tags$br(),
        paste0("Date Range: ", current_date_range())
      )
    }
  })  
  

  # output$site_info_box <- renderValueBox({
  #   valueBox(
  #     current_site(), "Site", icon = icon("broadcast-tower"),
  #     color = "purple"
  #   )
  # })
  
  # output$date_range_box <- renderValueBox({
  #   valueBox(
  #     current_date_range(), "Date Range", icon = icon("calendar-alt"),
  #     color = "teal"
  #   )
  # })
  
  # output$annotation_progress_box <- renderValueBox({
  #   valueBox(
  #     current_qc_progress(), "Quality Control Progress", icon = icon("percent"),
  #     color = "light-blue"
  #   )
  # })

  ## Datatable output for import ####
  output$key <- renderDataTable({
    datatable(data_inventory(),
              selection = "single")
  })
  
  ## UI objects for annotations ####
  output$parameter_qc <- renderUI({
    selectInput("parameter_qc", "Select a parameter to QC",
                sensor_parameters(), multiple = FALSE)
  })
  
  # NA values are currently getting plotted as the mean of the selected parameter
  # Only want to change the value before plotting, not the underlying data frame
  observeEvent(input$parameter_qc, {

    if(input$parameter_qc %in% sensor_parameters_df$parameter){
      parameter_mean(
        current_data$df %>%
          summarize(mean = mean(!!sym(input$parameter_qc), na.rm=T)) %>%
          pull(mean)
      )
    }
    
  })
  
  observeEvent(input$sensor_qc, {
    # Update parameters based on sensor selection
    sensor_parameters(
      sensor_parameters_df %>%
        filter(sensor %in% input$sensor_qc,
               parameter %in% colnames(current_data$df)) %>%
        pull(parameter)
    )
  })
  
  ## Load Data ####
  # Action to take if run query button pressed
  observeEvent(input$loadData, {
    
    selected_file <- data_inventory()[input$key_rows_selected,]$Filename
    
    if(length(selected_file) != 0){
      
      current_file(selected_file)
      
      # Read in CSV given filepath
      current_data$df <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/", 
                                              selected_file))
      
      # Convert timestamp to POSIXct
      current_data$df <- current_data$df %>%
        select(-timestamp) %>%
        rename(timestamp = timestamp3) %>%
        mutate(timestamp = ymd_hms(timestamp)) 
      
      # Check if current filename has previous annotations and read them in
      data_identifier <- gsub("L1-data.csv", "", current_file())
      
      # If L2 annotations exist, read them in 
      # Otherwise read in L1 annotation
      if(data_identifier %in% annotation_directory$identifier){
        annotation_filename <- annotation_directory %>%
          filter(identifier == data_identifier) %>%
          pull(name)
        
        qc_output$codes <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/",
                                               annotation_filename)) %>%
          mutate_all(as.character) %>%
          mutate(timestamp = ymd_hms(timestamp))
        
      } else {
        # Read in the L1 flags
        qc_output$flags <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/", 
                                                   gsub("-data", "-flags", selected_file))) %>%
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

      }
      
      current_site(data_inventory()[input$key_rows_selected,]$Site)
      current_date_range(paste(min(current_data$df$timestamp),
                               strftime(max(current_data$df$timestamp), 
                                        '%Y-%m-%d'),
                               sep = " to "))
      
    } else {
      showModal(modalDialog(
        title = "No Data Selected", 
        div("Load data by selecting the row in the table that represents the data of interest. 
            You can subset the table using the subset options to the left of the table."),
        
        easyClose = TRUE
      ))
    }
  })
  
  # Reactives for plotting ####
  
  # Return dataframe containing timestamps and codes 
  # Codes are collapsed to a single row for plotting 
  code_timestamps <- reactive({
    # Determine if observations for current sensor have been annotated
    qc_output$codes %>%
      filter(sensor == sensor_flag()) %>%
      group_by(timestamp) %>%
      summarize(code = paste(code, collapse = ", "))
  })
  
  # Return dataframe containing timestamps and flags 
  # There should only be one flag per timestamp 
  flag_timestamps <- reactive({
    qc_output$flags %>%
      filter(sensor == sensor_flag()) %>%
      select(-ID, -sensor)
  })
  
  subset_data <- reactive({
    
    current_data$df %>%
      select(timestamp, all_of(input$parameter_qc)) %>%
      merge(flag_timestamps(), by="timestamp", all.x=TRUE) %>%
      merge(code_timestamps(), by="timestamp", all.x=TRUE) %>%
      # Provide values to missing data
      mutate(!!input$parameter_qc := case_when(
        flag == "Missing Data" ~ parameter_mean(),
        T ~ .data[[input$parameter_qc]]
      ))
  })
  
  # Reformats label_mode input for plotly color argument (ex: Codes to code)
  label_type <- reactive({
    gsub("s", "", tolower(input$label_mode))
  })
  
  ## Plots ####
  
  # Generate a plot of the data 
  output$plot_qc <- renderPlotly({
    
    req(input$parameter_qc)
    
    reset_plot_status()
    
    plot_ly(subset_data(), x = ~timestamp, y = ~get(input$parameter_qc), 
            color = ~get(label_type()), # Format Codes or Flags to code or flag, respectively 
            key=~timestamp, type = "scatter") %>%
      rangeslider(type = "date"
                  #borderwidth = 1,
                  #thickness = .15) %>%
                  #yaxis = list(
                  #range = c(40,60)
      ) %>%
      layout(legend = list(orientation = 'h', # https://plotly.com/python/reference/layout/#layout-legend
                           y = -.6),
             yaxis = list(title = input$parameter_qc),
             xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
                          rangeselector = list(
                            buttons = list(
                              list(count = 24, label = "1 day", step = "hour", stepmode = "todate"),
                              list(count = 3, label = "3 days", step = "day", stepmode = "todate"),
                              list(count = 7, label = "1 wk", step = "day", stepmode = "todate"),
                              list(count = 1, label = "1 mo", step = "month", stepmode = "todate")
                            )
                          )),
             # rangeslider = list(bgcolor = '#000',
             #                    type = "date")),
             showlegend = TRUE) %>%
      toWebGL()  # Conversion from SVG drastically improves performance
  })
  
  ## Apply QC logic ####
  
  # Reactive used to determine which points are in selection
  getPlotlySelection <- reactive({
    dat <- subset_data()
    
    brush_subset <- event_data("plotly_selected") %>%
      mutate(key = ymd_hms(key))
  
    # Return subset of data within selection
    dat %>%
      filter(timestamp %in% brush_subset$key)
  })
  
  # Quality control summary box
  # output$quality_control_summary <- renderUI({
  #   
  #   if(is.null(event_data("plotly_selected"))){
  #     div("0 Points selected")
  #   } else {
  #     dat <- getPlotlySelection()
  # 
  #     n1 <- length(dat$status[dat$status == "Not evaluated"])
  #     n2 <- sum(is.na(dat$code))
  #     total <- nrow(dat)
  #     
  #     div(paste(total, "points selected", sep = " "), tags$br(),
  #         paste(n1, "flags approved", sep = " "), tags$br(),
  #         paste(n2, "codes applied", sep= " ")
  #     )
  #   }
  # })
  
  ## ... QC UI box ####
  # UI options will depend on status of QC and is hierarchical
  # If no points selected, show instructions. Once selected:
  # ... First: any flags not yet approved/rejected
  # ... Second: provide option to add code(s)
  # ... Third: revise flags and/or code(s)
  output$quality_control_box <- renderUI({
    
    # If a single point in the selection has an unevaluated flag, do that first 
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
      
    } else {
      dat <- getPlotlySelection()
      
      if("Not evaluated" %in% dat$status){
        n <- length(dat$status[dat$status == "Not evaluated"])
        total <- nrow(dat)
        
        div(id = "quality_control_panel",
            div(id = "flag_div",
                tags$h3("Approve or Revise Initial Flags"), 
                tags$b(paste(n, "of", total, "selected observations have pending flags. ", sep = " ")),
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
                      div(id = "rejection_options")
                    )),
                  div(cancelButtonUI("mod1"))),
                
                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                    #reject_flags{color:white} #accept_flags{color:white}")))
                
            ))
      }
    } 
  })
  
  ## ... Accept flags ####
  # If a user accepts flags, status column in flag dataframe is updated
  observeEvent(input$accept_flags,{
    
    dat <- getPlotlySelection()
    
    n <- sum(is.na(dat$code))
    total <- nrow(dat)
    
    in_progress_qc$flags <- qc_output$flags %>%
      mutate(status = case_when(
        timestamp %in% dat$timestamp &
          sensor == sensor_flag() &
          status == "Not evaluated" ~ "Approved",
        T ~ status
      ))
    
    removeUI("#flag_div")
    insertUI("#quality_control_panel", where = "afterBegin", immediate = TRUE,
             ui = assign_codes_ui(dat, n, total)
    )

  })

  ## ... Revise flags ####
  # If a user accepts rejects flags, status and flag columns in flag dataframe are updated
  observeEvent(input$reject_flags, {
    
    removeClass("accept_flags", class = "btn-primary")
    removeClass("reject_flags", class = "btn-danger")

    insertUI("#rejection_options", where = "afterBegin", immediate = TRUE,
             ui = div(
               selectInput("revise_flags", "Reject and update flags", 
                           choices = unname(qc_flags)),
               actionButton("confirm_revisions", "Confirm revised flags")
             ))
    
    addClass("confirm_revisions", class = "btn-primary")
             
  })
  
  observeEvent(input$confirm_revisions,{
    dat <- getPlotlySelection()
    n <- sum(is.na(dat$code))
    total <- nrow(dat)
    
    in_progress_qc$flags <- qc_output$flags %>%
      mutate(status = case_when(
        timestamp %in% dat$timestamp & 
          sensor == sensor_flag() &
          status == "Not evaluated" ~ "Revised",
        T ~ status
      ),
      flag = case_when(
        timestamp %in% dat$timestamp & 
          sensor == sensor_flag() ~ input$revise_flags,
        T ~ flag
      ))
    
    removeUI("#flag_div")
    insertUI("#quality_control_panel", where = "afterBegin", immediate = TRUE,
             ui = assign_codes_ui(dat, n, total))
    
  })
  
  ## ... Confirm Codes ####
  observeEvent(input$confirm_codes, {
    dat <- getPlotlySelection()

    qc_output$flags <- in_progress_qc$flags
    
    selected_codes <- c(input$sensor_code_selection, input$comment_code_selection)
    
    revised_codes <- data.frame()
    for(selected_code in selected_codes){
      revised_codes <- dat %>%
        select(timestamp) %>%
        mutate(sensor = sensor_flag(),
               code = selected_code) %>%
        bind_rows(revised_codes)
    }
    
    qc_output$codes <- qc_output$codes %>%
      filter(!(timestamp %in% dat$timestamp & sensor == sensor_flag())) %>%
      bind_rows(revised_codes)

    runjs("Shiny.setInputValue('plotly_selected-A', null);")    
  })
  
  button1 <- cancelButtonServer("mod1")
  button2 <- cancelButtonServer("mod2")
  #cancel_flag <- cancelSelectionsServer("flag")
  ## ... cancel selection logic ####
  
  observeEvent(button1(), {
    runjs("Shiny.setInputValue('plotly_selected-A', null);")    
    reset_plot_status(
      reset_plot_status() + 1
    )
  })
  observeEvent(button2(), {
    runjs("Shiny.setInputValue('plotly_selected-A', null);")    
    reset_plot_status(
      reset_plot_status() + 1
    )
  })
  output$table_selected_points <- renderDataTable({
    
    req(input$parameter_qc)
    
    # By default, the table will show the timestamp, the selected QAQC parameter, and the QC numeric flag
    data_subset <- current_data$df %>%
      select(timestamp, input$parameter_qc, unname(sensor_vector_l1[input$sensor_qc]))
    
    brush_subset <- event_data("plotly_selected")
    
    if (!is.null(brush_subset)){
      brush_subset <- brush_subset %>%
        mutate(key = ymd_hms(key))
      
      datatable(
        data_subset %>%
          filter(timestamp %in% brush_subset$key)
      )
    } else {
      datatable(data_subset)
    }
    
  })
  
  output$table_summary_qc <- renderDataTable({
    
    if(nrow(qc_output$codes) > 0){
      summary <- qc_output$codes %>%
        mutate(timestamp = ymd_hms(timestamp)) %>%
        group_by(sensor, code) %>%
        summarize(number_points_flagged = n(),
                  from = min(timestamp),
                  to = max(timestamp))
      
      datatable(summary)
    }
  })
  
  # observeEvent(input$apply_qc, {
  #   
  #   data_subset <- current_data$df %>%
  #     select(timestamp, ID, all_of(input$parameter_qc))
  #   
  #   brush_subset <- event_data("plotly_selected") %>%
  #       mutate(key = ymd_hms(key))
  #   
  #   codes <- qc_codes %>%
  #     filter(select_inputs %in% c(input$wq_qc_codes, input$met_qc_codes)) %$%
  #     paste(unique(.$code), collapse = ",")
  #   
  #   qc_output$codes <- data_subset %>%
  #     filter(timestamp %in% brush_subset$key) %>%
  #     mutate(sensor = sensor_flag(),
  #            code = codes,
  #            file = current_file(),
  #            ID = as.character(ID)) %>%
  #     select(-input$parameter_qc) %>%
  #     separate_rows(code, sep=",") %>%
  #     bind_rows(qc_output$codes)
  # })
  
  ## Remove QC flags ####
  output$remove_codes <- renderUI({
    selectInput("select_remove_codes", "Select QC codes to remove",
                choices = unique(qc_output$codes$code), multiple = TRUE)
    
  })
  
  observeEvent(input$confirm_removal,{
    
    if(!is.null(input$select_remove_codes)){
      points_to_remove <- qc_output$codes %>%
        filter(code %in% input$select_remove_codes) 
      
      qc_output$codes <- qc_output$codes %>%
        filter(!(code %in% input$select_remove_codes))
      
    }
  })
  
  ## Submit annotations ####
  observeEvent(input$submit_codes, {

    filename <- paste(gsub(".csv", "", current_file()),
                      "L2",
                      #format(Sys.time(), "%Y%m%d-%H%M%OS"),
                      sep = "-")

    setwd(tempdir())

    #if(nrow(qc_output$codes) > 0){

    write_csv(qc_output$codes,
              paste0(filename, ".csv"))

    drop_upload(paste0(filename, ".csv"),
                path = "Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/")

    showModal(modalDialog(
      title = "Annotations saved",
      div("Your annotations have been saved."),

      easyClose = TRUE
    ))

    #}

    setwd(original_wd)

  })
  
  assign_codes_ui <- function(dat, n, total){
    div(id = "provide_codes",
                          tags$h3("Add or Revise Quality Control Codes"),
                          tags$b(paste(n, "of", total, "selected observations have not been assigned a code. ", sep = " ")),
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
                            div(cancelButtonUI("mod2"))),
                          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                                #confirm_codes{color:white}")))
  )}
}
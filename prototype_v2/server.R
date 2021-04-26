
function(input, output, session) {
  ## Source submission server code
  source("./submission.R", local = TRUE)
  
  ## Reactive Objects ####
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  # current_data <- reactiveValues(df=dat)
  
  # Empty dataframes will hold points selected for level 1 QC flags and level 2 QC flags and codes
  qc_output <- reactiveValues(flags = tibble(ID = as.character(NA),
                                             #timestamp = as.POSIXct(NA),
                                             sensor = as.character(NA),
                                             #status = as.character(NA),
                                             flag = as.character(NA),
                                             .rows = 0),
                              codes = tibble(ID = as.character(NA),
                                             #timestamp = as.POSIXct(NA),
                                             sensor = as.character(NA),
                                             code = as.character(NA),
                                             .rows = 0))
  
  flag_summary <- reactive({
    
    qc_output$flags %>%
      count(flag) %>%
      rename(`total observations` = n)
    
  })
  
  code_summary <- reactive({
    
    code_summary <- qc_output$codes %>%
      count(code) %>%
      rename(`total observations` = n)

    if(nrow(code_summary) == 0){
      code_summary <- add_row(code_summary, code = "None")
    }
    
    return(code_summary)
  })
  
  current_file <- reactiveVal(NA) # Filename for data currently loaded
  current_site <- reactiveVal(NA) # Site for data currently loaded
  current_date_range <- reactiveVal(NA) # Date range for data currently loaded
  in_progress_qc <- reactiveValues() # Holds decision and outcomes of qc process until user cancels or confirms all decisions
  
  # Formatted name of selected sensors (transformed from UI-friendly versions)
  # sensor_flag <- reactive(unname(sensor_vector_l1[input$sensor_qc])) # Name of sensor in QC columns 
  view_mode <- reactive({input$view_mode})
  start_date <- reactive({input$start_date})
  
  reset_plot_status <- reactiveVal(0)
  
  current_min_date <- reactiveVal(NULL)
  current_max_date <- reactiveVal(NULL)
  
  ## Welcome tab logic ####
  observeEvent(input$initiate_submission, {
    updateTabItems(session, "tabs", "submit_data")
  })
  
  observeEvent(input$initiate_load, {
    updateTabItems(session, "tabs", "load_data")
  })
  
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
  
  full_dictionary_table <- reactive({
    full_definitions %>%
      filter(value %in% code_summary()$code | value %in% flag_summary()$flag)
    
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
      div(id = "sidebar_summary",
        hr(),
        paste0("Site: ", current_site()), tags$br(),
        paste0("Start Date: ", strftime(min(current_data$df$timestamp), '%Y-%m-%d')), tags$br(),
        paste0("End Date: ", strftime(max(current_data$df$timestamp), '%Y-%m-%d')),
        hr(),
        "Quality Control Summary", tags$br(),
        renderTable(flag_summary()),
        renderTable(code_summary()),
        
        tags$br(),
        
        actionLink("abbreviation_dictionary", "Flag and code definitions")
        
      )
    }
  })  
  
  observeEvent(input$abbreviation_dictionary, {
    
    showModal(modalDialog(
      title = "Flag and Code Definitions",
      div(renderTable(full_dictionary_table())),
      
      easyClose = TRUE
    ))
    
  })
  
  output$site_info_box <- renderValueBox({
    valueBox(
      current_site(), "Site", icon = icon("broadcast-tower"),
      color = "purple"
    )
  })

  output$date_range_box <- renderValueBox({
    valueBox(
      current_date_range(), "Date Range", icon = icon("calendar-alt"),
      color = "teal"
    )
  })
  
  # output$annotation_progress_box <- renderValueBox({
  #   valueBox(
  #     current_qc_progress(), "Quality Control Progress", icon = icon("percent"),
  #     color = "light-blue"
  #   )
  # })

  # ## Datatable output for import ####
  output$key <- renderDataTable({
    datatable(data_inventory(),
              options = list(
                pageLength = 5
              ),
              selection = "single")
  })
  
   
  output$start_date <- renderUI({
    dateInput("start_date", label = "Update start date",
              value = current_min_date(), min = current_min_date(), max = current_max_date())
  })
  
  ## Load Data ####
  # Action to take if run query button pressed
  observeEvent(input$loadData, {
    
    selected_file <- data_inventory()[input$key_rows_selected,]$Filename
    
    if(length(selected_file) != 0){
      
      qc_output$flags = tibble(ID = as.character(NA),
                               #timestamp = as.POSIXct(NA),
                               sensor = as.character(NA),
                               #status = as.character(NA),
                               flag = as.character(NA),
                               .rows = 0)
      qc_output$codes = tibble(ID = as.character(NA),
                               #timestamp = as.POSIXct(NA),
                               sensor = as.character(NA),
                               code = as.character(NA),
                               .rows = 0)
  
      current_file(selected_file)
      
      # Read in CSV given filepath
      current_data$df <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/L1_DATA_FLAGS/", 
                                              selected_file))
      
      # Convert timestamp to POSIXct
      current_data$df <- current_data$df %>%
        select(-timestamp) %>%
        rename(timestamp = timestamp3) %>%
        mutate(timestamp = ymd_hms(timestamp)) 
      
      current_data$df <- as.data.frame(lapply(current_data$df, function(x){
        if(is.integer(x)){
          as.numeric(x)
        } else {x}
      }))
      
      # Check if current filename has previous annotations and read them in
      data_identifier <- gsub("_L1-data.csv", "", current_file())
      
      ## ... Load L2 flags/codes ####
      # If L2 annotations exist, read them in 
      # Otherwise read in L1 annotation
      if(data_identifier %in% annotation_directory$identifier){
        
        # First check for codes
        if(any(grepl("L2-codes.csv", annotation_directory$name))){
          codes_filename <- gsub("L1-data", "L2-codes", current_file())
          
          qc_output$codes <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/L2_quality_control/L2_codes/",
                                                  codes_filename)) %>%
            mutate_all(as.character)# %>%
            #mutate(timestamp = ymd_hms(timestamp))
        }
        
        flags_filename <- gsub("L1-data", "L2-flags", current_file())
        
        qc_output$flags <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/L2_quality_control/L2_flags/",
                                                flags_filename)) #%>%
          #mutate_all(as.character) # %>%
          # mutate(status = case_when(
          #   status == "-1" ~ "Not evaluated",
          #   status == "0" ~ "Revised", 
          #   status == "1" ~ "Approved",
          #   T ~ "-2"
          # ))#,
          # flag = case_when(
          #   flag == "-5" ~ "Outside high range",
          #   flag == "-4" ~ "Outside low range",
          #   flag == "-3" ~ "Data rejected due to QAQC",
          #   flag == "-2" ~ "Missing Data",
          #   flag == "-1" ~ "Optional parameter, not collected",
          #   flag == "0" ~ "Passed L1 QC",
          #   flag == "1" ~ "Suspect Data",
          #   flag == "2" ~ "Reserved for Future Use",
          #   T ~ "Other Flags"
          # ))
        
      } else {
        # Read in the L1 flags
        qc_output$flags <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/L1_DATA_FLAGS/", 
                                                   gsub("-data", "-flags", selected_file))) # %>%
         # mutate_all(as.character) # %>%
          #mutate(status = "Not evaluated")#, # Whether L1 flag has been accepted, rejected, or needs to be evaluated
                 # flag = case_when(
                 #   flag == "-5" ~ "Outside high range",
                 #   flag == "-4" ~ "Outside low range",
                 #   flag == "-3" ~ "Data rejected due to QAQC",
                 #   flag == "-2" ~ "Missing Data",
                 #   flag == "-1" ~ "Optional parameter, not collected",
                 #   flag == "0" ~ "Passed L1 QC",
                 #   flag == "1" ~ "Suspect Data",
                 #   flag == "2" ~ "Reserved for Future Use",
                 #   T ~ "Other Flags"
                 #))

      }
      
      current_site(data_inventory()[input$key_rows_selected,]$Site)
      current_date_range(paste(strftime(min(current_data$df$timestamp),
                                        '%Y-%m-%d'),
                               strftime(max(current_data$df$timestamp), 
                                        '%Y-%m-%d'),
                               sep = " to "))
      
      # Set min and max for start date input
      current_min_date(min(current_data$df$timestamp))
      current_max_date(max(current_data$df$timestamp))
      
    } else {
      showModal(modalDialog(
        title = "No Data Selected", 
        div("Load data by selecting the row in the table that represents the data of interest. 
            You can subset the table using the subset options to the left of the table."),
        
        easyClose = TRUE
      ))
    }
  })
  
  # # Reformats label_mode input for plotly color argument (ex: Codes to code)
  label_type <- reactive({
    gsub("s", "", tolower(input$label_mode))
  })
  
  date_range_max <- reactive({

    if(input$date_interval == "All data"){
      return(current_max_date())
    } else if(input$date_interval == "1 day"){
      return(input$start_date + hours(24))
    } else if(input$date_interval == "1 week"){
      return(input$start_date + weeks(1))
    } else if(input$date_interval == "1 month"){
      return(input$start_date + months(1))
    }
  })
  
  # Generate a plot of the data 
  #output$plot_qc <- renderPlotly({
  #plot_object_1 <- annotation_plot_server("plot_1", "Plot 1: Select a parameter", current_data$df)
  data_plot_1 <- annotation_controls_server("control_plot_1", current_data, qc_output, view_mode) # UI controls
  data_plot_2 <- annotation_controls_server("control_plot_2", current_data, qc_output, view_mode) # UI controls
  
  # Combine output from annotation control server into a single list
  # Drop empty list objects so they don't get evaluated by the plot module
  # List objects are empty if no parameter is selected
  plotting_data <- reactive({
    
    dat_list <- compact(list(df1 = data_plot_1(), df2 = data_plot_2()))
    
    max_row_value <- 0
    # Assign a unique ID to each observation in the list
    # Necessary to determine which subplot was selected in QC
    if(length(dat_list) > 0){
      for(i in 1:length(dat_list)){
        first_row_value <- max_row_value + 1
        max_row_value <- max_row_value + nrow(dat_list[[i]])
        
        dat_list[[i]]$parameter_name <- colnames(dat_list[[i]])[3]
        dat_list[[i]]$plot_id <- first_row_value:max_row_value
      }
    }
    
    return(dat_list)
  })
  
  selections <- annotation_plot_server("plot", plotting_data, label_type, start_date, date_range_max, reset_plot_status)
  
  ## Apply QC logic ####

  quality_control_stage <- reactiveVal(NA)
  n_flags_unrevised <- reactiveVal(NA)
  n_codes_unassigned <- reactiveVal(NA)
  total_points_in_selection <- reactiveVal(NA)
  selection <- reactiveValues(df = tibble())
  sensor_flag <- reactiveVal(NA)
  
  # # Reactive used to determine which points are in selection
  getPlotlySelection <- reactive({
    req(event_data("plotly_selected"))

    dat <- bind_rows(plotting_data()) %>%
      filter(plot_id %in% event_data("plotly_selected")$key)
    
    selected_parameter <- dat %>%
      select(parameter_name) %>%
      distinct() %>%
      pull(parameter_name)
    
    selected_sensor <- sensor_parameters_df %>%
      filter(parameter == selected_parameter) %>%
      pull(sensor_abbreviation)
    
    sensor_flag(selected_sensor)
    
    return(dat)
    # dat <- subset_data()
    # 
    # brush_subset <- event_data("plotly_selected") %>%
    #   mutate(key = as.character(key))
    # 
    # # Return subset of data within selection
    # dat %>%
    #   mutate(ID = as.character(ID)) %>%
    #   filter(ID %in% brush_subset$key)
  })

  ## ... QC UI box ####
  # UI options will depend on status of QC and is hierarchical
  # If no points selected, show instructions. Once selected:
  # ... First: any flags not yet approved/rejected
  # ... Second: provide option to add code(s)
  # ... Third: revise flags and/or code(s)

  getQualityControlUI <- reactive({
    if(is.null(event_data("plotly_selected"))){
      div(
        actionLink("basic_instructions", "Need help getting started?")
        
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "ordered_list_instructions.css")
        # ),
        # tags$h3("Quality Control Instructions"), tags$br(),
        # tags$ol(
        #   tags$li("Select a sensor and parameter above to plot data"),
        #   tags$li("Select points using the \"box selection\" or \"lasso selection\" tools on the plot toolbar. Click and drag the selection tool over the points to review and annotate."),
        #   tags$li("Review and update primary flags that have been assigned to each point. If the primary flag is -4 or -5, you must provide an updated flag."),
        #   tags$li("Assign quality control codes that provide additional context to the flag. Apply either a general or sensor-specific code. You can also tag points with one or more comment codes that provide additional context.")
        # )
      )
    } else if(!is.data.frame(event_data("plotly_selected"))){
      div(
        actionLink("basic_instructions", "Need help getting started?")
        
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "ordered_list_instructions.css")
        # ),
        # tags$h3("Quality Control Instructions"), tags$br(),
        # tags$ol(
        #   tags$li("Select a sensor and parameter above to plot data"),
        #   tags$li("Select points using the \"box selection\" or \"lasso selection\" tools on the plot toolbar. Click and drag the selection tool over the points to review and annotate."),
        #   tags$li("Review and update primary flags that have been assigned to each point. If the primary flag is -4 or -5, you must provide an updated flag."),
        #   tags$li("Assign quality control codes that provide additional context to the flag. Apply either a general or sensor-specific code. You can also tag points with one or more comment codes that provide additional context.")
        # )
      )
    } else if(quality_control_stage() == "revise out of bounds"){
      div(id = "accept_flag_div",
          tags$h3("Reassign Primary Flags"),
          tags$b(paste(n_flags_unrevised(), "of", total_points_in_selection(), "selected observations have been flagged as out of bounds (-4 or -5). ", sep = " ")),
          "Flag these points as \"rejected\" or \"suspect\". ", 
          "Note that any other points in the selection that are not flagged as -4 or -5 will also be updated with the updated flag you select.",
          tags$br(), tags$br(),

          splitLayout(
            cellWidths = "50%",
            div(
              selectInput("revise_flags", "Select updated flag",
                          choices = qc_flags),
              actionButton("confirm_revisions", "Confirm revised flags", class = "btn-primary"),
              actionButton("cancel_selection", "Cancel selection")
            ),
            div()),

          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                    #confirm_revisions{color:white}")))

      )
    } else if (quality_control_stage() == "revise secondary flags"){
      div(
        tags$h3("Revise Quality Control Flag"),

        splitLayout(
          cellWidths = "50%",
          div(
            selectInput("revise_flags", "Select updated flag",
                        choices = qc_flags),
            actionButton("confirm_revisions", "Confirm revised flags", class = "btn-primary"),
            actionButton("cancel_selection", "Cancel selection")
          ),
          div()),

        tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                    #confirm_revisions{color:white}")))

      )

    } else if(quality_control_stage() == "revise codes"){
      div(id = "revise_codes_div",
          tags$h3("Add or Revise Quality Control Codes"),
          "Assign a general or sensor code for ", tags$b("all"), " selected points. You may also select a comment code. ",
          "Note that you are allowed to assign codes to any point, regardless of flag. ",
          "All points that are not flagged as \"0\" require a general or sensor code to be assigned.", 
          tags$br(), tags$br(),

          splitLayout(
            div(splitLayout(
              div(
                selectInput("sensor_code_selection", "Select a general or sensor code",
                            choices = c("", sensor_codes)),
                actionButton("confirm_codes", "Confirm code selections", class = "btn-primary"),
                actionButton("skip_revising_codes", "Do not assign a code"),
                actionButton("cancel_selection", "Cancel selection")),
              selectInput("comment_code_selection", "Select a comment codes",
                          choices = c("", comment_codes))
            )),
            div()),

          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}
                                                #confirm_codes{color:white}")))
      )
    } else if(quality_control_stage() == "update annotations"){
      div(id = "update_annotations_div",
          tags$h3("Update Quality Control Flags and/or Codes"),
          "You may revise flags or codes. ",
          "If you revise flags, you will then be given the option to revise the codes.",
          tags$br(), tags$br(),
          
          splitLayout(
            cellWidths = "50%",
            splitLayout(
              div(
                actionButton("revise_flags_button", "Revise flags"),
                actionButton("revise_codes_button", "Revise codes"), tags$br(), tags$br(),
                actionButton("cancel_selection", "Cancel selection"))
            ),
            div()))    
      }
  })
  
  observeEvent(input$basic_instructions, {
    showModal(modalDialog(
      div(
        tags$h3("Quality Control Instructions"), tags$br(),
        tags$ol(
          tags$li("Select a sensor and parameter above to plot data"),
          tags$li("Select points using the \"box selection\" or \"lasso selection\" tools on the plot toolbar. Click and drag the selection tool over the points to review and annotate."),
          tags$li("Review and update primary flags that have been assigned to each point. If the primary flag is -4 or -5, you must provide an updated flag."),
          tags$li("Assign quality control codes that provide additional context to the flag. Apply either a general or sensor-specific code. You can also tag points with one or more comment codes that provide additional context.")
        )
      )
    ))
  })
  
  # # Triggers start of QC workflow when a selection event occurs
  observeEvent(!is.null(event_data("plotly_selected")), {

    if(is.data.frame(event_data("plotly_selected"))){
      selection$df <- getPlotlySelection()
      n_flags_unrevised(length(selection$df$flag[selection$df$flag %in% c(-4, -5)]))
      n_codes_unassigned(length(selection$df$code[selection$df$code == "Code required"]))
      total_points_in_selection(nrow(selection$df))

      js$collapseBox("plot_controls_box")
      
      if(n_flags_unrevised() > 0){
        quality_control_stage("revise out of bounds")
      } else if(all(selection$df$flag == -2)){
        quality_control_stage("revise codes")
      } else{
        quality_control_stage("update annotations")
      }
    }
  })

  ## ... Accept flags ####
  # If a user accepts flags, status column in flag dataframe is updated
  # observeEvent(input$accept_flags,{
  # 
  #   in_progress_qc$flags <- qc_output$flags %>%
  #     mutate(status = case_when(
  #       ID %in% selection$df$ID &
  #         sensor == sensor_flag() &
  #         status == "Not evaluated" ~ "Approved",
  #       T ~ status
  #     ))
  # 
  #   quality_control_stage("revise codes")
  # 
  # })

  ## ... Reject and revise flags ####
  # If a user accepts rejects flags, change UI to allow for new selection
  # observeEvent(input$reject_flags, {
  #   quality_control_stage("revise flags")
  # 
  # })

  observeEvent(input$confirm_revisions,{
    in_progress_qc$flags <- qc_output$flags %>%
      # mutate(status = case_when(
      #   ID %in% selection$df$ID &
      #     sensor == sensor_flag() &
      #     status == "Not evaluated" ~ "Revised",
      #   T ~ status
      # ),
      mutate(flag = case_when(
        ID %in% selection$df$ID &
          sensor == sensor_flag() &
          flag != -2 ~ as.integer(input$revise_flags),
        T ~ flag
      ))

    quality_control_stage("revise codes")

  })

  ## ... Confirm Codes ####
  observeEvent(input$confirm_codes, {

    qc_output$flags <- in_progress_qc$flags

    if(input$comment_code_selection == ""){
      selected_codes <- c(input$sensor_code_selection)
    } else {
      selected_codes <- c(input$sensor_code_selection, input$comment_code_selection)
    }
    
    revised_codes <- data.frame()
    for(selected_code in selected_codes){
      revised_codes <- selection$df %>%
        select(ID) %>%
        mutate(sensor = sensor_flag(),
               code = selected_code,
               ID = as.character(ID)) %>%
        bind_rows(revised_codes)
    }

    qc_output$codes <- qc_output$codes %>%
      filter(!(ID %in% selection$df$ID & sensor == sensor_flag())) %>%
      bind_rows(revised_codes)

    #js$expandBox("plot_controls_box")
    runjs("Shiny.setInputValue('plotly_selected-A', null);")
    
  })

  observeEvent(input$skip_revising_codes, {

    qc_output$flags <- in_progress_qc$flags

    runjs("Shiny.setInputValue('plotly_selected-A', null);")
    #js$expandBox("plot_controls_box")
    
    reset_plot_status(
      reset_plot_status() + 1
    )

  })

  ## ... revise annotations ####
  observeEvent(input$revise_flags_button, {
    quality_control_stage("revise secondary flags")

  })

  observeEvent(input$revise_codes_button,{

    in_progress_qc$flags <- qc_output$flags

    quality_control_stage("revise codes")

  })

  observeEvent(input$cancel_selection, {

    runjs("Shiny.setInputValue('plotly_selected-A', null);")
    #js$expandBox("plot_controls_box")
    
    reset_plot_status(
      reset_plot_status() + 1
    )

  })

  output$quality_control_box <- renderUI({
    getQualityControlUI()
  })




  # output$table_selected_points <- renderDataTable({
  #
  #   req(input$parameter_qc)
  #
  #   # By default, the table will show the timestamp, the selected QAQC parameter, and the QC numeric flag
  #   data_subset <- current_data$df %>%
  #     select(timestamp, input$parameter_qc, unname(sensor_vector_l1[input$sensor_qc]))
  #
  #   brush_subset <- event_data("plotly_selected")
  #
  #   if (!is.null(brush_subset)){
  #     # brush_subset <- brush_subset %>%
  #     #   mutate(key = ymd_hms(key))
  #
  #     datatable(
  #       data_subset %>%
  #         filter(ID %in% brush_subset$key)
  #     )
  #   } else {
  #     datatable(data_subset)
  #   }
  #
  # })

  output$table_summary_qc <- renderDataTable({

    if(nrow(qc_output$codes) > 0){
      summary <- qc_output$codes %>%
        #mutate(timestamp = ymd_hms(timestamp)) %>%
        group_by(sensor, code) %>%
        summarize(number_points_flagged = n())#,
                  #from = min(timestamp),
                  #to = max(timestamp))

      datatable(summary)
    }
  })

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

    flags_filename <- gsub("L1-data", "L2-flags", current_file())
    codes_filename <- gsub("L1-data", "L2-codes", current_file())

    setwd(tempdir())

    output_flags <- qc_output$flags # %>%
      # mutate(flag = case_when(
      #   flag == "Outside high range" ~ "-5",
      #   flag == "Outside low range" ~ "-4",
      #   flag == "Data rejected due to QAQC" ~ "-3",
      #   flag == "Missing Data" ~ "-2",
      #   flag == "Optional parameter, not collected" ~ "-1",
      #   flag == "Passed L1 QC" ~ "0",
      #   flag == "Suspect Data" ~ "1",
      #   flag == "Reserved for Future Use" ~ "2",
      #   T ~ "3"
      # )) %>%
      # mutate(status = case_when(
      #   status == "Not evaluated" ~ "-1",
      #   status == "Revised" ~ "0",
      #   status == "Approved" ~ "1",
      #   T ~ "-2"
      # ))

    # Upload flags even if no changes made
    write_csv(output_flags, flags_filename)

    drop_upload(flags_filename,
                path = "Marine_GEO_CPOP_PROCESSING/L2_quality_control/L2_flags")

    if(nrow(qc_output$codes) > 0){

      write_csv(qc_output$codes, codes_filename)

      drop_upload(codes_filename,
                  path = "Marine_GEO_CPOP_PROCESSING/L2_quality_control/L2_codes")

    }

    showModal(modalDialog(
      title = "Annotations saved",
      div("Your annotations have been saved."),

      easyClose = TRUE
    ))

    setwd(original_wd)

  })

}

function(input, output, session) {
  ## Source submission server code
  # source("./submission.R", local = TRUE)
  
  ## Reactive Objects ####
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())

  # Empty dataframes will hold points selected for level 1 QC flags and level 2 QC flags and codes
  qc_output <- reactiveValues(flags = tibble(observation_id = as.numeric(NA),
                                             parameter = as.character(NA),
                                             flag = as.character(NA),
                                             modified = as.logical(NA),
                                             .rows = 0),
                              codes = tibble(code_id = as.numeric(NA),
                                             observation_id = as.numeric(NA),
                                             parameter = as.character(NA),
                                             main_code = as.character(NA),
                                             comment_code = as.character(NA),
                                             main_code_modified = as.logical(NA),
                                             comment_code_modified = as.logical(NA),
                                             .rows = 0))

  flag_summary <- reactive({
    
    qc_output$flags %>%
      count(flag) %>%
      rename(`total observations` = n)
    
  })
  
  code_summary <- reactive({
    
    main_code_summary <- qc_output$codes %>%
      count(main_code) %>%
      rename(code = main_code)
    
    comment_code_summary <- qc_output$codes %>%
      count(comment_code) %>%
      rename(code = comment_code)
    
    code_summary <- bind_rows(main_code_summary, comment_code_summary) %>%
      rename(`total observations` = n)
    
    if(nrow(code_summary) == 0){
      code_summary <- add_row(code_summary, code = "None")
    }
    
    return(code_summary %>%
             filter(!is.na(code)))
  })
  
  full_dictionary_table <- reactive({
    full_definitions %>%
      filter(value %in% code_summary()$code | value %in% flag_summary()$flag)
    
  })
  
  current_site <- reactiveVal(NA) # Site for data currently loaded
  current_date_range <- reactiveVal(NA) # Date range for data currently loaded
  in_progress_qc <- reactiveValues() # Holds decision and outcomes of qc process until user cancels or confirms all decisions
  
  # Formatted name of selected sensors (transformed from UI-friendly versions)
  #view_mode <- reactive({input$view_mode})
  start_date <- reactive({input$start_date})
  label_type <- reactive({input$label_mode})
  
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
  })
  
  ## Subset data inventory ####
  data_inventory <- reactive({
    subset_key <- key

    subset_key
  })
  
  ## Infoboxes for data information ####
  output$data_info_box <- renderUI({
    
    if(!is.na(current_site())){
      div(id = "sidebar_summary",
        tags$hr(),
        
        paste0("Site: ", current_site()), tags$br(),
        paste0("Start Date: ", strftime(current_min_date(), '%Y-%m-%d')), tags$br(),
        paste0("End Date: ", strftime(current_max_date(), '%Y-%m-%d')), tags$br(),
        reference_message(),
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
  # output$site_info_box <- renderValueBox({
  #   valueBox(
  #     current_site(), "Site", icon = icon("broadcast-tower"),
  #     color = "purple"
  #   )
  # })
  # 
  # output$date_range_box <- renderValueBox({
  #   valueBox(
  #     current_date_range(), "Date Range", icon = icon("calendar-alt"),
  #     color = "teal"
  #   )
  # })
  # 
  # output$annotation_progress_box <- renderValueBox({
  #   valueBox(
  #     current_qc_progress(), "Quality Control Progress", icon = icon("percent"),
  #     color = "light-blue"
  #   )
  # })

  output$year_selection <- renderUI({
    
    req(input$site_selection)
    
    available_years <- key %>%
      filter(site_code == input$site_selection) %>%
      count(year) %>%
      pull(year)
    
    selectInput("year_selection", "Select a year", choices = available_years, multiple = FALSE)
    
  })
  
  output$month_selection <- renderUI({
    
    req(input$year_selection)
    
    available_months <- key %>%
      filter(site_code == input$site_selection,
             year == input$year_selection) %>%
      count(month) %>%
      pull(month)
    
    selectInput("month_selection", "Select a month", 
                choices = names(formatted_months)[formatted_months %in% available_months], multiple = FALSE)
  
    
  })
  
  # ## Datatable output for import ####
  # output$key <- renderDataTable({
  #   datatable(data_inventory(),
  #             options = list(
  #               pageLength = 5
  #             ),
  #             selection = "single")
  # })
  
   
  output$start_date <- renderUI({
    dateInput("start_date", label = "Update start date",
              value = min(current_data$df$timestamp), min = min(current_data$df$timestamp), max = max(current_data$df$timestamp))
  })
  
  ## Load Data ####
  # Action to take if run query button pressed
  observeEvent(input$loadData, {
    
    selected_site <- input$site_selection
    selected_year <- input$year_selection
    selected_month <- unname(formatted_months[input$month_selection])
    
    
    if(nrow(filter(key, year == selected_year,
                   month == selected_month,
                   site_code == selected_site)) > 0){
      
        con <- DBI::dbConnect(odbc::odbc(),
                              Driver = "MySQL ODBC 8.0 ANSI Driver",
                              Server = "si-mysqlproxy01.si.edu",
                              Port = 7003,
                              Database = "orc_data_lake",
                              UID = "datLakeDev",
                              PWD = Sys.getenv('password'))
        
        wq_dat <- tbl(con, "water_quality_l1")
        wq_qc_dat <- tbl(con, "water_quality_expert_flags")
        wq_codes_dat <- tbl(con, "water_quality_codes")
        
        current_data$df <- wq_dat %>%
          filter(year(timestamp_1min) == selected_year,
                 month(timestamp_1min) == selected_month,
                 site_code == selected_site) %>%
          collect() %>%
          select(-timestamp) %>%
          rename(timestamp = timestamp_1min)
        
        current_ids <- current_data$df$observation_id
        
        raw_flags <- wq_qc_dat %>%
          filter(observation_id %in% current_ids) %>%
          collect()
        
        raw_codes <- wq_codes_dat %>%
          filter(observation_id %in% current_ids) %>%
          collect()
        
        dbDisconnect(con)
        
        reference_message("")

        # Remove any reference data if it exists
        # reference_data$df <- data.frame()
        
        qc_output$flags <- raw_flags %>%
          pivot_longer(Turbidity_FNU_f:fDOM_RFU_f, names_to = "parameter", values_to = "flag") %>%
          mutate(modified = F)
    
        if(nrow(raw_codes) > 0){
          qc_output$codes <- raw_codes %>%
            mutate(comment_code_modified = F,
                   main_code_modified = F)
        }
        
        current_site(selected_site)
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
  
  reference_message <- reactiveVal("")
  
  ## Load reference data ####
  observeEvent(input$loadReferenceData, {
    
    if(nrow(current_data$df) > 0){

      month_interval <- as.numeric(input$selectReferenceRange)
      
      min_left_bound <- current_min_date() %m-% months(month_interval)
      max_left_bound <- current_min_date()
      min_right_bound <- current_max_date()
      max_right_bound <- current_max_date() %m+% months(month_interval)
      
      con <- DBI::dbConnect(odbc::odbc(),
                            Driver = "MySQL ODBC 8.0 ANSI Driver",
                            Server = "si-mysqlproxy01.si.edu",
                            Port = 7003,
                            Database = "orc_data_lake",
                            UID = "datLakeDev",
                            PWD = Sys.getenv('password'))

      wq_dat <- tbl(con, "water_quality_l1")

      reference_data <- wq_dat %>%
        filter(site_code == !!input$site_selection) %>%
        filter((timestamp_1min < max_left_bound & timestamp_1min > min_left_bound) |
               (timestamp_1min > min_right_bound & timestamp_1min < max_right_bound)) %>%
        collect() %>%
        select(-timestamp) %>%
        rename(timestamp = timestamp_1min)

      dbDisconnect(con)

      current_data$df <- current_data$df %>%
        bind_rows(reference_data)
      
      reference_message("Reference points loaded")
      
      updateTabItems(session, "tabs", "annotate_data")
      
    } else {
      showModal(modalDialog(
        title = "No Data Selected",
        div("You must select data for markup prior to loading reference data."),
        
        easyClose = TRUE
      ))
    }
    
  })
  
  date_range_max <- reactive({

    if(input$date_interval == "All data"){
      return(max(current_data$df$timestamp))
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
  data_plot_1 <- annotation_controls_server("control_plot_1", current_data, qc_output) # UI controls
  data_plot_2 <- annotation_controls_server("control_plot_2", current_data, qc_output) # UI controls
  
  # Combine output from annotation control server into a single list
  # Drop empty list objects so they don't get evaluated by the plot module
  # List objects are empty if no parameter is selected
  plotting_data <- reactive({
    
    dat_list <- compact(list(df1 = data_plot_1(), df2 = data_plot_2()))
    
    max_row_value <- 0
    # Assign a unique id to each observation in the list
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
  
  selections <- annotation_plot_server("plot", plotting_data, label_type, start_date, date_range_max, reset_plot_status, color_dictionary_flags)
  
  ## Apply QC logic ####

  quality_control_stage <- reactiveVal(NA)
  n_flags_unrevised <- reactiveVal(NA)
  n_codes_unassigned <- reactiveVal(NA)
  total_points_in_selection <- reactiveVal(NA)
  selection <- reactiveValues(df = tibble())
  parameter_flag <- reactiveVal(NA)
  
  # # Reactive used to determine which points are in selection
  getPlotlySelection <- reactive({
    req(event_data("plotly_selected"))

    dat <- bind_rows(plotting_data()) %>%
      filter(plot_id %in% event_data("plotly_selected")$key)
    
    selected_parameter <- dat %>%
      select(parameter_name) %>%
      distinct() %>%
      pull(parameter_name)
    
    parameter_flag(selected_parameter)
    
    return(dat)
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
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "ordered_list_instructions.css")
        # ),
        actionLink("basic_instructions", "Need help getting started?")
        # tags$h3("Quality Control Instructions"), tags$br(),
        # tags$ol(
        #   tags$li("Select a sensor and parameter above to plot data"),
        #   tags$li("Select points using the \"box selection\" or \"lasso selection\" tools on the plot toolbar. Click and drag the selection tool over the points to review and annotate."),
        #   tags$li("Review and update primary flags that have been assigned to each point. If the primary flag is -4 or -5, you must provide an updated flag."),
        #   tags$li("Assign quality control codes that provide additional context to the flag. Apply either a general or sensor-specific code. You can also tag points with one or more comment codes that provide additional context.")
        # )
      )
    } else if(!is.data.frame(event_data("plotly_selected")) | quality_control_stage() == "only_reference_points_selected"){
      div(
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
  #
  
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
      
      selection$df <- getPlotlySelection() %>%
        filter(flag != "Ref")
      
      if(nrow(selection$df) == 0){
        quality_control_stage("only_reference_points_selected")
        
      } else {
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
      
    }
  })

  observeEvent(input$confirm_revisions,{
    
    in_progress_qc$flags <- qc_output$flags %>%
      # If a flag is updated, change modified to T
      mutate(modified = case_when(
        observation_id %in% selection$df$observation_id &
          parameter == paste0(parameter_flag(), "_f") &
          flag != -2 & flag != as.integer(input$revise_flags) ~ T,
        T ~ modified
      )) %>%
      mutate(flag = case_when(
        observation_id %in% selection$df$observation_id &
          parameter == paste0(parameter_flag(), "_f") &
          flag != -2 ~ as.integer(input$revise_flags),
        T ~ flag
      )) 
    
    quality_control_stage("revise codes")

  })

  ## ... Confirm Codes ####
  observeEvent(input$confirm_codes, {
    
    # If no code inputs selected, cancel
    if(input$comment_code_selection == "" & input$sensor_code_selection == ""){
      showModal(modalDialog(
        div("Don't forget to select at least one code!"),
        
        easyClose = TRUE
      ))
    } else{
      
      qc_output$flags <- in_progress_qc$flags
      
      if(input$comment_code_selection == ""){
        comment_code_selection <- NA_character_
      } else {
        comment_code_selection <- input$comment_code_selection
      }
      
      if(input$sensor_code_selection == ""){
        sensor_code_selection <- NA_character_
      } else {
        sensor_code_selection <- input$sensor_code_selection
      }
      
      # Update existing codes 
      updated_codes <- qc_output$codes %>%
        mutate(
          main_code_modified = case_when(
            observation_id %in% selection$df$observation_id &
              parameter == parameter_flag() & 
              sensor_code_selection != main_code ~ T,
            T ~ main_code_modified),
          comment_code_modified = case_when(
            observation_id %in% selection$df$observation_id &
              parameter == parameter_flag() & 
              comment_code_selection != comment_code ~ T,
            T ~ comment_code_modified)
        ) %>%
        mutate(main_code = case_when(
          observation_id %in% selection$df$observation_id &
            parameter == parameter_flag() ~ input$sensor_code_selection,
          T ~ main_code
        )) %>%
        mutate(comment_code = case_when(
          observation_id %in% selection$df$observation_id &
            parameter == parameter_flag() ~ comment_code_selection,
          T ~ comment_code
        )) 
      
      if(length(selection$df$observation_id[!selection$df$observation_id %in% updated_codes$observation_id]) > 0){
        # Create new rows if observation dobservation_id not have any pre-existing codes
        new_codes <- tibble(
          observation_id = selection$df$observation_id[!selection$df$observation_id %in% updated_codes$observation_id],
          parameter = parameter_flag(),
          main_code = sensor_code_selection,
          comment_code = comment_code_selection
        ) %>%
          mutate(
            main_code_modified = case_when(
              !is.na(main_code) ~ T,
              T ~ F),
            comment_code_modified = case_when(
              !is.na(comment_code) ~ T,
              T ~ F))
        
        qc_output$codes <- bind_rows(updated_codes, new_codes)
        
      } else {
        qc_output$codes <- updated_codes
      }
      
      runjs("Shiny.setInputValue('plotly_selected-A', null);")
    }
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
    quality_control_stage("revise secondary flags")
    
  })

  observeEvent(input$revise_codes_button,{

    in_progress_qc$flags <- qc_output$flags

    quality_control_stage("revise codes")

  })

  observeEvent(input$cancel_selection, {

    runjs("Shiny.setInputValue('plotly_selected-A', null);")
    
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
  #         filter(id %in% brush_subset$key)
  #     )
  #   } else {
  #     datatable(data_subset)
  #   }
  #
  # })

  output$table_summary_qc <- renderDataTable({

    if(nrow(qc_output$codes) > 0){
      # summary <- qc_output$codes %>%
      #   #mutate(timestamp = ymd_hms(timestamp)) %>%
      #   group_by(parameter, code) %>%
      #   summarize(number_points_flagged = n())#,
      #             #from = min(timestamp),
      #             #to = max(timestamp))
      # 
      # datatable(summary)
    }
  })

  ## Remove QC flags ####
  # output$remove_codes <- renderUI({
  #   selectInput("select_remove_codes", "Select QC codes to remove",
  #               choices = unique(qc_output$codes$code), multiple = TRUE)
  # 
  # })

  # observeEvent(input$confirm_removal,{
  # 
  #   if(!is.null(input$select_remove_codes)){
  #     points_to_remove <- qc_output$codes %>%
  #       filter(code %in% input$select_remove_codes)
  # 
  #     qc_output$codes <- qc_output$codes %>%
  #       filter(!(code %in% input$select_remove_codes))
  # 
  #   }
  # })

  ## Submit annotations ####
  observeEvent(input$submit_codes, {

    output_flags <- qc_output$flags %>%
      filter(modified) 

    con <- getDatabaseConnection()

    if(nrow(output_flags) > 0){
      
      for(current_parameter in unique(output_flags$parameter)){
        
        param_flags <- output_flags %>%
          filter(parameter == current_parameter)
        
        for(current_flag in unique(param_flags$flag)){
          
          vals <- param_flags %>%
            filter(flag == current_flag) %>%
            pull(observation_id)
          
          sql <- paste0("UPDATE water_quality_expert_flags SET `", current_parameter, "` = \"", current_flag, "\" WHERE `observation_id` IN (?)")
          
          dbSendQuery(con, sql, list(vals))
        }
      }
    }
    
    # Quality Control Codes
    # Step 1 - If code_ID is NA, write those codes
    new_codes <- qc_output$codes %>%
      filter(is.na(code_id)) %>%
      select(-main_code_modified, -comment_code_modified)
    
    if(nrow(new_codes) > 0){
      DBI::dbWriteTable(con,
                        value = new_codes,
                        name = "water_quality_codes", append = TRUE)
    }
    
    # Step 2 - Update other modified codes
    modified_main_codes <- qc_output$codes %>%
      filter(main_code_modified & !is.na(code_id)) %>%
      select(-main_code_modified, -comment_code_modified)
    
    if(nrow(modified_main_codes) > 0){
      
      for(current_code in unique(modified_main_codes$main_code)){
        
        vals <- modified_main_codes %>%
          filter(main_code == current_code) %>%
          pull(code_id)
        
        sql <- paste0("UPDATE water_quality_codes SET `main_code` = \"", current_code, "\" WHERE `code_id` IN (?)")
        
        dbSendQuery(con, sql, list(vals))
        print(modified_main_codes %>%
                filter(main_code == current_code))
      }
    }
    
    modified_comment_codes <- qc_output$codes %>%
      filter(comment_code_modified & !is.na(code_id)) %>%
      select(-main_code_modified, -comment_code_modified)
    
    if(nrow(modified_comment_codes) > 0){
      
      for(current_code in unique(modified_comment_codes$comment_code)){
        
        vals <- modified_comment_codes %>%
          filter(comment_code == current_code) %>%
          pull(observation_id)
        
        sql <- paste0("UPDATE water_quality_codes SET `comment_code` = \"", current_code, "\" WHERE `observation_id` IN (?)")
        
        print(modified_comment_codes %>%
                filter(comment_code == current_code))
        dbSendQuery(con, sql, list(vals))
      }
    }
    
    current_ids <- qc_output$flags$observation_id
    wq_codes_dat <- tbl(con, "water_quality_codes")
    
    qc_output$codes <- wq_codes_dat %>%
      filter(observation_id %in% current_ids) %>%
      collect() %>%
      mutate(main_code_modified = FALSE,
             comment_code_modified = FALSE)
    
    dbDisconnect(con)
    
    # Reset modified status so if user continues annotating, previous changes are re-sent
    qc_output$flags <- qc_output$flags %>%
      mutate(modified = FALSE)
    
    showModal(modalDialog(
      title = "Annotations saved",
      div("Your annotations have been saved."),

      easyClose = TRUE
    ))

  })

}
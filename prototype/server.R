
function(input, output, session) {
  ## Reactive Objects ####
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  
  # Empty dataframes will hold points selected for level 1 and level 2 QC codes
  qc_output <- reactiveValues(l1 = tibble(ID = as.character(NA),
                                          timestamp = as.POSIXct(NA),
                                          sensor = as.character(NA),
                                          code = as.character(NA),
                                          .rows = 0),
                              l2 = tibble(ID = as.character(NA),
                                          timestamp = as.POSIXct(NA), 
                                          sensor = as.character(NA),
                                          code = as.character(NA),
                                          .rows = 0))
  
  ## Sensor - Parameter UI logic
  sensor_parameters <- reactiveVal("")
  current_sensor <- reactiveVal(NA)
  parameter_mean <- reactiveVal(NA)
  current_file <- reactiveVal(NA)
  current_site <- reactiveVal(NA)
  current_date_range <- reactiveVal(NA)
  
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
  
  output$annotation_progress_box <- renderValueBox({
    valueBox(
      current_qc_progress(), "Quality Control Progress", icon = icon("percent"),
      color = "light-blue"
    )
  })

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
  
  # label_mode_results <- reactive({
  #   if(input$label_mode == "Most up to date annotations"){
  #     
  #   } else if (input$label_mode == "Level 1 annotations only"){
  #     
  #   } else {
  #     
  #   }
  # })
  
  # getSensorFlags <- reactive({
  #   
  #   if(input$sensor_qc %in% names(sensor_vector_l1)){
  #     sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
  #     
  #     unique(
  #       current_data$df %>%
  #         select(sensor_l1_flag) %>%
  #         pull(sensor_l1_flag)
  #     )
  #   } else {
  #     ""
  #   }
  #   
  # })
  
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
      
      l1_flags <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/", 
                                       gsub("QAQC", "flagging_table", selected_file))) %>%
        mutate_all(as.character) %>%
        mutate(timestamp = ymd_hms(timestamp))
                                
      # Check if current filename has previous annotations and read them in
      if(current_file() %in% annotation_directory$filename){
        annotation_filename <- annotation_directory %>%
          filter(filename == current_file()) %>%
          pull(name)
        
        qc_output$l2 <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/",
                                               annotation_filename)) %>%
          mutate_all(as.character) %>%
          mutate(timestamp = ymd_hms(timestamp))
        
      } else {
        qc_output$l2 <- l1_flags %>%
          rename(code = code_char) %>%
          mutate(file = current_file()) %>%
          filter(!is.na(code)) %>%
          select(-code_num)
      }
      
      qc_output$l1 <- l1_flags %>%
        rename(code = code_num) %>%
        select(timestamp, ID, sensor, code) %>%
        mutate(code = case_when(
          code == "-5" ~ "Outside high range (L1)",
          code == "-4" ~ "Outside low range (L1)",
          code == "-3" ~ "Data rejected due to QAQC (L1)",
          code == "-2" ~ "Missing Data (L1)",
          code == "-1" ~ "Optional parameter, not collected (L1)",
          code == "0" ~ "Passed L1 QC",
          code == "1" ~ "Suspect Data (L1)",
          code == "2" ~ "Reserved for Future Use (L1)",
          T ~ "Other Codes (L1)"
        ))
      
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
  
  output$date_range <- renderUI({
    
    if(nrow(current_data$df) > 0){
      dateInput(inputId = "date_anchor",
                label = "Select a date to anchor the x-axis",
                value = min(current_data$df$timestamp),
                min = min(current_data$df$timestamp),
                max = max(current_data$df$timestamp),
                startview = "year")
    }
  })
  
  ## Plots ####
  # Generate a plot of the data 
  output$plot_qc <- renderPlotly({
    
    req(input$parameter_qc)
    #sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
    #sensor_l2_flag <- paste0(sensor_l1_flag, "C")
    sensor_flag <- gsub("_0", "", (unname(sensor_vector_l1[input$sensor_qc])))
    
    # Determine if observations for current sensor have been annotated
    l2_timestamps <- qc_output$l2 %>%
      filter(sensor == sensor_flag) %>%
      mutate(description = qc_flag_descriptions[code]) %>%
      group_by(timestamp) %>%
      summarize(l2_description = paste(description, collapse = ", "))
    
    # Summarize and append L1 flags to data
    l1_timestamps <- qc_output$l1 %>%
      filter(sensor == sensor_flag) %>%
      group_by(timestamp) %>%
      summarize(l1_description = paste(code, collapse = ", "))
    
    dat <- current_data$df %>%
      select(timestamp, all_of(input$parameter_qc)) %>%
      merge(l2_timestamps, by="timestamp", all.x=TRUE) %>%
      merge(l1_timestamps, by="timestamp", all.x=TRUE) %>%
      # Provide values to missing data
      mutate(!!input$parameter_qc := case_when(
        grepl("Missing Data", l1_description) ~ parameter_mean(),
        T ~ .data[[input$parameter_qc]]
      ))
    
    if(input$label_mode == "Highest level annotations"){
      dat <- dat %>%
        # Change label for plots that have a L2 flag
        mutate(plot_labels = case_when(
          !is.na(l2_description) ~ l2_description,
          T ~ l1_description
        )) 
      
    } else if (input$label_mode == "Level 2 annotations only"){
      dat <- dat %>%
        # Change label for plots that have a L2 flag
        mutate(plot_labels = case_when(
          !is.na(l2_description) ~ l2_description,
          T ~ "No Level 2 annotation"
        ))
      
    } else {
      dat <- dat %>%
        # Change label for plots that have a L2 flag
        mutate(plot_labels = case_when(
          !is.na(l1_description) ~ l1_description,
          T ~ "No Level 1 annotation"
        ))      
    }
    
    plot_ly(dat, x = ~timestamp, y = ~get(input$parameter_qc), 
            color = ~plot_labels, key=~timestamp,
            type = "scatter") %>%
      layout(legend = list(orientation = 'h',
                           y = -.5),
             xaxis = list(title = "",
                          # range = c(
                          #   as.character(input$date_anchor),
                          #   substr(
                          #     as.character(max(current_data$df$timestamp)),
                          #     start = 1, stop = 10)),
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 1,
                                label = "1 day",
                                step = "day",
                                stepmode = "tocount"),
                              list(
                                count = 7,
                                label = "1 wk",
                                step = "day",
                                stepmode = "tocount"),
                              list(
                                count = 1,
                                label = "1 mo",
                                step = "month",
                                stepmode = "tocount")
                            )
                          ),
                          rangeslider = list(type = "date")),
             showlegend = TRUE) %>%
      toWebGL()  # Conversion from SVG drastically improves performance
    
  })
  
  output$plot_reference <- renderPlot({
    
    # Prevents error message from getting written to UI upon app startup
    tryCatch({
      current_data$df %>%
        select(timestamp, input$parameter_reference) %>%
        # melt the data to long form
        gather(key="variable", value = "measurement", -timestamp, na.rm = TRUE) %>%
        ggplot(aes(timestamp, measurement)) +
        geom_point() +
        # coord_cartesian(xlim = ranges$x, expand = TRUE) +
        # facet_grid(variable ~ .) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
              text=element_text(size=18)) +
        ylab("")
      
    }, error = function(e){
      
    })
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
    
    if(nrow(qc_output$l2) > 0){
      summary <- qc_output$l2 %>%
        mutate(timestamp = ymd_hms(timestamp)) %>%
        group_by(sensor, code) %>%
        summarize(number_points_flagged = n(),
                  from = min(timestamp),
                  to = max(timestamp))
      
      datatable(summary)
    }
  })
  
  ## Apply QC logic ####
  observeEvent(input$apply_qc, {
    
    # sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
    # sensor_l2_flag <- paste0(sensor_l1_flag, "C")
    sensor_flag <- gsub("_0", "", unname(sensor_vector_l1[input$sensor_qc]))
    
    data_subset <- current_data$df %>%
      # mutate(!!input$parameter_qc := case_when(
      #   !!sym(sensor_l1_flag) == "Missing Data" ~ parameter_mean(),
      #   T ~ !!sym(input$parameter_qc)
      # )) %>%
      select(timestamp, ID, all_of(input$parameter_qc))
    
    brush_subset <- event_data("plotly_selected") %>%
        mutate(key = ymd_hms(key))
    
    codes <- qc_flags %>%
      filter(select_inputs %in% c(input$wq_qc_flags, input$met_qc_flags)) %$%
      paste(unique(.$code), collapse = ",")
    
    qc_output$l2 <- data_subset %>%
      filter(timestamp %in% brush_subset$key) %>%
      mutate(sensor = sensor_flag,
             code = codes,
             file = current_file(),
             ID = as.character(ID)) %>%
      select(-input$parameter_qc) %>%
      separate_rows(code, sep=",") %>%
      bind_rows(qc_output$l2)
  })
  
  ## Remove QC flags ####
  output$remove_flags <- renderUI({
    selectInput("select_remove_flags", "Select QC flags to remove",
                choices = unique(qc_output$l2$code), multiple = TRUE)
    
  })
  
  observeEvent(input$confirm_removal,{
    
    if(!is.null(input$select_remove_flags)){
      points_to_remove <- qc_output$l2 %>%
        filter(code %in% input$select_remove_flags) 
      
      qc_output$l2 <- qc_output$l2 %>%
        filter(!(code %in% input$select_remove_flags))
      
    }
  })
  
  ## Submit annotations ####
  observeEvent(input$confirm_flags, {
    
    filename <- paste(gsub(".csv", "", current_file()),
                      "L2",
                      #format(Sys.time(), "%Y%m%d-%H%M%OS"),
                      sep = "-")
    
    setwd(tempdir())
    
    #if(nrow(qc_output$l2) > 0){
    
    write_csv(qc_output$l2,
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
}
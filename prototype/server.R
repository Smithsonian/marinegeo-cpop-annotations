
function(input, output, session) {
  ## Reactive Objects ####
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  # Empty dataframe will hold points selected for QC and associated QC codes
  qc_output <- reactiveValues(df = tibble(timestamp = as.POSIXct(NA), 
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
  
  output$filter_flag <- renderUI({
    selectInput("filter_flag", "Plot Initial QC Flags",
                getSensorFlags(), multiple = TRUE, selected = getSensorFlags())
  })
  
  getSensorFlags <- reactive({
    
    if(input$sensor_qc %in% names(sensor_vector_l1)){
      sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
      
      unique(
        current_data$df %>%
          select(sensor_l1_flag) %>%
          pull(sensor_l1_flag)
      )
    } else {
      ""
    }
    
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
        rename(timestamp = timestamp2) %>%
        mutate(timestamp = ymd_hms(timestamp)) 
      
      # Check if current filename has previous annotations and read them in
      if(current_file() %in% annotation_directory$filename){
        annotation_filename <- annotation_directory %>%
          filter(filename == current_file()) %>%
          pull(name)
        
        qc_output$df <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/",
                                               annotation_filename)) %>%
          mutate_all(as.character) %>%
          mutate(timestamp = ymd_hms(timestamp))
        
      } else {
        qc_output$df <- current_data$df %>%
          select(timestamp, any_of(unname(sensor_vector_l2))) %>%
          pivot_longer(cols = any_of(unname(sensor_vector_l2)),
                       names_to = "sensor",
                       values_to = "code") %>%
          mutate(file = current_file()) %>%
          filter(!is.na(code))
      }
      
      current_data$df <- current_data$df %>%
        mutate_at(unname(sensor_vector_l1),
                  funs(case_when(
                    . == "-5" ~ "Outside high range",
                    . == "-4" ~ "Outside low range",
                    . == "-3" ~ "Data rejected due to QAQC",
                    . == "-2" ~ "Missing Data",
                    . == "-1" ~ "Optional parameter, not collected",
                    . == "0" ~ "Passed initial QAQC check",
                    . == "1" ~ "Suspect Data",
                    . == "2" ~ "Reserved for Future Use",
                    T ~ "Other Codes"
                  )))
      
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
  
  ## Plotly conversion ####
  # Generate a plot of the data 
  output$plot_qc <- renderPlotly({
    
    # Prevents error message from getting written to UI upon app startup
    #tryCatch({
      
      sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
      sensor_l2_flag <- paste0(sensor_l1_flag, "C")
      
      # Determine if observations for current sensor have been annotated
      l2_timestamps <- qc_output$df %>%
        filter(sensor == sensor_l2_flag) %>%
        pull(timestamp)
      
      current_data$df %>%
        select(timestamp, all_of(sensor_l1_flag), all_of(input$parameter_qc)) %>%
        # Provide values to missing data
        mutate(!!input$parameter_qc := case_when(
          !!sym(sensor_l1_flag) == "Missing Data" ~ parameter_mean(),
          T ~ !!sym(input$parameter_qc)
        )) %>%
        # Create plotly object
        plot_ly(x = ~timestamp, y = ~get(input$parameter_qc), color = ~get(sensor_l1_flag), key=~timestamp, 
                type = "scatter") %>%
        layout(legend = list(orientation = 'h'),
               xaxis = list(title = ""),
               showlegend = TRUE) %>%
        toWebGL()  # Conversion from SVG drastically improves performance
      
      # current_data$df %>%
      #   select(timestamp, all_of(sensor_l1_flag), all_of(input$parameter_qc)) %>%
      #   filter(!!sym(sensor_l1_flag) %in% input$filter_flag) %>%
      #   mutate(numeric_label = case_when(
      #     timestamp %in% l2_timestamps ~ "QC Flag Applied",
      #     T ~ !!sym(sensor_l1_flag)
      #   )) %>%
      #   mutate(!!input$parameter_qc := case_when(
      #     !!sym(sensor_l1_flag) == "Missing Data" ~ parameter_mean(),
      #     T ~ !!sym(input$parameter_qc)
      #   )) %>%
      #   ggplot(aes_string("timestamp", all_of(input$parameter_qc), color = "numeric_label")) +
      #   geom_point() +
      #   scale_color_manual(values = c("Outside high range" = "#67001f",
      #                                 "Outside low range" = "#b2182b",
      #                                 "Data rejected due to QAQC" = "#d6604d",
      #                                 "Missing Data" = "#f4a582",
      #                                 "Optional parameter, not collected" = "#fddbc7",
      #                                 "Passed initial QAQC check" = "grey", 
      #                                 "Suspect Data" = "#e08214", 
      #                                 "QC Flag Applied" = "#2166ac")) +
      #   coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
      #   theme_bw() + 
      #   theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
      #         axis.title = element_blank(),
      #         legend.position = "top",
      #         legend.title=element_blank(), 
      #         text=element_text(size=18)) +
      #   ylab("")
      
    #}, error = function(e){
    #})
  })
  
  
  ## Plots ####
  # Generate a plot of the data 
  # output$plot_qc <- renderPlot({
  #   
  #   # Prevents error message from getting written to UI upon app startup
  #   tryCatch({
  #     
  #     sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
  #     sensor_l2_flag <- paste0(sensor_l1_flag, "C")
  #     
  #     # Determine if observations for current sensor have been annotated
  #     l2_timestamps <- qc_output$df %>%
  #       filter(sensor == sensor_l2_flag) %>%
  #       pull(timestamp)
  #       
  #     current_data$df %>%
  #       select(timestamp, all_of(sensor_l1_flag), all_of(input$parameter_qc)) %>%
  #       filter(!!sym(sensor_l1_flag) %in% input$filter_flag) %>%
  #       mutate(numeric_label = case_when(
  #         timestamp %in% l2_timestamps ~ "QC Flag Applied",
  #         T ~ !!sym(sensor_l1_flag)
  #       )) %>%
  #       mutate(!!input$parameter_qc := case_when(
  #         !!sym(sensor_l1_flag) == "Missing Data" ~ parameter_mean(),
  #         T ~ !!sym(input$parameter_qc)
  #       )) %>%
  #       ggplot(aes_string("timestamp", all_of(input$parameter_qc), color = "numeric_label")) +
  #       geom_point() +
  #       scale_color_manual(values = c("Outside high range" = "#67001f",
  #                                     "Outside low range" = "#b2182b",
  #                                     "Data rejected due to QAQC" = "#d6604d",
  #                                     "Missing Data" = "#f4a582",
  #                                     "Optional parameter, not collected" = "#fddbc7",
  #                                     "Passed initial QAQC check" = "grey", 
  #                                     "Suspect Data" = "#e08214", 
  #                                     "QC Flag Applied" = "#2166ac")) +
  #       coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE) +
  #       theme_bw() + 
  #       theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
  #             axis.title = element_blank(),
  #             legend.position = "top",
  #             legend.title=element_blank(), 
  #             text=element_text(size=18)) +
  #       ylab("")
  # 
  #     }, error = function(e){
  #   })
  # })
  
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
  
  # output$plot_facet <- renderPlot({
  #   # Plot allows techs to visualize 1 to all parameters without slowly moving from parameter to parameter on the main panel
  #   # Defaults to null until parameters are selected in UI
  #   if(!is.null(input$facet_parameters)){
  #     # Prevents error message from getting written to UI upon app startup
  #     tryCatch({
  #       current_data$df %>%
  #         select(timestamp, input$facet_parameters) %>%
  #         # melt the data to long form
  #         gather(key="variable", value = "measurement", -timestamp, na.rm = TRUE) %>%
  #         ggplot(aes(timestamp, measurement)) +
  #         geom_point() +
  #         # coord_cartesian(xlim = ranges$x, expand = FALSE) + #, ylim = ranges$y
  #         facet_grid(variable ~ ., scales = "free_y") +
  #         theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  #         ylab("")
  #       
  #     }, error = function(e){
  #       
  #     })
  #   }
  # })
  
  # Adapted from https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
  # ranges <- reactiveValues(x = NULL, y = NULL)
  
  # observeEvent(input$plot_dblclick, {
  #     
  #   brush <- input$plot_brush
  #     
  #     if (!is.null(brush)) {
  #       ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"),
  #                     as.POSIXct(brush$xmax, origin = "1970-01-01"))
  #       ranges$y <- c(brush$ymin, brush$ymax)
  # 
  #     } else {
  #       ranges$x <- NULL
  #       ranges$y <- NULL
  #     }
  # 
  # })
  
  output$table_selected_points <- renderDataTable({
    
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
    
    # brush_subset <- brushedPoints(data_subset, input$plot_brush,
    #                               yvar = input$parameter_qc) 
    # 
    # print(brush_subset)
    # 
    # if(nrow(brush_subset) == 0){
    #   datatable(data_subset)
    # } else {datatable(brush_subset)}
    
  })
  
  output$table_summary_qc <- renderDataTable({
    
    if(nrow(qc_output$df) > 0){
      summary <- qc_output$df %>%
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
    
    sensor_l1_flag <- unname(sensor_vector_l1[input$sensor_qc])
    sensor_l2_flag <- paste0(sensor_l1_flag, "C")
    
    data_subset <- current_data$df %>%
      mutate(!!input$parameter_qc := case_when(
        !!sym(sensor_l1_flag) == "Missing Data" ~ parameter_mean(),
        T ~ !!sym(input$parameter_qc)
      )) %>%
      select(timestamp, input$parameter_qc)
    
    brush_subset <- event_data("plotly_selected") %>%
        mutate(key = ymd_hms(key))
    
    # # Returns points under the brush
    # brush_subset <- brushedPoints(data_subset, input$plot_brush,
    #                               yvar = input$parameter_qc) 
    # 
    # # Right now, bind WQ and MET codes together
    codes <- qc_flags %>%
      filter(select_inputs %in% c(input$wq_qc_flags, input$met_qc_flags)) %$%
      paste(unique(.$code), collapse = ",")
    # 
    # #codes <- paste0(input$wq_qc_flags, input$met_qc_flags, sep= ",")
    
    # qc_output$df <- brush_subset %>%
    #   mutate(sensor = sensor_l2_flag,
    #          code = codes,
    #          file = current_file()) %>%
    #   select(-input$parameter_qc) %>%
    #   separate_rows(code, sep=",") %>%
    #   bind_rows(qc_output$df)
    
    qc_output$df <- data_subset %>%
      filter(timestamp %in% brush_subset$key) %>%
      mutate(sensor = sensor_l2_flag,
             code = codes,
             file = current_file()) %>%
      select(-input$parameter_qc) %>%
      separate_rows(code, sep=",") %>%
      bind_rows(qc_output$df)
  })
  
  ## Remove QC flags ####
  output$remove_flags <- renderUI({
    selectInput("select_remove_flags", "Select QC flags to remove",
                choices = unique(qc_output$df$code), multiple = TRUE)
    
  })
  
  observeEvent(input$confirm_removal,{
    
    if(!is.null(input$select_remove_flags)){
      points_to_remove <- qc_output$df %>%
        filter(code %in% input$select_remove_flags) 
      
      qc_output$df <- qc_output$df %>%
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
    
    #if(nrow(qc_output$df) > 0){
    
    write_csv(qc_output$df,
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
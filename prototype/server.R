
function(input, output) {
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  # Empty dataframe will hold points selected for QC and associated QC codes
  qc_output <- reactiveValues(df = tibble(timestamp = as.POSIXct(NA), 
                                          parameter = as.character(NA),
                                          code = as.character(NA), 
                                          .rows = 0))
  
  ## Sensor - Parameter UI logic
  sensor_parameters <- reactiveVal("")
  current_sensor <- reactiveVal(NA)
  
  output$parameter_qc <- renderUI({
    selectInput("parameter_qc", "Select a parameter to QC",
                sensor_parameters(), multiple = FALSE)
  })
  
  observeEvent(input$sensor_qc, {
    # Update parameters based on sensor selection
    sensor_parameters(
      sensor_parameters_df %>%
        filter(sensor %in% input$sensor_qc) %>%
        pull(parameter)
    )
  })
  
  ## Load Data ####
  # Action to take if run query button pressed
  observeEvent(input$loadData, {
    
    selected_file <- input$file

    # Read in CSV given filepath
    current_data$df <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/", selected_file)) 

    # Convert timestamp to POSIXct
    current_data$df <- current_data$df %>%
      select(-timestamp) %>%
      rename(timestamp = timestamp2) %>%
      mutate(timestamp = ymd_hms(timestamp),
             qc_tech_flag = FALSE)
    
    # Check if current filename has current annotations
    qc_output$df <- annotations_df %>%
      filter(file == selected_file) %>%
      bind_rows(qc_output$df)
    
    current_data$df <- current_data$df %>%
      mutate(qc_tech_flag = ifelse(timestamp %in% qc_output$df$timestamp,
                                   TRUE, qc_tech_flag))
  })
  
  ## Plots ####
  # Generate a plot of the data 
  output$plot_qc <- renderPlot({
    
    # Prevents error message from getting written to UI upon app startup
    tryCatch({
      
      sensor_numeric_flag <- unname(named_sensor_vector[input$sensor_qc])
      
      parameter_mean <- current_data$df %>%
        summarize(mean = mean(!!sym(input$parameter_qc), na.rm=T)) %>%
        pull(mean)
      
      current_data$df %>%
        select(timestamp, qc_tech_flag, all_of(sensor_numeric_flag), all_of(input$parameter_qc)) %>%
        mutate(numeric_label = case_when(
          qc_tech_flag ~ "QC Flag Applied",
          !!sym(sensor_numeric_flag) == "-5" ~ "Outside high range",
          !!sym(sensor_numeric_flag) == "-4" ~ "Outside low range",
          !!sym(sensor_numeric_flag) == "-3" ~ "Data rejected due to QAQC",
          !!sym(sensor_numeric_flag) == "-2" ~ "Missing Data",
          !!sym(sensor_numeric_flag) == "-1" ~ "Optional parameter, not collected",
          !!sym(sensor_numeric_flag) == "0" ~ "Passed initial QAQC check",
          !!sym(sensor_numeric_flag) == "1" ~ "Suspect Data",
          !!sym(sensor_numeric_flag) == "2" ~ "Reserved for Future Use",
          T ~ "Other Codes"
        )) %>%
        mutate(!!input$parameter_qc := case_when(
          !!sym(sensor_numeric_flag) == "-2" ~ parameter_mean,
          T ~ !!sym(input$parameter_qc)
        )) %>%
        ggplot(aes_string("timestamp", all_of(input$parameter_qc), color = "numeric_label")) +
        geom_point() +
        scale_color_manual(values = c("Outside high range" = "#67001f",
                                      "Outside low range" = "#b2182b",
                                      "Data rejected due to QAQC" = "#d6604d",
                                      "Missing Data" = "#f4a582",
                                      "Optional parameter, not collected" = "#fddbc7",
                                      "Passed initial QAQC check" = "grey", 
                                      "Suspect Data" = "#e08214", 
                                      "QC Flag Applied" = "#2166ac")) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
              legend.position = "top",
              legend.title=element_blank(), 
              text=element_text(size=18)) +
        ylab("")

      }, error = function(e){
    })
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
        coord_cartesian(xlim = ranges$x, expand = FALSE) +
        # facet_grid(variable ~ .) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
              text=element_text(size=18)) +
        ylab("")
      
    }, error = function(e){
      
    })
  })
  
  output$plot_facet <- renderPlot({
    # Plot allows techs to visualize 1 to all parameters without slowly moving from parameter to parameter on the main panel
    # Defaults to null until parameters are selected in UI
    if(!is.null(input$facet_parameters)){
      # Prevents error message from getting written to UI upon app startup
      tryCatch({
        current_data$df %>%
          select(timestamp, input$facet_parameters) %>%
          # melt the data to long form
          gather(key="variable", value = "measurement", -timestamp, na.rm = TRUE) %>%
          ggplot(aes(timestamp, measurement)) +
          geom_point() +
          # coord_cartesian(xlim = ranges$x, expand = FALSE) + #, ylim = ranges$y
          facet_grid(variable ~ ., scales = "free_y") +
          theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
          ylab("")
        
      }, error = function(e){
        
      })
    }
  })
  
  # Adapted from https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_dblclick, {
    # If the user has selected zoom, then the everything within a drawn box will be zoomed into
    # Or an existing zoom will be cancelled
    if(input$figure_functionality == "Zoom"){
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(as.POSIXct(brush$xmin, origin = "1970-01-01"),
                      as.POSIXct(brush$xmax, origin = "1970-01-01"))
        ranges$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    } else if(input$figure_functionality == "Select"){

    }

  })
  
  ## Datatable output ####
  output$table_selected_points <- renderDataTable({
    
    # By default, the table will show the timestamp, the selected QAQC parameter, and the QC numeric flag
    data_subset <- current_data$df %>%
      select(timestamp, input$parameter_qc, unname(named_sensor_vector[input$sensor_qc]))
    
    # If the brush functionality is "select", the table will be further subset to those points selected
    if(input$figure_functionality == "Select"){
      brush_subset <- brushedPoints(data_subset, input$plot_brush,
                                    yvar = input$parameter_qc) 
      datatable(brush_subset)
      
    } else {
      datatable(data_subset)
    }
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
    
    # If the brush functionality is "select", all selected QC codes will be associated with each selected point
    if(input$figure_functionality == "Select"){
      data_subset <- current_data$df %>%
        select(timestamp, input$parameter_qc)
      
      # Returns points under the brush
      brush_subset <- brushedPoints(data_subset, input$plot_brush,
                                    yvar = input$parameter_qc) 
      
      # Right now, bind WQ and MET codes together
      codes <- qc_flags %>%
        filter(select_inputs %in% c(input$wq_qc_flags, input$met_qc_flags)) %$%
        paste(unique(.$code), collapse = ",")
      
      #codes <- paste0(input$wq_qc_flags, input$met_qc_flags, sep= ",")
      
      qc_output$df <- brush_subset %>%
        mutate(# timestamp = as.character(timestamp),
               parameter = input$parameter_qc,
               sensor = input$sensor_qc,
               code = codes,
               file = input$file) %>%
        select(-input$parameter_qc) %>%
        separate_rows(code, sep=",") %>%
        bind_rows(qc_output$df)

      current_data$df <- current_data$df %>%
        mutate(qc_tech_flag = ifelse(timestamp %in% brush_subset$timestamp,
                                        TRUE, qc_tech_flag))
      
    } else {
      showModal(modalDialog(
        title = "Select data mode not activated",
        "QC flags will only be associated with data while the \"Select\" mode is active above. Move toggle from \"Zoom\" to \"Select\""
      ))
    }
  })
  
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
      
      current_data$df <- current_data$df %>%
        mutate(qc_tech_flag = ifelse(timestamp %in% points_to_remove$timestamp & !(timestamp %in% qc_output$df$timestamp),
                                     FALSE, qc_tech_flag))
    }
  })
  
  observeEvent(input$confirm_flags, {
    
    filename <- paste(input$tech_id, 
                      format(Sys.time(), "%Y%m%d-%H%M%OS"),
                      sep = "_")
    
    setwd(tempdir())
    
    if(nrow(qc_output$df) > 0){
      
      write_csv(qc_output$df,
                paste0(filename, ".csv"))
      
      drop_upload(paste0(filename, ".csv"),
                  path = "Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/")
      
      showModal(modalDialog(
        title = "Annotations saved", 
        div("Your annotations have been saved. To continue using this application without submitting the same annotations again, either refresh or remove all annotations."),
        
        easyClose = TRUE
      ))
      
    }
    
    setwd(original_wd)
    
  })
}
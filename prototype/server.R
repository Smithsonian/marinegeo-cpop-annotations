# shiny app demo showing SERC and STRI water quality and met data


function(input, output) {
  # Initiate empty object to hold imported data
  current_data <- reactiveValues(df=data.frame())
  # Empty dataframe will hold points selected for QC and associated QC codes
  qc_output <- reactiveValues(df = tibble(timestamp = as.character(NA),
                                          parameter = as.character(NA),
                                          code = as.character(NA), 
                                          .rows = 0))
  
  
  ## Load Data ####
  # Action to take if run query button pressed
  observeEvent(input$loadData, {
    
    # Create an object for the selected timestamp in the UI
    selected_time <- input$date_range

    # Get output path from key for given timestamp
    subset <- key %>%
      filter(full_timestamp == selected_time)

    # Some files may be duplicated in the directory
    # This will need to fixed outside of this application, but will use unique() for now
    filepath <- unique(subset$output)
    
    if(length(filepath) == 1){
      importFiles(filepath)
    } else {
      showModal(modalDialog(
        title = "Duplicate timestamp",
        "Multiple files exist for the current timestamp. Each file should represent a unique timestamp." 
      ))
    }
  })
  
  # Import files and append to reactive object
  importFiles <- function(filepath){
    
    # Read in CSV given filepath
    current_data$df <- drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/TEST_STRI/", filepath)) 
    
    # Convert timestamp to POSIXct
    current_data$df <- current_data$df %>%
      mutate(timestamp = ymd_hms(timestamp),
             qc_tech_flag = FALSE)
    
  }
  
  ## Plots ####
  # Generate a plot of the data 
  output$plot_qc <- renderPlot({
    
    # Prevents error message from getting written to UI upon app startup
    tryCatch({
    current_data$df %>%
      select(timestamp, qc_tech_flag, input$parameter_qc) %>%
      # melt the data to long form
      gather(key="variable", value = "measurement", -timestamp, -qc_tech_flag, na.rm = TRUE) %>%
      ggplot(aes(timestamp, measurement, color = qc_tech_flag)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      # #facet_grid(variable ~ .) +
      theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
            legend.position = "top") +
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
        coord_cartesian(xlim = ranges$x, expand = FALSE) + #, ylim = ranges$y
        # #facet_grid(variable ~ .) +
        theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
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
          facet_grid(variable ~ .) +
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
    
    # By default, the table will show the timestamp and the two parameters selected on the left panel
    data_subset <- current_data$df %>%
      select(timestamp, input$parameter_qc, input$parameter_reference)
    
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
        group_by(parameter, code) %>%
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
      
      brush_subset <- brushedPoints(data_subset, input$plot_brush,
                                    yvar = input$parameter_qc) 
      
      # Right now, bind WQ and MET codes together
      codes <- qc_flags %>%
        filter(select_inputs %in% c(input$wq_qc_flags, input$met_qc_flags)) %$%
        paste(unique(.$code), collapse = ",")
      
      #codes <- paste0(input$wq_qc_flags, input$met_qc_flags, sep= ",")
      
      qc_output$df <- brush_subset %>%
        mutate(timestamp = as.character(timestamp),
               parameter = input$parameter_qc,
               code = codes) %>%
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
        mutate(timestamp = ymd_hms(timestamp)) %>%
        filter(code %in% input$select_remove_flags) 
      
      qc_output$df <- qc_output$df %>%
        mutate(timestamp = ymd_hms(timestamp)) %>%
        filter(!(code %in% input$select_remove_flags))
      
      current_data$df <- current_data$df %>%
        mutate(qc_tech_flag = ifelse(timestamp %in% points_to_remove$timestamp & !(timestamp %in% qc_output$df$timestamp),
                                     FALSE, qc_tech_flag))
    }
  })
}
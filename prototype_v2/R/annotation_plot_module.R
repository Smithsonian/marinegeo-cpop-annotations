
# Provides UI elements for the select inputs that allow users to select sensors and parameters
annotation_controls_UI <- function(id){
  # the div id allows it's removal
  ns <- NS(id)
  
  div(id = id,

      selectInput(ns("sensor_qc"), "Select a sensor",
                 c("", names(sensor_vector_l1)), multiple = FALSE),

      # conditional dropdown for sensor parameters at selected site and sensor
      uiOutput(ns("parameter_qc"))
  )
}
      
      
annotation_controls_server <- function(id, current_data, qc_output, view_mode){
  
  moduleServer(id, function(input, output, session) {
    
    sensor_parameters <- reactiveVal(NA) # Holds parameters available for currently selected sensor in plot
    parameter_mean <- reactiveVal(0) # Mean of currently selected parameter to fill in for missing data
    formatted_sensor_name <- reactiveVal(NA)
    sensor_flag <- reactive(unname(sensor_vector_l1[input$sensor_qc])) # Name of sensor in QC columns 
    
    # # Update vector of sensor parameters for input and save the formatted version of the sensor name 
    observeEvent(input$sensor_qc, {

      # Update parameters based on sensor selection
      sensor_parameters(
        sensor_parameters_df %>%
          filter(sensor %in% input$sensor_qc,
                 parameter %in% colnames(current_data$df)) %>%
          pull(parameter)
      )
    })
    
    # ## UI objects for annotations ####
    output$parameter_qc <- renderUI({
      selectInput(session$ns("parameter_qc"), "Select a parameter",
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
    
    # Return dataframe containing timestamps and codes
    # Codes are collapsed to a single row for plotting
    code_timestamps <- reactive({
      # Determine if observations for current sensor have been annotated
      qc_output$codes %>%
        filter(sensor == sensor_flag()) %>%
        group_by(ID) %>%
        summarize(code = paste(code, collapse = ", "))
    })

    # Return dataframe containing timestamps and flags
    # There should only be one flag per timestamp
    flag_timestamps <- reactive({
      qc_output$flags %>%
        filter(sensor == sensor_flag()) %>%
        select(-sensor)
    })
    
    reactive({

      if(is.null(input$parameter_qc)){
        return(NULL)
      } else if(input$parameter_qc == ""){
        return(NULL)
      }
      
      df <- current_data$df %>%
        select(timestamp, ID, all_of(input$parameter_qc)) %>%
        merge(flag_timestamps(), by="ID", all.x=TRUE) %>%
        merge(code_timestamps(), by="ID", all.x=TRUE) %>%
        # Provide values to missing data
        mutate(!!input$parameter_qc := case_when(
          flag == "Missing Data" ~ parameter_mean(),
          T ~ .data[[input$parameter_qc]]
        )) %>%
        mutate(code = case_when(
          is.na(code) ~ "No code applied",
          T ~ code
        )) %>%
        mutate(code = as.factor(code),
               flag = as.factor(flag))

      if(view_mode() == "Flags that require review"){
        df <- df %>%
          filter(status == "Not evaluated")

      } else if(view_mode() == "Only accepted flags"){
        df <- df %>%
          filter(status == "Approved")

      } else if(view_mode() == "Only rejected flags"){
        df <- df %>%
          filter(status == "Revised")

      } else if(view_mode() == "Points that require codes"){
        df <- df %>%
          filter(code == "No code applied")
      }
      
      return(df)
    })
  })
}
  
annotation_plot_UI <- function(id){
  # the div id allows it's removal
  ns <- NS(id)
  
  div(id = id,
      #plotlyOutput(ns("plot_qc"), height = "600px")
      uiOutput(ns("plot_window"))
  )
}

annotation_plot_server <- function(id, plotting_data, label_type, start_date, date_range_max){
  
  moduleServer(id, function(input, output, session) {
    
    output$plot_window <- renderUI({
      plotlyOutput(session$ns("plot_qc"), height = getPlotHeight())
    })
    
    getPlotHeight <- reactive({
      if(length(plotting_data()) > 1){
        return("700px")
      } else { return("400px")}
    })
    
    getPlotLabels <- reactive({
      
      label_list <- lapply(plotting_data(), function(i){
        unique(unlist(i[label_type()]))
      })
      
      raw_labels <- unique(unlist(label_list))
      
      labels <- unname(factor(raw_labels, levels = c(as.character(raw_labels))))
      
    })
    
    output$plot_qc <- renderPlotly({
      
      plot_objects <- lapply(plotting_data(), function(i){
        
        plotted_parameter <- colnames(i)[3]
        
        plot <- i %>%
          mutate(!!label_type() := factor(.data[[label_type()]], levels = as.character(getPlotLabels())))
        
        plot_ly(plot, x = ~timestamp, y = ~get(plotted_parameter),
                color = ~get(label_type()), # Format Codes or Flags to code or flag, respectively
                key=~ID, type = "scatter", legendgroup = ~get(label_type()), showlegend = F) %>%
          layout(legend = list(orientation = 'h'), # https://plotly.com/python/reference/layout/#layout-legend
                 #y = -.6),
                 yaxis = list(title = plotted_parameter),
                 xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
                              range = c(start_date(), as.Date(date_range_max())))) %>%
          toWebGL()
      })
      
      if(length(plot_objects) > 0){
        
        dummy_df <- setNames(data.frame(col1 = getPlotLabels()), label_type())
        dummy_df$x <- plotting_data()[[1]]$timestamp[1]
        dummy_df$y <- unlist(plotting_data()[[1]][colnames(plotting_data()[[1]])[3]])[1]
        dummy_df$ID <- plotting_data()[[1]]$ID[1]
        
        plot_objects[[1]] <- plot_objects[[1]] %>%
          add_trace(data = dummy_df, x = ~x, y = ~y, color = ~get(label_type()), type = "scatter",
                    showlegend = TRUE, legendgroup = ~get(label_type()), hoverinfo = 'none')
        
        if(length(plot_objects) > 1){
          subplot(plot_objects, nrows = 2, shareX = T, titleY = TRUE)
        } else {
          plot_objects[[1]]
        }
        
      }
    })
  })
}


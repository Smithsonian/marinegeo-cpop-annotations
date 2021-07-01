
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
      
      
annotation_controls_server <- function(id, current_data, qc_output){
  
  moduleServer(id, function(input, output, session) {
    
    sensor_parameters <- reactiveVal(NA) # Holds parameters available for currently selected sensor in plot
    parameter_mean <- reactiveVal(0) # Mean of currently selected parameter to fill in for missing data
    formatted_sensor_name <- reactiveVal(NA)
    #sensor_flag <- reactive(unname(sensor_vector_l1[input$sensor_qc])) # Name of sensor in QC columns 
    
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
        filter(parameter == input$parameter_qc) %>%
        mutate(code = case_when(
          is.na(comment_code) ~ main_code,
          is.na(main_code) ~ comment_code,
          T ~ paste(main_code, comment_code, sep = ", ")
        )) %>%
        select(-c(main_code_modified, comment_code_modified, parameter, code_id, site_code, deployment_id))
        #group_by(id) %>%
        #summarize(code = paste(code, collapse = ", "))
    })

    # Return dataframe containing timestamps and flags
    # There should only be one flag per timestamp
    flag_timestamps <- reactive({
      qc_output$flags %>%
        filter(parameter == paste0(input$parameter_qc, "_f")) %>%
        select(-parameter, -modified, -site_code, -deployment_id, -observation_id)
    })
    
    reactive({
      
      if(is.null(input$parameter_qc)){
        return(NULL)
      } else if(input$parameter_qc == ""){
        return(NULL)
      }
      
      df <- current_data$df %>%
        select(timestamp, observation_id, all_of(input$parameter_qc)) %>%
        merge(flag_timestamps(), by="timestamp", all.x=TRUE) %>%
        merge(code_timestamps(), by="timestamp", all.x=TRUE) %>%
        # Provide values to missing data
        mutate(!!input$parameter_qc := case_when(
          flag == -2 ~ parameter_mean(),
          T ~ .data[[input$parameter_qc]]
        )) %>%
        mutate(code = case_when(
          is.na(code) &
            flag %in% c(-5, -4, -3, -2, 1) ~ "Code required",
          is.na(code) ~ "Code not required",
          T ~ code
        )) %>%
        mutate(flag = as.character(flag),
               code = as.character(code)) %>%
        mutate(
          flag = case_when(
            is.na(flag) ~ "Ref",
            T ~ flag
          ),
          code = case_when(
            flag == "Ref" ~ "Ref",
            T ~ code
          )
        ) %>%
        mutate(code = as.factor(code),
               flag = as.factor(flag)) 

        

      # if(view_mode() == "Flags that require review"){
      #   df <- df %>%
      #     filter(flag %in% c(-4, -5))
      # 
      # } else if(view_mode() == "Points that require codes"){
      #   df <- df %>%
      #     filter(code == "Code required")
      # }
      
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

annotation_plot_server <- function(id, plotting_data, label_type, start_date, date_range_max, reset_plot_status, color_dictionary_flags){

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

      return(labels)
    })

    inactive_label <- reactive({
      if(label_type() == "flag"){
        return("code")
      } else return("flag")
    })

    getPlotColors <- reactive({

      # Only return color matches if the label type is flag 
      # codes are generated on the fly for the legend (main and comment codes are pasted together) and cannot be predetermined
      if(label_type() == "flag"){
        color_subset <- color_dictionary_flags[names(color_dictionary_flags) %in% as.character(getPlotLabels())]
        color_subset_ordered <- color_subset[order(factor(names(color_subset), levels = as.character(getPlotLabels())))]
        
        return(color_subset_ordered)
        
      } else{
        return(NULL)
      }
    })

    output$plot_qc <- renderPlotly({

      reset_plot_status()

      # Create a plot for each df in the plotting data list
      plot_objects <- lapply(plotting_data(), function(i){

        plotted_parameter <- colnames(i)[3]

        # Modifies column, either flag or code, depending on label type input
        # Transforms to a leveled factor, alphabetical
        plot <- i %>%
          mutate(!!label_type() := factor(.data[[label_type()]], levels = as.character(getPlotLabels())))

        plot_ly(plot, x = ~timestamp, y = ~get(plotted_parameter),
                color = ~get(label_type()), # Format Codes or Flags to code or flag, respectively
                colors = as.character(getPlotColors()),
                hovertext = ~get(inactive_label()),
                hover = 'text',
                key=~plot_id, unselected = list(marker = list(opacity = .75)), type = "scatter",
                legendgroup = ~get(label_type())) %>% #,  showlegend = T) %>%
          config(displaylogo = FALSE, displayModeBar = TRUE) %>%
          layout(legend = list(orientation = 'h'), # https://plotly.com/python/reference/layout/#layout-legend
                 showlegend = T,
                 dragmode = "select",
                 yaxis = list(title = plotted_parameter),
                 xaxis = list(title = "", # https://plotly.com/python/reference/layout/xaxis/
                              range = c(start_date(), as.Date(date_range_max())))) %>%
          toWebGL()
      })
      
      # if(nrow(reference_data$df) > 0 & length(plotting_data()) > 0){
      #   plotted_parameter <- colnames(plotting_data()[[1]])[3]
      #   
      #   plot_objects <- lapply(plot_objects, function(i){
      #     
      #     i %>%
      #       add_trace(data = reference_data$df,
      #                 x = ~timestamp, y = ~get(plotted_parameter), type = "scatter")
      #     
      #   })
      #     
      # }
      
      if(length(plot_objects) > 0){

        if(length(plot_objects) > 1){
          subplot(plot_objects, nrows = 2, shareX = T, titleY = TRUE, margin = .1)

        } else {
          plot_objects[[1]]
        }

      }
    })

    reactive({
      return(event_data("plotly_selected"))
    })

  })
}


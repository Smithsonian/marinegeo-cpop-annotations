# shiny app demo showing SERC and STRI water quality and met data


function(input, output) {
  
  # builds ui dropdown selctor for the sensor variables from all the valid dataframe column names
  output$parameterSelector = renderUI({
    if(input$sensor == "none selected"){
      return(NULL)
    } else{
      # select all the column names in csv that are not in ignore list
      parameter_list <- colnames(getSensorData())[!names(getSensorData()) %in% ignore_list]
      # create the select input UI element with only the relevant parameters listed
      # the selected argument refers back to itself in order to remember what was selected if and when 
      # the input has to reactively update to another changing input (such as time frame) 

      selectInput('parameter', 'Parameter', choices=parameter_list, 
                  selected = input$parameter, 
                  multiple = TRUE)
      
    }
  })
  
  # returns the current subset of data based on site(s) and sensor
  getSensorData <- reactive ({
    # filter by sites if one or more have been chosen 
    if(!is.null(input$sites)){
      get(gsub(" ", "", input$sensor)) %>%
      filter(Site %in% input$site) %>%
      #10.3 seconds
      #filter(Timestamp > max(Timestamp) - hours(input$timeSpan))
      filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
      
      # otherwise return the entire dataset of the selected sensor
    } else {
      get(gsub(" ", "", input$sensor)) %>%
        #filter(Timestamp > max(Timestamp) - hours(input$timeSpan)) 
        filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
    }

  })
  
  # Generate a plot of the data 
  output$plot <- renderPlot({
    if(is.null(input$parameter) | input$sensor == "none selected"){return(NULL)}
    
    # Generates a facet grid plot for one to many parameters
    getSensorData() %>%
      select(Timestamp, Site, input$parameter) %>%
      # melt the data to long form
      gather(key="variable", value = "measurement", -Timestamp, -Site, na.rm = TRUE) %>%
      ggplot(aes(Timestamp, measurement, color= Site)) + geom_point() + 
      facet_grid(variable ~ .) + ylab("")
    
  })
  
}
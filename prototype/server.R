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
      selectInput('parameter', 'Parameter', choices=parameter_list, multiple = TRUE)
    }
  })
  
  # returns the current subset of data based on site(s) and sensor
  getSensorData <- reactive ({
    # filter by sites if one or more have been chosen 
    if(!is.null(input$sites)){
      get(gsub(" ", "", input$sensor)) %>%
        filter(Site %in% input$site) %>%
        filter(Timestamp > max(Timestamp) - hours(input$timeSpan))
      # otherwise return the entire dataset of the selected sensor
    } else {
      get(gsub(" ", "", input$sensor)) %>%
        filter(Timestamp > max(Timestamp) - hours(input$timeSpan)) 
    }
    
  })
  
  # Generate a plot of the data 
  output$plot <- renderPlot({
    if(is.null(input$parameter)){return(NULL)}
    
    # If there's only one parameter chosen generate a single plot
    if(length(input$parameter) == 1){
      getSensorData() %>%
        ggplot(aes_string(x="Timestamp", y=input$parameter, color="Site")) + geom_point()
      
    # if multiple parameters are chosen generate a facet plot
    } else {
      getSensorData() %>%
        select(Timestamp, Site, input$parameter) %>%
        # melt the data to long form 
        gather(key="variable", value = "measurement", -Timestamp, -Site, na.rm = TRUE) %>%
        ggplot(aes(Timestamp, measurement, color= Site)) + geom_point() + facet_grid(variable ~ .)
    }
    
    
    
    # getSensorData() %>%
    #   ggplot(aes_string(x="Timestamp", y=input$parameter, colour=input$parameter))+
    #   geom_point(aes_string(x="Timestamp", y=input$parameter))+
    #   #ylab(getLabel(input$parameter, sensorAttributeLookup(input$sensor, units)))+
    #   scale_colour_gradientn(colours = palette(c("black","dark blue","blue", "royalblue2", "skyblue3")))+
    #   scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")+
    #   theme_bw() +
    #   theme(panel.border = element_blank(),
    #         panel.grid.major = element_blank(),
    #         plot.title = element_text(hjust = 0.5),
    #         panel.grid.minor = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.title.y = element_text(size=16),
    #         axis.text = element_text(size=14),
    #         legend.position="none", # position of legend or none
    #         legend.direction="horizontal", # orientation of legend
    #         legend.title= element_blank(), # no title for legend
    #         legend.key.size = unit(0.5, "cm"), # size of legend
    #         axis.line.x = element_line(color="black", size = 1),
    #         axis.line.y = element_line(color="black", size = 1))
  })
  #   
#   
#   # load dataset from external source
#   # returns a single sensor dataset when the site and sensor selectors are altered
#   sensorData <- reactive({
#     if(is.null(input$sensor)){return(NULL)} # check that the sensor has a valid value selected
#     cat(file=stderr(), "Switching to", input$sensor, "\n") # log to the console the new sensor type
#     d <- loadSensorCSV(sensorAttributeLookup(input$sensor, path), sensorAttributeLookup(input$sensor, na))
#     return(d)
#   })
#   
#   # Helper function to filter the sensor dataset to the time of interest
#   # function filter dataset to time period (number hours) from the most recent timestamp value
#   sensorDataTimePeriod <- reactive({
#     d <- sensorData() %>% 
#       mutate(Timestamp = mdy_hm(Timestamp)) %>%
#       filter(Timestamp > max(Timestamp)-hours(input$timeSpan))
# 
#     return(d)
#   })
#   
#   # Generate a plot of the data ----
#   output$singleParamPlot <- renderPlot({
#     if(all(!input$parameter %in% names(sensorData()))){return(NULL)}
#     
#     sensorDataTimePeriod() %>%
#       ggplot(aes_string(x="Timestamp", y=input$parameter, colour=input$parameter))+
#       geom_point(aes_string(x="Timestamp", y=input$parameter))+
#       ylab(getLabel(input$parameter, sensorAttributeLookup(input$sensor, units)))+
#       scale_colour_gradientn(colours = palette(c("black","dark blue","blue", "royalblue2", "skyblue3")))+
#       scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")+
#       theme_bw() +
#       theme(panel.border = element_blank(),
#             panel.grid.major = element_blank(),
#             plot.title = element_text(hjust = 0.5),
#             panel.grid.minor = element_blank(),
#             axis.title.x = element_blank(),
#             axis.title.y = element_text(size=16),
#             axis.text = element_text(size=14),
#             legend.position="none", # position of legend or none
#             legend.direction="horizontal", # orientation of legend
#             legend.title= element_blank(), # no title for legend
#             legend.key.size = unit(0.5, "cm"), # size of legend
#             axis.line.x = element_line(color="black", size = 1),
#             axis.line.y = element_line(color="black", size = 1))
#   })
#   
#   # reactive function return the lastest record
#   latestRecord <- reactive({
#     if(is.null(sensorData())){return(NULL)}
#     d <- sensorData() %>% arrange(Timestamp) %>% tail(1)
#     return(d)
#   })
#   
#   
#   # render text with the data from the latest record collected
#   output$latestTime <- renderUI({
#     r <- latestRecord()
#     str1 <- paste("<h5>", "Last Updated:", r$Timestamp, "</h5>")
#     HTML(str1)
#   })
#   
#   # Download sensor dataset for filter time period
#   
#   # # function to get the min/max dates of the filtered dataset
#   # TimePeriodMinMax <- reactive({
#   #   minDate <- sensorDataTimePeriod() %>% pull(Timestamp) %>% min()
#   #   maxDate <-sensorDataTimePeriod() %>% pull(Timestamp) %>% max()
#   #   return(c(minDate, maxDate))
#   # })
#   
# 
#   # # Downloadable csv of selected dataset for the desired time period ----
#   # output$downloadData <- downloadHandler(
#   #   filename = function() {
#   #     lower <- format(TimePeriodMinMax()[1], "%Y%m%d%H%M")
#   #     upper <- format(TimePeriodMinMax()[2], "%Y%m%d%H%M")
#   #     paste(input$sensor, "_", lower, "-", upper, ".csv", sep = "")
#   #   },
#   #   content = function(file) {
#   #     write.csv(sensorDataTimePeriod(), file, row.names = FALSE)
#   #   }
#   # )
  
  
}
# shiny app demo showing SERC and STRI water quality and met data


function(input, output) {
  # Turn key into a reactive dataframe to monitor which datasets have been imported
  data_tracker <- reactiveValues(df = key)
  
  # Initiate empty objects that will store each sensor type's data as they're imported
  MET_data <- reactiveValues(df = data.frame())
  WaterLevel_data <- reactiveValues(df = data.frame())
  WaterQuality_data <- reactiveValues(df = data.frame())
  
  # builds ui dropdown selctor for the sensor variables from all the valid dataframe column names
  output$parameterSelector = renderUI({
    
    if(input$sensor == "none selected" | is.null(input$site)){
      
      div(
        "Select a site and sensor to view available parameters",
      
        selectInput('parameter', 'Parameter', choices=c(), 
                   multiple = TRUE)
        )
      
    } else{

      # select all the column names in csv that are not in ignore list
      # parameter_list <- colnames(getSensorData())[!names(getSensorData()) %in% ignore_list]
      parameter_list <- getParameters()
      
      # create the select input UI element with only the relevant parameters listed
      # the selected argument refers back to itself in order to remember what was selected if and when 
      # the input has to reactively update to another changing input (such as time frame) 
      
      selectInput('parameter', 'Parameter', choices=parameter_list, 
                  selected = input$parameter, 
                  multiple = TRUE)
      
    }
  })
  
  # Obtains parameters only present for selected sensors
  # Provides parameters selectInput options
  getParameters <- function(){
    current_parameters <- parameters %>%
      filter(sensor %in% tolower(gsub(" ", "_", input$sensor))) %>%
      filter(!(cols %in% ignore_list))
    
    return(unique(current_parameters$cols))
  }
  
  # Action to take if run query button pressed
  observeEvent(input$runQuery, {
    
    year_start <- year(input$date_range[1])
    year_end <- year(input$date_range[2])
    
    subset <- data_tracker$df %>%
      filter(sensor %in% input$sensor) %>%
      filter(site %in% input$site) %>%
      filter(year >= year_start & year <= year_end)
    
    # if any files are not yet imported
    if(nrow(subset) > 0 & !all(subset$imported)) {
      importFiles(subset)
    }
    
  })
  
  # Import files and append to the correct sensor dataframe
  importFiles <- function(subset){
    
    # Filter to the list of files that need to be imported 
    import_subset <- filter(subset, imported == FALSE)
    
    # If there's only one CSV that will need to be imported
    if(nrow(import_subset) == 1) {
      # Paste together directory and filepath
      data <- fread(paste0("./data/", import_subset$filepath)) %>%
        mutate(Timestamp = mdy_hm(Timestamp))
      
      # if there are multiple CSVs that need to be imported
    } else if(nrow(import_subset) > 1){
      data <- rbindlist(lapply(paste0("./data/", import_subset$filepath), fread), fill=TRUE) %>%
        mutate(Timestamp = mdy_hm(Timestamp))
    }
    
    # Update data tracker to indicate that certain files have been imported and will not need to be re-imported
    data_tracker$df <- mutate(data_tracker$df, 
                              imported = ifelse(filepath %in% import_subset$filepath, TRUE, imported))
    
    # Attach the data to the correct dataframe based on the selected sensor
    # If no data has been imported for a given sensor, replace the reactiveValues object
    # If there is data, append it
    if(first(subset$sensor) == "MET"){
      if(nrow(MET_data$df) == 0) {
        MET_data$df <- data
      } else   MET_data$df <- bind_rows(MET_data$df, data)
        
    } else if(first(subset$sensor) == "Water Quality"){
      if(nrow(WaterQuality_data$df) == 0) {
        WaterQuality_data$df <- data
      } else   WaterQuality_data$df <- bind_rows(WaterQuality_data$df, data)
      
    } else if (first(subset$sensor) == "Water Level") {
      if(nrow(WaterLevel_data$df) == 0) {
        WaterLevel_data$df <- data
      } else   WaterLevel_data$df <- bind_rows(WaterLevel_data$df, data)
    }
  }
  
  # returns the current subset of data based on site(s) and sensor
  getSensorData <- function(){
    if(input$sensor == "MET"){
      MET_data$df %>%
        filter(Site %in% input$site) %>%
        filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
      
    } else if(input$sensor == "Water Quality"){
      WaterQuality_data$df %>%
        filter(Site %in% input$site) %>%
        filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
      
    } else if (input$sensor == "Water Level") {
      WaterLevel_data$df %>%
        filter(Site %in% input$site) %>%
        filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
    }
    
    # Previous code, I'd like to return to but reactiveValues do not work with get() apparently
    # Subset currently selected sensor data by site and time selections
    #get(paste0(gsub(" ", "", input$sensor),"_data$df")) %>%
      # filter(Site %in% input$site) %>%
      # filter(Timestamp > ymd(input$date_range[1]) & Timestamp < ymd(input$date_range[2]))
    #}

  }
  
  # Generate a plot of the data 
  output$plot <- renderPlot({
    #if(is.null(input$parameter) | input$sensor == "none selected"){return(NULL)}
    if(input$runQuery == 0){return(NULL)}
    
    # Trigger plot creation when run query button is clicked
    input$runQuery
    
    # Isolate prevents graph from updating whenever other inputs change
    isolate({
      # Generates a facet grid plot for one to many parameters
      getSensorData() %>%
        select(Timestamp, Site, input$parameter) %>%
        # melt the data to long form
        gather(key="variable", value = "measurement", -Timestamp, -Site, na.rm = TRUE) %>%
        ggplot(aes(Timestamp, measurement, color = Site)) + 
        geom_line() + 
        facet_grid(variable ~ .) + 
        theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
        ylab("") 
    })
  }, height = function(){
    input$GetScreenHeight * .6
  })
  
  # Download handler creates filename and prepares data for download
  output$download <- downloadHandler(
    filename = function(){
      paste0("marinegeo_cpop-", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(getSensorData(), file)
    }
  )
}
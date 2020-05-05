## Functions used in the CPOP portal shiny app

# Obtains parameters only present for selected sensors
# Provides parameters selectInput options
getParameters <- function(){
  current_parameters <- parameters %>%
    filter(sensor %in% tolower(gsub(" ", "_", input$sensor))) %>%
    filter(!(cols %in% ignore_list))
  
  return(unique(current_parameters$cols))
}

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


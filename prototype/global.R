# shiny app demo showing SERC and STRI water quality, water level, and met data
# Global script runs first when initializing application
# Packages and data are loaded 

library(shinydashboard)
library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)
library(DT)

# The key is an inventory of available CSVs that can be imported
# and includes year - sensor - status (QAQC or RAW) - site information based on filepaths
key <- as.data.frame(list(list.files("./data/")), col.names = "filepath") %>%
  filter(filepath != "headers.csv") %>%
  mutate(name = gsub(".csv", "", filepath),
         # Imported tracks whether the application has imported particular datasets
         imported = FALSE) %>%
  # Separate file name into columns
  separate(name, into = c("year", "sensor", "status", "site"), sep = "_") %>%
  mutate(sensor = gsub("([a-z])([A-Z])", "\\1 \\2", sensor, perl = TRUE)) %>%
  filter(status == "QAQC")

# initiate a blank data frame
dataset <- data.frame()

# had to specify columns to get rid of the total column
for (i in 1:nrow(key)){
  
  #read in files using the fread function from the data.table package
  temp_data <- fread(paste0("./data/", key$filepath[i]), 
                     stringsAsFactors = F) %>%
    mutate(Sensor = key$sensor[i],
           # Year = year(strptime(Timestamp, format = "%m/%d/%Y %H:%M")),
           # Month = month(strptime(Timestamp, format = "%m/%d/%Y %H:%M")),
           # Day = day(strptime(Timestamp, format = "%m/%d/%Y %H:%M")),
           Date = as.Date(strptime(Timestamp, format = "%m/%d/%Y %H:%M")),
           Year_Month = floor_date(Date, unit = "month")) %>%
    select(Site, Sensor, Year_Month) %>%
    distinct()
  
  # for each iteration, bind the new data to the building dataset
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) 
}

# list of parameters for available sensors
parameters <- fread("./data/headers.csv")
# Parameters that shouldn't be selectable by the user
ignore_list <- c("Timestamp", "Site", "Status")


jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

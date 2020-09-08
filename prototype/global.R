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
library(rdrop2)
library(DT)

# Load dropbox token
drop_auth(rdstoken = "droptoken.rds")

# Get filenames to populate UI for now
bundled_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/") %>%
  pull(name)

# key <- drop_read_csv("Marine_GEO_CPOP_PROCESSING/TEST_STRI/STRIdeploymentrecord.csv") %>%
#   mutate(full_timestamp = paste(as.character(startstamp), as.character(stopstamp), sep = "-"))

## For demo ##
#key <- tibble(full_timestamp = "2019-10-29 07:54:00 - 2019-11-19 13:48:00")
sensor_parameters_df <- read_csv("./data/sensor_parameters.csv")

qc_flags <- read_csv("./data/qc_technician_codes.csv") %>%
  mutate(select_inputs = paste(code, description, sep=" - "))

wq_qc_flags <- qc_flags %>%
  filter(category == "water quality") 

met_qc_flags <- qc_flags %>%
  filter(category == "met") 

# The key is an inventory of available CSVs that can be imported
# and includes year - sensor - status (QAQC or RAW) - site information based on filepaths
# key <- as.data.frame(list(list.files("./data/")), col.names = "filepath") %>%
#   filter(filepath != "headers.csv") %>%
#   mutate(name = gsub(".csv", "", filepath),
#          # Imported tracks whether the application has imported particular datasets
#          imported = FALSE) %>%
#   # Separate file name into columns
#   separate(name, into = c("year", "sensor", "status", "site"), sep = "_") %>%
#   mutate(sensor = gsub("([a-z])([A-Z])", "\\1 \\2", sensor, perl = TRUE)) %>%
#   filter(status == "QAQC")

named_sensor_vector <- c("Turbidity" = "Turbidity_0",
                         "Conductivity" = "Conductivity_Temp_0",
                         "Optical Dissolved Oxygen" = "Optical_DO_0")

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Get filenames of annotations
annotation_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/") %>%
  pull(name)

annotations_df <- data.frame()

for(filename in annotation_directory){
  annotations_df <- annotations_df %>%
    bind_rows(drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/",
                         filename)))
  
}

annotations_df <- annotations_df %>%
  mutate(timestamp = ymd_hms(timestamp))

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# list of parameters for available sensors
## For demo ####
# # Changed Temp_.C to Temp_C
# parameters <- c("Battery_V",	"Temp_C",	"Cond_µS_cm",	"SpCond_µS_cm",	"Sal_psu",	
# "nLF_Cond_µS_cm",	"TDS_mg_L",	"ODO__sat",	"ODO__local", "ODO_mg_L",	"pH",	"pH_mV",	"Turbidity_FNU",	"TSS_mg_L",	"Chlorophyll_RFU",	"Chlorophyll_ug_L",
# "BGA_PE_RFU",	"BGA_PE_ug_L",	"fDOM_RFU",	"fDOM_QSU",	"Depth_m")
# 


jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

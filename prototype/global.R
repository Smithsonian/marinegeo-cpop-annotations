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

# key <- drop_read_csv("Marine_GEO_CPOP_PROCESSING/TEST_STRI/STRIdeploymentrecord.csv") %>%
#   mutate(full_timestamp = paste(as.character(startstamp), as.character(stopstamp), sep = "-"))

## For demo ##
key <- tibble(full_timestamp = "2019-10-29 07:54:00 - 2019-11-19 13:48:00")
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

# list of parameters for available sensors
## For demo ####
# Changed Temp_.C to Temp_C
parameters <- c("Battery_V",	"Temp_C",	"Cond_µS_cm",	"SpCond_µS_cm",	"Sal_psu",	
"nLF_Cond_µS_cm",	"TDS_mg_L",	"ODO__sat",	"ODO__local", "ODO_mg_L",	"pH",	"pH_mV",	"Turbidity_FNU",	"TSS_mg_L",	"Chlorophyll_RFU",	"Chlorophyll_ug_L",
"BGA_PE_RFU",	"BGA_PE_ug_L",	"fDOM_RFU",	"fDOM_QSU",	"Depth_m")

df <- read.csv("./data/QAQC_example.csv") %>%
  select(timestamp, any_of(parameters), Turbidity_0, Conductivity_Temp_0, Optical_DO_0) %>%
  mutate(Turbidity_0 = as.character(Turbidity_0),
         Conductivity_Temp_0 = as.character(Conductivity_Temp_0),
         Optical_DO_0 = as.character(Optical_DO_0)) %>%
  mutate(Turbidity_0 = case_when(
    Turbidity_0 == "0" ~ "Passed Checks",
    Turbidity_0 == "1" ~ "Suspect Data"
  )) %>%
  mutate(Conductivity_Temp_0 = case_when(
    Conductivity_Temp_0 == "0" ~ "Passed Checks",
    Conductivity_Temp_0 == "1" ~ "Suspect Data"
  )) %>%
  mutate(Optical_DO_0 = case_when(
    Optical_DO_0 == "0" ~ "Passed Checks",
    Optical_DO_0 == "1" ~ "Suspect Data"
  )) %>%
  filter(Turbidity_FNU < 1000)

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

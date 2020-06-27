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
library(rdrop2)
library(DT)

# Load dropbox token
drop_auth(rdstoken = "droptoken.rds")

key <- drop_read_csv("Marine_GEO_CPOP_PROCESSING/TEST_STRI/STRIdeploymentrecord.csv") %>%
  mutate(full_timestamp = paste(as.character(startstamp), as.character(stopstamp), sep = "-"))

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
parameters <- c("Fault_Code",	"Battery_V",	"Cable_Pwr_V",	"Temp_.C",	"Cond_µS_cm",	"SpCond_µS_cm",	"Sal_psu",	
"nLF_Cond_µS_cm",	"TDS_mg_L",	"ODO_._sat",	"ODO_mg_L",	"pH",	"pH_mV",	"Turbidity_FNU",	"TSS_mg_L",	"Chlorophyll_RFU",	"Chlorophyll_µg_L",
"BGA.PE_RFU",	"BGA.PE_µg_L",	"fDOM_RFU",	"fDOM_QSU",	"Press_psi_a",	"Depth_m")

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

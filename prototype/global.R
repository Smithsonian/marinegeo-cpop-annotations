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
drop_auth(rdstoken = "./droptoken.rds")

# Get filenames to populate UI for now
bundled_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/QAQC_dir/") %>%
  pull(name)

key <- as_tibble(bundled_directory) %>%
  rename(Filename = value) %>%
  separate(Filename, into = c("Site", "File Type", "Date", "Status"), sep = "_", remove = FALSE) %>%
  mutate(Status = gsub(".csv", "", Status)) %>%
  select(everything(), Filename) %>%
  arrange(Date)

sensor_parameters_df <- read_csv("./data/sensor_parameters.csv")

qc_flags <- read_csv("./data/qc_technician_codes.csv") %>%
  mutate(select_inputs = paste(code, description, sep=" - "))

wq_qc_flags <- qc_flags %>%
  filter(category == "water quality") 

met_qc_flags <- qc_flags %>%
  filter(category == "met") 

sensor_vector_l1 <- c("Turbidity" = "Turbidity_0",
                      "Conductivity" = "Conductivity_Temp_0",
                      "Optical Dissolved Oxygen" = "Optical_DO_0",
                      "Depth" = "Depth_0",
                      "Fluorescent Dissolved Organic Matter" = "fDOM_0",
                      "Wiper" = "Wiper_0",
                      "pH" = "pH_0",
                      "EXO2 Sonde" = "EXO2_Sonde_0",
                      "Total Algae" = "Total_Algae_BGA_PE_0")

sensor_vector_l2 <- paste0(sensor_vector_l1, "C")

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Get filenames of annotations
annotation_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/") 

# If there are no annotations in the directory a data frame with no rows or columns is returned. 
# The app must have a df with a column "name"
if(nrow(annotation_directory) == 0){
  annotation_directory <- tibble(filename = NA_character_,
                                 name = NA_character_,
                                 technician_code = NA_character_,
                                 .rows = 0)
  # annotation_files <- annotation_directory %>%
  #   pull(name)
  # 
  # annotations_df <- data.frame()
  # 
  # for(filename in annotation_files){
  #   annotations_df <- annotations_df %>%
  #     bind_rows(drop_read_csv(paste0("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/",
  #                                    filename)))
  #   
  # }
  # 
  # annotations_df <- annotations_df %>%
  #   mutate(timestamp = ymd_hms(timestamp))
} else {
  annotation_directory <- annotation_directory %>%
    select(name) %>%
    mutate(filename = gsub("-L2", "", name))
    # separate(name, into = c("filename", "technician_code"), sep = "-", remove = FALSE) %>%
    # mutate(filename = paste0(filename, ".csv"),
    #        technician_code = gsub(".csv", "", technician_code))
}

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

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
library(tibble)
library(data.table)
library(rdrop2)
library(DT)
library(plotly)
library(shinyjs)

# Load dropbox token
drop_auth(rdstoken = "./droptoken.rds")

#introduction_text <- read_file("./prototype_v2/data/intro.txt")
introduction_text <- read_file("./data/intro.txt")

# Get filenames to populate UI for now
bundled_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/L1_DATA_FLAGS/") %>%
  pull(name)

key <- as_tibble(bundled_directory) %>%
  rename(Filename = value) %>%
  separate(Filename, into = c("Site", "File Type", "Date", "Status"), sep = "_", remove = FALSE) %>%
  mutate(Status = gsub(".csv", "", Status)) %>%
  select(everything(), Filename) %>%
  filter(Status == "L1-data") %>%
  arrange(Date)

sensor_parameters_df <- read_csv("./data/sensor_parameters.csv")

qc_codes <- read_csv("./data/qc_technician_codes.csv") %>%
  mutate(select_inputs = paste(code, description, sep=" - ")) %>%
  filter(category == "water quality") %>%
  mutate(code_type = case_when(
    substr(code, 1, 1) == "C" ~ "comment",
    substr(code, 1, 1) == "S" ~ "sensor",
    substr(code, 1, 1) == "G" ~ "general",
    T ~ NA_character_
  ))

qc_code_descriptions <- qc_codes$description
names(qc_code_descriptions) <- qc_codes$code

sensor_codes_df <- qc_codes %>%
  filter(code_type == "sensor" | code_type == "general")

sensor_codes <- sensor_codes_df$code
names(sensor_codes) <- sensor_codes_df$description

comment_codes_df <- qc_codes %>%
  filter(code_type == "comment")

comment_codes <- comment_codes_df$code
names(comment_codes) <- comment_codes_df$description

sensor_vector_l1 <- c("Turbidity" = "tu",
                      "Conductivity" = "ct",
                      "Optical Dissolved Oxygen" = "op",
                      "Depth" = "de",
                      "Fluorescent Dissolved Organic Matter" = "fd",
                      "Wiper" = "wi",
                      "pH" = "ph",
                      "EXO2 Sonde" = "ex",
                      "Total Algae" = "ta")

qc_flags <- c("-5" = "Outside high range",
              "-4" = "Outside low range",
              "-3" = "Data rejected due to QAQC",
              "-2" = "Missing Data",
              "-1" = "Optional parameter, not collected",
              "0" = "Passed L1 QC",
              "1" = "Suspect Data")

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Get filenames of annotations
annotation_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/STRI_DATA_PROCESSING/technician_portal_output/", recursive = T) 

# If there are no annotations in the directory a data frame with no rows or columns is returned. 
# The app must have a df with a column "name"
if(nrow(annotation_directory) == 0){
  annotation_directory <- tibble(identifier = NA_character_, .rows = 0)

} else {
  # site-code_filetype_year_level_annotationtype
  annotation_directory <- annotation_directory %>%
    filter(`.tag` == "file") %>%
    select(name) %>%
    separate(name, into = c("site_code", "file_type", "year", "level", "annotation_type"), sep = "_", remove = FALSE) %>%
    mutate(annotation_type = gsub(".csv", "", annotation_type),
           identifier = paste(site_code, file_type, year, sep = "_"))
}

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

submissionDate <- function() format(Sys.time(), "%Y%m%d")

rosetta <- read_csv("./data/MarineGEO_rosetta.csv",
                    locale = locale(encoding = "Windows-1252"))

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsHeight = screen.height;
  Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'

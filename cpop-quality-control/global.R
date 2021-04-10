# Global script runs first when initializing application
# Packages and data are loaded 

library(shinydashboard)
library(shiny)
library(plyr)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(data.table)
library(DT)
library(plotly)
library(shinyjs)
library(DBI)

#introduction_text <- read_file("./prototype_v2/data/intro.txt")
introduction_text <- read_file("./data/intro.txt")

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")

wq_dat <- tbl(con, "water_quality_l1")

key <- wq_dat %>%
  group_by(year(timestamp), site_code) %>%
  summarize(n = n()) %>%
  collect() %>%
  rename(year = `year(timestamp)`) %>%
  select(-n)

dbDisconnect(con)


# Get filenames to populate UI for now
# bundled_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/L1_DATA_FLAGS/") %>%
#   pull(name)

# key <- as_tibble(bundled_directory) %>%
#   rename(Filename = value) %>%
#   separate(Filename, into = c("Site", "File Type", "Date", "Status"), sep = "_", remove = FALSE) %>%
#   mutate(Status = gsub(".csv", "", Status)) %>%
#   select(everything(), Filename) %>%
#   filter(Status == "L1-data") %>%
#   arrange(Date)

sensor_parameters_df <- read_csv("./data/sensor_parameters.csv")

qc_codes <- read_csv("./data/L2_sensor_codes.csv") %>%
  mutate(select_inputs = paste(code, description, sep=" - ")) %>%
  mutate(code_type = case_when(
    substr(code, 1, 1) == "C" ~ "comment",
    substr(code, 1, 1) == "S" ~ "sensor",
    substr(code, 1, 1) == "G" ~ "general",
    T ~ NA_character_
  ))

sensor_codes_df <- qc_codes %>%
  filter(code_type == "sensor" | code_type == "general")

sensor_codes <- sensor_codes_df$code
names(sensor_codes) <- sensor_codes_df$select_inputs

comment_codes_df <- qc_codes %>%
  filter(code_type == "comment")

comment_codes <- comment_codes_df$code
names(comment_codes) <- comment_codes_df$select_inputs

sensor_vector_l1 <- c("Turbidity" = "tu",
                      "Conductivity" = "ct",
                      "Optical Dissolved Oxygen" = "op",
                      "Depth" = "de",
                      "Fluorescent Dissolved Organic Matter" = "fd",
                      "Wiper" = "wi",
                      "pH" = "ph",
                      "EXO2 Sonde" = "ex",
                      "Total Algae" = "ta")

# qc_flags <- c("-5 (Outside high range)" = -5,
#               "-4 (Outside low range)" = -4,
#               "-3 (Data rejected due to QAQC)" = -3,
#               "-2 (Missing Data)" = -2,
#               "-1 (Optional parameter, not collected)" = -1,
#               "0 (Passed L1 QC)" = 0,
#               "1 (Suspect Data)" = 1)

qc_flags <- c("-3 (Data rejected due to QAQC)" = -3,
              "1 (Suspect Data)" = 1)

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Get filenames of annotations
# annotation_directory <- drop_dir("Marine_GEO_CPOP_PROCESSING/L2_quality_control/", recursive = T) 

# If there are no annotations in the directory a data frame with no rows or columns is returned. 
# The app must have a df with a column "name"
# if(nrow(annotation_directory) == 0){
#   annotation_directory <- tibble(identifier = NA_character_, .rows = 0)
# 
# } else {
#   # site-code_filetype_year_level_annotationtype
#   annotation_directory <- annotation_directory %>%
#     filter(`.tag` == "file") %>%
#     select(name) %>%
#     separate(name, into = c("site_code", "file_type", "year", "level", "annotation_type"), sep = "_", remove = FALSE) %>%
#     mutate(annotation_type = gsub(".csv", "", annotation_type),
#            identifier = paste(site_code, file_type, year, sep = "_"))
# }

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

submissionDate <- function() format(Sys.time(), "%Y%m%d")

rosetta <- read_csv("./data/MarineGEO_rosetta.csv",
                    locale = locale(encoding = "Windows-1252"))

saveAnnotations <- function(x, table_name){
  
  con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
  
  rs <- DBI::dbSendQuery(con, paste0('SHOW COLUMNS FROM ', table_name, ';'))
  table_column_names <- DBI::dbFetch(rs)
  dbClearResult(rs)
  primary_key <- which(table_column_names$Key == "PRI")

  # Single query per row of data
  for(i in 1:nrow(x)) {
    
    # Row of data to vector
    dat_values <- sapply(x[i, ], as.character)
    
    # Insert or update - no rows are deleted
    row_query <- paste0("INSERT INTO ",
                      table_name,
                      "(", paste(table_column_names$Field, collapse = ", "), ") ", # column names
                      "VALUES",
                      "('", paste(dat_values, collapse = "', '"), "') ", # new records
                      "ON DUPLICATE KEY UPDATE ",
                      paste(table_column_names$Field[-primary_key], dat_values[-primary_key], sep = " = '", collapse = "', "), 
                      "';")
    
    print(row_query)

    DBI::dbSendQuery(con, row_query)
    #print(myquery)
    
  }
  
  dbDisconnect(con)
  
}

# jscode <-
#   '$(document).on("shiny:connected", function(e) {
#   var jsHeight = screen.height;
#   Shiny.onInputChange("GetScreenHeight",jsHeight);
# });
# '

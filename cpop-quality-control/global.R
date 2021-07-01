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

#app_location <- "local"
app_location <- "shiny_server"

if(app_location == "shiny_server"){
  library(dbplyr, lib.loc = "/home/lonnemanm/library")
}

getDatabaseConnection <- function(){
  DBI::dbConnect(odbc::odbc(),
                 Driver = "MySQL ODBC 8.0 ANSI Driver",
                 Server = "si-mysqlproxy01.si.edu",
                 Port = 7003,
                 Database = "orc_data_lake",
                 UID = "datLakeDev",
                 PWD = Sys.getenv('password'))
}

con <- getDatabaseConnection()

table_names <- c("water_quality_l1", "korexo_panbdt_instrument1_l1", "korexo_usairl_instrument1_l1")

key <- data.frame()

for(table in table_names){
  wq_dat <- tbl(con, table)
  
  df <- wq_dat %>%
    group_by(year(timestamp), month(timestamp), site_code) %>%
    summarize(n = n(), table_name = table) %>%
    collect() %>%
    rename(year = `year(timestamp)`,
           month = `month(timestamp)`) %>%
    select(-n) %>%
    arrange(table_name, site_code, year, month)
  
  key <- df %>%
    group_by(table_name, site_code) %>%
    mutate(one_month = dplyr::lead(month, n = 1),
           two_month = dplyr::lead(month, n = 2)) %>%
    mutate(leading_months = case_when(
      is.na(one_month) ~ 0,
      is.na(two_month) ~ 1,
      T ~ 2
    )) %>%
    select(-one_month, -two_month) %>%
    bind_rows(key)
}

dbDisconnect(con)

flag_definitions <- read_csv("./data/qc_flag_definitions.csv") %>%
  mutate(value = as.character(value))

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

# qc_flags_full <- c("-5 (Outside high range)" = -5,
#               "-4 (Outside low range)" = -4,
#               "-3 (Data rejected due to QAQC)" = -3,
#               "-2 (Missing Data)" = -2,
#               "-1 (Optional parameter, not collected)" = -1,
#               "0 (Passed L1 QC)" = 0,
#               "1 (Suspect Data)" = 1)

qc_flags <- c("-3 (Data rejected due to QAQC)" = -3,
              "1 (Suspect Data)" = 1)

formatted_months <- c("1 - January" = 1,
                      "2 - February" = 2,
                      "3 - March" = 3,
                      "4 - April" = 4,
                      "5 - May" = 5,
                      "6 - June" = 6,
                      "7 - July" = 7,
                      "8 - August" = 8,
                      "9 - September" = 9,
                      "10 - October" = 10,
                      "11 - November" = 11,
                      "12 - December" = 12)

full_definitions <- qc_codes %>%
  select(code, description) %>%
  rename(value = code, 
         definition = description) %>%
  bind_rows(flag_definitions) %>%
  select(-color)

color_dictionary_flags <- flag_definitions$color
names(color_dictionary_flags) <- flag_definitions$value
color_dictionary_flags <- c(color_dictionary_flags, c("Ref" = "grey50"))

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

submissionDate <- function() format(Sys.time(), "%Y%m%d")

rosetta <- read_csv("./data/MarineGEO_rosetta.csv",
                    locale = locale(encoding = "Windows-1252"))

jscode_box_collapse <- "
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}}
"

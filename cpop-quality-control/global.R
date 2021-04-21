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

app_location <- "local"
#app_location <- "shiny_server"

if(app_location == "shiny_server"){
  library(dbplyr, lib.loc = "/home/lonnemanm/library")
}

#introduction_text <- read_file("./prototype_v2/data/intro.txt")
introduction_text <- read_file("./data/intro.txt")

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "MySQL ODBC 8.0 ANSI Driver",
                      Server = "si-mysqlproxy01.si.edu",
                      Port = 7003,
                      Database = "orc_data_lake",
                      UID = "datLakeDev",
                      PWD = Sys.getenv('password'))
                      #PWD = "Da&LdN5uj6w#Nzwp")

wq_dat <- tbl(con, "water_quality_l1")

key <- wq_dat %>%
  group_by(year(timestamp), month(timestamp), site_code) %>%
  summarize(n = n()) %>%
  collect() %>%
  rename(year = `year(timestamp)`,
         month = `month(timestamp)`) %>%
  select(-n)

dbDisconnect(con)

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

parameters <- sensor_parameters_df %>%
  pull(parameter)

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

submissionDate <- function() format(Sys.time(), "%Y%m%d")

rosetta <- read_csv("./data/MarineGEO_rosetta.csv",
                    locale = locale(encoding = "Windows-1252"))

# saveAnnotations <- function(x, table_name){
#   
#   con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
#   
#   rs <- DBI::dbSendQuery(con, paste0('SHOW COLUMNS FROM ', table_name, ';'))
#   table_column_names <- DBI::dbFetch(rs)
#   dbClearResult(rs)
#   primary_key <- which(table_column_names$Key == "PRI")
# 
#   # Single query per row of data
#   for(i in 1:nrow(x)) {
#     
#     # Row of data to vector
#     dat_values <- sapply(x[i, ], as.character)
#     
#     # Insert or update - no rows are deleted
#     row_query <- paste0("INSERT INTO ",
#                       table_name,
#                       "(", paste(table_column_names$Field, collapse = ", "), ") ", # column names
#                       "VALUES",
#                       "('", paste(dat_values, collapse = "', '"), "') ", # new records
#                       "ON DUPLICATE KEY UPDATE ",
#                       paste(table_column_names$Field[-primary_key], dat_values[-primary_key], sep = " = '", collapse = "', "), 
#                       "';")
#     
#     print(row_query)
# 
#     DBI::dbSendQuery(con, row_query)
#     #print(myquery)
#     
#   }
#   
#   dbDisconnect(con)
#   
# }

# jscode <-
#   '$(document).on("shiny:connected", function(e) {
#   var jsHeight = screen.height;
#   Shiny.onInputChange("GetScreenHeight",jsHeight);
# });
# '

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

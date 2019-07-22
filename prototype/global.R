# shiny app demo showing SERC and STRI water quality and met data
# Global script runs first when initializing application
# Packages and data are loaded 

library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)

# The key is an inventory of available CSVs that can be imported
# and includes year - sensor - status (QAQC or RAW) - site information based on filepaths
key <- as.data.frame(list(list.files("./data/")), col.names = "filepath") %>%
  filter(filepath != "headers.csv") %>%
  mutate(name = gsub(".csv", "", filepath),
         # Imported tracks whether the application has imported particular datasets
         imported = FALSE) %>%
  # Separate file name into columns
  separate(name, into = c("year", "sensor", "status", "site"), by = "_") %>%
  mutate(sensor = gsub("([a-z])([A-Z])", "\\1 \\2", sensor, perl = TRUE)) %>%
  filter(status == "QAQC")

# list of parameters for available sensors
parameters <- fread("./data/headers.csv")
# Parameters that shouldn't be selectable by the user
ignore_list <- c("Timestamp", "Site", "Status")



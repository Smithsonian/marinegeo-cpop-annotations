# shiny app demo showing SERC and STRI water quality and met data
# Global script runs first when initializing application
# Packages are loaded and data is loaded 

library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(data.table)

# Sites and sensors currently available
# This will be automatically generated in future iterations
site_list <- c("SERC", "STRI")
sensor_list <- c("water quality", "MET")
# Parameters that shouldn't be selectable by the user
ignore_list <- c("Timestamp", "Site", "Status")

# read in data
waterquality <- fread("./data/2015_Water_Quality_QAQC_SERC.csv") %>%
  bind_rows(fread("./data/2016_Water_Quality_QAQC_SERC.csv")) %>%
  bind_rows(fread("./data/2017_Water_Quality_QAQC_SERC.csv")) %>%
  bind_rows(fread("./data/2018_Water_Quality_RAW_SERC.csv")) %>%
  bind_rows(fread("./data/2019_Water_Quality_RAW_SERC.csv")) %>%
  bind_rows(fread("./data/2015_Water_Quality_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2016_Water_Quality_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2017_Water_Quality_QAQC_STRI.csv")) %>%
  mutate(Timestamp = mdy_hm(Timestamp))

MET <- fread("./data/2013_MET_QAQC_STRI.csv") %>%
  bind_rows(fread("./data/2014_MET_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2015_MET_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2016_MET_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2017_MET_QAQC_STRI.csv")) %>%
  bind_rows(fread("./data/2015_MET_QAQC_SERC.csv")) %>%
  bind_rows(fread("./data/2016_MET_QAQC_SERC.csv")) %>%
  bind_rows(fread("./data/2017_MET_QAQC_SERC.csv")) %>%
  mutate(Timestamp = mdy_hm(Timestamp))

# "pretty" labels for graph display 
# not yet functional
waterquality_labels <- c("Timestamp" = "Timestamp", 
                         "Temperature" = "Temperature (°C)", 
                         "Specific.Conductivity"="Specific Conductivity (mS/cm)",
                         "Conductivity" = "Conductivity (mS/cm)",
                         "TDS" = "TDS (g/L)",
                         "Salinity"= "Salinity (psu)",
                         "Dissolved.Oxygen"= "DO (%)",
                         "Dissolved.Oxygen.2"= "DO (mg/l)",
                         "Pressure"= "Pressure (psi)",
                         "Depth"= "Depth (meters)",
                         "pH" = "pH",
                         "Turbidity"= "Turbidity (FNU)",
                         "Chlorophyll"= "Chlorophyll (RFU)",
                         "Chlorophyll.2"= "Chlorophyll (µg/l)",
                         "BGA.PE"= "BGA-PE (RFU)",
                         "BGA.PE 2"= "BGA-PE (µg/l)",
                         "FDOM"= "FDOM (RFU)",
                         "FDOM.2"= "FDOM (ppb QSE)",
                         "Battery"= "Battery (Volts)", 
                         "Site" = "site"
)

met.labels <- c("Air.Temperature"="Air Temperature (°C)",
                "Barometric.Pressure"="Barometric Pressure (mbar)",
                "Daily.Rain"="Daily Rain (mm)",
                "PAR"="PAR",
                "Rain.Rate"="Rain Rate (mm/hr)",
                "Relative.Humidity"="Relative Humidity (%)",
                "Wind.Direction"="Wind Direction (°)",
                "Wind.Gust"="Wind Gust (mph)",
                "Wind.Speed"="Wind Speed (mph)",
                "Solar.Radiation" = "Solar Radiation (mm/hr)"
)


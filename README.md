# CPOP Annotation Portal
An R Shiny app to annotate chemical and physical sensor data. Currently in beta stage, this application allows technicians to load level 1 curated water quality data from MarineGEO sites. Technicians can accept or revise level 1 quality control flags and assign codes. The quality control process is based on NERRS (National Estuarine Research Reserve System)  flagging protocols. Output quality control flags/codes can then be used for additional higher level QAQC protocols.  

Packages:  
shinydashboard  
shiny  
lubridate  
ggplot2  
readr  
magrittr  
dplyr  
tidyr  
data.table  
rdrop2  
DT  
plotly  
shinyjs 

This application is in beta stage. Data will eventually be read in from Hive tables in a Apache Hadoop installation. Output quality control annotations will be saved to Hive tables. 
library(tidyverse)
library(lubridate)
library(data.table)

sms_input <- read_csv("./resources/submission_package/SMS_2020-12-18.csv", skip = 8,
              locale = locale(encoding = "Windows-1252"))

rosetta <- read_csv("./prototype_v2/data/MarineGEO_rosetta.csv",
                    locale = locale(encoding = "Windows-1252"))

sensors <- read_csv("./prototype_v2/data/sensor_parameters.csv")
                    
sms_match <- rosetta %>%
  filter(site_code == "USA-IRL",
         file_source == "KorEXO",
         stop_date == "Present",
         !is.na(original_file_variable))

# Begin conversion to L1

# Reassign column names
name_match = match(names(df), sms_match$original_file_variable)
names(df)[na.omit(name_match)] = sms_match$mgeo_cpop_variable_R[!is.na(name_match)]

# Create timestamp
df <- df %>%
  mutate(Time_HH_mm_ss = as.character(Time_HH_mm_ss),
         timestamp = mdy_hms(paste(Date_MM_DD_YYYY,Time_HH_mm_ss, sep=" ")),
         ID = row_number()) %>%
  select(timestamp, everything(), 
         -c(Date_MM_DD_YYYY, Time_HH_mm_ss, Time_Fract_Sec, Site_Name))

dt <- dt%>%
  mutate(ex =0, ct = 0, op = 0, ph = 0, tu = 0, ta = 0, fd = 0, wi = 0, de = 0)

dt <- dt %>%
  mutate(ex = fifelse(Battery_V < 5, -4, ex, na = -2))

dt<-dt %>% 
  
  #Turbidity
  
  mutate(tu = fifelse(Turbidity_FNU > 4000, -5, tu, na = -2)) %>% 
  
  mutate(tu = fifelse(tu < (0), -4, tu, na = -2))

#Conductivity
dt<-dt %>% 
  mutate(ct = fifelse(Sal_psu > 70, -5, ct, na = -2)) %>% 
  
  mutate(ct = fifelse(Sal_psu < (0), -4, ct, na = -2)) %>% 
  
  
  mutate(ct = fifelse(Temp_C > 45, -5, ct, na = -2)) %>% 
  
  mutate(ct = fifelse(Temp_C < (-5), -4, ct, na = -2)) %>% 
  
  
  mutate(ct = fifelse(Cond_microS_cm /1000 > 200, -5, ct, na = -2)) %>% 
  
  mutate(ct = fifelse(Cond_microS_cm< 0 , -4, ct, na = -2))

#pH
dt<-dt %>% 
  mutate(ph = fifelse(pH> 14, -5, ph, na = -2)) %>% 
  
  mutate(ph = fifelse(pH < (2), -4, ph, na = -2))

##depth
dt<-dt %>% 
  
  mutate(de = fifelse(Depth_m> 61, -5, de, na = -2)) %>% 
  
  mutate(de = fifelse(Depth_m < (0), -4, de, na = -2))

dt<-dt %>% 
  mutate(op = fifelse(ODO_mg_L> 50, -5, op, na = -2)) %>% 
  
  mutate(op = fifelse(ODO_mg_L < (0), -4, op, na = -2)) 


dt<-dt %>% 
  mutate(ta = fifelse(Chlorophyll_microg_L> 400, -5, ta, na = -2)) %>% 
  
  mutate(ta = fifelse(Chlorophyll_microg_L < (0), -4, ta, na = -2)) 

dt<-dt %>% 
  mutate(fd = fifelse(fDOM_RFU> 300, -5, fd, na = -2)) %>% 
  
  mutate(fd = fifelse(fDOM_RFU < (0), -4, fd, na = -2)) 

#added flagging table
#creating vectors to store flag names
flagnames <- c("ex", "ct", "op", "ph", "tu", "ta",
               "fd", "wi", "de") 

#create flagging table for numeric flags
L1 <- dt %>%
  #mutating prevents errors due to mix of character and int classes
  mutate(ex = as.character(ex)) %>% 
  mutate(ct = as.character(ct)) %>% 
  mutate(op = as.character(op)) %>% 
  mutate(ph = as.character(ph)) %>% 
  mutate(tu = as.character(tu)) %>% 
  mutate(ta = as.character(ta)) %>% 
  mutate(fd = as.character(fd)) %>% 
  mutate(wi = as.character(wi)) %>% 
  mutate(de = as.character(de)) %>% 
  select(c(any_of(flagnames), ID)) %>%
  pivot_longer(cols = any_of(flagnames),
               names_to = "sensor",
               values_to = "flag") %>%
  filter(!is.na(flag))

## Calibration curation ####

cal_raw <- read_csv("./resources/submission_package/Calibration File Export - 121820 105005.csv", 
                skip = 2, col_names = F, locale = locale(encoding = "Windows-1252")) %>% 
  rowid_to_column()

# cal <- cal_raw %>%
#   filter(X1 != "----------")

cal_raw %>%
  filter(X1 == "Instrument Type=") %>%
  count(X2)

cal_raw %>%
  filter(X1 == "Sensor Type=") %>%
  count(X2)

cal_raw %>%
  filter(X1 == "Parameter Type=") %>%
  count(X2)

cal_raw %>%
  filter(X1 == "Sensor Serial Number=") %>%
  count(X2)

colnames(sms_input)

# current_log <- read_delim(paste0("./Dropbox (Smithsonian)/MarineGEO Water Monitoring Panama Bocas/KOREXO_calibration_files/",
#                                  log), 
#                           delim = "\n", col_names=F) %>% 
#   separate(X1, into=c("attribute", "value"), sep=" = ") %>% 
#   mutate(value = gsub("\"", "", value)) %>% 
#   rowid_to_column() 

# Each "sub table" in the calibration file - indicated by [ ] - are extracted and turned into a variable
# Collectively these sub table titles are called categories: 
subtable_rows <- cal_raw %>% 
  filter(X1 == "----------") %>%
  select(rowid)

subtable_sensors <- cal_raw %>%
  filter(X1 == "Sensor Type=") %>%
  select(X2)

subtable_index <- cbind(subtable_rows, subtable_sensors) %>%
  rename(max_rowid = rowid, sensor = X2)

cal <- cal_raw %>%
  filter(X1 != "----------")

calibration_log <- data.frame()
calibration_points <- data.frame()

index <- 0

# For each category, extract it's attributes
for(i in 1:nrow(subtable_index)){ 
  print("NEXT")
  print(i)
  print(index)

  next_index <- subtable_index$max_rowid[i]
  
  print(next_index)
  
  calibration_subset <- cal %>%
    filter(rowid > index & rowid < next_index)
  
  calibration_point_index <- calibration_subset %>% 
    filter(grepl("\\[Cal Point", X1)) %>%
    select(X1, rowid)
  
  if(nrow(calibration_point_index) != 0){
    for(j in 1:nrow(calibration_point_index)){
      
      if(j == nrow(calibration_point_index)){
        next_point_index <- max(calibration_subset$rowid) + 1
      } else {
        next_point_index <- calibration_point_index$rowid[j + 1]
      }
      
      print("cal points")
      current_point_index <- calibration_point_index$rowid[j]
      print(current_point_index)
      print(next_point_index)
      
      calibration_points <- calibration_subset %>%
        filter(rowid > current_point_index & rowid < next_point_index) %>%
        select(-rowid) %>%
        pivot_wider(names_from = X1, values_from = X2) %>%
        mutate(calibration_point = j,
               calibration_id = index + 1) %>%
        bind_rows(calibration_points)
    }
    
    max_calibration_point_rowid <- min(calibration_point_index$rowid)
  } else {
    max_calibration_point_rowid <- next_index
  }

  calibration_log <- calibration_subset %>%
    filter(rowid < max_calibration_point_rowid) %>%
    select(-rowid) %>%
    pivot_wider(names_from = X1, values_from = X2) %>%
    bind_rows(calibration_log)
  
  index <- next_index
  
}

df <- cal %>%
  filter(rowid > 49 & rowid < 78) %>%
  select(-rowid)
 #%>%
#  pivot_wider(names_from = X1, values_from = X2)

# Clean up calibration log and bind to synthesis table of all logs
calibration_logs_longform <- calibration_log %>%
  select(-rowid) %>% #removes this variable
  mutate(logfile = log) %>% 
  mutate(value = ifelse(value == "" | value == "N/A" | value == "NaN", NA, value)) %>%
  filter(!is.na(value)) %>% 
  bind_rows(calibration_logs_longform) 

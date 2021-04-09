wq_dat <- tbl(con, "water_quality_l1")

library(lubridate)

wq_dat %>%
  group_by(year(timestamp), site_code) %>%
  summarize(n = n())


test <- wq_dat %>%
  filter(year(timestamp) == 2018) %>%
  collect()

library(DBI)
dbDisconnect(con)

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")

wq_dat <- tbl(con, "water_quality_l1")

key <- wq_dat %>%
  group_by(year(timestamp), site_code) %>%
  summarize(n = n()) %>%
  collect() %>%
  rename(year = `year(timestamp)`)

dbDisconnect(con)

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
wq_dat <- tbl(con, "water_quality_l1")
wq_qc_dat <- tbl(con, "water_quality_primary_flags")

df <- wq_dat %>%
  filter(year(timestamp) == 2018,
         site_code == "PAN-BDT") %>%
  collect()

current_ids <- df$id

raw_flags <- wq_qc_dat %>%
  filter(id %in% current_ids) %>%
  collect()

flags <- raw_flags %>%
  pivot_longer(Turbidity_FNU_f:fDOM_RFU_f, names_to = "sensor", values_to = "flag") %>%
  mutate(sensor = case_when(
    sensor == "Turbidity_FNU_f" ~ "tu",
    sensor == "Sal_psu_f" ~ "ct",
    sensor == "Temp_C_f" ~ "ct",              
    sensor == "Cond_microS_cm_f" ~ "ct",      
    sensor == "pH_f" ~ "ph",                  
    sensor == "Depth_m_f" ~ "de",              
    sensor == "ODO_mg_L_f" ~ "op",           
    sensor == "Chlorophyll_microg_L_f" ~ "ta",
    sensor == "fDOM_RFU_f" ~ "fd",
    T ~ NA_character_
  )) %>%
  group_by(id, sensor) %>%
  summarize(flag = min(flag))

flags %>%
  filter(sensor == "ct") %>%
  group_by(id) %>%
  summarize(n = n_distinct(flag)) %>%
  filter(n > 1)

flags %>%
  filter(id == 859675)

ct_group <- flags %>%
  filter(sensor == "ct") %>%
  group_by(id) %>%
  summarize(flag = min(flag))


grouped <- flags %>%
  group_by(id, sensor) %>%
  summarize(flag = min(flag))

wq_dat %>%
  group_by(year(timestamp), site_code) %>%
  summarize(n = n_distinct(timestamp3), min_time = min(timestamp3), max_time = max(timestamp3))

code_ex <- tibble(id = c(1, 2, 3), 
       sensor = c("ct", "tu", "ct"),
       code = c("GCC", "GCM", "GCR"))

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
wq_codes <- tbl(con, "water_quality_codes")

DBI::dbWriteTable(con,
                  value = code_ex,
                  name = "water_quality_codes", append = TRUE)

wq_codes

cmd <- paste("update MyTable values ", values)
result <- sqlQuery(con, cmd, as.is=TRUE) 

cmd <- "
UPDATE water_quality_codes 
SET `code` = \"ZZZ\" 
WHERE id = 1;
"

dbGetQuery(con, cmd)

## Annotation app 

# Save Flags

# 1. use delete! 

# ID - sensor - flag




# First collect IDs of any codes in the database


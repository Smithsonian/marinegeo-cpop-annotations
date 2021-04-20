library(DBI)
library(tidyverse)
library(lubridate)

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
wq_dat <- tbl(con, "water_quality_l1")
wq_qc_dat <- tbl(con, "water_quality_primary_flags")
wq_qc_2 <- tbl(con, "water_quality_secondary_flags")


# Get summary (by year) numbers of water quality data
key <- wq_dat %>%
  group_by(year(timestamp), site_code) %>%
  summarize(n = n()) %>%
  collect() %>%
  rename(year = `year(timestamp)`)

DBI::dbDisconnect(con)


# Pull in flags based on a ID query 
df <- wq_dat %>%
  filter(year(timestamp) == 2018,
         site_code == "PAN-BDT") %>%
  collect()

current_ids <- df$id

raw_flags <- wq_qc_dat %>%
  filter(id %in% current_ids) %>%
  collect()

raw_flags2 <- wq_qc_2 %>%
  filter(id %in% current_ids) %>%
  collect()

DBI::dbDisconnect(con)

## Append a table 
code_ex <- tibble(
  id = c(1, 2, 3), 
  sensor = c("ct", "tu", "ct"),
  code = c("GCC", "GCM", "GCR")
)

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
wq_codes <- tbl(con, "water_quality_codes")

DBI::dbWriteTable(con,
                  value = code_ex,
                  name = "water_quality_codes", append = TRUE)

wq_codes

# Text query 
cmd <- "
UPDATE water_quality_codes 
SET `code` = \"ZZZ\" 
WHERE id = 1;
"

result <- sqlQuery(con, cmd, as.is=TRUE) 

# Compare three methods of appending and overwriting

code_ex <- tibble(
  id = c(1, 2, 3), 
  sensor = c("ct", "tu", "ct"),
  code = c("GCC", "GCM", "GCR")
)

code_ex_append <- tibble(
  code_id = c(4, 5, 6),
  id = c(4, 5, 6), 
  sensor = c("ct", "tu", "ct"),
  code = c("GCC", "GCM", "GCR")
)

code_ex_write <- tibble(
  code_id = c(7, 8, 9),
  id = c(4, 5, 6), 
  sensor = c("ct", "tu", "ct"),
  code = c("GCC", "GCM", "GCR")
)

system.time(DBI::dbAppendTable(con, "water_quality_codes", value = code_ex_append)) # .13 sec
system.time(DBI::dbWriteTable(con, "water_quality_codes", value = code_ex_write, append = T)) # .1 sec
system.time(DBI::dbWriteTable(con, "water_quality_codes", value = code_ex_write, overwrite = T)) # .21 sec


## Use loop to do row-wise insertion

x <- code_ex_replacement

rs <- DBI::dbSendQuery(con, paste0('SHOW COLUMNS FROM ', "water_quality_codes", ';'))
col_names <- DBI::dbFetch(rs)
dbClearResult(rs)
pri <- which(col_names$Key == "PRI")
table <- "water_quality_codes"
# For each row of table, update db
for(i in 1:nrow(x)) {
  
  # Transform ith row of dataset into character vector
  values <- sapply(x[i, ], as.character)
  
  # Build the INSERT/UPDATE query
  myquery <- paste0("INSERT INTO ",
                    table,
                    "(", paste(col_names$Field, collapse = ", "), ") ", # column names
                    "VALUES",
                    "('", paste(values, collapse = "', '"), "') ", # new records
                    "ON DUPLICATE KEY UPDATE ",
                    paste(col_names$Field[-pri], values[-pri], sep = " = '", collapse = "', "), # everything minus primary keys
                    "';")
  
  print(myquery)
  #if(verbose) cat("Performing query", i, "of", nrow(x), ":\n", myquery, "\n\n")
  
  # Send query to database
  DBI::dbSendQuery(con, myquery)
  #print(myquery)
}

## Test larger insertion methods 

full_qc <- read_csv("D:/data/Dropbox (Smithsonian)/marinegeo_resources/test_datalake/data/final_output_qc.csv")
full_dat <- read.csv("D:/data/Dropbox (Smithsonian)/marinegeo_resources/test_datalake/data/final_output.csv")

mda_2016_id <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2016", as.character(timestamp))) %>%
  pull(id)

con <- DBI::dbConnect(odbc::odbc(), "test data lake db")
wq_dat <- tbl(con, "water_quality_l1")
system.time(tbl(con, "water_quality_primary_flags") %>% filter(id %in% mda_2016_id) %>% collect())


wq_qc_2 <- tbl(con, "water_quality_secondary_flags")

upload_me <- full_qc %>%
  filter(id %in% mda_2016_id)

# Tragically, this took 639 seconds
system.time(DBI::dbAppendTable(con, name = "water_quality_primary_flags", value = upload_me))

# try 2017 using WriteTable - Still took too long, cancelled before it resolved fully. 
mda_2017_id <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2017", as.character(timestamp))) %>%
  pull(id)

upload_me2 <- full_qc %>%
  filter(id %in% mda_2017_id)

system.time(DBI::dbWriteTable(con, name = "water_quality_primary_flags", value = upload_me2, append = T))


# load data infile gave a permission denied error

mda_2017_id <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2017", as.character(timestamp))) %>%
  pull(id)

upload_me3 <- full_qc %>%
  filter(id %in% mda_2017_id)

write.table(upload_me3, "test.csv", row.names = F, col.names = F, sep = "\t")

query = "LOAD DATA INFILE 'test.csv' INTO TABLE water_quality_primary_flags"
system.time(dbGetQuery(con, query))

## 

mda_2016 <- full_dat %>%
  mutate(timestamp = as.character(timestamp),
         timestamp2 = as.character(timestamp2),
         timestamp3 = as.character(timestamp3)) %>%
  filter(site_code == "USA-MDA",
         grepl("2016", timestamp)) %>%
  mutate(timestamp = gsub("T", " ", timestamp),
         timestamp2 = gsub("T", " ", timestamp2),
         timestamp3 = gsub("T", " ", timestamp3)) %>%
  mutate(timestamp = gsub("Z", " ", timestamp),
         timestamp2 = gsub("Z", " ", timestamp2),
         timestamp3 = gsub("Z", " ", timestamp3))

system.time(DBI::dbWriteTable(con, name = "water_quality_l1", value = mda_2016, append = T))

DBI::dbDisconnect(con)


## Test deletion of QC rows
# Total time for ~ 72000 rows = 680 seconds 
# Total time for deletion and replacement: 20 minutes...D
mda_2017_id <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2017", as.character(timestamp))) %>%
  pull(id)

qc_2017 <- wq_qc_dat %>%
  filter(id %in% mda_2017_id) %>%
  collect()

# Currently 72000 rows
min(qc_2017$id)

# PREPARED STATEMEN
sql <- "delete from water_quality_primary_flags  where id = ?"
vals <- qc_2017$id

# BIND PARAM AND EXECUTE ACTION
system.time(dbSendQuery(con, sql, list(vals)))


## Try a more selective insert, single insert query
# 20% of a yearly bundle
# Took 132 seconds
full_qc <- read_csv("D:/data/Dropbox (Smithsonian)/marinegeo_resources/test_datalake/data/final_output_qc.csv")
full_dat <- read.csv("D:/data/Dropbox (Smithsonian)/marinegeo_resources/test_datalake/data/final_output.csv")

mda_2016_id <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2016", as.character(timestamp))) %>%
  pull(id)

qc_raw <- wq_qc_dat %>%
  filter(id %in% mda_2016_id) %>%
  collect()

qc_subset <- qc_raw[1:15000,] %>%
  mutate(pH_f = 0)

# pH_f column altered
vals <- qc_subset$id
  
sql <- 
"UPDATE water_quality_primary_flags 
SET `pH_f` = \"0\" 
WHERE `id` IN (?)"

system.time(dbSendQuery(con, sql, list(vals)))

# 1000 points - 8.9 seconds - not bad. 

qc_subset <- qc_raw[1:1000,] %>%
  mutate(pH_f = 0)

# pH_f column altered
vals <- qc_subset$id

sql <- 
  "UPDATE water_quality_primary_flags 
SET `pH_f` = \"3\" 
WHERE `id` IN (?)"

system.time(dbSendQuery(con, sql, list(vals)))


## Upload a two week section of data 
# 10 seconds! 

mda2017 <- full_dat %>%
  filter(site_code == "USA-MDA",
         grepl("2017", timestamp)) %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  filter(timestamp < min(timestamp) + weeks(2)) %>%
  mutate(timestamp = as.character(timestamp),
         timestamp2 = as.character(timestamp2),
         timestamp3 = as.character(timestamp3)) %>%
  mutate(timestamp = gsub("T", " ", timestamp),
         timestamp2 = gsub("T", " ", timestamp2),
         timestamp3 = gsub("T", " ", timestamp3)) %>%
  mutate(timestamp = gsub("Z", " ", timestamp),
         timestamp2 = gsub("Z", " ", timestamp2),
         timestamp3 = gsub("Z", " ", timestamp3))

system.time(DBI::dbWriteTable(con, name = "water_quality_l1", value = mda2017, append = T))


## Test with new schema (observation ID, timestamp, timestamp_15min etc.)
data_subset <- full_dat[1:1000,] 

system.time(DBI::dbWriteTable(con, name = "test_null_values", value = data_subset, append = T))

flag_subset <- full_qc[1:1000,]

system.time(DBI::dbWriteTable(con, name = "water_quality_primary_flags", value = flag_subset, append = T))

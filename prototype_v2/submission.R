# # Submission time will store the time a user initially submits data using the humanTime function
# submission_time <- reactiveVal(0)
# 
# # Holds ID to be appended to outputs
# getSubmissionID <- reactiveVal(NA)
# 
# # Dataframe to hold significant event details associated with deployments
# significant_events <- reactiveValues(df = tibble(
#   event_date = NA_character_,
#   event_description = NA_character_,
#   .rows = 0
# ))
# 
# # Dataframe that inventories the filenames of items provided to the fileInputs.
# # Status reflects what to do with each item
# uploaded_files <- reactiveValues(df = tibble(
#   file_type = NA_character_, # Calibration or Korexo file
#   filename = NA_character_,
#   datapath = NA_character_,
#   status = NA_character_, # Not processed, invalid file type, duplicate, processed
#   .rows = 0
# ))
# 
# # Lists to hold output of calibration file processing.
# # Each valid calibration file will have a named dataframe object in each list
# calibration_uploads <- reactiveValues(
#   log = list(),
#   points = list(),
#   log_df = data.frame(),
#   points_df = data.frame()
# )
# 
# ## Initial upload page ####
# 
# # If the user is modifying an existing deployment, a select input appears with available deployments
# output$select_existing_deployment <- renderUI({
#   
#   if(input$submission_action == "modify existing deployment" & input$submission_site != ""){
#     
#     div(
#       selectInput("select_deployment", "Select a deployment to modify", choices = c("deployment 1", "deployment 2"))
#     )
#   }
# })
# 
# observeEvent(input$to_deployment_metadata, {
#   updateTabsetPanel(session, "submission_box", "deployment_metadata")
# })
# 
# ## Provide deployment metadata ####
# 
# # Add event data to table, clear text input for event
# observeEvent(input$confirm_event, {
#   significant_events$df <- significant_events$df %>%
#     add_row(event_date = as.character(input$significant_event_date),
#             event_description = input$significant_event_description)
#   
#   updateTextInput(session, inputId = "significant_event_description", value = "")
# })
# 
# # Table displaying significant events - if no rows, then a special row indicating the absence of events is added
# output$display_significant_events <- renderUI({
#   if(nrow(significant_events$df) > 0){
#     div(renderTable(significant_events$df))
#   } else{
#     div(renderTable(
#       significant_events$df %>%
#         add_row(event_description = "No events recorded for this deployment period",
#                 event_date = "")
#     ))
#   }
# })
# 
# observeEvent(input$to_calibration_upload, {
#   updateTabsetPanel(session, "submission_box", "calibration_file")
# })
# 
# ## Calibration Upload and review ####
# 
# # Once submit calibration files is clicked, any new files are 
# observeEvent(input$submit_calibration_files, {
#   
#   if(!is.null(input$fileCalibration$name)){
#    
#     uploaded_files$df <- uploaded_files$df %>%
#       add_row(file_type = "korexo",
#               filename = input$fileCalibration$name,
#               datapath = input$fileCalibration$datapath) %>%
#       mutate(status = case_when(
#         !grepl(".csv", filename) ~ "invalid file",
#         is.na(status) ~ "not processed",
#         T ~ status
#       ))
#   }
# })
# 
# output$calibration_output <- renderUI({
#   if(nrow(calibration_uploads$log_df) > 0){
#     renderTable(calibration_uploads$log_df)
#   }
# })
# 
# 
# observe({
#   
#   to_process <- uploaded_files$df %>%
#     filter(status == "not processed")
#   
#   if(nrow(to_process) > 0){
#     for(i in 1:nrow(to_process)){
#       
#       output <- processCalibrationFile(to_process[i,]$datapath)
#   
#       calibration_uploads$log[[to_process[i,]$filename]] <- output[[1]]
#       
#       calibration_uploads$points[[to_process[i,]$filename]] <- output[[2]]
#     }  
#   }
#   
#   calibration_uploads$log_df <- bind_rows(calibration_uploads$log)
#   calibration_uploads$points_df <- bind_rows(calibration_uploads$points)
#   
# })
## ... submit data button ####
# observeEvent(input$confirm_submission, {
#   
#   # Get the current time
#   submission_time(humanTime())
#   
#   getSubmissionID(paste(
#     input$submission_site,
#     submission_time(),
#     sep = "_")
#   )
#   
#   # Create inital submission receipt (save email and submission time)
#   # if(!no_db_testing) initialReceipt() 
#   
#   # And data entry date - protocol - site information can all be extracted
#   file_test <- checkFileExtensions()
#   
#   # Only upload data if all files are .xlsx
#   if(file_test){
    # showModal(modalDialog(
    #   title = "Data Uploading", 
    #   div("Thank you for submitting data to MarineGEO! Your data is currently undergoing QA tests, 
    #         and being saved to our database. This will take a few moments. If you are uploading data for multiple sites or more than three protocols, it could take from one to three minutes. Once complete, this page will update, and you will have access to a report 
    #         with additional details about the status of your submission."),
    #   
    #   easyClose = TRUE
    # ))
    # 
    # Extract and format calibration data
    # processCalibrationFile()
    
    # Process Korexo file 
    # processKorexoData()
    
    # Upload initial files to dropbox 
    # saveInitialData()
    # 
    # # Run beta QA process 
    # QAQC()
    # 
    # if(!no_db_testing) {
    #   saveCuratedData()
    #   saveSubmissionMetadata()
    # }
    # 
#  }
#

# processKorexoData <- function(){
#   # FOR USA-IRL
#   df <- read_csv(input$fileKorexo$datapath, skip = 8,
#                         locale = locale(encoding = "Windows-1252"))
#   
#   sms_match <- rosetta %>%
#     filter(site_code == "USA-IRL",
#            file_source == "KorEXO",
#            stop_date == "Present",
#            !is.na(original_file_variable))
#   
#   # Begin conversion to L1
#   
#   # Reassign column names
#   name_match = match(names(df), sms_match$original_file_variable)
#   names(df)[na.omit(name_match)] = sms_match$mgeo_cpop_variable_R[!is.na(name_match)]
#   
#   # Create timestamp
#   df <- df %>%
#     mutate(Time_HH_mm_ss = as.character(Time_HH_mm_ss),
#            timestamp = mdy_hms(paste(Date_MM_DD_YYYY,Time_HH_mm_ss, sep=" ")),
#            ID = row_number()) %>%
#     select(timestamp, everything(), 
#            -c(Date_MM_DD_YYYY, Time_HH_mm_ss, Time_Fract_Sec, Site_Name))
#   
#   dt <- df %>%
#     mutate(ex =0, ct = 0, op = 0, ph = 0, tu = 0, ta = 0, fd = 0, wi = 0, de = 0)
#   
#   dt <- dt %>%
#     mutate(ex = fifelse(Battery_V < 5, -4, ex, na = -2))
#   
#   dt<-dt %>% 
#     
#     #Turbidity
#     
#     mutate(tu = fifelse(Turbidity_FNU > 4000, -5, tu, na = -2)) %>% 
#     
#     mutate(tu = fifelse(tu < (0), -4, tu, na = -2))
#   
#   #Conductivity
#   dt<-dt %>% 
#     mutate(ct = fifelse(Sal_psu > 70, -5, ct, na = -2)) %>% 
#     
#     mutate(ct = fifelse(Sal_psu < (0), -4, ct, na = -2)) %>% 
#     
#     
#     mutate(ct = fifelse(Temp_C > 45, -5, ct, na = -2)) %>% 
#     
#     mutate(ct = fifelse(Temp_C < (-5), -4, ct, na = -2)) %>% 
#     
#     
#     mutate(ct = fifelse(Cond_microS_cm /1000 > 200, -5, ct, na = -2)) %>% 
#     
#     mutate(ct = fifelse(Cond_microS_cm< 0 , -4, ct, na = -2))
#   
#   #pH
#   dt<-dt %>% 
#     mutate(ph = fifelse(pH> 14, -5, ph, na = -2)) %>% 
#     
#     mutate(ph = fifelse(pH < (2), -4, ph, na = -2))
#   
#   ##depth
#   dt<-dt %>% 
#     
#     mutate(de = fifelse(Depth_m> 61, -5, de, na = -2)) %>% 
#     
#     mutate(de = fifelse(Depth_m < (0), -4, de, na = -2))
#   
#   dt<-dt %>% 
#     mutate(op = fifelse(ODO_mg_L> 50, -5, op, na = -2)) %>% 
#     
#     mutate(op = fifelse(ODO_mg_L < (0), -4, op, na = -2)) 
#   
#   
#   dt<-dt %>% 
#     mutate(ta = fifelse(Chlorophyll_microg_L> 400, -5, ta, na = -2)) %>% 
#     
#     mutate(ta = fifelse(Chlorophyll_microg_L < (0), -4, ta, na = -2)) 
#   
#   dt<-dt %>% 
#     mutate(fd = fifelse(fDOM_RFU> 300, -5, fd, na = -2)) %>% 
#     
#     mutate(fd = fifelse(fDOM_RFU < (0), -4, fd, na = -2)) 
#   
#   #added flagging table
#   #creating vectors to store flag names
#   flagnames <- c("ex", "ct", "op", "ph", "tu", "ta",
#                  "fd", "wi", "de") 
#   
#   #create flagging table for numeric flags
#   L1 <- dt %>%
#     #mutating prevents errors due to mix of character and int classes
#     mutate(ex = as.character(ex)) %>% 
#     mutate(ct = as.character(ct)) %>% 
#     mutate(op = as.character(op)) %>% 
#     mutate(ph = as.character(ph)) %>% 
#     mutate(tu = as.character(tu)) %>% 
#     mutate(ta = as.character(ta)) %>% 
#     mutate(fd = as.character(fd)) %>% 
#     mutate(wi = as.character(wi)) %>% 
#     mutate(de = as.character(de)) %>% 
#     select(c(any_of(flagnames), ID)) %>%
#     pivot_longer(cols = any_of(flagnames),
#                  names_to = "sensor",
#                  values_to = "flag") %>%
#     filter(!is.na(flag))
#   
#   print(L1)
#   
#   current_data$df <- df
#   
#   qc_output$flags <- L1 %>%
#     mutate_all(as.character) %>%
#     mutate(status = "Not evaluated", # Whether L1 flag has been accepted, rejected, or needs to be evaluated
#            flag = case_when(
#              flag == "-5" ~ "Outside high range",
#              flag == "-4" ~ "Outside low range",
#              flag == "-3" ~ "Data rejected due to QAQC",
#              flag == "-2" ~ "Missing Data",
#              flag == "-1" ~ "Optional parameter, not collected",
#              flag == "0" ~ "Passed L1 QC",
#              flag == "1" ~ "Suspect Data",
#              flag == "2" ~ "Reserved for Future Use",
#              T ~ "Other Flags"
#            ))
#   
#   current_site(input$submission_site)
#   current_date_range(paste(min(current_data$df$timestamp),
#                            strftime(max(current_data$df$timestamp), 
#                                     '%Y-%m-%d'),
#                            sep = " to "))
#   
#   # Set min and max for start date input
#   current_min_date(min(current_data$df$timestamp))
#   current_max_date(max(current_data$df$timestamp))
#   
# }

# processCalibrationFile <- function(datapath){
#   
#   # There are two types of calibration files (so far):
#   # 1. SMS and STRI: The first line is "KorEXO Calibration File Export", followed by a blank row
#   # 2. SERC: The first line is the beginning of the calibration data
#   # For the first case, skip the first two rows.
#   
#   calibration_first_row <- read.table(datapath, nrows = 1, sep=",", colClasses = "character")
#   
#   if(calibration_first_row[1,1] == "KorEXO Calibration File Export"){
#     num_rows_to_skip = 2
#   } else if(calibration_first_row[1,1] == "Last Calibration Time="){
#     num_rows_to_skip = 0
#   }
#   
#   cal_raw <- read_csv(datapath, skip = num_rows_to_skip, col_names = F, 
#                       locale = locale(encoding = "Windows-1252")) %>% 
#     rowid_to_column()
#   
#   subtable_rows <- cal_raw %>% 
#     filter(X1 == "----------") %>%
#     select(rowid)
#   
#   subtable_sensors <- cal_raw %>%
#     filter(X1 == "Sensor Type=") %>%
#     select(X2)
#   
#   subtable_index <- cbind(subtable_rows, subtable_sensors) %>%
#     rename(max_rowid = rowid, sensor = X2)
#   
#   cal <- cal_raw %>%
#     filter(X1 != "----------")
#   
#   calibration_log <- data.frame()
#   calibration_points <- data.frame()
#   
#   index <- 0
#   
#   # For each category, extract it's attributes
#   for(i in 1:nrow(subtable_index)){ 
#     print("NEXT")
#     print(i)
#     print(index)
#     
#     next_index <- subtable_index$max_rowid[i]
#     
#     print(next_index)
#     
#     calibration_subset <- cal %>%
#       filter(rowid > index & rowid < next_index)
#     
#     calibration_point_index <- calibration_subset %>% 
#       filter(grepl("\\[Cal Point", X1)) %>%
#       select(X1, rowid)
#     
#     if(nrow(calibration_point_index) != 0){
#       for(j in 1:nrow(calibration_point_index)){
#         
#         if(j == nrow(calibration_point_index)){
#           next_point_index <- max(calibration_subset$rowid) + 1
#         } else {
#           next_point_index <- calibration_point_index$rowid[j + 1]
#         }
#         
#         print("cal points")
#         current_point_index <- calibration_point_index$rowid[j]
#         print(current_point_index)
#         print(next_point_index)
#         
#         calibration_points <- calibration_subset %>%
#           filter(rowid > current_point_index & rowid < next_point_index) %>%
#           select(-rowid) %>%
#           pivot_wider(names_from = X1, values_from = X2) %>%
#           mutate(calibration_point = j,
#                  calibration_id = index + 1) %>%
#           bind_rows(calibration_points)
#       }
#       
#       calibration_points <- calibration_points %>%
#         mutate(submission_id = getSubmissionID())
#       
#       max_calibration_point_rowid <- min(calibration_point_index$rowid)
#     } else {
#       max_calibration_point_rowid <- next_index
#     }
#     
#     calibration_log <- calibration_subset %>%
#       filter(rowid < max_calibration_point_rowid) %>%
#       select(-rowid) %>%
#       pivot_wider(names_from = X1, values_from = X2) %>%
#       mutate(calibration_id = index + 1) %>%
#       bind_rows(calibration_log)
#     
#     index <- next_index
#     
#   }
#   
#   calibration_log <- calibration_log %>%
#     mutate(submission_id = getSubmissionID())
#   
#   colnames(calibration_log) <- processCalibrationHeaders(colnames(calibration_log))
#   colnames(calibration_points) <- processCalibrationHeaders(colnames(calibration_points))
#   
#   # write_csv(calibration_log, "./data/output_cal_log.csv")
#   # write_csv(calibration_points, "./data/output_cal_points.csv")
#   
#   return(list(calibration_log, calibration_points))
# }
# 
# processCalibrationHeaders <- function(cols){
#   
#   new_cols <- gsub(" ", "_",
#                    gsub("=", "", tolower(cols)))
#   
#   return(new_cols)
# }

# checkFileExtensions <- function(){
#   
#   # # Temporary!! delete and uncomment below 
#   # if(any(!grepl("csv", input$fileKorexo$name))){
#   #   
#   #   showModal(modalDialog(
#   #     title = "Invalid File", 
#   #     div(
#   #       "The uploaded Korexo and calibration files must be CSV files. ",
#   #       "Refresh the application to restart the application process."
#   #     ),
#   #     easyClose = TRUE
#   #   ))
#   #   
#   #   return(FALSE)
#   
#   if(is.null(input$fileKorexo$name) | is.null(input$fileCalibration$name)){
# 
#     showModal(modalDialog(
#       title = "Missing file",
#       div("You must upload both a Korexo and calibration file in CSV format."),
#       easyClose = TRUE
#     ))
# 
#     return(FALSE)
# 
#   } else if(any(!grepl("csv", input$fileKorexo$name))){
# 
#     showModal(modalDialog(
#       title = "Invalid Korexo File",
#       div(
#         "The uploaded Korexo and calibration files must be CSV files. ",
#         "Refresh the application to restart the application process."
#       ),
#       easyClose = TRUE
#     ))
# 
#     return(FALSE)
# 
#   } else if (any(!grepl("csv", input$fileCalibration$name))){
#     showModal(modalDialog(
#       title = "Invalid Calibration File",
#       div(
#         "The uploaded Korexo and calibration files must be CSV files.",
#         "Refresh the application to restart the application process."
#       ),
#       easyClose = TRUE
#     ))
# 
#     return(FALSE)
#     
#   } else return(TRUE)
# }


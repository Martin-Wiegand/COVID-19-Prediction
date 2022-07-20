csv_in_file <- function(x){
  file.path(MainFilePath, "csv_in", paste0(x, ".csv"))
}

csv_out_file <- function(x){
  file.path(MasterFilePath, "csv_out", paste0(x, ".csv"))
}

rds_file <- function(x){
  file.path(MasterFilePath, "rds", paste0(x, ".rds"))
}

pdf_file <- function(x){
  file.path(MasterFilePath, "plots", paste0(x, ".pdf"))
}

png_file <- function(x){
  file.path(MasterFilePath, "plots", paste0(x, ".png"))
}

txt_file <- function(x){
  file.path(MasterFilePath, "txt", paste0(x, ".txt"))
}

# ICU Wards
# ICU_check needs to be updated to account for additional list elements 
ICU_wards <- c("ADD NEURO ICU",
               "ADD GENERAL ICU",
               "ADD D4 IDA UNIT",
               "ADD J3 WARD",
               "ADD J3-ITU WARD",
               "ADD J3-ITU WARD")

conv_times <- c("2020-01-01",
                "2020-01-01",
                "2020-04-02",
                "2020-04-09",
                "2020-04-09",
                "2021-01-02") %>% as.Date

re_conv_times <- c("2099-01-01",
                   "2099-01-01",
                   "2099-04-02",
                   "2020-05-22",
                   "2020-05-22",
                   "2099-01-01") %>% as.Date


# Mechanical ventilation

mechanical_ventilation_list <-
  read.csv(csv_in_file("2020-05-14-ventmode_values"),
           stringsAsFactors = FALSE) %>%
  as_tibble

O2_device <- read.csv(csv_in_file("2020-05-05-fsheet_o2device_values"))
O2_device_list <- O2_device

O2_device_list$ventilation <- ifelse(O2_device_list$ventilation %in% c("mechanical_ventilation","tracheostomy"),
               "yes",
               "no")
names(O2_device_list) <- c("measured_value","mechanical_ventilation")


# ECMO

ECMO <- read.csv(csv_in_file("2020-05-01-ecmo_patients"),as.is = T,sep = ",",colClasses = "character")
ECMO_ids <- ECMO$STUDY_SUBJECT_DIGEST %>% as.character

NOT_ECMO <- read.csv(csv_in_file("2020-05-01-transfers_not_ecmo"),as.is = T,sep = ",",colClasses = "character")
NOT_ECMO_ids <- NOT_ECMO$STUDY_SUBJECT_DIGEST %>% as.character


# Load File Names
create_filenames <- function(DateAppendix, stub){
  adt_file <<- paste0(stub, "ADT_V",DateAppendix,".csv",sep="")
  adm_file <<- paste0(stub, "ADM_V",DateAppendix,".csv",sep="")
  fsheet_file <<- paste0(stub, "FSHEET_V",DateAppendix,".csv",sep="")
  fsheetio_file <<- paste0(stub, "FSHEET_IO_V",DateAppendix,".csv",sep="")
  diag_file <<- paste0(stub, "DIAGNOSIS_PL_v",DateAppendix,".csv",sep="")
  radio_file <<- paste0(stub, "RADIOLOGY_V",DateAppendix,".csv",sep="")
  med_file <<- paste0(stub, "MEDICATION_V",DateAppendix,".csv",sep="")
  med_admin_file <<- paste0(stub, "MEDICATION_ADMIN_V",DateAppendix,".csv",sep="")
  medhist_file <<- paste0(stub, "MEDHIST_V",DateAppendix,".csv",sep="")
  tests_file <<- paste0(stub, "TESTS_ALL_V",DateAppendix,".csv",sep="")
  dnacpr_file <<- paste0(stub, "DNACPR_V",DateAppendix,".csv",sep="")
  notes_file <<- paste0(stub, "NOTES_V",DateAppendix,".csv",sep="")
  clinfit_file <<- paste0(stub, "FSHEET_CLINFIT_",DateAppendix,".csv",sep="")
  dnar_file <<- paste0(stub, "DNACPR_V",DateAppendix,".csv",sep="")
}

file_date_rds <- function(x){
  print(file.info(file.path(MasterFilePath, "rds", paste0(x, ".rds")))$mtime)
}

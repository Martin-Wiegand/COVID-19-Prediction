################# LIBRARIES

library("tidyverse")
library("lubridate")

################# SOURCE

MasterFilePathCUH <- "path/to/folder/cuh"
MasterFilePathSDHS <- "/path/to/folder"
is_cuh <- file.exists(MasterFilePathCUH)

if (is_cuh){
  MasterFilePath <- paste0(MasterFilePathCUH, "/")
  MainFilePath <- paste0(MasterFilePath, "Load/Load-MW/")
  RDSFilePath <- paste0(MasterFilePath, "RDS/")
  raw_path <- paste0("Y:/", "raw/")
} else {
  MasterFilePath <- paste0(MasterFilePathSDHS, "/")
  MainFilePath <- paste0(MasterFilePath, "Load/Load-MW/")
  RDSFilePath <- paste0(MasterFilePath, "RDS/")
  raw_path <- paste0(MasterFilePath, "raw/")
}

filepath <- function(x,MFP = MainFilePath){paste0(MFP,x)}
RDSfilepath <- function(x,MFP = RDSFilePath){paste0(MFP,x)}
masterfilepath <- function(x,MFP = MasterFilePath){paste0(MFP,x)}

source(filepath("0-definitions.R"))
source(filepath("0-Auxil.R"))
source(filepath("0-summarise-functions.R"))


################# LOAD RAW

# Create filenames according to pull date

# All files
create_filenames(DateAppendix = "2021-03-21", stub = "")

# Exceptions/ Manual names
clininfo_file <- "CLIN_INFO_NOPID2020-06-24.csv"
medhist_file <- "MEDHIST_V2021-02-09.csv"
med_admin_file <- "MEDICATION_ADMIN_V2021-02-09.csv"
diag_file <- "DIAGNOSIS_PL_V2021-02-09.csv"
tests_file <- "TESTS_ALL_V2021-02-09.csv"

# Pull RAW data
source(filepath("1-load-adt.R"))
source(filepath("1-load-dnar.R"))
source(filepath("1-load-adm.R"))
source(filepath("1-load-clinfit.R"))
source(filepath("1-load-clininfo.R"))
source(filepath("1-load-diag.R"))
source(filepath("1-load-fsheet.R"))
source(filepath("1-load-fsheet_io.R"))
source(filepath("1-load-med.R"))
source(filepath("1-load-med_admin.R"))
source(filepath("1-load-medhist.R"))
source(filepath("1-load-notes.R"))
source(filepath("1-load-radio.R"))
source(filepath("1-load-tests.R"))
source(filepath("1-load-frailty.R"))

# set censoring
tests_raw <- readRDS(rds_file("tests_raw"))
fsheet_raw <- readRDS(rds_file("fsheet_raw"))

# Pull Date
fsheet_raw %>% arrange(desc(MEASURE_TIME))
adt_raw %>% arrange(desc(IN_DTTM))
tests_raw %>% arrange(desc(ResultDate))
med_admin_raw %>% arrange(desc(TimeAdministered))
# This date and time should be the exact time you want to regard the data as ending
min(max(tests_raw$COLLECTED_DATETIME %>% na.omit),max(fsheet_raw$MEASURE_TIME %>% na.omit))
pull_date <- ymd_hms("2021-03-19 22:00:00", tz = "Europe/London") # Pat. 19093BAA6AF36AAA6 discharged time but no discharge event (right before pull)

# Modify data, pull tests, flow sheets vals and fashion plots, create RDS files
source(filepath("2-adt.R"))
source(filepath("2-adm.R"))
source(filepath("2-tests-automated.R"))
source(filepath("2-diag.R"))
source(filepath("2-fsheet.R"))
source(filepath("2-fsheet-io.R"))
source(filepath("2-med.R"))
source(filepath("2-med_admin.R"))
source(filepath("2-notes.R"))
source(filepath("2-check-dnar.R"))
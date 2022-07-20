dnar_raw <-
  read.epic.csv(dnar_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               ORDER_STATUS = "character",
                               DESCRIPTION = "character",
                               PROC_START_TIME = "iso8601space",
                               UPDATE_DATE = "iso8601space"))
dnar_raw %>% summary

saveRDS(dnar_raw, file = rds_file("dnar_raw"))

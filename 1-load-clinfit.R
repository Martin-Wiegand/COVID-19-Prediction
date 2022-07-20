clinfit_raw <-
  read.epic.csv(clinfit_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               MEASURE_TIME = "iso8601space",
                               measured_value = "iso8601space"))

saveRDS(clinfit_raw, file = rds_file("clinfit_raw"))

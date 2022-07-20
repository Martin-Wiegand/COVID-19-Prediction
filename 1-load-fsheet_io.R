fsheetio_raw <-
  read.epic.csv(fsheetio_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               #fsd_id = "character",
                               #line = "character",
                               #flo_meas_id = "character",
                               MEASURE_TIME = "iso8601space",
                               disp_name = "character",
                               FLO_MEAS_NAME = "character",
                               #measured_value = "numeric",
                               meas_comment = "character",
                               template = "character",
                               form = "character"))

fsheetio_raw %>% summary

fsheetio_raw %>%
  group_by(disp_name) %>%
  tally %>%
  print(n = Inf)

saveRDS(fsheetio_raw, file = rds_file("fsheetio_raw"))

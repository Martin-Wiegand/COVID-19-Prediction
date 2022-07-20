fsheet_raw <-
  read.epic.csv(fsheet_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               #fsd_id = "numeric",
                               #line = "integer",
                               #flo_meas_id = "numeric",
                               MEASURE_TIME = "iso8601space",
                               disp_name = "character",
                               measured_value = "character",
                               meas_comment = "character",
                               template = "character",
                               form = "character"))

fsheet_raw %>% summary

# Height flowsheet items occasionally have a meas_comment with something like:
#
# patient estimates 5'8"
#
# This causes problems since " is used to enclose the fields. Particularly bad
# is when the quotes nevertheless balance across the file, meaning the problem
# may not result in an import error.

# check for super long disp_name, which often indicated error in parsing
fsheet_raw %>%
  filter(nchar(disp_name) > 80)

# check for disp names
fsheet_raw %>% group_by(disp_name) %>% tally %>% print(n = Inf)

# checking the end of the file is helpful to spot the error
tail(fsheet_raw)


saveRDS(fsheet_raw, file = rds_file("fsheet_raw"))

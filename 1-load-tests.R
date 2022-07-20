tests_raw <-
  read.epic.csv(tests_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               TestGroupName = "character",
                               TestName = "character",
                               ResultValue = "character",
                               ReferenceLow = "character",
                               ReferenceHigh = "character",
                               ResultUnit = "character",
                               ResultDate = "iso8601space",
                               Method = "character",
                               ORDERING_DEPARTMENT_NAME = "character",
                               COLLECTED_DATETIME = "iso8601space",
                               ORDERED_DATETIME = "iso8601space",
                               RECEIVED_DATETIME = "iso8601space"
                               #OrderProcId = "character"
                               ))

tests_raw %>% summary

saveRDS(tests_raw, file = rds_file("tests_raw"))

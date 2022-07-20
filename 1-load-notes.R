notes_raw <-
  read.epic.csv(notes_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character"))

notes_raw %>% summary
saveRDS(notes_raw, file = rds_file("notes_raw"))

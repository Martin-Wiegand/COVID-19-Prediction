# At the moment using 1-load-frailty instead to load manually corrected STUDY_SUBJECT_DIGEST file (frailty_mod.csv)
clininfo_file <- "CLIN_INFO_NOPID2020-06-24.csv"
clininfo_raw <-
  read.epic.csv(clininfo_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               Date.of.Admission..1st.or.Index.Episode. = "iso8601space"))

clininfo_raw <- clininfo_raw %>%
  mutate(STUDY_SUBJECT_DIGEST = str_replace(STUDY_SUBJECT_DIGEST, "000000000000000000000000000000000000", ""),
         STUDY_SUBJECT_DIGEST = str_replace(STUDY_SUBJECT_DIGEST, "0x", ""))

saveRDS(clininfo_raw,rds_file("clininfo_raw"))

medhist_raw <-
  read.epic.csv(medhist_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               CONTACT_DATE = "character",
                               DX_NAME = "character",
                               MEDICAL_HX_DATE = "character",
                               COMMENTS = "character",
                               MED_HX_ANNOTATION = "character",
                               HX_LNK_ENC_CSN = "character"
                               ))

medhist_raw %>%
  group_by(DX_NAME) %>%
  tally %>%
  arrange(desc(n))

saveRDS(medhist_raw, file = rds_file("medhist_raw"))

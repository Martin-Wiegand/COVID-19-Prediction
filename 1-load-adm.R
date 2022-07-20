adm_raw <-
  read.epic.csv(adm_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               PAT_ENC_CSN = NA,
                               IN_DTTM = "iso8601space",
                               HOSP_DISCH_TIME = "iso8601space",
                               GENDER_DESC = "character",
                               ETHNIC_GROUP_GROUPED = "character",
                               DATE_OF_DEATH = "iso8601space",
                               AGE_AT_ADM = NA,
                               ADM_SERVICE = "character",
                               ADT_DEPARTMENT_NAME = "character",
                               DISCH_DEST = NA,
                               DISCH_DECEASED = NA,
                               READMIT_WITHIN_30 = NA))
adm_raw %>% summary

saveRDS(adm_raw, file = rds_file("adm_raw"))

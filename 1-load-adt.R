adt_raw <-
  read.epic.csv(adt_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               EVENT_TYPE_C = NA,
                               EVENT_TYPE = "character",
                               IN_DTTM = "iso8601space",
                               HOSP_DISCH_TIME = "iso8601space",
                               ADT_DEPARTMENT_NAME = "character",
                               ROOM_NAME = "character",
                               BED_LABEL = "character",
                               ADT_SERV_AREA_NAME = "character",
                               HOSP_SERV_NAME = "character",
                               PAT_ENC_CSN = NA))

adt_raw %>% summary
saveRDS(adt_raw, file = rds_file("adt_raw"))

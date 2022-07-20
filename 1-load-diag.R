diag_raw <-
  read.epic.csv(diag_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               DX_DESCRIPTION = "character",
                               DX_DESC_DISPLAYED = "character",
                               PROBLEM_CMT = "character",
                               DIAGNOSIS_ENTERED_DATE = "iso8601space",
                               DIAGNOSIS_DATE = "iso8601space",
                               RESOLVED_DATE = "iso8601space",
                               DIAGNOSIS_STATUS = "character",
                               ICD10_1 = "character",
                               ICD10_2 = "character",
                               ICD10_3 = "character",
                               ICD10_4 = "character",
                               ICD10_LIST = "character",
                               SNOMED_CONCEPTS = "character"))

diag_raw %>% summary
saveRDS(diag_raw, file = rds_file("diag_raw"))

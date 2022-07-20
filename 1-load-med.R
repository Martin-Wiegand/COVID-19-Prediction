first_admissions <- readRDS(rds_file("first_admissions"))

med_raw <-
  read.table(medication_file,
                skip = 0,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               DrugName = "character",
                               StartDate = "iso8601space",
                               EndDate = "iso8601space",
                               Dose = "character",
                               Strength = "character",
                               DoseUnit = "character",
                               DoseFrequency = "character",
                               RouteOfMedication = "character",
                               InOrOutPatient = "character",
                               FormOfMedication = "character",
                               THERA_CLASS = "character",
                               PHARM_CLASS = "character",
                               PHARM_SUBCLASS = "character",
                               PAT_ENC_CSN_ID = NA,
                               OrderStatusCat = "character",
                               ProviderType = "character"))

med_raw %>% summary

saveRDS(rds_file("medication_data_raw"))

saveRDS(med_raw, file = rds_file("medication_data_raw"))

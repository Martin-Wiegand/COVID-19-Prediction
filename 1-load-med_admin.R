med_admin_raw <-
  read.epic.csv(med_admin_file,
                colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                               TimeAdministered = "iso8601space",
                               DrugName = "character",
                               #DoseAsLabelled = "numeric",
                               #InfusionRate = "numeric",
                               DoseUnitAbbreviated = "character",
                               RouteOfMedicationAbbreviated = "character",
                               DepartmentName = "character",
                               MAR_ENC_CSN = NA,
                               MARAction = "character"))

med_admin_raw %>% summary

saveRDS(med_admin_raw, file = rds_file("med_admin_raw"))

diag_raw <- readRDS(rds_file("diag_raw"))
adm_data <- readRDS(rds_file("adm"))
medhist_raw <- readRDS(rds_file("medhist_raw"))

# this will be very incomplete
diag_raw %>%
  filter(str_detect(DX_DESCRIPTION, coll("PREG", ignore_case = TRUE)))

# likely extraordinarily incomplete data
diag_raw %>% filter(str_detect(DX_DESCRIPTION, coll("SMOK", ignore_case = TRUE)))

###############################################################################
# Explore data looking for COVID-19 in problem list

covid_diag_possibly <- diag_raw %>%
  filter(str_detect(DX_DESCRIPTION, coll("COVID", ignore_case = TRUE)) |
           str_detect(DX_DESCRIPTION, coll("CORONAV", ignore_case = TRUE)) |
           str_detect(DX_DESCRIPTION, coll("Cov", ignore_case = TRUE)))

covid_diagnosis_combinations <- covid_diag_possibly %>%
  select(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
  group_by(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
  tally %>%
  arrange(desc(n))
# Note DX_DESC_DISPLAYED is not the same for all DX_DESCRIPTION

#write.csv(covid_diagnosis_combinations %>% select(-n),
#          file = csv_out_file("2020-12-18-covid_diagnosis_combinations"),
#          row.names = FALSE)

covid_diagnosis_combinations_input <-
  read.csv(file = csv_in_file("2020-12-18-covid_diagnosis_combinations"),
           stringsAsFactors = FALSE,
           colClasses = c(ICD10_4 = "character"))

covid_diag_possibly_annotated <- covid_diag_possibly %>%
  left_join(covid_diagnosis_combinations_input,
            by = c("DX_DESCRIPTION", "ICD10_1", "ICD10_2", "ICD10_3", "ICD10_4"))

covid_diag_unannotated <- covid_diag_possibly_annotated %>%
  filter(is.na(covid_diag_confirmed))

# write.csv(covid_diag_unannotated %>%
#             select(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
#             group_by(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
#             tally %>%
#             arrange(desc(n)) %>%
#             select(-n),
#           file = csv_out_file("2020-12-18-covid_diag_unannotated"),
#           row.names = FALSE)

  # check that all possible COVID diagnosis codes are annotated
if (covid_diag_unannotated %>% nrow != 0){
  cat("***** WARNING **** unannotated COVID diagnosis codes")
} else {
  covid_diag <- covid_diag_possibly_annotated
}

covid_diag <- covid_diag %>%
  filter(DIAGNOSIS_STATUS != "Deleted") %>%
  filter(RESOLVED_DATE != DIAGNOSIS_DATE | DIAGNOSIS_STATUS != "Resolved")

saveRDS(covid_diag, file = rds_file("covid_diag"))

###############################################################################

diag_raw %>%
  filter(str_detect(DX_DESCRIPTION, coll("COVID", ignore_case = TRUE)) |
           str_detect(DX_DESCRIPTION, coll("CORONAV", ignore_case = TRUE)) |
           str_detect(DX_DESCRIPTION, coll("Cov", ignore_case = TRUE))) %>%
  filter(!str_detect(ICD10_LIST, "U07.1")) %>%
  group_by(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
  tally

# IMO0001 seems to no COVID-19 - ?possibly code used when clinician enters
# completely free text? Has a bunch of random stuff, mostly
# "Endotracheally intubated"
diag_raw %>%
  filter(str_detect(ICD10_LIST, "IMO0001")) %>%
  select(DX_DESCRIPTION, DX_DESC_DISPLAYED, ICD10_LIST) %>%
  print(n = Inf)

# U07.1 = "Use this code when COVID-19 has been confirmed by laboratory testing
#           irrespective of severity of clinical signs or symptoms"
#
# U07.2 = "Use this code when COVID-19 is diagnosed clinically or
#          epidemiologically but laboratory testing is inconclusive or not
#          available"
#
# B34.2 = "Coronavirus infection, unspecified site
#          EXCL COVID-19, virus identified U07.1
#          EXCL COVID-19, virus not identified U07.2
#          EXCL SARS U04.9"
# B97.2 = "Coronavirus as the cause of diseases classified to other chapters"
#
# R68.8 = "Other specified general symptoms and signs"

# ICD10 codes:
icd10_confirmed_covid19 <- c("U07.1")

# Treat B34.2 as suspected?
diag_raw %>%
  filter(str_detect(ICD10_LIST, "B34.2")) %>%
  select(DX_DESCRIPTION, DX_DESC_DISPLAYED, ICD10_LIST)

# Treat B97.2 as ??
diag_raw %>%
  filter(str_detect(ICD10_LIST, "B97.2")) %>%
  group_by(DX_DESCRIPTION, DX_DESC_DISPLAYED, ICD10_LIST) %>%
  tally %>%
  print(n = Inf)

# R68.8 is not safe to assume it is COVID-19-related, since it sometimes
# contains unrelated stuff, like "Multiple organ failures".
diag_raw %>%
  filter(str_detect(ICD10_LIST, "R68.8")) %>%
  group_by(DX_DESCRIPTION, DX_DESC_DISPLAYED, ICD10_LIST) %>%
  tally

icd10_suspected_covid19 <- c("R68.8")

###############################################################################
# Investigate Problem List for those never tested

covid_raw <- readRDS(file = rds_file("covid_raw"))

# COPIED FROM 2-test, probably not ideal to do this
patients_never_tested <-
  adm_data %>%
  filter(!STUDY_SUBJECT_DIGEST %in% covid_raw$STUDY_SUBJECT_DIGEST) %>%
  pull(STUDY_SUBJECT_DIGEST)

# All patients never tested have an icd10_confirmed_covid19 code in Problem
# List
diag_raw %>%
  filter(STUDY_SUBJECT_DIGEST %in% patients_never_tested) %>%
  mutate(icd10_confirmed_covid19 =
           str_detect(ICD10_LIST, icd10_confirmed_covid19)) %>%
  group_by(STUDY_SUBJECT_DIGEST) %>%
  summarise(icd10_confirmed_covid19 = any(icd10_confirmed_covid19))

# One of them never ends up in ICU
adt_data %>%
  filter(str_detect(STUDY_SUBJECT_DIGEST, coll("FAB")))

# Manually, check that the above code works correctly
diag_raw %>%
  filter(STUDY_SUBJECT_DIGEST %in% patients_never_tested[9]) %>%
  filter(str_detect(ICD10_LIST, icd10_confirmed_covid19)) %>%
  select(DX_DESC_DISPLAYED, ICD10_LIST)

###############################################################################
# Investigate Problem list for those without a positive SARS-CoV-2 test

# COPIED FROM 2-tests
# Patients never tested positiv in hospital
patients_never_covid_positiv <-
  covid_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  filter(all(ResultValue == "Not detected")) %>%
  pull(STUDY_SUBJECT_DIGEST)

# All patients never tested have an icd10_confirmed_covid19 code in Problem
# List
diag_raw %>%
  filter(STUDY_SUBJECT_DIGEST %in% patients_never_covid_positiv) %>%
  mutate(icd10_confirmed_covid19 =
           str_detect(ICD10_LIST, icd10_confirmed_covid19)) %>%
  group_by(STUDY_SUBJECT_DIGEST) %>%
  summarise(icd10_confirmed_covid19 = any(icd10_confirmed_covid19))

# One parient with only B34.2 in Problem List
diag_raw %>%
  filter(str_detect(STUDY_SUBJECT_DIGEST, "0x0C0628FF5FB9F6D9FDC")) %>%
  select(DX_DESC_DISPLAYED, DX_DESCRIPTION, ICD10_LIST)

###############################################################################

hypertension_diag_possibly <- diag_raw %>%
  filter(str_detect(DX_DESCRIPTION, coll("hypertension", ignore_case = TRUE)))

hypertension_diagnosis_combinations <- hypertension_diag_possibly %>%
  select(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
  group_by(DX_DESCRIPTION, ICD10_1, ICD10_2, ICD10_3, ICD10_4) %>%
  tally %>%
  arrange(desc(n))
# Note DX_DESC_DISPLAYED is not the same for all DX_DESCRIPTION

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
hypertension_diag <- diag_raw %>%
  filter(grepl("I10|I11|I12|I13|I14|I15", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(hypertension_diag, file = rds_file("hypertension_diag"))


# Actually cleaner to just use ICD-10 code
# What to do with resolved?
diabetes_diag <- diag_raw %>%
  filter(grepl("E10|E11|E12|E13|E14", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(diabetes_diag, file = rds_file("diabetes_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
dyslipidaemia_diag <- diag_raw %>%
  filter(grepl("E78.0|E78.1|E78.2|E78.3|E78.4|E78.5|E78.6|E78.7|E78.8|E78.9|E88.81", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(dyslipidaemia_diag, file = rds_file("dyslipidaemia_diag"))


# Actually cleaner to just use ICD-10 code
# What to do with resolved?
liver_diag <- diag_raw %>%
  filter(grepl("K70|K71|K72|K73|K74|K75|K76|K77", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(liver_diag, file = rds_file("liver_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
asthma_diag <- diag_raw %>%
  filter(grepl("J45", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(asthma_diag, file = rds_file("asthma_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
malignancy_nh_diag <- diag_raw %>%
  filter(grepl("C0|C1|C2|C3|C4|C5|C6|C7", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(malignancy_nh_diag, file = rds_file("malignancy_nh_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
malignancy_h_diag <- diag_raw %>%
  filter(grepl("C8|C9", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(malignancy_h_diag, file = rds_file("malignancy_h_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
stroke_diag <- diag_raw %>%
  filter(grepl("I63|I65|I66", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(stroke_diag, file = rds_file("stroke_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
respiratory_diag <- diag_raw %>%
  filter(grepl("J41|J42|J43|J44|J47|J6|J7|I27", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(respiratory_diag, file = rds_file("respiratory_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
kidney_diag <- diag_raw %>%
  filter(grepl("N18.1|N18.2|N18.3|N18.4|N18.5|N18.6|N18.9|I13", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(kidney_diag, file = rds_file("kidney_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
heart_list <- c("I20","I21","I22","I23","I24","I25")

heart_diag <- diag_raw %>%
  filter(grepl("I20|I21|I22|I23|I24|I25|I34|I35|I36|I37|I42|I43|I44|I50", ICD10_LIST) |
           ICD10_1 %in% heart_list |
           ICD10_2 %in% heart_list |
           ICD10_3 %in% heart_list |
           ICD10_4 %in% heart_list) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(heart_diag, file = rds_file("heart_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
dementia_diag <- diag_raw %>%
  filter(grepl("F01|F02|F03|G30|G31|F10.97|F19.97|F10.27", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(dementia_diag, file = rds_file("dementia_diag"))

# Actually cleaner to just use ICD-10 code
# What to do with resolved?
immunocompromised_diag <- diag_raw %>%
  filter(grepl("D80|D81|D82|D83|D84|D8", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(immunocompromised_diag, file = rds_file("immunocompromised_diag"))


endocrine_diag <- diag_raw %>%
  filter(grepl("E00|E01|E02|E03|E04|E05|E06|E07|E20|E21|E22|E23|E24|E25|E26|E27|E28|E29|E30|E31|E32|E33|E34|E35|E89.0|E89.1|E89.2|E89.3|E89.4|E89.5|E89.6", ICD10_LIST)) %>%
  filter(DIAGNOSIS_STATUS != "Deleted")

saveRDS(endocrine_diag, file = rds_file("endocrine_diag"))

##
copd_diag <- diag_raw %>% filter(grepl("J44",ICD10_LIST))
saveRDS(copd_diag, file = rds_file("copd_diag"))


## Combination of 
combination_diag <- diag_raw %>%
  mutate(DIAGNOSIS = case_when(grepl("I10|I11|I12|I13|I14|I15", ICD10_LIST) ~ "HYPERTENSION",
                               grepl("E10|E11|E12|E13|E14", ICD10_LIST) ~ "DIABETES",
                               grepl("K70|K71|K72|K73|K74|K75|K76|K77", ICD10_LIST) ~ "LIVER_DISEASE",
                               grepl("J45", ICD10_LIST) ~ "ASTHMA",
                               grepl("C0|C1|C2|C3|C4|C5|C6|C7", ICD10_LIST) ~ "MALIGNANCY_NH",
                               grepl("C8|C9", ICD10_LIST) ~ "MALIGNANCY_H",
                               grepl("I63|I65|I66", ICD10_LIST) ~ "STROKE",
                               grepl("J41|J42|J43|J44|J47|J6|J7|I27", ICD10_LIST) ~ "RESPIRATORY_DISEASE",
                               grepl("N18.1|N18.2|N18.3|N18.4|N18.5|N18.6|N18.9|I13", ICD10_LIST) ~ "RENAL_DISEASE",
                               grepl("D80|D81|D82|D83|D84|D85", ICD10_LIST) ~ "IMMUNOCOMPROMISED",
                               grepl("E00|E01|E02|E03|E04|E05|E06|E07|E20|E21|E22|E23|E24|E25|E26|E27|E28|E29|E30|E31|E32|E33|E34|E35|E89.0|E89.1|E89.2|E89.3|E89.4|E89.5|E89.6",ICD10_LIST) ~ "ENDOCRINE DISEASE",
                               (grepl("I20|I21|I22|I23|I24|I25|I34|I35|I36|I37|I42|I43|I44|I50", ICD10_LIST) |
                                  ICD10_1 %in% heart_list |
                                  ICD10_2 %in% heart_list |
                                  ICD10_3 %in% heart_list |
                                  ICD10_4 %in% heart_list) ~ "HEART_DISEASE",
                               grepl("F01|F02|F03|G30|G31|F10.97|F19.97|F10.27", ICD10_LIST) ~ "DEMENTIA")) %>%
  filter(!is.na(DIAGNOSIS))

saveRDS(combination_diag,rds_file("combination_diag"))



# combination_diag <- diag_raw %>%

# Current ICD 10 List

hypertension_icd10 <- "I10|I11|I12|I13|I14|I15"
diabetes_icd10 <- "E10|E11|E12|E13|E14"
liver_disease_icd10 <- "K70|K71|K72|K73|K74|K75|K76|K77"
asthma_icd10 <- "J45"
malignancy_nh_icd10 <- "C0|C1|C2|C3|C4|C5|C6|C7"
malignancy_h_icd10 <- "C8|C9"
stroke_icd10 <- "I63|I65|I66"
respiratory_disease_icd10 <- "J41|J42|J43|J44|J47|J6|J7|I27"
renal_disease_icd10 <- "N18.1|N18.2|N18.3|N18.4|N18.5|N18.6|N18.9|I13"
immunocompromised_icd10 <- "D80|D81|D82|D83|D84|D85"
endocrine_disease_icd10 <- "E00|E01|E02|E03|E04|E05|E06|E07|E20|E21|E22|E23|E24|E25|E26|E27|E28|E29|E30|E31|E32|E33|E34|E35|E89.0|E89.1|E89.2|E89.3|E89.4|E89.5|E89.6"
heart_disease_icd10 <- "I20|I21|I22|I23|I24|I25|I34|I35|I36|I37|I42|I43|I44|I50"
dementia_icd10 <- "F01|F02|F03|G30|G31|F10.97|F19.97|F10.27"

medhist_combination = 
medhist_raw %>% 
  mutate(HYPERTENSION = grepl(hypertension_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(DIABETES = grepl(diabetes_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(LIVER_DISEASE = grepl(liver_disease_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(ASTHMA = grepl(asthma_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(MALIGNANCY_NH = grepl(malignancy_nh_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(MALIGNANCY_H = grepl(malignancy_h_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(STROKE = grepl(stroke_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(RESPIRATORY_DISEASE = grepl(respiratory_disease_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(RENAL_DISEASE = grepl(renal_disease_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(IMMUNOCOMPROMISED = grepl(immunocompromised_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(ENDOCRINE_DISEASE = grepl(endocrine_disease_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(HEART_DISEASE = grepl(heart_disease_icd10,CURRENT_ICD10_LIST,ignore.case = T)) %>%
  mutate(DEMENTIA = grepl(dementia_icd10,CURRENT_ICD10_LIST,ignore.case = T))  %>%
  select(STUDY_SUBJECT_DIGEST,CONTACT_DATE,HYPERTENSION:DEMENTIA)

# Remove rows without any diagnosis matches
medhist_combination <-
medhist_combination %>% filter_at(vars(HYPERTENSION:DEMENTIA),any_vars(.))


combination_diag <- diag_raw %>%
  mutate(HYPERTENSION = grepl(hypertension_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(DIABETES = grepl(diabetes_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(LIVER_DISEASE = grepl(liver_disease_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(ASTHMA = grepl(asthma_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(MALIGNANCY_NH = grepl(malignancy_nh_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(MALIGNANCY_H = grepl(malignancy_h_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(STROKE = grepl(stroke_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(RESPIRATORY_DISEASE = grepl(respiratory_disease_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(RENAL_DISEASE = grepl(renal_disease_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(IMMUNOCOMPROMISED = grepl(immunocompromised_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(ENDOCRINE_DISEASE = grepl(endocrine_disease_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(HEART_DISEASE = grepl(heart_disease_icd10,ICD10_LIST,ignore.case = T)) %>%
  mutate(DEMENTIA = grepl(dementia_icd10,ICD10_LIST,ignore.case = T))

# Remove deleted patientsdiagnoses + same day resolves
combination_diag <- combination_diag %>%
  filter(DIAGNOSIS_STATUS != "Deleted") %>%
  filter(RESOLVED_DATE != DIAGNOSIS_DATE | DIAGNOSIS_STATUS != "Resolved") %>% 
  select(STUDY_SUBJECT_DIGEST,DIAGNOSIS_DATE,HYPERTENSION:DEMENTIA) 

# Remove rows without any diagnosis matches
combination_diag <-
  combination_diag %>% filter_at(vars(HYPERTENSION:DEMENTIA),any_vars(.))

# Join med hist and diag data
medhist_diag_joined <-
medhist_combination %>% rename(DIAGNOSIS_DATE = CONTACT_DATE) %>%
  mutate(ORIGIN = "MEDHIST") %>%
  mutate(DIAGNOSIS_DATE = as.POSIXct(DIAGNOSIS_DATE)) %>%
  add_row(combination_diag %>% mutate(ORIGIN = "DIAG"))


saveRDS(medhist_diag_joined,rds_file("medhist_diag_joined"))


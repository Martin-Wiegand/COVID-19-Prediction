# LOAD
med_raw <- readRDS(rds_file("med_raw"))

write.csv(table(med_raw$Order_Class),csv_out_file("PTA_meds_class"))

# ?maybe
pta_meds <- med_raw %>%
  filter(ProviderType == "Documenting provider")

dose_frequency_tally <- pta_meds %>%
  group_by(DoseFrequency) %>%
  tally %>%
  print(n = Inf)

write.csv(dose_frequency_tally,
          file = csv_out_file("2021-03-22-pta_meds_dose_frequency_tally"),
          row.names = FALSE)

#-------------------------------
drugs_to_check_for <-
  c("ramipril",
    "candesartan",
    "amlodipine",
    "bisoprolol",
    "atorvastatin",
    "metformin",
    "gliclazide",
    "budesonide",
    "beclometasone",
    "ibuprofen",
    "prednisolone",
    "methotrexate")
# antihypertensives, antibacterials and antibiotics

check_any_drug <- function(name){
  med_raw %>%
    filter(str_detect(DrugName, coll(name, ignore_case = TRUE))) %>%
    group_by(DrugName, RouteOfMedication) %>%
    tally
}

# run this once
drugs_to_check_for_results <- lapply(drugs_to_check_for, check_any_drug)
i <- 0

# run this repeatedly to work through list
i <- i + 1; drugs_to_check_for[i]; drugs_to_check_for_results[i]



# ------- STEROIDS
med_raw %>%
  filter(grepl("HYDROCORTISONE|PREDNISOLONE|DEXAMETHASONE|METHYLPREDNISOLONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

steroids_presc_data <- med_raw %>%
  filter(grepl("HYDROCORTISONE|PREDNISOLONE|DEXAMETHASONE|METHYLPREDNISOLONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("MOUTHWASH",DrugName,ignore.case = T)) 
steroids_presc_data$DrugName %>% table

saveRDS(steroids_presc_data,
        file = rds_file("steroids_presc"))


# ------- ACE INHIBITORS
med_raw %>%
  filter(grepl("PRIL",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

# Only want inhalation version
ace_presc_data <- med_raw %>%
  filter(grepl("CAPTOPRIL|ENALAPRIL|PERINDOPRIL|LISINOPRIL|RAMIPRIL",DrugName,ignore.case = T))
ace_presc_data$DrugName %>% table

saveRDS(ace_presc_data,
        file = rds_file("ace_presc"))



# ------- ANGIOTENSION BLOCKERS
med_raw %>%
  filter(grepl("candesartan|irbesartan|olmesartan|losartan|valsartan|telmisartan|eprosartan|artan",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

arb_presc_data <- med_raw %>%
  filter(grepl("CANDESARTAN|IRBESARTAN|LOSARTAN|OLMESARTAN",DrugName,ignore.case = T))
arb_presc_data$DrugName %>% table

saveRDS(arb_presc_data,
        file = rds_file("arb_presc"))

# ------- Vasopressor
med_raw %>%
  filter(grepl("Norepinephrine|epinephrine|Vasopressin|METHYLENE|dopamine|Phenylephrine|Dobutamine|ARGIPRESSIN",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

vasopressor_presc_data <- med_raw %>%
  filter(grepl("NORADRENALINE|METARAMINOL|METHYLENE|PHENYLEPHRINE|ARGIPRESSIN|VASOPRESSIN",DrugName,ignore.case = T))
vasopressor_presc_data$DrugName %>% table

saveRDS(vasopressor_presc_data,
        file = rds_file("vasopressor_presc"))


# ------- FLUDOCORTISONE
med_raw %>%
  filter(grepl("FLUDROCORTISONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

fludrocortisone_presc_data <- med_raw %>%
  filter(grepl("FLUDROCORTISONE",DrugName,ignore.case = T))
fludrocortisone_presc_data$DrugName %>% table

saveRDS(fludrocortisone_presc_data,
        file = rds_file("fludrocortisone_presc"))


# ------- PREDNISOLONE
med_raw %>%
  filter(grepl("PREDNISOLONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

prednisolone_presc_data <- med_raw %>%
  filter(grepl("PREDNISOLONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("METH",DrugName,ignore.case = T)) %>%
  filter(!grepl("MOUTHWASH",DrugName,ignore.case = T)) %>%
  filter(!grepl("EYE DROPS",DrugName,ignore.case = T)) %>%
  filter(!grepl("OINTMENT",DrugName,ignore.case = T)) %>%
  filter(!grepl("CREAM",DrugName,ignore.case = T)) %>%
  filter(!grepl("EAR DROPS",DrugName,ignore.case = T))

saveRDS(prednisolone_presc_data,
        file = rds_file("prednisolone_presc"))


# ------- BETA BLOCKERS
med_raw %>%
  filter(grepl("BISOPROLOL|ATENOL|PROPRANOLOL|CARVEDILOL",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

beta_blockers_presc_data <- med_raw %>%
  filter(grepl("BISOPROLOL",DrugName,ignore.case = T)) %>%
  filter(!grepl("ATENOL",DrugName,ignore.case = T)) %>%
  filter(!grepl("PROPRANOLOL",DrugName,ignore.case = T)) %>%
  filter(!grepl("CARVEDILOL",DrugName,ignore.case = T))

saveRDS(beta_blockers_presc_data,
        file = rds_file("beta_blockers_presc"))



# ------- Inotropes
med_raw %>%
  filter(grepl("ENOXIMONE|DOBUTAMINE|ADRENALINE|DOPAMINE|MILRINONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

inotropes_presc_data <- med_raw %>%
  filter(grepl("ENOXIMONE|DOBUTAMINE|ADRENALINE|DOPAMINE|MILRINONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("NORADRENALINE",DrugName,ignore.case = T))
inotropes_presc_data$DrugName %>% table

saveRDS(inotropes_presc_data,
        file = rds_file("inotropes_presc"))

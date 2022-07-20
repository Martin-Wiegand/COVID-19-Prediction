# LOAD
med_admin_raw <- readRDS(file = rds_file("med_admin_raw"))
include_list <- read.csv(file = csv_in_file("MEDICATION_MARACTION"))

#######

drugs_to_check_for <-
c(# inotropes and vasopressors
  "metaraminol",
  "noradrenaline",
  "adrenaline",
  "dobutamine",
  # paralysis agents
  "suxamethonium",
  "atracurium",
  "rocuronium",
  "vecuronium",
  # sedatives and analgesics
  "propofol",
  "remifentanil",
  "fentanyl",
  "morphine",
  "diamorphine",
  "oxycodone",
  "tramadol",
  "codeine",
  "clonidine",
  "dexmedetomidine",
  # steroids
  "hydrocortisone",
  "prednisolone",
  "dexamethasone",
  # Nebulisers
  "epoprostenol",
  "salbutamol",
  # antihypertensives,
  "ramipril",
  # antibacterials,
  # antibiotics
  "tazobactam",
  # other
  "ibuprofen",
  "bisoprolol",
  "fostair")

check_any_drug <- function(name){
  med_admin_raw %>%
    filter(str_detect(DrugName, coll(name, ignore_case = TRUE))) %>%
    group_by(DrugName, RouteOfMedicationAbbreviated) %>%
   tally
}

# run this once
drugs_to_check_for_results <- lapply(drugs_to_check_for, check_any_drug)
i <- 0

# run this repeatedly to work through list
i <- i + 1; drugs_to_check_for[i]; drugs_to_check_for_results[i]





# ------- PULL DRUGS

# Show all drugs
med_admin_raw$DrugName %>% table


# ------- NORADRENALINE
noradrenaline_data <- med_admin_raw %>% filter(grepl("NORADRENALINE",DrugName))
noradrenaline_data$DrugName %>% table
noradrenaline_data$DoseUnitAbbreviated %>% table


saveRDS(noradrenaline_data,
        file = rds_file("noradrenaline"))

# ------- ADRENALINE
adrenaline_data <- med_admin_raw %>% filter(grepl("\\<ADRENALINE",DrugName))
adrenaline_data$DrugName %>% table
adrenaline_data$DoseUnitAbbreviated %>% table

saveRDS(adrenaline_data,
        file = rds_file("adrenaline"))

# ------- TOCILIZUMAB
tocilizumab_data <- med_admin_raw %>% filter(grepl("\\<TOCILIZUMAB",DrugName))
tocilizumab_data$DrugName %>% table
tocilizumab_data$DoseUnitAbbreviated %>% table

saveRDS(tocilizumab_data,
        file = rds_file("tocilizumab"))

# ------- clazakizumab/sirukumab/olokizumab
clazakizumab_data <- med_admin_raw %>% filter(grepl("\\<clazakizumab",DrugName,ignore.case = T))
clazakizumab_data$DrugName %>% table
clazakizumab_data$DoseUnitAbbreviated %>% table

saveRDS(clazakizumab_data,
        file = rds_file("clazakizumab"))

# ------- SARILUMAB
sarilumab_data <- med_admin_raw %>% filter(grepl("\\<SARILUMAB",DrugName))
sarilumab_data$DrugName %>% table
sarilumab_data$DoseUnitAbbreviated %>% table

saveRDS(tocilizumab_data,
        file = rds_file("tocilizumab"))

# ------- EPOPROSTENOL
med_admin_raw %>%
  filter(str_detect(DrugName, coll("epo", ignore_case = TRUE))) %>%
  group_by(DrugName) %>%
  tally

# Only want inhalation version
epoprostenol_data <- med_admin_raw %>%
  filter(DrugName == "EPOPROSTENOL INHALATION")
epoprostenol_data$DrugName %>% table
epoprostenol_data$DoseUnitAbbreviated %>% table

saveRDS(epoprostenol_data,
        file = rds_file("epoprostenol"))

# ------- ATRACURIUM
med_admin_raw %>%
  filter(str_detect(DrugName, coll("atrac", ignore_case = TRUE))) %>%
  group_by(DrugName) %>%
  tally

atracurium_data <- med_admin_raw %>% filter(grepl("ATRACURIUM",DrugName))
atracurium_data$DrugName %>% table
atracurium_data$DoseUnitAbbreviated %>% table

saveRDS(atracurium_data,
        file = rds_file("atracurium"))

# ------- ROCURIUM
med_admin_raw %>%
  filter(str_detect(DrugName, coll("rocu", ignore_case = TRUE))) %>%
  group_by(DrugName) %>%
  tally

rocuronium_data <- med_admin_raw %>% filter(grepl("ROCURONIUM",DrugName))
rocuronium_data$DrugName %>% table
rocuronium_data$DoseUnitAbbreviated %>% table

png(png_file("Medications/Rocuronium"),
    width = 1200,
    height = 800)
rocuronium_data$DoseAsLabelled %>% as.numeric %>% hist
dev.off()

saveRDS(rocuronium_data,
        file = rds_file("rocuronium"))

# ------- VECURONIUM
# search terms have not been manually checked
med_admin_raw %>%
  filter(str_detect(DrugName, coll("vecur", ignore_case = TRUE))) %>%
  group_by(DrugName) %>%
  tally

vecuronium_data <- med_admin_raw %>% filter(grepl("VECURONIUM",DrugName))
vecuronium_data$DrugName %>% table
vecuronium_data$DoseUnitAbbreviated %>% table

png(png_file("Medications/Vecuronium"),
    width = 1200,
    height = 800)
vecuronium_data$DoseAsLabelled %>% as.numeric %>% hist
dev.off()

saveRDS(vecuronium_data,
        file = rds_file("vecuronium"))

# ------- FUROSEMIDE
# search terms have not been manually checked
furosemide_data <- med_admin_raw %>% filter(grepl("FUROS",DrugName))

furosemide_data %>%
  group_by(DrugName) %>%
  tally

saveRDS(furosemide_data,
        file = rds_file("furosemide"))

# ------- PHENYLEPHRINE
# search terms have not been manually checked
med_admin_raw %>%
  filter(str_detect(DrugName, coll("phenylep", ignore_case = TRUE))) %>%
  group_by(DrugName) %>%
  tally

phenylephrine_data <- med_admin_raw %>% filter(grepl("PHENYLEPHRINE",DrugName))
phenylephrine_data$DrugName %>% table
phenylephrine_data$DoseUnitAbbreviated %>% table

png(png_file("Medications/Phenylephrine"),
    width = 1200,
    height = 800)
phenylephrine_data$DoseAsLabelled %>% as.numeric %>% hist
dev.off()

saveRDS(phenylephrine_data,
        file = rds_file("phenylephrine"))





# ------- ACE INHIBITORS
# search terms have not been manually checked
med_admin_raw %>%
  filter(grepl("PRIL",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

# Only want inhalation version
ace_data <- med_admin_raw %>%
  filter(grepl("CAPTOPRIL|ENALAPRIL|PERINDOPRIL|LISINOPRIL|RAMIPRIL",DrugName,ignore.case = T))
ace_data$DrugName %>% table
ace_data$DoseUnitAbbreviated %>% table

write.csv(file = csv_out_file("ACE_INHIBITORS_INCLUDED_MEDS"),x = ace_data$DrugName %>% table %>% cbind)


saveRDS(ace_data,
        file = rds_file("ace"))



# ------- ANGIOTENSION BLOCKERS
# search terms have not been manually checked
med_admin_raw %>%
  filter(grepl("candesartan|irbesartan|olmesartan|losartan|valsartan|telmisartan|eprosartan|artan",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

arb_data <- med_admin_raw %>%
  filter(grepl("CANDESARTAN|IRBESARTAN|LOSARTAN|OLMESARTAN",DrugName,ignore.case = T))
arb_data$DrugName %>% table
arb_data$DoseUnitAbbreviated %>% table

write.csv(file = csv_out_file("ARB_INCLUDED_MEDS"),x = arb_data$DrugName %>% table %>% cbind)


saveRDS(arb_data,
        file = rds_file("arb"))



# ------- FLUDROCORTISONE
# search terms have not been manually checked

med_admin_raw %>%
  filter(grepl("FLUDROCORTISONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

fludrocortisone_data <- med_admin_raw %>%
  filter(grepl("FLUDROCORTISONE",DrugName,ignore.case = T))
fludrocortisone_data$DrugName %>% table
fludrocortisone_data$DoseUnitAbbreviated %>% table

write.csv(file = csv_out_file("FLUDROCORTISONE_INCLUDED_MEDS"),x = fludrocortisone_data$DrugName %>% table %>% cbind)


saveRDS(fludrocortisone_data,
        file = rds_file("fludrocortisone"))






# ------- Spirolactone
# search terms have not been manually checked

med_admin_raw %>%
  filter(grepl("spironolactone",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

spironalactone_data <- med_admin_raw %>%
  filter(grepl("SPIRONOLACTONE",DrugName,ignore.case = T)) 

spironalactone_data$DrugName %>% table
spironalactone_data$DoseUnitAbbreviated %>% table

  write.csv(file = csv_out_file("SPIRONOLACTONE_INCLUDED_MEDS"),x = spironalactone_data$DrugName %>% table %>% cbind)

saveRDS(spironalactone_data,
        file = rds_file("spironalactone"))



# ------- PREDNISOLONE
med_admin_raw %>%
  filter(grepl("PREDNISOLONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

# DrugName == "DEXAMETHASONE TABLETS FOR OUTPATIENT DOSING" should not
# be included, but this was at DepartmentName == "CUH NUFFIELD CAMBRIDGE"
# so will be excluded anyway from our analysis
prednisolone_data <- med_admin_raw %>%
  filter(grepl("PREDNISOLONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("METH",DrugName,ignore.case = T)) %>%
  filter(!grepl("MOUTHWASH",DrugName,ignore.case = T)) %>%
  filter(!grepl("EYE DROPS",DrugName,ignore.case = T)) %>%
  filter(!grepl("OINTMENT",DrugName,ignore.case = T)) %>%
  filter(!grepl("CREAM",DrugName,ignore.case = T)) %>%
  filter(!grepl("EAR DROPS",DrugName,ignore.case = T)) %>%
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) 

#write.csv(file = csv_out_file("STEROIDS_INCLUDED_MEDS"),x = steroids_data$DrugName %>% table %>% cbind)
prednisolone_data$DoseUnitAbbreviated %>% table

saveRDS(prednisolone_data,
        file = rds_file("prednisolone"))


# ------- FLUDRO
med_admin_raw %>%
  filter(grepl("FLUDRO",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

# DrugName == "DEXAMETHASONE TABLETS FOR OUTPATIENT DOSING" should not
# be included, but this was at DepartmentName == "CUH NUFFIELD CAMBRIDGE"
# so will be excluded anyway from our analysis
fludrocortisone_data <- med_admin_raw %>%
  filter(grepl("FLUDROCORTISONE",DrugName,ignore.case = T)) 
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) 

#write.csv(file = csv_out_file("STEROIDS_INCLUDED_MEDS"),x = steroids_data$DrugName %>% table %>% cbind)
  fludrocortisone_data$DoseUnitAbbreviated %>% table

saveRDS(fludrocortisone_data,
        file = rds_file("fludrocortisone"))



# ------- STEROIDS
med_admin_raw %>%
  filter(grepl("HYDROCORTISONE|PREDNISOLONE|DEXAMETHASONE|METHYLPREDNISOLONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

# DrugName == "DEXAMETHASONE TABLETS FOR OUTPATIENT DOSING" should not
# be included, but this was at DepartmentName == "CUH NUFFIELD CAMBRIDGE"
# so will be excluded anyway from our analysis
steroids_data <- med_admin_raw %>%
  filter(grepl("HYDROCORTISONE|PREDNISOLONE|DEXAMETHASONE|METHYLPREDNISOLONE|FLUDROCORTISONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("MOUTHWASH",DrugName,ignore.case = T)) %>%
  filter(!grepl("EYE DROPS",DrugName,ignore.case = T)) %>%
  filter(!grepl("OINTMENT",DrugName,ignore.case = T)) %>%
  filter(!grepl("CREAM",DrugName,ignore.case = T)) %>%
  filter(!grepl("EAR DROPS",DrugName,ignore.case = T)) %>%
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) 

#write.csv(file = csv_out_file("STEROIDS_INCLUDED_MEDS"),x = steroids_data$DrugName %>% table %>% cbind)
steroids_data$DoseUnitAbbreviated %>% table

saveRDS(steroids_data,
        file = rds_file("steroids"))


# ------- STEROIDS COVID
# DrugName == "DEXAMETHASONE TABLETS FOR OUTPATIENT DOSING" should not
# be included, but this was at DepartmentName == "CUH NUFFIELD CAMBRIDGE"
# so will be excluded anyway from our analysis
steroids_data <- med_admin_raw %>%
  filter(grepl("HYDROCORTISONE|PREDNISOLONE|DEXAMETHASONE|METHYLPREDNISOLONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("MOUTHWASH",DrugName,ignore.case = T)) %>%
  filter(!grepl("EYE DROPS",DrugName,ignore.case = T)) %>%
  filter(!grepl("OINTMENT",DrugName,ignore.case = T)) %>%
  filter(!grepl("CREAM",DrugName,ignore.case = T)) %>%
  filter(!grepl("EAR DROPS",DrugName,ignore.case = T)) %>%
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) 

#write.csv(file = csv_out_file("STEROIDS_INCLUDED_MEDS"),x = steroids_data$DrugName %>% table %>% cbind)
steroids_data$DoseUnitAbbreviated %>% table

saveRDS(steroids_data,
        file = rds_file("steroids_covid"))



# ------- Vasopressor
med_admin_raw %>%
  filter(grepl("Norepinephrine|epinephrine|Vasopressin|METHYLENE|dopamine|Phenylephrine|Dobutamine|ARGIPRESSIN",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

vasopressor_data <- med_admin_raw %>%
  filter(grepl("NORADRENALINE|METARAMINOL|METHYLENE|PHENYLEPHRINE|ARGIPRESSIN|VASOPRESSIN",DrugName,ignore.case = T)) %>%
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) %>%
  filter(RouteOfMedicationAbbreviated %in% c("IV Infusion", "IV Injection", "Intravenous"))

#vasopressor_data$DrugName %>% table
#vasopressor_data$DoseUnitAbbreviated %>% table

write.csv(file = csv_out_file("VASOPRESSOR_INCLUDED_MEDS"),x = vasopressor_data$DrugName %>% table %>% cbind)


saveRDS(vasopressor_data,
        file = rds_file("vasopressor"))


# ------- Inotropes
med_admin_raw %>%
  filter(grepl("ENOXIMONE|DOBUTAMINE|ADRENALINE|DOPAMINE|MILRINONE",DrugName,ignore.case = T)) %>%
  group_by(DrugName) %>%
  tally %>% 
  print(n = Inf)

inotropes_data <- med_admin_raw %>%
  filter(grepl("ENOXIMONE|DOBUTAMINE|ADRENALINE|DOPAMINE|MILRINONE",DrugName,ignore.case = T)) %>%
  filter(!grepl("NORADRENALINE",DrugName,ignore.case = T)) %>%
  filter(MARAction %in% (include_list %>% filter(include_for_steroids_inotropes_vasopressors == "yes") %>% pull(MARAction))) %>%
  filter(RouteOfMedicationAbbreviated %in% c("IV Infusion", "IV Injection", "Intravenous"))

inotropes_data$DrugName %>% table
inotropes_data$DoseUnitAbbreviated %>% table

write.csv(file = csv_out_file("INOTROPES_INCLUDED_MEDS"),x = inotropes_data$DrugName %>% table %>% cbind)

saveRDS(inotropes_data,
        file = rds_file("inotropes"))


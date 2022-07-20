adt_raw <- readRDS(rds_file("adt_raw"))
# Alter Event type numbers, so they can be numerically sorted
# 0: Admission
# 1: Transfer
# 2: Discharge

adt_raw <- adt_raw %>%
  mutate(EVENT_TYPE_C = ifelse(EVENT_TYPE_C == 1,
                                          0,
                                          ifelse(EVENT_TYPE_C == 3,
                                                 1,
                                                 2)))

adt_raw_stays <-
  adt_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(STUDY_SUBJECT_DIGEST,IN_DTTM,EVENT_TYPE_C) %>%
  mutate(STAY = cumsum(ifelse(EVENT_TYPE_C == 0,
                              1,
                              0))) 

saveRDS(adt_raw_stays,rds_file("adt_raw_stays"))

# Outpatients Issue
adt_department_name_annotated <-
  read.csv(csv_in_file("2020-12-18-adt_department_names_annotated"),
           colClasses = c("ADT_DEPARTMENT_NAME" = "character",
                          "is_outpatient" = "logical"))

adt_raw <- adt_raw %>%
  left_join(adt_department_name_annotated, by = "ADT_DEPARTMENT_NAME")

# unlabelled outpatients
adt_department_name_unlabelled <- adt_raw %>%
  filter(is.na(is_outpatient)) %>%
  group_by(ADT_DEPARTMENT_NAME) %>%
  tally

write.csv(adt_department_name_unlabelled %>%
            select(-n),
          file = csv_out_file("adt_department_name_unlabelled"),
          row.names = FALSE)

# All Rosie
adt_raw_stays %>%
  group_by(STUDY_SUBJECT_DIGEST, STAY) %>%
  mutate(all_ros = all(str_detect(ADT_DEPARTMENT_NAME, "ROS") |
                         ADT_DEPARTMENT_NAME == "POST-DISCHARGE")) %>%
  filter(all_ros) %>%
  arrange(IN_DTTM)

# Check order of admissions
firsts <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(STUDY_SUBJECT_DIGEST,IN_DTTM,EVENT_TYPE_C) %>%
  slice(1)

firsts$EVENT_TYPE_C %>% table # - All Hospital Stays start with Admission

# Check order of eventtypes
lasts <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(STUDY_SUBJECT_DIGEST,IN_DTTM,EVENT_TYPE_C) %>%
  slice(n())

lasts$EVENT_TYPE_C %>% table


# CHeck multiple Admissions

adt_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  filter(EVENT_TYPE_C == 0) %>%
  arrange(STUDY_SUBJECT_DIGEST) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>% # Multiple re-admissions now. One instance of dialysis, 8 re-admissions. Count these?
  arrange(desc(count))

adt_raw <-
  adt_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(STUDY_SUBJECT_DIGEST,IN_DTTM,EVENT_TYPE_C) %>%
  mutate(STAY = cumsum(ifelse(EVENT_TYPE_C == 0,
                              1,
                              0))) %>%
  group_by(STUDY_SUBJECT_DIGEST, STAY) %>%
  mutate(all_is_outpatient = all(is_outpatient |
                                   ADT_DEPARTMENT_NAME == "POST-DISCHARGE")) %>%
  group_by(STUDY_SUBJECT_DIGEST)

# Pathways of patient excluded due to all ADT begin outpatients
adt_raw %>%
  filter(all_is_outpatient) %>%
  group_by(STUDY_SUBJECT_DIGEST, STAY) %>%
  summarise(adt_pathway = paste(ADT_DEPARTMENT_NAME, collapse = "; ")) %>%
  ungroup %>%
  select(adt_pathway) %>%
  distinct

# this is only a 4 hour stay on G2, so suggests it is functioning as outpatient
adt_raw %>%
  filter(all_is_outpatient) %>%
  group_by(STUDY_SUBJECT_DIGEST, STAY) %>%
  filter("ADD G2 WARD" %in% ADT_DEPARTMENT_NAME)

adt_raw <- adt_raw %>%
  filter(!all_is_outpatient)


# COmpare to adm_data -
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  filter(any(!is.na(HOSP_DISCH_TIME)))

adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  filter(any(EVENT_TYPE_C == 2))

# Ad Days since admission (LOS) earliest in time <-> in_dttm of event = DISCHARGE
adt_raw <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  arrange(STUDY_SUBJECT_DIGEST,IN_DTTM,EVENT_TYPE_C) %>%
  mutate(LOS = difftime(IN_DTTM,min(IN_DTTM),unit = "days"))

# LOS 2 is earliest in time <-> hosp_disch
adt_raw <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  mutate(LOS2 = ifelse(!is.na(HOSP_DISCH_TIME),
                       difftime(HOSP_DISCH_TIME,min(IN_DTTM),unit = "days"),
                       NA))

LOSconcluded <- adt_raw %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  filter(EVENT_TYPE_C == 2) %>%
  pull(LOS2) %>%
  as.numeric

LOSconcluded %>% summary

# Add ICU marker


# Check for no new ICU ward names
possible_icu_wards <- adt_raw %>%
  filter(str_detect(ADT_DEPARTMENT_NAME, coll("J3", ignore_case = TRUE)) |
         str_detect(ADT_DEPARTMENT_NAME, coll("ICU", ignore_case = TRUE)) |
         str_detect(ADT_DEPARTMENT_NAME, coll("IDA", ignore_case = TRUE))) %>%
  group_by(ADT_DEPARTMENT_NAME) %>%
  tally

if (nrow(possible_icu_wards) != 5){
  cat("************ warning: possible new ICU ward name *****")
}

adt_raw <-
adt_raw %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  arrange(STUDY_SUBJECT_DIGEST,STAY,IN_DTTM) %>%
  mutate(ICU = ICU_check(IN = IN_DTTM,DEPT = ADT_DEPARTMENT_NAME,iw = ICU_wards,cv = conv_times,pd = pull_date))

adt_raw <-
  adt_raw %>% 
  mutate(is_level2_care_or_above =
           case_when(ADT_DEPARTMENT_NAME == "ADD NEURO ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD GENERAL ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD D4 IDA UNIT" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD SURG-ICU WARD" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD PAED ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3 WARD" & IN_DTTM >= ymd("2020-04-09") & IN_DTTM <= ymd("2020-05-25")~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3-ITU WARD" & IN_DTTM >= ymd("2020-04-09") & IN_DTTM <= ymd("2020-05-25") ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3-ITU WARD" & IN_DTTM >= ymd("2021-01-02") ~ TRUE,
                     TRUE ~ FALSE))

adt_raw_stays <-
  adt_raw_stays %>% 
  mutate(is_level2_care_or_above =
           case_when(ADT_DEPARTMENT_NAME == "ADD NEURO ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD GENERAL ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD D4 IDA UNIT" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD SURG-ICU WARD" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD PAED ICU" ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3 WARD" & IN_DTTM >= ymd("2020-04-09") & IN_DTTM <= ymd("2020-05-25")~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3-ITU WARD" & IN_DTTM >= ymd("2020-04-09") & IN_DTTM <= ymd("2020-05-25") ~ TRUE,
                     ADT_DEPARTMENT_NAME == "ADD J3-ITU WARD" & IN_DTTM >= ymd("2021-01-02") ~ TRUE,
                     TRUE ~ FALSE))

adt_raw_stays <-
  adt_raw_stays %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  arrange(STUDY_SUBJECT_DIGEST,STAY,IN_DTTM) %>%
  mutate(ICU = ICU_check(IN = IN_DTTM,DEPT = ADT_DEPARTMENT_NAME,iw = ICU_wards,cv = conv_times,pd = pull_date))

saveRDS(adt_raw_stays,rds_file("adt_raw_stays"))

# ECMO - Check direct ICU discharges (5 Patients)
 fatalities <- adm_fatalities$STUDY_SUBJECT_DIGEST

 ICU_transfers <-
 adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
   arrange(STUDY_SUBJECT_DIGEST,IN_DTTM) %>%
   #filter(!STUDY_SUBJECT_DIGEST %in% fatalities) %>%
   left_join(adm_raw %>% select(STUDY_SUBJECT_DIGEST,STAY,DATE_OF_DEATH,DISCH_DECEASED),by = c("STUDY_SUBJECT_DIGEST","STAY")) %>%
   filter(any(EVENT_TYPE_C == 2)) %>%
   filter(nth(ICU,n = -2) | grepl("ADD MAIN THEATRE",nth(ADT_DEPARTMENT_NAME,n = -2))) %>%
   #group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
   #mutate(DISCH)
   arrange(HOSP_DISCH_TIME) %>%
   filter(!DISCH_DECEASED)
 
 ICU_transfers %>% print(n = Inf)

 write.csv(x = ICU_transfers,
           file = csv_out_file("ECMO_potential_2021_04_12"))

 
 
 
# PRINT ADMISSION WITH ICU STAY
ICUsummary <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  summarise(ICUduringSTAY = any(ICU))

table(ICUsummary$ICUduringSTAY)/nrow(ICUsummary)

png(png_file("Cohort/HospitalStaysWithICU"),height = 800,width = 800)
table(ICUsummary$ICUduringSTAY) %>% barplot(names = c("No ICU Stay (77.41%)","ICU Stay (22.59%)"),main = "ICU Stay during Hospital visit")
dev.off()


# IDs of ICU admitted patients
ICU_admitted_patients <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  # filter(IN_DTTM <= as.Date("2020-04-06")) %>%
  filter(any(ICU)) %>%
  slice(1) %>%
  pull(STUDY_SUBJECT_DIGEST)


# Pre-ICU Stay length

adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  arrange(STUDY_SUBJECT_DIGEST,STAY,IN_DTTM,EVENT_TYPE_C) %>%
  # filter(IN_DTTM <= as.Date("2020-04-06")) %>%
  filter(ICU) %>%
  slice(n = 1) %>%
  pull(LOS) %>%
  as.numeric %>%
  summary

# Number of ICU STAYS

adt_raw <-
adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  mutate(ICUSTAY = ICU_stay(ICU)) %>%
  mutate(STATUS = ifelse(all(EVENT_TYPE_C != 2),
                         "Active",
                         "Concluded")) %>%
  select(STUDY_SUBJECT_DIGEST,STAY,ICUSTAY,STATUS,everything())

# OUTCOMES

adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  arrange(STUDY_SUBJECT_DIGEST,STAY,IN_DTTM,EVENT_TYPE_C) %>%
  # filter((IN_DTTM) <= as.Date(as.Date("2020-04-06"))) %>%
  filter(any(ICU)) %>%
  filter(any(EVENT_TYPE_C == 2)) %>%
  slice(n = n()-1) %>%
  pull(ADT_DEPARTMENT_NAME) %>%
  table

adt_raw %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  # filter(IN_DTTM <= as.Date("2020-04-06")) %>%
  filter(any(ICU)) %>%
  filter(any(EVENT_TYPE_C == 2)) %>%
  slice(n()-1) %>%
  pull(ADT_DEPARTMENT_NAME) %>%
  table

# Save modified file
saveRDS(object = adt_raw, file = rds_file("adt"))


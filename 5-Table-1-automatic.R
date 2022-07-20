
admission_result <- function(x,before,after){
  deparse(substitute(x)) %>% print
  
  x_sum = 
  x %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
    filter(IN_DTTM <= before & after <= IN_DTTM) %>%
    filter(DATETIME >= TIME_0) %>%
    arrange(DATETIME) %>%
    slice(1) %>%
    pull(VALUE) %>%
    summary %>%
    round(digits = 5)
  
  print(paste0(x_sum[3]," [",x_sum[2],",",x_sum[5],"]"))
  return(paste0(x_sum[3]," [",x_sum[2],",",x_sum[5],"]"))
}

any_binary <- function(x,variable){
  x %>% 
    group_by(STUDY_SUBJECT_DIGEST) %>% 
    summarise_at(vars(variable),any(.)) %>% 
    pull(VAR) %>% 
    table
  
}

##### Load data matched to hospital visits
adm_data_rel <- readRDS(rds_file("adm_data_lm"))
adm_data <- readRDS(rds_file("adm"))

bp_std <- readRDS(rds_file("bp_std"))
pulse_std <- readRDS(rds_file("pulse_std"))
temp_std <- readRDS(rds_file("temp_std"))
rr_std <- readRDS(rds_file("rr_std"))
sf_ratio_std <- readRDS(rds_file("sf_ratio_std"))
spo2_std <- readRDS(rds_file("spo2_std"))

urea_std <- readRDS(rds_file("urea_std"))
creatinine_std <- readRDS(rds_file("creatinine_std"))
sodium_std <- readRDS(rds_file("sodium_std"))
wcc_std <- readRDS(rds_file("wcc_std"))
neutrophils_std <- readRDS(rds_file("neutrophils_std"))
lymphocytes_std <- readRDS(rds_file("lymphocytes_std"))
crp_std <- readRDS(rds_file("crp_std"))
ddimer_std <- readRDS(rds_file("ddimer_std"))
troponin_std <- readRDS(rds_file("troponin_std"))
ph_std <- readRDS(rds_file("ph_std"))
il6_std <- readRDS(rds_file("il6_std"))
bmi_calc_std <- readRDS(rds_file("bmi_calc_std"))

# data_non_palliative_wave_1 = readRDS(rds_file("data_non_palliative_2021_01_15_6h_shift_prediction_2days_uniform_wave1"))
# data_non_palliative_wave_2 = readRDS(rds_file("data_non_palliative_2021_01_15_6h_shift_prediction_2days_uniform_wave2"))
data_non_palliative_wave_2 <- readRDS(rds_file("data_non_palliative_2021_04_13_6h_shift_prediction_2days_uniform_wave2"))

# adm_data_rel_wave1 = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_1$STUDY_SUBJECT_DIGEST,data_non_palliative_wave_1$STAY))
adm_data_rel_wave2 = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_2$STUDY_SUBJECT_DIGEST,data_non_palliative_wave_2$STAY))

# data_non_palliative_wave_1 = data_non_palliative_wave_1 %>% mutate(NONSOCOMIAL = TIME_0 != IN_DTTM + hours(6)) 
data_non_palliative_wave_2 = data_non_palliative_wave_2 %>% mutate(NONSOCOMIAL = TIME_0 != IN_DTTM + hours(6)) 


adm_data_rel_wave1_nonsoc = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_1 %>% 
                                                                                                filter(NONSOCOMIAL) %>% 
                                                                                                pull(STUDY_SUBJECT_DIGEST),
                                                                                              data_non_palliative_wave_1 %>% 
                                                                                                filter(NONSOCOMIAL) %>% 
                                                                                                pull(STAY)))
adm_data_rel_wave1_soc = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_1 %>% 
                                                                                                filter(!NONSOCOMIAL) %>% 
                                                                                                pull(STUDY_SUBJECT_DIGEST),
                                                                                              data_non_palliative_wave_1 %>% 
                                                                                                filter(!NONSOCOMIAL) %>% 
                                                                                                pull(STAY)))


adm_data_rel_wave2_nonsoc = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_2 %>% 
                                                                                                filter(NONSOCOMIAL) %>% 
                                                                                                pull(STUDY_SUBJECT_DIGEST),
                                                                                              data_non_palliative_wave_2 %>% 
                                                                                                filter(NONSOCOMIAL) %>% 
                                                                                                pull(STAY)))
adm_data_rel_wave2_soc = adm_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(data_non_palliative_wave_2 %>% 
                                                                                             filter(!NONSOCOMIAL) %>% 
                                                                                             pull(STUDY_SUBJECT_DIGEST),
                                                                                           data_non_palliative_wave_2 %>% 
                                                                                             filter(!NONSOCOMIAL) %>% 
                                                                                             pull(STAY)))



print_table_1 <- function(adm_rel_wave,data_non_palliative,before,after){
print("Admission dates")
adm_rel_wave %>% pull(IN_DTTM) %>% summary %>% print
print("Number of patients")
n = adm_rel_wave %>% nrow %>% print
print("Female n,%")
c(nrow(adm_rel_wave %>% filter(GENDER_DESC == "Female")),nrow(adm_rel_wave %>% filter(GENDER_DESC == "Female"))/n*100) %>% print
print("Age Median [IQR]")
age = adm_rel_wave %>% pull(AGE_AT_ADM) 
age_sum = age %>% summary
print(paste0(age_sum[3]," [",age_sum[2],",",age_sum[5],"]"))

print("AGE BANDS")
adm_rel_wave %>% 
  mutate(BELOW_45 = AGE_AT_ADM < 45) %>%
  mutate(BELOW_50 = AGE_AT_ADM < 50) %>%
  mutate(BELOW_55 = AGE_AT_ADM < 55) %>%
  mutate(BELOW_60 = AGE_AT_ADM < 60) %>%
  mutate(BELOW_65 = AGE_AT_ADM < 65) %>%
  mutate(BELOW_70 = AGE_AT_ADM < 70) %>%
  mutate(BELOW_75 = AGE_AT_ADM < 75) %>%
  mutate(BELOW_80 = AGE_AT_ADM < 80) %>%
  mutate(BELOW_85 = AGE_AT_ADM < 85) %>%
  mutate(BELOW_90 = AGE_AT_ADM < 90) %>%
  mutate(BELOW_95 = AGE_AT_ADM < 95) %>%
  ungroup %>%
  select(BELOW_45:BELOW_95) %>% colSums %>% cbind %>% print

admission_result(x = bmi_calc_std,before = before,after = after)

print("Frailty on admission")
frailty_sum = data_non_palliative %>% filter(FRAILTY_MISSING == 0) %>% group_by(STUDY_SUBJECT_DIGEST) %>% arrange(INDEX) %>% pull(FRAILTY) %>% summary
print(paste0(frailty_sum[3]," [",frailty_sum[2],",",frailty_sum[5],"]"))

print("% Hospital acquired")
n_hosp_acq = data_non_palliative %>% filter(IN_DTTM + hours(6) != TIME_0) %>% n_groups()
print(n_hosp_acq/n*100)

print("LOS")
adm_rel_wave %>% pull(LOS) %>% summary %>% print

# admission_result(x = bp_std,before = before,after = after)


print("---- OUTCOMES ----")
deaths = adm_rel_wave %>% pull(DISCH_DECEASED)
print("Deaths in hospital")
paste0(sum(deaths),",",round(mean(deaths)*100,3),"%") %>% print

print("Discharged alive - including ECMOs (Wave 1: 5 patients)")
adm_rel_wave %>% filter(DISCH_DECEASED == 0) %>% filter(!is.na(HOSP_DISCH_TIME)) %>%  n_groups %>% print

print("Ongoing as of pull date")
adm_rel_wave$HOSP_DISCH_TIME %>% is.na %>% sum %>% print


print("--- Treatments ---")

print("ICU")
data_non_palliative %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(ICU = any(ICU)) %>% pull(ICU) %>% table %>% print
print("VENTILATION")
data_non_palliative %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(VENTILATION = any(VENTILATION == 1)) %>% pull(VENTILATION) %>% table %>% print
print("CARDIOVASCULAR")
data_non_palliative %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(CARDIO_VASCULAR_SUPPORT = any(CARDIO_VASCULAR_SUPPORT == 1)) %>% pull(CARDIO_VASCULAR_SUPPORT) %>% table %>% print
print("RRT")
data_non_palliative %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(RRT = any(RRT == 1)) %>% pull(RRT) %>% table %>% print
print("STEROIDS")
data_non_palliative %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(STEROIDS = any(STEROIDS == 1)) %>% pull(STEROIDS) %>% table%>% print

print("--- VITALS ---")
admission_result(x = bp_std,before = before,after = after)
admission_result(x = pulse_std,before = before,after = after)
admission_result(x = temp_std,before = before,after = after)
admission_result(x = rr_std,before = before,after = after)
admission_result(x = spo2_std,before = before,after = after)
admission_result(x = sf_ratio_std,before = before,after = after)

print("--- TESTS ---")
admission_result(x = urea_std,before = before,after = after)
admission_result(x = creatinine_std,before = before,after = after)
admission_result(x = sodium_std,before = before,after = after)
admission_result(x = crp_std,before = before,after = after)
admission_result(x = wcc_std,before = before,after = after)
admission_result(x = neutrophils_std,before = before,after = after)
admission_result(x = lymphocytes_std,before = before,after = after)
admission_result(x = ddimer_std,before = before,after = after)
admission_result(x = troponin_std,before = before,after = after)
admission_result(x = ph_std,before = before,after = after)
admission_result(x = il6_std,before = before,after = after)
}



sink(txt_file("Table_1_Wave_1"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave1,data_non_palliative = data_non_palliative_wave_1,before = as.Date("2020-09-18"),after = as.Date("2020-03-01"))
sink(NULL)

sink(txt_file("Table_1_Wave_2"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave2,data_non_palliative = data_non_palliative_wave_2,before = as.Date("2022-09-18"),after = as.Date("2020-09-18"))
sink(NULL)

sink(txt_file("Table_1_Wave_1_nonsoc"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave1_nonsoc,data_non_palliative = data_non_palliative_wave_1 %>% filter(NONSOCOMIAL),before = as.Date("2020-09-18"),after = as.Date("2020-03-01"))
sink(NULL)

sink(txt_file("Table_1_Wave_2_nonsoc"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave2_nonsoc,data_non_palliative = data_non_palliative_wave_2 %>% filter(NONSOCOMIAL),before = as.Date("2022-09-18"),after = as.Date("2020-09-18"))
sink(NULL)

sink(txt_file("Table_1_Wave_1_soc"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave1_soc,data_non_palliative = data_non_palliative_wave_1 %>% filter(!NONSOCOMIAL),before = as.Date("2020-09-18"),after = as.Date("2020-03-01"))
sink(NULL)

sink(txt_file("Table_1_Wave_2_soc"),split = T)
print_table_1(adm_rel_wave = adm_data_rel_wave2_soc,data_non_palliative = data_non_palliative_wave_2 %>% filter(!NONSOCOMIAL),before = as.Date("2022-09-18"),after = as.Date("2020-09-18"))
sink(NULL)

# Wave 2

# 
sink(txt_file("Table_1_Wave_2"),split = T)
admission_result(x = urea_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = creatinine_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = sodium_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = wcc_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = neutrophils_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = lymphocytes_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = urea_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = crp_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = ddimer_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = troponin_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))

print("Patients")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% n_groups()
print("Outcomes")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(desc(INDEX)) %>% slice(1) %>% pull(STATUS_WITHIN_W_OF_LM) %>%
  table

print("LOS")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(LOS = max(INDEX)) %>% pull(LOS) %>% summary

print("ICU")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(ICU = any(ICU)) %>% pull(ICU) %>% table
print("VENTILATION")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(VENTILATION = any(VENTILATION)) %>% pull(VENTILATION) %>% table
print("CARDIOVASCULAR")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(CARDIO_VASCULAR_SUPPORT = any(CARDIO_VASCULAR_SUPPORT)) %>% pull(CARDIO_VASCULAR_SUPPORT) %>% table
print("RRT")
data_non_palliative_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST) %>% summarise(RRT = any(RRT)) %>% pull(RRT) %>% table


admission_result(x = bp_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = pulse_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = rr_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = temp_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
# admission_result(x = sf_std)
admission_result(x = bp_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))
admission_result(x = spo2_std,before = as.Date("2022-01-01"),after = as.Date("2020-09-18"))

sink(NULL)

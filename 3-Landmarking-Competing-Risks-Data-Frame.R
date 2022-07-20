library("dplyr")
library("tidyr")
library("purrr")
library("survival")
library("cmprsk")
library("crrstep")
library("crrp")
library("Matrix")
library("riskRegression")
library("stringr")

# Load data
adm_data <- readRDS(rds_file("adm"))
adt_data <- readRDS(rds_file("adt"))
adt_data_raw_stays <- readRDS(rds_file("adt_raw_stays")) 

# Vital signs & other observations
ventmode <- readRDS(rds_file("ventmode_join"))
dnar_raw <- readRDS(rds_file("dnar_raw")) %>% distinct
acvpu_raw <- readRDS(rds_file("acvpu_raw"))
o2_device_raw <- readRDS(rds_file("o2_device_raw"))
cfs <- readRDS(rds_file("cfs"))
bmi_calc <- readRDS(rds_file("bmi"))
frailty_raw <- readRDS(rds_file("frailty_raw"))
bp <- readRDS(rds_file("bp"))
bp <- bp %>% mutate(measured_value = MAP) # make things consistent
temp <- readRDS(rds_file("temp_mod")) %>%
  filter(convert.numeric(measured_value) <= 43 & 25 <= convert.numeric(measured_value))
pulse <- readRDS(rds_file("hr_data"))
pulse <- pulse %>% 
  filter(convert.numeric(measured_value) <= 200 & 30 <=convert.numeric(measured_value))
spo2 <- readRDS(rds_file("spo2"))
spo2 <- spo2 %>% filter(as.numeric(measured_value) > 75)
rr <- readRDS(rds_file("rr_raw"))
sf_ratio <- readRDS(rds_file("spo2_fio2"))
sf_ratio <- sf_ratio %>% rename(measured_value = ratio) # consistent naming
pf_ratio <- readRDS(rds_file("bloodgas_data_arterial_pfratio"))
pf_ratio <- pf_ratio %>% 
  mutate(MEASURE_TIME = ResultDate) %>%
  mutate(measured_value = convert.numeric(pf_ratio))  # consistent naming

news2 <- readRDS(rds_file("news2"))
gcs <- readRDS(rds_file("gcs_raw"))
rrt_io <- readRDS(rds_file("rrt_io"))

# BLood Tests
il6 <- readRDS(rds_file("il6"))
il1 <- readRDS(rds_file("il1_beta"))
tnfa <- readRDS(rds_file("tnf_alpha"))
ig <-readRDS(rds_file("interferon_gamma"))
il10 <- readRDS(rds_file("il10"))
crp <- readRDS(rds_file("crp"))
wcc <- readRDS(rds_file("wbc"))
ferritin <- readRDS(rds_file("ferritin"))
ddimer <- readRDS(rds_file("ddimer"))
pct <- readRDS(rds_file("pct"))
lymphocytes <- readRDS(rds_file("lymphocytes"))
neutrophils <- readRDS(rds_file("neutrophils"))
nl_ratio <- readRDS(rds_file("nl_ratio"))
il_ratio <- readRDS(rds_file("il6_il10_ratio"))
rdw <- readRDS(rds_file("rdw"))
eosinophils <- readRDS(rds_file("eosinophils"))
monocytes <- readRDS(rds_file("monocytes"))
platelets <- readRDS(rds_file("platelets"))
lactate <- readRDS(rds_file("lactate"))
hb <- readRDS(rds_file("hb"))

alanine <- readRDS(rds_file("alt"))
albumin <- readRDS(rds_file("albumin"))
aspartate <- readRDS(rds_file("ast"))
bilirubin <- readRDS(rds_file("bilirubin"))
chloride <- readRDS(rds_file("chloride"))
creatinine <- readRDS(rds_file("creatinine"))
ldh <- readRDS(rds_file("ldh"))
potassium <- readRDS(rds_file("potassium"))
sodium <- readRDS(rds_file("sodium"))
troponin <- readRDS(rds_file("troponin"))
urea <- readRDS(rds_file("urea"))

bloodgas_data <- readRDS(rds_file("bloodgas_data"))
ph <- bloodgas_data %>% 
  mutate(measured_value = case_when(blood_specimen_type_simplified == "Arterial blood" ~ ph,
                                    blood_specimen_type_simplified != "Arterial blood" ~ ph + 0.033)) %>%
  select(STUDY_SUBJECT_DIGEST,measured_value,ResultDate) %>%
  rename(COLLECTED_DATETIME = ResultDate)

basophil <- readRDS(rds_file("basophil"))
cortisol <- readRDS(rds_file("cortisol"))
phosphate <- readRDS(rds_file("phosphate"))
magnesium <- readRDS(rds_file("magnesium"))
alkaline_phosphatase <- readRDS(rds_file("alkaline_phosphatase"))
aptt <- readRDS(rds_file("aptt"))
pt <- readRDS(rds_file("pt"))

# Medication
inotropes <- readRDS(rds_file("inotropes"))
vasopressors <- readRDS(rds_file("vasopressor"))
steroids <- readRDS(rds_file("steroids_covid"))

steroids_mod = steroids %>%
  mutate(DrugNameSimplified = case_when(grepl("DEX",DrugName) ~ "DEXAMETHASONE",
                                        grepl("HYDRO",DrugName) ~ "HYDROCORTISONE",
                                        grepl("PRED",DrugName) & !grepl("METH",DrugName) ~ "PREDNISALONE",
                                        grepl("METHYL",DrugName) ~ "METHYLPREDISALONE")) %>%
  mutate(DoseAsLabelled = case_when(DrugName == "HYDROCORTISONE SODIUM SUCCINATE INFUSION" & is.na(DoseAsLabelled) ~ 50,
                                    DrugName == "HYDROCORTISONE SODIUM SUCCINATE  100MG INJECTION" & is.na(DoseAsLabelled) ~ 100,
                                    DrugName == "DEXAMETHASONE  2MG TABLETS" & is.na(DoseAsLabelled) ~ 2,
                                    DrugName == "DEXAMETHASONE  2MG/5ML ORAL SOLUTION SUGAR-FREE" & is.na(DoseAsLabelled) ~ 2,
                                    DrugName == "DEXAMETHASONE  3.3MG/ML SOLUTION FOR INJECTION AMPOULE" & is.na(DoseAsLabelled) ~ 3.3,
                                    DrugName == "DEXAMETHASONE  6.6MG/2ML INJECTION" & is.na(DoseAsLabelled) ~ 6.6,
                                    DrugName == "METHYLPREDNISOLONE  40MG POWDER FOR SOLUTION FOR INJECTION" & is.na(DoseAsLabelled) ~ 40,
                                    DrugName == "PREDNISOLONE  1MG TABLETS" & is.na(DoseAsLabelled) ~ 1,
                                    DrugName == "PREDNISOLONE  5MG TABLETS" & is.na(DoseAsLabelled) ~ 5,
                                    DrugName == "PREDNISOLONE (AS SODIUM PHOSPHATE)  10MG/ML ORAL SOLUTION" & is.na(DoseAsLabelled) ~ 10,
                                    TRUE ~ DoseAsLabelled)) 

# DETERMINE TIME 0
# tested positive
covid_pos <- readRDS(rds_file("covid_pos"))
covid_pos <- adt_data_raw_stays %>% mutate(ICU = "Not assigned") %>%
  do(Hosp_Match(.,data = covid_pos,buffer.in = days(14)))

covid_pos <- covid_pos %>% group_by(STUDY_SUBJECT_DIGEST) %>% arrange(DATETIME) %>% slice(1)
covid_pos <- covid_pos %>% rename(COVID_POS_FIRST_DATE = DATETIME) %>% select(STUDY_SUBJECT_DIGEST,STAY,COVID_POS_FIRST_DATE,IN_DTTM)

# covid diagnosis
covid_diag <- readRDS(rds_file("covid_diag"))
covid_diag <- covid_diag %>% filter(covid_diag_confirmed)
covid_diag <- adt_data_raw_stays %>% mutate(ICU = "Not assigned") %>%
  do(Hosp_Match(.,data = covid_diag %>% rename(DATETIME = DIAGNOSIS_ENTERED_DATE),buffer.in = days(14)))

covid_diag <- covid_diag %>% group_by(STUDY_SUBJECT_DIGEST) %>% arrange(DATETIME) %>% slice(1)
covid_diag <- covid_diag %>% rename(COVID_DIAG_FIRST_DATE = DATETIME) %>% select(STUDY_SUBJECT_DIGEST,STAY,COVID_DIAG_FIRST_DATE,IN_DTTM)

covid_diag_test <- covid_diag %>% full_join(covid_pos,by = c("STUDY_SUBJECT_DIGEST","IN_DTTM"))
covid_diag_test <- covid_diag_test %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  mutate(DIAG_TEST_FIRST = min(na.omit(c(COVID_DIAG_FIRST_DATE,COVID_POS_FIRST_DATE)))) %>%
  select(STUDY_SUBJECT_DIGEST,IN_DTTM,COVID_DIAG_FIRST_DATE,COVID_POS_FIRST_DATE,DIAG_TEST_FIRST) %>%
  arrange(DIAG_TEST_FIRST) %>%
  slice(1)

# When to consider infections hospital acquired
nonsocomial_threshold = 10
time_shift = hours(6)

adm_data_rel <- adm_data %>% inner_join(covid_diag_test,by = c("STUDY_SUBJECT_DIGEST","IN_DTTM"))
adm_data_rel <- adm_data_rel %>% mutate(TIME_0 = case_when(difftime(DIAG_TEST_FIRST,IN_DTTM,unit = "days") <= nonsocomial_threshold ~ IN_DTTM + time_shift,
                                                           difftime(DIAG_TEST_FIRST,IN_DTTM,unit = "days") > nonsocomial_threshold ~ DIAG_TEST_FIRST))
adm_data_rel <- adm_data_rel %>% mutate(NONSOCOMIAL = case_when(difftime(DIAG_TEST_FIRST,IN_DTTM,unit = "days") <= nonsocomial_threshold ~ F,
                                                           difftime(DIAG_TEST_FIRST,IN_DTTM,unit = "days") > nonsocomial_threshold ~ T))

# STRANGE DATE FORMAT ISSUES WITH THIS ONE, 
# fix manually
adm_data_rel <- adm_data_rel %>% mutate(TIME_0 = case_when(STUDY_SUBJECT_DIGEST == "2CB10A46FD4E987E580AF9C8B5861CCBC4B84DFF8D3C1BB4BEB032B37ACB94D4"~ as.POSIXct("2020-03-29 00:59:00"),
                                       TRUE ~ TIME_0))

saveRDS(adm_data_rel,rds_file("adm_data_lm"))
adm_data_rel <- readRDS(rds_file("adm_data_lm"))

adt_data_rel <- adt_data %>% filter(paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(adm_data_rel$STUDY_SUBJECT_DIGEST,adm_data_rel$STAY))
adt_data_rel <- adt_data_rel %>%
  left_join(adm_data_rel %>% select(STUDY_SUBJECT_DIGEST,STAY,TIME_0),by = c("STUDY_SUBJECT_DIGEST","STAY"))

saveRDS(adt_data_rel,rds_file("adt_data_lm"))
adt_data_rel <- readRDS(rds_file("adt_data_lm"))

# GCS
gcs <- gcs %>% mutate(measured_value = convert.numeric(measured_value))
gcs <- adt_data_rel %>% do(Hosp_Match(.,data = gcs %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))
gcs <- gcs %>% mutate(REL_TIME = difftime(DATETIME,TIME_0,unit = "days"))

# Frailty
cfs <- cfs %>% mutate(VALUE = sapply(X = cfs$measured_value,FUN = function(x) return(as.numeric(str_split(string = x,pattern = " ")[[1]][1])))) 
cfs %>% select(measured_value,VALUE)

# Ventilation
ventmode <- adt_data_rel %>%
  do(Hosp_Match(.,data = ventmode %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))

ventmode <- ventmode %>%
  mutate(REL_TIME = difftime(DATETIME,TIME_0,unit = "days"))

# 02
o2_device <- adt_data_rel %>%
  do(Hosp_Match(.,data = o2_device_raw %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))

o2_device <- o2_device %>% filter(measured_value != "None (Room air)")
o2_device <- o2_device %>% mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days"))
o2_device <- o2_device %>% mutate(VALUE = measured_value)

# Renal replacement therapy
rrt <- readRDS(rds_file("rrt")) %>%
  filter(measured_value == "Yes")
rrt <- adt_data_rel %>%
  do(Hosp_Match(.,data = rrt %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))
rrt <- rrt %>% 
  mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days"))

rrt_io <- adt_data_rel %>%
  do(Hosp_Match(.,data = rrt_io %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))
rrt_io <- rrt_io %>%
  mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days"))

# ACVPU
acvpu <- adt_data_rel %>%
  do(Hosp_Match(.,data = acvpu_raw %>% rename(DATETIME = MEASURE_TIME),add_time_0 = TRUE,buffer.in = hours(48)))

acvpu <- acvpu %>% mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days"))
acvpu <- acvpu %>% mutate(VALUE = measured_value)

# DNAR
dnar <- adt_data_rel %>%
  do(Hosp_Match(.,data = dnar_raw %>% rename(DATETIME = PROC_START_TIME),add_time_0 = TRUE,buffer.in = hours(48)))

dnar <- dnar %>% 
  mutate(START_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days")) %>% 
  mutate(END_AFTER_ADMISSION = difftime(UPDATE_DATE,TIME_0,unit = "days"))
dnar <- dnar %>% mutate(VALUE = 1)

# ICU
# prep adt data to have relative
adt_data_rel <- adt_data_rel %>% 
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>% 
  mutate(RELATIVE_TIME = as.numeric(difftime(IN_DTTM,min(TIME_0),unit = "days")))

# Comorbidities
# Medhist/Diag joined is a combination of medical history and diagnosis codes
# replaces the diagnosis codes alone
medhist_diag_joined <- readRDS(rds_file("medhist_diag_joined"))
medhist_diag_joined <- medhist_diag_joined %>% 
  filter(!is.na(DIAGNOSIS_DATE))



# Medication
inotropes <- adt_data_rel %>%
  do(Hosp_Match(.,data = inotropes %>% rename(DATETIME = TimeAdministered),add_time_0 = T,buffer.in = hours(48)))
inotropes <- inotropes %>%
  mutate(REL_TIME = difftime(DATETIME,TIME_0,unit = "days"))

# Vasopressors
vasopressors <- adt_data_rel %>%
  do(Hosp_Match(.,data = vasopressors %>% rename(DATETIME = TimeAdministered),add_time_0 = T,buffer.in = hours(48)))
vasopressors <- vasopressors %>%
  mutate(REL_TIME = difftime(DATETIME,TIME_0,unit = "days"))

# Steroids
steroids <- adt_data_rel %>%
  do(Hosp_Match(.,data = steroids %>% rename(DATETIME = TimeAdministered),add_time_0 = T,buffer.in = hours(48)))
steroids <- steroids %>%
  mutate(REL_TIME = difftime(DATETIME,TIME_0,unit = "days"))

# Standardise
# Do NOT standardise
# these two functions only match to hospital visits and introduce relative time since TIME 0
standardise_test <- function(x,save.res = T){
  temp <- adt_data_rel %>% 
    do(Hosp_Match(.,data = x %>% rename(DATETIME = COLLECTED_DATETIME),add_time_0 = T,buffer.in = hours(48))) %>% 
    ungroup() %>%
    mutate(unadjusted_value = convert.numeric(ResultValue))  %>%
    mutate(ResultValue = convert.numeric(ResultValue)) %>%
    #mutate(ResultValue = (ResultValue - mean(ResultValue,na.rm = T))/sd(ResultValue,na.rm = T)) %>%
    mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days")) %>%
    rename(VALUE = ResultValue)
  
  if(save.res){saveRDS(temp,rds_file(paste0(as.character(substitute(x)),"_std")))}
  
  temp %>%
    return()
}

# Standardise Meas
standardise_meas <- function(x,save.res = T, buffer.in = hours(48)){
  
  temp <- adt_data_rel %>% 
    do(Hosp_Match(.,data = x %>% rename(DATETIME = MEASURE_TIME),add_time_0 = T,buffer.in = buffer.in)) %>% 
    ungroup() %>%
    mutate(unadjusted_value = convert.numeric(measured_value))  %>%
    mutate(measured_value = convert.numeric(measured_value)) %>%
    mutate(COLLECTION_AFTER_ADMISSION = difftime(DATETIME,TIME_0,unit = "days")) %>%
    rename(VALUE = measured_value)
  
  if(save.res){saveRDS(temp,rds_file(paste0(as.character(substitute(x)),"_std")))}
  
  temp %>%
    return()
}


############################ ONLY RUN FOR UPDATED DATA
# Add scaled variable + match data to hospital stay, between TIME_0 and DISCH

# Standardise & save test/measurement results
il6_std <- standardise_test(il6)
il1_std <- standardise_test(il1)
tnfa_std <- standardise_test(tnfa)
ig_std <-standardise_test(ig)
il10_std <- standardise_test(il10)
crp_std <- standardise_test(crp)
wcc_std <- standardise_test(wcc)
ferritin_std <- standardise_test(ferritin)
ddimer_std <- standardise_test(ddimer)
pct_std <- standardise_test(pct)
lymphocytes_std <- standardise_test(lymphocytes)
neutrophils_std <- standardise_test(neutrophils)
nl_ratio_std <- standardise_test(nl_ratio)
il_ratio_std <- standardise_test(il_ratio)
rdw_std <- standardise_test(rdw)
eosinophils_std <- standardise_test(eosinophils)
monocytes_std <- standardise_test(monocytes)
platelets_std <- standardise_test(platelets)
lactate_std <- standardise_test(lactate)
alanine_std <- standardise_test(alanine)
albumin_std <- standardise_test(albumin)
aspartate_std <- standardise_test(aspartate)
bilirubin_std <- standardise_test(bilirubin)
basophil_std <- standardise_test(basophil)
chloride_std <- standardise_test(chloride)
creatinine_std <- standardise_test(creatinine)
cortisol_std <- standardise_test(cortisol)
ldh_std <- standardise_test(ldh)
potassium_std <- standardise_test(potassium)
sodium_std <- standardise_test(sodium)
troponin_std <- standardise_test(troponin)
urea_std <- standardise_test(urea)
aptt_std <- standardise_test(aptt)
pt_std <- standardise_test(pt)
magnesium_std <- standardise_test(magnesium)
phosphate_std <- standardise_test(phosphate)
alkaline_phosphatase_std <- standardise_test(alkaline_phosphatase)
hb_std <- standardise_test(hb)

bmi_calc_std <- standardise_meas(bmi_calc, buffer.in = days(365))
bp_std <- standardise_meas(bp)
temp_std <- standardise_meas(temp)
pulse_std <- standardise_meas(pulse)
spo2_std <- standardise_meas(spo2)
sf_ratio_std <- standardise_meas(sf_ratio)
pf_ratio_std <- standardise_meas(pf_ratio)
rr_std <- standardise_meas(rr)
news2_std <- standardise_meas(news2)
ph <- ph %>% rename(MEASURE_TIME = COLLECTED_DATETIME)
ph_std <- standardise_meas(ph)
gcs_std <- standardise_meas(gcs_raw)



# X1/X2 conversion
# Convert covariates to excess over threshold
sodium_x1 <- sodium_std%>% mutate(VALUE = case_when(unadjusted_value < 135 ~ (135 - unadjusted_value),
                                                    unadjusted_value >= 135 ~ 0))
sodium_x2 <- sodium_std%>% mutate(VALUE = case_when(unadjusted_value > 145 ~ (unadjusted_value - 145),
                                                    unadjusted_value <= 145 ~ 0))
ph_x1 <- ph_std%>% mutate(VALUE = case_when(unadjusted_value < 7.35 ~ (7.35 - unadjusted_value),
                                            unadjusted_value >= 7.35 ~ 0))
ph_x2 <- ph_std%>% mutate(VALUE = case_when(unadjusted_value > 7.45 ~ (unadjusted_value - 7.45),
                                            unadjusted_value <= 7.45 ~ 0))
bmi_x1 <- bmi_calc_std %>% mutate(VALUE = case_when(unadjusted_value < 18.5 ~ (18.5 - unadjusted_value),
                                            unadjusted_value >= 18.5 ~ 0))
bmi_x2 <- bmi_calc_std%>% mutate(VALUE = case_when(unadjusted_value > 25 ~ (unadjusted_value - 25),
                                            unadjusted_value <= 25 ~ 0))


# Competing risk status
# 0 Censored
# 1 Discharged alive
# 2 Discharged dead
adm_data_rel <- adm_data_rel %>%
  mutate(DEATH_AFTER_ADMISSION = DISCH_DECEASED == 1) %>%
  mutate(DEATH_REL = case_when(DISCH_DECEASED == 1 ~ difftime(DATE_OF_DEATH,TIME_0,unit = "days"),
                               DISCH_DECEASED == 0 & is.na(HOSP_DISCH_TIME) ~ difftime(pull_date,TIME_0,unit = "days"),
                               DISCH_DECEASED == 0 & !is.na(HOSP_DISCH_TIME) ~ difftime(HOSP_DISCH_TIME,IN_DTTM,unit = "days"))) %>%
  mutate(STATUS = case_when(DISCH_DECEASED == 1 ~ 2,
                            DISCH_DECEASED == 0 & is.na(HOSP_DISCH_TIME) ~ 0,
                            DISCH_DECEASED == 0 & !is.na(HOSP_DISCH_TIME) ~ 1)) %>%
  mutate(STATUS_TIME = case_when(STATUS == 2 ~ difftime(DATE_OF_DEATH,TIME_0,unit = "days"),
                                 !DEATH_AFTER_ADMISSION & is.na(HOSP_DISCH_TIME) ~ difftime(pull_date,TIME_0,unit = "days"),
                                 !DEATH_AFTER_ADMISSION & !is.na(HOSP_DISCH_TIME) ~ difftime(HOSP_DISCH_TIME,TIME_0,unit = "days"))) 



# Basematrix Landmarking competing risks
outcomes_lm_cr <- function(x,
                           relative_days,
                           w){
  
  cutoff <- floor(x$STATUS_TIME) %>% as.numeric
  
  # x %>% select(STUDY_SUBJECT_DIGEST,)
  tibble(STUDY_SUBJECT_DIGEST = x$STUDY_SUBJECT_DIGEST,
         STAY = x$STAY,
         INDEX = 0:cutoff,
         INDEX_W = 0:cutoff + w) %>%
    mutate(REAL_TIME = x$TIME_0 + ddays(INDEX)) %>%
    mutate(TIME_0 = x$TIME_0) %>%
    mutate(IN_DTTM = x$IN_DTTM) %>%
    mutate(STATUS_TIME_FROM_LM = pmin(x$STATUS_TIME - INDEX,w)) %>%
    mutate(STATUS_WITHIN_W_OF_LM = ifelse(x$STATUS_TIME <= INDEX_W,
                                          x$STATUS,
                                          0)) %>%

    return
}

# Landmark base data set, competing risks
base_matrix_lm_cr_2 <- adm_data_rel %>% 
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  do(outcomes_lm_cr(.,
                    w = 2,
                    relative_days = relative_days)) %>%
  filter(INDEX >= 0)

base_matrix_lm_cr_3 <- adm_data_rel %>% 
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  do(outcomes_lm_cr(.,
                    w = 3,
                    relative_days = relative_days)) %>%
  filter(INDEX >= 0)



saveRDS(base_matrix_lm_cr_2,rds_file("base_matrix_lm_cr_2"))
saveRDS(base_matrix_lm_cr_3,rds_file("base_matrix_lm_cr_3"))


# Add LOCF measurement values
add_marker <- function(ID,STAY,time1,var,marker_mat){
  if(missing(marker_mat)){
    print(var)
    relevant_data <-
      blood_marker %>% 
      filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
      filter(STAY == STAY) %>%
      filter(TestName == var) %>% 
      filter(!is.na(unadjusted_value))
  }else{
    relevant_data <-
      marker_mat %>% 
      filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
      filter(STAY == STAY) %>% 
      filter(!is.na(unadjusted_value))
  }

  
  if(nrow(relevant_data) > 0){
    if(missing(marker_mat)){
      af <- stepfun(x = relevant_data$COLLECTION_AFTER_ADMISSION,
                    y = c(0,relevant_data$unadjusted_value))
    }else{
      af <- stepfun(x = relevant_data$COLLECTION_AFTER_ADMISSION,
                    y = c(0,relevant_data$unadjusted_value))
    }

    
    af(v = time1) %>% return 
  }else{
    return(rep(0,length(time1)))
  }
}

# Add LOCF measurement values
add_dnar <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    dnar %>% 
    filter(DESCRIPTION == "ACTIVE TREATMENT, DNACPR") %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    STARTS_INDICES <- relevant_data$START_AFTER_ADMISSION %>% ceiling %>% as.numeric
    END_INDICES <- relevant_data$END_AFTER_ADMISSION %>% ceiling %>% as.numeric

    dnar_days <- mapply(FUN = function(x,y) return(x:y),x = STARTS_INDICES,y = END_INDICES) %>% unlist %>% unique %>% as.numeric
    
    return(time1 %in% dnar_days)
    
    return(time1 >= start)
  }else{
    return(rep(0,length(time1)))
  }
}

add_pall <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    dnar %>% 
    filter(DESCRIPTION == "SYMPTOM CONTROL,DNACPR") %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    STARTS_INDICES <- relevant_data$START_AFTER_ADMISSION %>% ceiling %>% as.numeric
    END_INDICES <- relevant_data$END_AFTER_ADMISSION %>% ceiling %>% as.numeric
    
    dnar_days <- mapply(FUN = function(x,y) return(x:y),x = STARTS_INDICES,y = END_INDICES) %>% unlist %>% unique %>% as.numeric
    
    return(time1 %in% dnar_days)
    
    return(time1 >= start)
  }else{
    return(rep(0,length(time1)))
  }
}



add_rr_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    rr_std %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(SCORE = case_when(unadjusted_value <= 8 ~ 3,
                               unadjusted_value <= 11 &  9 <= unadjusted_value  ~ 1,
                               unadjusted_value <= 20 &  12 <= unadjusted_value ~ 0,
                               unadjusted_value <= 24 &  21 <= unadjusted_value ~ 2,
                               25 <= unadjusted_value ~ 3))
    
    relevant_data <- relevant_data %>%
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = max(SCORE))
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
    
  }else{
    return(rep(0,length(time1)))
  }
}

add_temp_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    temp_std %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(SCORE = case_when(unadjusted_value <= 35 ~ 3,
                               unadjusted_value <= 36 &  35 < unadjusted_value  ~ 1,
                               unadjusted_value <= 38 &  36 < unadjusted_value ~ 0,
                               unadjusted_value <= 39 &  38 < unadjusted_value ~ 1,
                               39 < unadjusted_value ~ 2))
    
    relevant_data <- relevant_data %>%
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = max(SCORE))
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
  }else{
    return(rep(0,length(time1)))
  }
}

add_pulse_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    pulse_std %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(SCORE = case_when(unadjusted_value <= 40 ~ 3,
                               unadjusted_value <= 50 &  40 < unadjusted_value  ~ 1,
                               unadjusted_value <= 90 &  50 < unadjusted_value ~ 0,
                               unadjusted_value <= 110 &  90 < unadjusted_value ~ 1,
                               unadjusted_value <= 130 &  110 < unadjusted_value ~ 2,
                               130 < unadjusted_value ~ 3))
    
    relevant_data <- relevant_data %>%
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = max(SCORE))
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
  }else{
    return(rep(0,length(time1)))
  }
}

add_o2_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    o2_device %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = 2)
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
    
  }else{
    return(rep(0,length(time1)))
  }
}

add_acvpu_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    acvpu %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      mutate(SCORE = case_when(measured_value == "A" ~ 0,
                               measured_value == "U" ~ 3)) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = max(SCORE))
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
    
  }else{
    return(rep(0,length(time1)))
  }
}

add_bp_score <- function(ID,STAY,time1,var){
  print(var)
  relevant_data <-
    bp_std %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    relevant_data <- relevant_data %>% 
      mutate(SCORE = case_when(SYST <= 90 ~ 3,
                               SYST <= 100 &  90 < SYST  ~ 2,
                               SYST <= 110 &  100 < SYST ~ 1,
                               SYST <= 220 &  110 < SYST ~ 0,
                               220 < SYST ~ 3))
    
    relevant_data <- relevant_data %>%
      mutate(TIME = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(TIME) %>%
      summarise(MAX.SCORE = max(SCORE))
    
    tibble(TIME = as.numeric(time1)) %>% left_join(relevant_data,by = "TIME") %>%
      mutate(MAX.SCORE = ifelse(is.na(MAX.SCORE),0,MAX.SCORE)) %>%
      pull(MAX.SCORE) %>%
      return
  }else{
    return(rep(0,length(time1)))
  }
}


# Add frequent measurement
add_meas <- function(ID,STAY,time1,meas_data,func,na_value,buffer = hours(24)){
  if(any(grepl("TestName",names(meas_data)))) print(unique(meas_data$TestName))
  if(any(grepl("disp_name",names(meas_data)))) print(unique(meas_data$disp_name))
  print(unique(ID))
  
  relevant_data <-
    meas_data %>% 
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == STAY) %>%
    filter(!is.na(unadjusted_value))
 
  if(nrow(relevant_data) > 0){

    recent_data <- relevant_data %>%
      mutate(DAY = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      filter(DAY %in% time1)

    if(nrow(recent_data) != 0){
      ret_val <-
        recent_data %>% group_by(DAY) %>%
        summarise(RET_VALUE = do.call(what = func,
                                      args = list(x = unadjusted_value)),
                  .groups = "drop") 
      
      tibble(DAY = time1) %>% 
        left_join(ret_val,by = "DAY") %>%
        mutate(RET_VALUE = ifelse(is.na(RET_VALUE),
                                  na_value,
                                  RET_VALUE)) %>%
        pull(RET_VALUE)  %>%
        return
    }else{
      return(rep(na_value,length(time1))) 
    }
     
      
  }else{
    return(rep(na_value,length(time1)))
  }
}

# Add slope hourly slope
add_daily_diff <- function(ID,STAY,time1,data){

  relevant_data <-
    data %>% 
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == STAY) %>%
    filter(!is.na(unadjusted_value))
  
  if(nrow(relevant_data) > 0){
     
    recent_data_1d <- relevant_data %>%
      mutate(DAY = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      group_by(DAY) %>%
      summarise(VALUE_1D_BACK = median(unadjusted_value))
   
    recent_data_2d <- relevant_data %>%
      mutate(DAY = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION)) + 1) %>%
      group_by(DAY) %>%
      summarise(VALUE_2D_BACK = median(unadjusted_value))
    
    recent_data_join = inner_join(recent_data_1d,recent_data_2d,by = "DAY")
    
    
    return(tibble(DAY = time1) %>% 
             left_join(recent_data_join %>% 
                         mutate(DIFF = VALUE_1D_BACK - VALUE_2D_BACK),by = "DAY") %>% 
             pull(DIFF))
  }else{
    return(rep(NA,length(time1)))
  }
}


# Add slope hourly slope
add_slope <- function(ID,STAY,time1,meas_data,extra.buffer = 0){

  relevant_data <-
    meas_data %>% 
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == STAY) %>%
    filter(!is.na(unadjusted_value))
  
  if(nrow(relevant_data) > 0){
    
    recent_data <- relevant_data %>%
      distinct %>%
      mutate(DAY = as.numeric(ceiling(COLLECTION_AFTER_ADMISSION))) %>%
      filter(DAY %in% time1)
    
    # Divide into landmark increments, include up to 1 day + extra.buffer backwards from LM
    (sapply(X = time1,
           FUN = function(x){slope(x = round(recent_data %>% filter(DAY == x | DAY == x-extra.buffer) %>% pull(COLLECTION_AFTER_ADMISSION),2),
                                   y = recent_data %>% filter(DAY == x | DAY == x-extra.buffer) %>% pull(unadjusted_value))})  %>%
      as.numeric)/24 %>%
      return
    
  }else{
    return(rep(0,length(time1)))
  }
}

slope <- function(x,y){
  if(length(x) < 2){
    return(0)
  }else{
    ret = coefficients(lm(y ~ x))[2] 
    if(is.na(ret)) return(0)
    else ret %>%
      return()
  }
}

# Add ICU
add_ICU <- function(ID,STAY,time1,buffer = hours(24)){

  relevant_data <-
    adt_data_rel %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == STAY)  
  
  if(nrow(relevant_data %>% filter(!is.na(ICUSTAY))) > 0){

    OUT_TIMES = lead(relevant_data$IN_DTTM,1)
    OUT_TIMES = replace_na(data = OUT_TIMES,replace = pull_date)
    IN_TIMES = relevant_data$IN_DTTM
    ICUs = which(!is.na(relevant_data$ICUSTAY))
    
    ICU_IN_LM = ceiling(difftime(IN_TIMES[ICUs],min(IN_TIMES),unit = "days"))
    ICU_OUT_LM = ceiling(difftime(OUT_TIMES[ICUs],min(IN_TIMES),unit = "days"))
    
    ICU_LMS = as.numeric(unlist(mapply(x = ICU_IN_LM,y = ICU_OUT_LM,FUN = function(x,y) x:y)))
    
    return(time1 %in% ICU_LMS)
    
  }else{
    return(rep(FALSE,length(time1)))
  }
}

# ADd Steroids

add_steroids <- function(ID,STAYD,time1,buffer = hours(24)){

  relevant_data <-
    steroids_mod %>% 
    filter(STAY == unique(STAYD)) %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 

  
  if(nrow(relevant_data) > 0){

    check_cumdose = function(upper,data,treatment_dose){
      res =
      data %>% filter(TimeAdministered <= upper & TimeAdministered >= upper - hours(24)) %>%
        summarise(CumDose = sum(DoseAsLabelled)) %>%
        ungroup %>%
        mutate(STEROIDS = CumDose >= treatment_dose) %>%
        pull(STEROIDS)
      
      if(length(res) > 0){
        return(any(res))
      }else{
        return(FALSE)
      }
    }
    
    wrapper = function(data,treatment_dose){
      return(sapply(X = data %>% pull(TimeAdministered),
                    treatment_dose = treatment_dose,
                    FUN = check_cumdose,
                    data = data) %>% 
               unlist)
    }
    
    drugframe =
    bind_rows(
    tibble(TIME = relevant_data %>% filter(DrugNameSimplified =="HYDROCORTISONE") %>% pull(TimeAdministered),
           DOSE = wrapper(data =  relevant_data %>% filter(DrugNameSimplified =="HYDROCORTISONE"),
                  treatment_dose = 100)),
    tibble(TIME = relevant_data %>% filter(DrugNameSimplified =="DEXAMETHASONE") %>% pull(TimeAdministered),
           DOSE = wrapper(data =  relevant_data %>% filter(DrugNameSimplified =="DEXAMETHASONE"),
                          treatment_dose = 6)),
    tibble(TIME = relevant_data %>% filter(DrugNameSimplified =="PREDNISALONE") %>% pull(TimeAdministered),
           DOSE = wrapper(data =  relevant_data %>% filter(DrugNameSimplified =="PREDNISALONE"),
                          treatment_dose = 30)),
    tibble(TIME = relevant_data %>% filter(DrugNameSimplified =="METHYLPREDISALONE") %>% pull(TimeAdministered),
           DOSE = wrapper(data =  relevant_data %>% filter(DrugNameSimplified =="METHYLPREDISALONE"),
                          treatment_dose = 0))) 
  
    earliest_time = drugframe %>%
      filter(DOSE) %>%
      arrange(TIME) %>%
      slice(1) %>%
      pull(TIME)
    
    if(!is_empty(earliest_time)){
      return(earliest_time < time1)
    }else{
      return(rep(FALSE,length(time1)))
    }
    
    
  }else{
    return(rep(FALSE,length(time1)))
  }
}

# Add LOCF measurement values
add_dummy <- function(ID,STAY,time1,var,threshold,meas_data){
  if(missing(meas_data)){
    relevant_data <-
      blood_marker %>% 
      filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
      filter(STAY == unique(STAY)) %>%
      filter(TestName == var) %>%
      filter(!is.na(unadjusted_value))
  }else{
    relevant_data <-
      meas_data %>% 
      filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
      filter(STAY == unique(STAY)) %>%
      filter(!is.na(unadjusted_value))
  }

  
  if(nrow(relevant_data) > 0){
    meas_times <- relevant_data$COLLECTION_AFTER_ADMISSION
    distance <- function(y){
      z = y-meas_times
      return(length(which(z[z>=0] < threshold)))
    }
    
    sapply(X = time1,FUN = distance) %>% return 
  }else{
    return(rep(0,length(time1)))
  }
}

# Resp failure
add_rf <- function(ID,STAY,time1){
  relevant_data <-
    combination_ratio_r3_high %>% 
    filter(!is.na(REL_TIME)) %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == STAY) 
  
  if(nrow(relevant_data) > 0){
      af <- stepfun(x = as.numeric(relevant_data$REL_TIME),
                    y = c(0,as.numeric(relevant_data$R3_high)))
      
      check <- function(x){any(af(v = seq(x-1,x,by = 1/24)) == 1)}
      sapply(X = time1,FUN = check) %>% return 
  }else{
    return(FALSE)
  }
}


# Add diagnosis
add_diag <- function(ID,diagnosis_data,calendar_time,buffer){
  relevant_data <-
    diagnosis_data %>% 
    filter(STUDY_SUBJECT_DIGEST == unique(ID))
  
  if(nrow(relevant_data) > 0){
    diagnosis_date_first <- relevant_data$DIAGNOSIS_DATE %>% min
    
    (diagnosis_date_first <= calendar_time)  %>% return 
  }else{
    return(FALSE)
  }
}

# Add bmi
add_bmi <- function(ID,ST,time1,buffer){
  
  relevant_data <- bmi_calc_std %>% 
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == unique(ST))
  

    if(nrow(relevant_data) > 0){

      af <- stepfun(x = as.numeric(relevant_data$COLLECTION_AFTER_ADMISSION),
                    y = c(0,as.numeric(relevant_data$unadjusted_value)))

      check <- function(x){ifelse(max(relevant_data$COLLECTION_AFTER_ADMISSION) <= x - days(buffer),NA,af(x))}

      sapply(X = time1,FUN = check) %>% return 
    }else{
      return(rep(0,length(time1)))
    }
}

# Add GCS
add_gcs <- function(ID,ST,time1,raw = T){
  print(unique(ID))
  relevant_data <- gcs %>% 
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == unique(ST))
  
  if(nrow(relevant_data) > 0){
    
    check <- function(x){
      ret <- relevant_data %>% 
        filter(REL_TIME <= x & REL_TIME >= x - days(1)) %>%
        ungroup %>%
        mutate(interpretation = case_when(measured_value <= 8 ~ "SEVERE",
                                          measured_value > 8 & measured_value <= 12 ~ "MODERATE",
                                          measured_value >= 13 ~ "MINOR")) %>%
        arrange(measured_value) %>%
        slice(1) %>%
        pull(interpretation) 
      
      if(length(ret) == 0) return(NA)
      else return(ret)
      }
    
    sapply(X = time1,FUN = check) %>% unlist %>% return 
  }else{
    return(rep(NA,length(time1)))
  }
}

ph_threshold <- function(x){
  x <- min(x)
  
  return(ifelse(x <= 7.45 & x >= 7.35,
                "REFERENCE",
                ifelse(x < 7.35,
                       "ACIDOSIS",
                       "ALKALOSIS")))
}

add_vent <- function(ID,STAY,time1){
  relevant_data <- ventmode %>%
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == unique(STAY)) %>%
    filter(mechanical_ventilation == "yes")
  
  if(nrow(relevant_data) > 0){
    
    vent_times <- relevant_data %>% pull(REL_TIME) %>%
      floor %>%
      unique
    
    (time1 %in% vent_times) %>% return 
  }else{
    return(rep(FALSE,length(time1)))
  }
}

add_niv <- function(ID,time1){
  relevant_data <- niv %>%
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) 
  
  if(nrow(relevant_data) > 0){
    
    vent_times <- relevant_data %>% pull(MEASURE_TIME) %>%
      # floor %>%
      unique %>% 
      as.POSIXct(format = "%Y-%m-%d %H:%M")
    
    check_vent = function(x,y){
      any(vent_times <y & x < vent_times)
    }
    
 
    
    tibble(LOWER = time1 - hours(24),
           UPPER = time1) %>%
      rowwise() %>%
      mutate(NIV = check_vent(LOWER,UPPER)) %>%
    pull(NIV) %>% return 
  }else{
    return(rep(FALSE,length(time1)))
  }
}

add_medication <- function(ID,STAY,time1,med_data,ever= F){
  relevant_data <- 
    med_data %>%
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == unique(STAY))
  
  if(nrow(relevant_data) > 0){
    
    if(ever){
      med_time <- relevant_data %>% pull(REL_TIME) %>%
        min
      
      (time1 > med_time) %>% as.numeric %>% return
    }else{
      med_times <- relevant_data %>% pull(REL_TIME) %>%
        ceiling %>%
        unique
      
      (time1 %in% med_times) %>% as.numeric %>% return
    }

    

  }else{
    return(rep(0,length(time1)))
  }
}

add_rrt <- function(ID,STAY,time1){
  relevant_data <- 
    rrt_io %>%
    distinct() %>%
    filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    filter(STAY == unique(STAY))
  
  if(nrow(relevant_data) > 0){
    
    med_times <- relevant_data %>% pull(COLLECTION_AFTER_ADMISSION) %>%
      ceiling %>%
      unique
    
    (time1 %in% med_times) %>% as.numeric %>% return 
  }else{
    return(rep(0,length(time1)))
  }
}


# *********************************************** Add Covariates to Skeleton Landmark dataframe

# ICU ADMISSION
marker_matrix_lm_cr_2 <- base_matrix_lm_cr_2 %>%
  mutate(ICU = add_ICU(STUDY_SUBJECT_DIGEST,STAY,INDEX))


# DIAGNOSES
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(HYPERTENSION = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(HYPERTENSION),calendar_time = REAL_TIME)) %>%
  mutate(DIABETES = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(DIABETES),calendar_time = REAL_TIME)) %>%
  mutate(LIVER_DISEASE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(LIVER_DISEASE),calendar_time = REAL_TIME)) %>%
  mutate(ASTHMA = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(ASTHMA),calendar_time = REAL_TIME)) %>%
  mutate(MALIGNANCY_NH = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(MALIGNANCY_NH),calendar_time = REAL_TIME)) %>%
  mutate(MALIGNANCY_H = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(MALIGNANCY_H),calendar_time = REAL_TIME)) %>%
  mutate(STROKE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(STROKE),calendar_time = REAL_TIME)) %>%
  mutate(RESPIRATORY_DISEASE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(RESPIRATORY_DISEASE),calendar_time = REAL_TIME)) %>%
  mutate(RENAL_DISEASE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(RENAL_DISEASE),calendar_time = REAL_TIME)) %>%
  mutate(IMMUNOCOMPROMISED = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(IMMUNOCOMPROMISED),calendar_time = REAL_TIME)) %>%
  mutate(ENDOCRINE_DISEASE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(ENDOCRINE_DISEASE),calendar_time = REAL_TIME)) %>%
  mutate(HEART_DISEASE = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(HEART_DISEASE),calendar_time = REAL_TIME)) %>%
  mutate(DEMENTIA = add_diag(ID = STUDY_SUBJECT_DIGEST,diagnosis_data = medhist_diag_joined %>% filter(DEMENTIA),calendar_time = REAL_TIME)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  # All these sections are functionally the same
  mutate(CRP_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "CRP")) %>% # Get most recent marker at each landmark
  mutate(CRP_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "CRP",threshold = 2)) %>% # Check how many measurements fall within "threshold" days before landmark
  mutate(CRP_MISSING = (CRP_COUNT == 0)) %>% # 0 measurements = missing
  mutate(CRP_LOCF = ifelse(CRP_MISSING,0,CRP_LOCF)) %>% # Measurements that are too old are set to 0
  mutate(CRP_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "CRP"),extra.buffer = 1)) %>% # Add slope over the last 'extra.buffer' + 1 days (artifact from earlier iterations)
  mutate(CRP_SLOPE_MISSING = as.numeric(CRP_COUNT < 2)) %>% # missingness for slope, consider missing if 1 or no measurements are recent enough
  mutate(CRP_SLOPE = ifelse(CRP_SLOPE_MISSING == 1,0,CRP_SLOPE)) %>%  # Adjust slope for missingness
  mutate(CRP_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = crp_std)) %>%
  mutate(CRP_DAILY_CHANGE_MISSING = is.na(CRP_DAILY_CHANGE)) %>%
  mutate(CRP_DAILY_CHANGE = ifelse(CRP_DAILY_CHANGE_MISSING == 1,0,CRP_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(WCC_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "WCC")) %>%
  mutate(WCC_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "WCC",threshold = 2)) %>%
  mutate(WCC_MISSING = (WCC_COUNT == 0)) %>%
  mutate(WCC_LOCF = ifelse(WCC_MISSING,0,WCC_LOCF)) %>%
  mutate(WCC_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "WCC"),extra.buffer = 1)) %>%
  mutate(WCC_SLOPE_MISSING = as.numeric(WCC_COUNT < 2)) %>%
  mutate(WCC_SLOPE = ifelse(WCC_SLOPE_MISSING == 1,0,WCC_SLOPE)) %>% 
  mutate(WCC_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = wcc_std)) %>%
  mutate(WCC_DAILY_CHANGE_MISSING = is.na(WCC_DAILY_CHANGE)) %>%
  mutate(WCC_DAILY_CHANGE = ifelse(WCC_DAILY_CHANGE_MISSING == 1,0,WCC_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(RDW_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "RDW")) %>%
  mutate(RDW_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "RDW",threshold = 2)) %>%
  mutate(RDW_MISSING = (RDW_COUNT == 0)) %>%
  mutate(RDW_LOCF = ifelse(RDW_MISSING,0,RDW_LOCF)) %>%
  mutate(RDW_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "RDW"),extra.buffer = 1)) %>%
  mutate(RDW_SLOPE_MISSING = as.numeric(RDW_COUNT < 2)) %>%
  mutate(RDW_SLOPE = ifelse(RDW_SLOPE_MISSING == 1,0,RDW_SLOPE)) %>%
  mutate(RDW_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = rdw_std)) %>%
  mutate(RDW_DAILY_CHANGE_MISSING = is.na(RDW_DAILY_CHANGE)) %>%
  mutate(RDW_DAILY_CHANGE = ifelse(RDW_DAILY_CHANGE_MISSING == 1,0,RDW_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(FERRITIN_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "FERRITIN")) %>%
  mutate(FERRITIN_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "FERRITIN",threshold = 2)) %>%
  mutate(FERRITIN_MISSING = (FERRITIN_COUNT == 0)) %>%
  mutate(FERRITIN_LOCF = ifelse(FERRITIN_MISSING,0,FERRITIN_LOCF)) %>%
  mutate(FERRITIN_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "FERRITIN"),extra.buffer = 1)) %>%
  mutate(FERRITIN_SLOPE_MISSING = as.numeric(FERRITIN_COUNT < 2)) %>%
  mutate(FERRITIN_SLOPE = ifelse(FERRITIN_SLOPE_MISSING == 1,0,FERRITIN_SLOPE)) %>%
  mutate(FERRITIN_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = ferritin_std)) %>%
  mutate(FERRITIN_DAILY_CHANGE_MISSING = is.na(FERRITIN_DAILY_CHANGE)) %>%
  mutate(FERRITIN_DAILY_CHANGE = ifelse(FERRITIN_DAILY_CHANGE_MISSING == 1,0,FERRITIN_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(HB_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "HB")) %>%
  mutate(HB_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "HB",threshold = 2)) %>%
  mutate(HB_MISSING = (HB_COUNT == 0)) %>%
  mutate(HB_LOCF = ifelse(HB_MISSING,0,HB_LOCF)) %>%
  mutate(HB_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "HB"),extra.buffer = 1)) %>%
  mutate(HB_SLOPE_MISSING = as.numeric(HB_COUNT < 2)) %>%
  mutate(HB_SLOPE = ifelse(HB_SLOPE_MISSING == 1,0,HB_SLOPE)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(HB_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = hb_std)) %>%
  mutate(HB_DAILY_CHANGE_MISSING = is.na(HB_DAILY_CHANGE)) %>%
  mutate(HB_DAILY_CHANGE = ifelse(HB_DAILY_CHANGE_MISSING == 1,0,HB_DAILY_CHANGE))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(IL1_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL1")) %>%
  mutate(IL1_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL1",threshold = 2)) %>%
  mutate(IL1_MISSING = (IL1_COUNT == 0)) %>%
  mutate(IL1_LOCF = ifelse(IL1_MISSING,0,IL1_LOCF)) %>%
  mutate(IL1_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "IL1"),extra.buffer = 1)) %>%
  mutate(IL1_SLOPE_MISSING = as.numeric(IL1_COUNT < 2)) %>%
  mutate(IL1_SLOPE = ifelse(IL1_SLOPE_MISSING == 1,0,IL1_SLOPE)) %>%
  mutate(IL1_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = il1_std)) %>%
  mutate(IL1_DAILY_CHANGE_MISSING = is.na(IL1_DAILY_CHANGE)) %>%
  mutate(IL1_DAILY_CHANGE = ifelse(IL1_DAILY_CHANGE_MISSING == 1,0,IL1_DAILY_CHANGE))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(IG_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IG")) %>%
  mutate(IG_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IG",threshold = 2)) %>%
  mutate(IG_MISSING = (IG_COUNT == 0)) %>%
  mutate(IG_LOCF = ifelse(IG_MISSING,0,IG_LOCF)) %>%
  mutate(IG_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "IG"),extra.buffer = 1)) %>%
  mutate(IG_SLOPE_MISSING = as.numeric(IG_COUNT < 2)) %>%
  mutate(IG_SLOPE = ifelse(IG_SLOPE_MISSING == 1,0,IG_SLOPE)) %>%
  mutate(IG_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = ig_std)) %>%
  mutate(IG_DAILY_CHANGE_MISSING = is.na(IG_DAILY_CHANGE)) %>%
  mutate(IG_DAILY_CHANGE = ifelse(IG_DAILY_CHANGE_MISSING == 1,0,IG_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PCT_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PCT")) %>%
  mutate(PCT_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PCT",threshold = 2)) %>%
  mutate(PCT_MISSING = (PCT_COUNT == 0)) %>%
  mutate(PCT_LOCF = ifelse(PCT_MISSING,0,PCT_LOCF)) %>%
  mutate(PCT_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "PCT"),extra.buffer = 1)) %>%
  mutate(PCT_SLOPE_MISSING = as.numeric(PCT_COUNT < 2)) %>%
  mutate(PCT_SLOPE = ifelse(PCT_SLOPE_MISSING == 1,0,PCT_SLOPE))%>%
  mutate(PCT_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = pct_std)) %>%
  mutate(PCT_DAILY_CHANGE_MISSING = is.na(PCT_DAILY_CHANGE)) %>%
  mutate(PCT_DAILY_CHANGE = ifelse(PCT_DAILY_CHANGE_MISSING == 1,0,PCT_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(DDIMER_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "DDIMER")) %>%
  mutate(DDIMER_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "DDIMER",threshold = 2)) %>%
  mutate(DDIMER_MISSING = (DDIMER_COUNT == 0)) %>%
  mutate(DDIMER_LOCF = ifelse(DDIMER_MISSING,0,DDIMER_LOCF)) %>%
  mutate(DDIMER_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "DDIMER"),extra.buffer = 1)) %>%
  mutate(DDIMER_SLOPE_MISSING = as.numeric(DDIMER_COUNT < 2)) %>%
  mutate(DDIMER_SLOPE = ifelse(DDIMER_SLOPE_MISSING == 1,0,DDIMER_SLOPE)) %>%
  mutate(DDIMER_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = ddimer_std)) %>%
  mutate(DDIMER_DAILY_CHANGE_MISSING = is.na(DDIMER_DAILY_CHANGE)) %>%
  mutate(DDIMER_DAILY_CHANGE = ifelse(DDIMER_DAILY_CHANGE_MISSING == 1,0,DDIMER_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(EOSINOPHILS_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "EOSINOPHILS")) %>%
  mutate(EOSINOPHILS_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "EOSINOPHILS",threshold = 2)) %>%
  mutate(EOSINOPHILS_MISSING = (EOSINOPHILS_COUNT == 0)) %>%
  mutate(EOSINOPHILS_LOCF = ifelse(EOSINOPHILS_MISSING,0,EOSINOPHILS_LOCF)) %>%
  mutate(EOSINOPHILS_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "EOSINOPHILS"),extra.buffer = 1)) %>%
  mutate(EOSINOPHILS_SLOPE_MISSING = as.numeric(EOSINOPHILS_COUNT < 2)) %>%
  mutate(EOSINOPHILS_SLOPE = ifelse(EOSINOPHILS_SLOPE_MISSING == 1,0,EOSINOPHILS_SLOPE))%>%
  mutate(EOSINOPHILS_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = eosinophils_std)) %>%
  mutate(EOSINOPHILS_DAILY_CHANGE_MISSING = is.na(EOSINOPHILS_DAILY_CHANGE)) %>%
  mutate(EOSINOPHILS_DAILY_CHANGE = ifelse(EOSINOPHILS_DAILY_CHANGE_MISSING == 1,0,EOSINOPHILS_DAILY_CHANGE)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(IL_RATIO_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL_RATIO")) %>%
  mutate(IL_RATIO_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL_RATIO",threshold = 2)) %>%
  mutate(IL_RATIO_MISSING = (IL_RATIO_COUNT == 0)) %>%
  mutate(IL_RATIO_LOCF = ifelse(IL_RATIO_MISSING,0,IL_RATIO_LOCF)) %>%
  mutate(IL_RATIO_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "IL_RATIO"),extra.buffer = 1)) %>%
  mutate(IL_RATIO_SLOPE_MISSING = as.numeric(IL_RATIO_COUNT < 2)) %>%
  mutate(IL_RATIO_SLOPE = ifelse(IL_RATIO_SLOPE_MISSING == 1,0,IL_RATIO_SLOPE))%>%
  mutate(IL_RATIO_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = il_ratio_std)) %>%
  mutate(IL_RATIO_DAILY_CHANGE_MISSING = is.na(IL_RATIO_DAILY_CHANGE)) %>%
  mutate(IL_RATIO_DAILY_CHANGE = ifelse(IL_RATIO_DAILY_CHANGE_MISSING == 1,0,IL_RATIO_DAILY_CHANGE)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%  
  mutate(IL10_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL10")) %>%
  mutate(IL10_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL10",threshold = 2)) %>%
  mutate(IL10_MISSING = (IL10_COUNT == 0)) %>%
  mutate(IL10_LOCF = ifelse(IL10_MISSING,0,IL10_LOCF)) %>%
  mutate(IL10_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "IL10"),extra.buffer = 1)) %>%
  mutate(IL10_SLOPE_MISSING = as.numeric(IL10_COUNT < 2)) %>%
  mutate(IL10_SLOPE = ifelse(IL10_SLOPE_MISSING == 1,0,IL10_SLOPE)) %>%
  mutate(IL10_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = il10_std)) %>%
  mutate(IL10_DAILY_CHANGE_MISSING = is.na(IL10_DAILY_CHANGE)) %>%
  mutate(IL10_DAILY_CHANGE = ifelse(IL10_DAILY_CHANGE_MISSING == 1,0,IL10_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(IL6_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL6")) %>%
  mutate(IL6_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "IL6",threshold = 2)) %>%
  mutate(IL6_MISSING = (IL6_COUNT == 0)) %>%
  mutate(IL6_LOCF = ifelse(IL6_MISSING,0,IL6_LOCF)) %>%
  mutate(IL6_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "IL6"),extra.buffer = 1)) %>%
  mutate(IL6_SLOPE_MISSING = as.numeric(IL6_COUNT < 2)) %>%
  mutate(IL6_SLOPE = ifelse(IL6_SLOPE_MISSING == 1,0,IL6_SLOPE))  %>%
  mutate(IL6_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = il6_std)) %>%
  mutate(IL6_DAILY_CHANGE_MISSING = is.na(IL6_DAILY_CHANGE)) %>%
  mutate(IL6_DAILY_CHANGE = ifelse(IL6_DAILY_CHANGE_MISSING == 1,0,IL6_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(LYMPHOCYTES_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LYMPHOCYTES")) %>%
  mutate(LYMPHOCYTES_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LYMPHOCYTES",threshold = 2)) %>%
  mutate(LYMPHOCYTES_MISSING = (LYMPHOCYTES_COUNT == 0)) %>%
  mutate(LYMPHOCYTES_LOCF = ifelse(LYMPHOCYTES_MISSING,0,LYMPHOCYTES_LOCF)) %>%
  mutate(LYMPHOCYTES_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "LYMPHOCYTES"),extra.buffer = 1)) %>%
  mutate(LYMPHOCYTES_SLOPE_MISSING = as.numeric(LYMPHOCYTES_COUNT < 2)) %>%
  mutate(LYMPHOCYTES_SLOPE = ifelse(LYMPHOCYTES_SLOPE_MISSING == 1,0,LYMPHOCYTES_SLOPE)) %>%
  mutate(LYMPHOCYTES_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = lymphocytes_std)) %>%
  mutate(LYMPHOCYTES_DAILY_CHANGE_MISSING = is.na(LYMPHOCYTES_DAILY_CHANGE)) %>%
  mutate(LYMPHOCYTES_DAILY_CHANGE = ifelse(LYMPHOCYTES_DAILY_CHANGE_MISSING == 1,0,LYMPHOCYTES_DAILY_CHANGE)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(MONOCYTES_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "MONOCYTES")) %>%
  mutate(MONOCYTES_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "MONOCYTES",threshold = 2)) %>%
  mutate(MONOCYTES_MISSING = (MONOCYTES_COUNT == 0)) %>%
  mutate(MONOCYTES_LOCF = ifelse(MONOCYTES_MISSING,0,MONOCYTES_LOCF)) %>%
  mutate(MONOCYTES_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "MONOCYTES"),extra.buffer = 1)) %>%
  mutate(MONOCYTES_SLOPE_MISSING = as.numeric(MONOCYTES_COUNT < 2)) %>%
  mutate(MONOCYTES_SLOPE = ifelse(MONOCYTES_SLOPE_MISSING == 1,0,MONOCYTES_SLOPE)) %>%
  mutate(MONOCYTES_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = monocytes_std)) %>%
  mutate(MONOCYTES_DAILY_CHANGE_MISSING = is.na(MONOCYTES_DAILY_CHANGE)) %>%
  mutate(MONOCYTES_DAILY_CHANGE = ifelse(MONOCYTES_DAILY_CHANGE_MISSING == 1,0,MONOCYTES_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(NEUTROPHILS_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "NEUTROPHILS")) %>%
  mutate(NEUTROPHILS_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "NEUTROPHILS",threshold = 2)) %>%
  mutate(NEUTROPHILS_MISSING = (NEUTROPHILS_COUNT == 0)) %>%
  mutate(NEUTROPHILS_LOCF = ifelse(NEUTROPHILS_MISSING,0,NEUTROPHILS_LOCF)) %>%
  mutate(NEUTROPHILS_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "NEUTROPHILS"),extra.buffer = 1)) %>%
  mutate(NEUTROPHILS_SLOPE_MISSING = as.numeric(NEUTROPHILS_COUNT < 2)) %>%
  mutate(NEUTROPHILS_SLOPE = ifelse(NEUTROPHILS_SLOPE_MISSING == 1,0,NEUTROPHILS_SLOPE)) %>%
  mutate(NEUTROPHILS_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = neutrophils_std)) %>%
  mutate(NEUTROPHILS_DAILY_CHANGE_MISSING = is.na(NEUTROPHILS_DAILY_CHANGE)) %>%
  mutate(NEUTROPHILS_DAILY_CHANGE = ifelse(NEUTROPHILS_DAILY_CHANGE_MISSING == 1,0,NEUTROPHILS_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(NL_RATIO_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "NL_RATIO")) %>%
  mutate(NL_RATIO_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "NL_RATIO",threshold = 2)) %>%
  mutate(NL_RATIO_MISSING = (NL_RATIO_COUNT == 0)) %>%
  mutate(NL_RATIO_LOCF = ifelse(NL_RATIO_MISSING,0,NL_RATIO_LOCF)) %>%
  mutate(NL_RATIO_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "NL_RATIO"),extra.buffer = 1)) %>%
  mutate(NL_RATIO_SLOPE_MISSING = as.numeric(NL_RATIO_COUNT < 2)) %>%
  mutate(NL_RATIO_SLOPE = ifelse(NL_RATIO_SLOPE_MISSING == 1,0,NL_RATIO_SLOPE)) %>%
  mutate(NL_RATIO_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = nl_ratio_std)) %>%
  mutate(NL_RATIO_DAILY_CHANGE_MISSING = is.na(NL_RATIO_DAILY_CHANGE)) %>%
  mutate(NL_RATIO_DAILY_CHANGE = ifelse(NL_RATIO_DAILY_CHANGE_MISSING == 1,0,NL_RATIO_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PLATELETS_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PLATELETS")) %>%
  mutate(PLATELETS_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PLATELETS",threshold = 2)) %>%
  mutate(PLATELETS_MISSING = (PLATELETS_COUNT == 0)) %>%
  mutate(PLATELETS_LOCF = ifelse(PLATELETS_MISSING,0,PLATELETS_LOCF)) %>%
  mutate(PLATELETS_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "PLATELETS"),extra.buffer = 1)) %>%
  mutate(PLATELETS_SLOPE_MISSING = as.numeric(PLATELETS_COUNT < 2)) %>%
  mutate(PLATELETS_SLOPE = ifelse(PLATELETS_SLOPE_MISSING == 1,0,PLATELETS_SLOPE)) %>%
  mutate(PLATELETS_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = platelets_std)) %>%
  mutate(PLATELETS_DAILY_CHANGE_MISSING = is.na(PLATELETS_DAILY_CHANGE)) %>%
  mutate(PLATELETS_DAILY_CHANGE = ifelse(PLATELETS_DAILY_CHANGE_MISSING == 1,0,PLATELETS_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(TNFA_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "TNFA")) %>%
  mutate(TNFA_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "TNFA",threshold = 2)) %>%
  mutate(TNFA_MISSING = (TNFA_COUNT == 0)) %>%
  mutate(TNFA_LOCF = ifelse(TNFA_MISSING,0,TNFA_LOCF)) %>%
  mutate(TNFA_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "TNFA"),extra.buffer = 1)) %>%
  mutate(TNFA_SLOPE_MISSING = as.numeric(TNFA_COUNT < 2)) %>%
  mutate(TNFA_SLOPE = ifelse(TNFA_SLOPE_MISSING == 1,0,TNFA_SLOPE))  

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(TNFA_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = tnfa_std)) %>%
  mutate(TNFA_DAILY_CHANGE_MISSING = is.na(TNFA_DAILY_CHANGE)) %>%
  mutate(TNFA_DAILY_CHANGE = ifelse(TNFA_DAILY_CHANGE_MISSING == 1,0,TNFA_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(ASPARTATE_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "ASPARTATE")) %>%
  mutate(ASPARTATE_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "ASPARTATE",threshold = 2)) %>%
  mutate(ASPARTATE_MISSING = (ASPARTATE_COUNT == 0)) %>%
  mutate(ASPARTATE_LOCF = ifelse(ASPARTATE_MISSING,0,ASPARTATE_LOCF)) %>%
  mutate(ASPARTATE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "ASPARTATE"),extra.buffer = 1)) %>%
  mutate(ASPARTATE_SLOPE_MISSING = as.numeric(ASPARTATE_COUNT < 2)) %>%
  mutate(ASPARTATE_SLOPE = ifelse(ASPARTATE_SLOPE_MISSING == 1,0,ASPARTATE_SLOPE))  %>%
  mutate(ASPARTATE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = aspartate_std)) %>%
  mutate(ASPARTATE_DAILY_CHANGE_MISSING = is.na(ASPARTATE_DAILY_CHANGE)) %>%
  mutate(ASPARTATE_DAILY_CHANGE = ifelse(ASPARTATE_DAILY_CHANGE_MISSING == 1,0,ASPARTATE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(LDH_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LDH")) %>%
  mutate(LDH_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LDH",threshold = 2)) %>%
  mutate(LDH_MISSING = (LDH_COUNT == 0)) %>%
  mutate(LDH_LOCF = ifelse(LDH_MISSING,0,LDH_LOCF)) %>%
  mutate(LDH_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "LDH"),extra.buffer = 1)) %>%
  mutate(LDH_SLOPE_MISSING = as.numeric(LDH_COUNT < 2)) %>%
  mutate(LDH_SLOPE = ifelse(LDH_SLOPE_MISSING == 1,0,LDH_SLOPE))  %>%
  mutate(LDH_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = ldh_std)) %>%
  mutate(LDH_DAILY_CHANGE_MISSING = is.na(LDH_DAILY_CHANGE)) %>%
  mutate(LDH_DAILY_CHANGE = ifelse(LDH_DAILY_CHANGE_MISSING == 1,0,LDH_DAILY_CHANGE))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%  
  mutate(TROPONIN_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "TROPONIN")) %>%
  mutate(TROPONIN_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "TROPONIN",threshold = 2)) %>%
  mutate(TROPONIN_MISSING = (TROPONIN_COUNT == 0)) %>%
  mutate(TROPONIN_LOCF = ifelse(TROPONIN_MISSING,0,TROPONIN_LOCF)) %>%
  mutate(TROPONIN_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "TROPONIN"),extra.buffer = 1)) %>%
  mutate(TROPONIN_SLOPE_MISSING = as.numeric(TROPONIN_COUNT < 2)) %>%
  mutate(TROPONIN_SLOPE = ifelse(TROPONIN_SLOPE_MISSING == 1,0,TROPONIN_SLOPE))  %>%
  mutate(TROPONIN_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = troponin_std)) %>%
  mutate(TROPONIN_DAILY_CHANGE_MISSING = is.na(TROPONIN_DAILY_CHANGE)) %>%
  mutate(TROPONIN_DAILY_CHANGE = ifelse(TROPONIN_DAILY_CHANGE_MISSING == 1,0,TROPONIN_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(APTT_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "APTT")) %>%
  mutate(APTT_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "APTT",threshold = 2)) %>%
  mutate(APTT_MISSING = (APTT_COUNT == 0)) %>%
  mutate(APTT_LOCF = ifelse(APTT_MISSING,0,APTT_LOCF)) %>%
  mutate(APTT_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "APTT"),extra.buffer = 1)) %>%
  mutate(APTT_SLOPE_MISSING = as.numeric(APTT_COUNT < 2)) %>%
  mutate(APTT_SLOPE = ifelse(APTT_SLOPE_MISSING == 1,0,APTT_SLOPE)) %>%
  mutate(APTT_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = aptt_std)) %>%
  mutate(APTT_DAILY_CHANGE_MISSING = is.na(APTT_DAILY_CHANGE)) %>%
  mutate(APTT_DAILY_CHANGE = ifelse(APTT_DAILY_CHANGE_MISSING == 1,0,APTT_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PT_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PT")) %>%
  mutate(PT_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PT",threshold = 2)) %>%
  mutate(PT_MISSING = (PT_COUNT == 0)) %>%
  mutate(PT_LOCF = ifelse(PT_MISSING,0,PT_LOCF)) %>%
  mutate(PT_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "PT"),extra.buffer = 1)) %>%
  mutate(PT_SLOPE_MISSING = as.numeric(PT_COUNT < 2)) %>%
  mutate(PT_SLOPE = ifelse(PT_SLOPE_MISSING == 1,0,PT_SLOPE)) %>%
  mutate(PT_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = pt_std)) %>%
  mutate(PT_DAILY_CHANGE_MISSING = is.na(PT_DAILY_CHANGE)) %>%
  mutate(PT_DAILY_CHANGE = ifelse(PT_DAILY_CHANGE_MISSING == 1,0,PT_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(MAGNESIUM_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "MAGNESIUM")) %>%
  mutate(MAGNESIUM_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "MAGNESIUM",threshold = 2)) %>%
  mutate(MAGNESIUM_MISSING = (MAGNESIUM_COUNT == 0)) %>%
  mutate(MAGNESIUM_LOCF = ifelse(MAGNESIUM_MISSING,0,MAGNESIUM_LOCF)) %>%
  mutate(MAGNESIUM_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "MAGNESIUM"),extra.buffer = 1)) %>%
  mutate(MAGNESIUM_SLOPE_MISSING = as.numeric(MAGNESIUM_COUNT < 2)) %>%
  mutate(MAGNESIUM_SLOPE = ifelse(MAGNESIUM_SLOPE_MISSING == 1,0,MAGNESIUM_SLOPE)) %>%
  mutate(MAGNESIUM_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = magnesium_std)) %>%
  mutate(MAGNESIUM_DAILY_CHANGE_MISSING = is.na(MAGNESIUM_DAILY_CHANGE)) %>%
  mutate(MAGNESIUM_DAILY_CHANGE = ifelse(MAGNESIUM_DAILY_CHANGE_MISSING == 1,0,MAGNESIUM_DAILY_CHANGE)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%  
  mutate(PHOSPHATE_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PHOSPHATE")) %>%
  mutate(PHOSPHATE_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "PHOSPHATE",threshold = 2)) %>%
  mutate(PHOSPHATE_MISSING = (PHOSPHATE_COUNT == 0)) %>%
  mutate(PHOSPHATE_LOCF = ifelse(PHOSPHATE_MISSING,0,PHOSPHATE_LOCF)) %>%
  mutate(PHOSPHATE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "PHOSPHATE"),extra.buffer = 1)) %>%
  mutate(PHOSPHATE_SLOPE_MISSING = as.numeric(PHOSPHATE_COUNT < 2)) %>%
  mutate(PHOSPHATE_SLOPE = ifelse(PHOSPHATE_SLOPE_MISSING == 1,0,PHOSPHATE_SLOPE))  %>%
  mutate(PHOSPHATE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = phosphate_std)) %>%
  mutate(PHOSPHATE_DAILY_CHANGE_MISSING = is.na(PHOSPHATE_DAILY_CHANGE)) %>%
  mutate(PHOSPHATE_DAILY_CHANGE = ifelse(PHOSPHATE_DAILY_CHANGE_MISSING == 1,0,PHOSPHATE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(ALKALINE_PHOSPHATASE_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "ALKALINE_PHOSPHATASE")) %>%
  mutate(ALKALINE_PHOSPHATASE_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "ALKALINE_PHOSPHATASE",threshold = 2)) %>%
  mutate(ALKALINE_PHOSPHATASE_MISSING = (ALKALINE_PHOSPHATASE_COUNT == 0)) %>%
  mutate(ALKALINE_PHOSPHATASE_LOCF = ifelse(ALKALINE_PHOSPHATASE_MISSING,0,ALKALINE_PHOSPHATASE_LOCF)) %>%
  mutate(ALKALINE_PHOSPHATASE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "ALKALINE_PHOSPHATASE"),extra.buffer = 1)) %>%
  mutate(ALKALINE_PHOSPHATASE_SLOPE_MISSING = as.numeric(ALKALINE_PHOSPHATASE_COUNT < 2)) %>%
  mutate(ALKALINE_PHOSPHATASE_SLOPE = ifelse(ALKALINE_PHOSPHATASE_SLOPE_MISSING == 1,0,ALKALINE_PHOSPHATASE_SLOPE)) %>%
  mutate(ALKALINE_PHOSPHATASE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = alkaline_phosphatase_std)) %>%
  mutate(ALKALINE_PHOSPHATASE_DAILY_CHANGE_MISSING = is.na(ALKALINE_PHOSPHATASE_DAILY_CHANGE)) %>%
  mutate(ALKALINE_PHOSPHATASE_DAILY_CHANGE = ifelse(ALKALINE_PHOSPHATASE_DAILY_CHANGE_MISSING == 1,0,ALKALINE_PHOSPHATASE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(LACTATE_LOCF = add_marker(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LACTATE")) %>%
  mutate(LACTATE_COUNT = add_dummy(STUDY_SUBJECT_DIGEST,STAY,time1 = INDEX,var = "LACTATE",threshold = 2)) %>%
  mutate(LACTATE_MISSING = (LACTATE_COUNT == 0)) %>%
  mutate(LACTATE_LOCF = ifelse(LACTATE_MISSING,0,LACTATE_LOCF)) %>%
  mutate(LACTATE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = blood_marker %>% filter(TestName == "LACTATE"),extra.buffer = 1)) %>%
  mutate(LACTATE_SLOPE_MISSING = as.numeric(LACTATE_COUNT < 2)) %>%
  mutate(LACTATE_SLOPE = ifelse(LACTATE_SLOPE_MISSING == 1,0,LACTATE_SLOPE))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(LACTATE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = lactate_std)) %>%
  mutate(LACTATE_DAILY_CHANGE_MISSING = is.na(LACTATE_DAILY_CHANGE)) %>%
  mutate(LACTATE_DAILY_CHANGE = ifelse(LACTATE_DAILY_CHANGE_MISSING == 1,0,LACTATE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BMI = add_bmi(ID = STUDY_SUBJECT_DIGEST,ST = STAY,time1 = INDEX,buffer = days(365))) %>%
  mutate(BMI_MISSING = (BMI == 0)) %>%
  mutate(BMI = ifelse((BMI == 0),0,BMI))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(GCS = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,marker_mat = gcs_std,var = "GCS"))# %>%
  # mutate(GCS_DUMMY = is.na(GCS)) %>%
  #mutate(GCS = ifelse(is.na(GCS),"MINOR",GCS)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%  
  mutate(VENTILATON = add_vent(STUDY_SUBJECT_DIGEST,STAY,INDEX)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  left_join(adm_data_rel %>% select(STUDY_SUBJECT_DIGEST,STAY,GENDER_DESC,ETHNIC_GROUP_GROUPED,AGE_AT_ADM),
            by = c("STUDY_SUBJECT_DIGEST","STAY")) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  # Observations work a little different, as we summarise. func applies a function that is applied to the eligible obs/tests.
  mutate(ALBUMIN_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = albumin_std,func = max,na_value = 0,buffer = hours(48))) %>%
  mutate(ALBUMIN_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = albumin_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(ALBUMIN_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = albumin_std,extra.buffer = 1)) %>%
  mutate(ALBUMIN_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = albumin_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(ALBUMIN_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = albumin_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(ALBUMIN_MISSING = ifelse(ALBUMIN_COUNT == 0,1,0)) %>%
  mutate(ALBUMIN_SLOPE_MISSING = ifelse(ALBUMIN_COUNT < 2,1,0)) %>%
  mutate(ALBUMIN_SLOPE = ifelse(ALBUMIN_SLOPE_MISSING == 1,0,ALBUMIN_SLOPE)) %>%
  mutate(ALBUMIN_SD = ifelse(ALBUMIN_SLOPE_MISSING == 1,1,ALBUMIN_SD)) %>%
  mutate(ALBUMIN_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "ALBUMIN",marker_mat = albumin_std)) %>%
  mutate(ALBUMIN_LOCF = ifelse(ALBUMIN_MISSING == 1,0,ALBUMIN_LOCF))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(ALBUMIN_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = albumin_std)) %>%
  mutate(ALBUMIN_DAILY_CHANGE_MISSING = is.na(ALBUMIN_DAILY_CHANGE)) %>%
  mutate(ALBUMIN_DAILY_CHANGE = ifelse(ALBUMIN_DAILY_CHANGE_MISSING == 1,0,ALBUMIN_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(ALANINE_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = alanine_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(ALANINE_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = alanine_std,func = max,na_value = 0,buffer = hours(48))) %>%
  mutate(ALANINE_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = alanine_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(ALANINE_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = alanine_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(ALANINE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = alanine_std,extra.buffer = 1)) %>%
  mutate(ALANINE_MISSING = ifelse(ALANINE_COUNT == 0,1,0)) %>%
  mutate(ALANINE_SLOPE_MISSING = ifelse(ALANINE_COUNT < 2,1,0)) %>%
  mutate(ALANINE_SLOPE = ifelse(ALANINE_SLOPE_MISSING == 1,0,ALANINE_SLOPE)) %>%
  mutate(ALANINE_SD = ifelse(ALANINE_SLOPE_MISSING == 1,1,ALANINE_SD)) %>%
  mutate(ALANINE_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "ALANINE",marker_mat = alanine_std)) %>%
  mutate(ALANINE_LOCF = ifelse(ALANINE_MISSING == 1,0,ALANINE_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(ALANINE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = alanine_std)) %>%
  mutate(ALANINE_DAILY_CHANGE_MISSING = is.na(ALANINE_DAILY_CHANGE)) %>%
  mutate(ALANINE_DAILY_CHANGE = ifelse(ALANINE_DAILY_CHANGE_MISSING == 1,0,ALANINE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BILIRUBIN_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bilirubin_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(BILIRUBIN_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bilirubin_std,func = max,na_value = 0,buffer = hours(48))) %>%
  mutate(BILIRUBIN_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =bilirubin_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(BILIRUBIN_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bilirubin_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(BILIRUBIN_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bilirubin_std,extra.buffer = 1)) %>%
  mutate(BILIRUBIN_MISSING = ifelse(BILIRUBIN_COUNT == 0,1,0)) %>%
  mutate(BILIRUBIN_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "BILIRUBIN",marker_mat = bilirubin_std)) %>%
  mutate(BILIRUBIN_SLOPE_MISSING = ifelse(BILIRUBIN_COUNT < 2,1,0)) %>%
  mutate(BILIRUBIN_SLOPE = ifelse(BILIRUBIN_SLOPE_MISSING == 1,0,BILIRUBIN_SLOPE)) %>%
  mutate(BILIRUBIN_SD = ifelse(BILIRUBIN_SLOPE_MISSING == 1,1,BILIRUBIN_SD)) %>%
  mutate(BILIRUBIN_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "BILIRUBIN",marker_mat = bilirubin_std)) %>%
  mutate(BILIRUBIN_LOCF = ifelse(BILIRUBIN_MISSING == 1,0,BILIRUBIN_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BILIRUBIN_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = bilirubin_std)) %>%
  mutate(BILIRUBIN_DAILY_CHANGE_MISSING = is.na(BILIRUBIN_DAILY_CHANGE)) %>%
  mutate(BILIRUBIN_DAILY_CHANGE = ifelse(BILIRUBIN_DAILY_CHANGE_MISSING == 1,0,BILIRUBIN_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CHLORIDE_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = chloride_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(CHLORIDE_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =chloride_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(CHLORIDE_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = chloride_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(CHLORIDE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = chloride_std,extra.buffer = 1)) %>%
  mutate(CHLORIDE_MISSING = ifelse(CHLORIDE_COUNT == 0,1,0)) %>%
  mutate(CHLORIDE_SLOPE_MISSING = ifelse(CHLORIDE_COUNT < 2,1,0)) %>%
  mutate(CHLORIDE_SLOPE = ifelse(CHLORIDE_SLOPE_MISSING == 1,0,CHLORIDE_SLOPE)) %>%
  mutate(CHLORIDE_SD = ifelse(CHLORIDE_SLOPE_MISSING == 1,1,CHLORIDE_SD)) %>%
  mutate(CHLORIDE_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "CHLORIDE",marker_mat = chloride_std)) %>%
  mutate(CHLORIDE_LOCF = ifelse(CHLORIDE_MISSING == 1,0,CHLORIDE_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CHLORIDE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = chloride_std)) %>%
  mutate(CHLORIDE_DAILY_CHANGE_MISSING = is.na(CHLORIDE_DAILY_CHANGE)) %>%
  mutate(CHLORIDE_DAILY_CHANGE = ifelse(CHLORIDE_DAILY_CHANGE_MISSING == 1,0,CHLORIDE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CORTISOL_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = cortisol_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(CORTISOL_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =cortisol_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(CORTISOL_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = cortisol_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(CORTISOL_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = cortisol_std,extra.buffer = 1)) %>%
  mutate(CORTISOL_MISSING = ifelse(CORTISOL_COUNT == 0,1,0)) %>%
  mutate(CORTISOL_SLOPE_MISSING = ifelse(CORTISOL_COUNT < 2,1,0)) %>%
  mutate(CORTISOL_SLOPE = ifelse(CORTISOL_SLOPE_MISSING == 1,0,CORTISOL_SLOPE)) %>%
  mutate(CORTISOL_SD = ifelse(CORTISOL_SLOPE_MISSING == 1,1,CORTISOL_SD)) %>%
  mutate(CORTISOL_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "CORTISOL",marker_mat = cortisol_std)) %>%
  mutate(CORTISOL_LOCF = ifelse(CORTISOL_MISSING == 1,0,CORTISOL_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CORTISOL_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = cortisol_std)) %>%
  mutate(CORTISOL_DAILY_CHANGE_MISSING = is.na(CORTISOL_DAILY_CHANGE)) %>%
  mutate(CORTISOL_DAILY_CHANGE = ifelse(CORTISOL_DAILY_CHANGE_MISSING == 1,0,CORTISOL_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CREATININE_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = creatinine_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(CREATININE_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = creatinine_std,func = max,na_value = 0,buffer = hours(48))) %>%
  mutate(CREATININE_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =creatinine_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(CREATININE_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = creatinine_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(CREATININE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = creatinine_std,extra.buffer = 1)) %>%
  mutate(CREATININE_MISSING = ifelse(CREATININE_COUNT == 0,1,0)) %>%
  mutate(CREATININE_SLOPE_MISSING = ifelse(CREATININE_COUNT < 2,1,0)) %>%
  mutate(CREATININE_SLOPE = ifelse(CREATININE_SLOPE_MISSING == 1,0,CREATININE_SLOPE)) %>%
  mutate(CREATININE_SD = ifelse(CREATININE_SLOPE_MISSING == 1,1,CREATININE_SD)) %>%
  mutate(CREATININE_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "CREATININE",marker_mat = creatinine_std)) %>%
  mutate(CREATININE_LOCF = ifelse(CREATININE_MISSING == 1,0,CREATININE_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(CREATININE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = creatinine_std)) %>%
  mutate(CREATININE_DAILY_CHANGE_MISSING = is.na(CREATININE_DAILY_CHANGE)) %>%
  mutate(CREATININE_DAILY_CHANGE = ifelse(CREATININE_DAILY_CHANGE_MISSING == 1,0,CREATININE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(UREA_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = urea_std,func = mean,na_value = 0,buffer = hours(48))) %>%
  mutate(UREA_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = urea_std,func = max,na_value = 0,buffer = hours(48))) %>%
  mutate(UREA_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =urea_std,func = sd,na_value = 1,buffer = hours(48))) %>%
  mutate(UREA_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = urea_std,func = length,na_value = 0,buffer = hours(48))) %>%
  mutate(UREA_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = urea_std,extra.buffer = 1)) %>%
  mutate(UREA_MISSING = ifelse(UREA_COUNT == 0,1,0)) %>%
  mutate(UREA_SLOPE_MISSING = ifelse(UREA_COUNT < 2,1,0)) %>%
  mutate(UREA_SLOPE = ifelse(UREA_SLOPE_MISSING == 1,0,UREA_SLOPE)) %>%
  mutate(UREA_SD = ifelse(UREA_SLOPE_MISSING == 1,1,UREA_SD)) %>%
  mutate(UREA_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "UREA",marker_mat = urea_std)) %>%
  mutate(UREA_LOCF = ifelse(UREA_MISSING == 1,0,UREA_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(UREA_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = urea_std)) %>%
  mutate(UREA_DAILY_CHANGE_MISSING = is.na(UREA_DAILY_CHANGE)) %>%
  mutate(UREA_DAILY_CHANGE = ifelse(UREA_DAILY_CHANGE_MISSING == 1,0,UREA_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PULSE_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pulse_std,func = mean,na_value = 0)) %>%
  mutate(PULSE_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pulse_std,func = min,na_value = 0)) %>%
  mutate(PULSE_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pulse_std,func = max,na_value = 0)) %>%
  mutate(PULSE_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =pulse_std,func = sd,na_value = 1)) %>%
  mutate(PULSE_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pulse_std,func = length,na_value = 0)) %>%
  mutate(PULSE_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pulse_std)) %>%
  mutate(PULSE_MISSING = ifelse(PULSE_COUNT == 0,1,0)) %>%
  mutate(PULSE_SLOPE_MISSING = ifelse(PULSE_COUNT < 2,1,0)) %>%
  mutate(PULSE_SLOPE = ifelse(PULSE_SLOPE_MISSING == 1,0,PULSE_SLOPE)) %>%
  mutate(PULSE_SD = ifelse(PULSE_SLOPE_MISSING == 1,1,PULSE_SD)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PULSE_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = pulse_std)) %>%
  mutate(PULSE_DAILY_CHANGE_MISSING = is.na(PULSE_DAILY_CHANGE)) %>%
  mutate(PULSE_DAILY_CHANGE = ifelse(PULSE_DAILY_CHANGE_MISSING == 1,0,PULSE_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(RR_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = rr_std,func = mean,na_value = 0)) %>%
  mutate(RR_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = rr_std,func = max,na_value = 0)) %>%
  mutate(RR_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = rr_std,func = min,na_value = 0)) %>%
  mutate(RR_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =rr_std,func = sd,na_value = 1)) %>%
  mutate(RR_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = rr_std,func = length,na_value = 0)) %>%
  mutate(RR_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = rr_std)) %>%
  mutate(RR_MISSING = ifelse(RR_COUNT == 0,1,0)) %>%
  mutate(RR_SLOPE_MISSING = ifelse(RR_COUNT < 2,1,0)) %>%
  mutate(RR_SLOPE = ifelse(RR_SLOPE_MISSING == 1,0,RR_SLOPE)) %>%
  mutate(RR_SD = ifelse(RR_SLOPE_MISSING == 1,1,RR_SD)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(RR_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = rr_std)) %>%
  mutate(RR_DAILY_CHANGE_MISSING = is.na(RR_DAILY_CHANGE)) %>%
  mutate(RR_DAILY_CHANGE = ifelse(RR_DAILY_CHANGE_MISSING == 1,0,RR_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PH_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = ph_std,func = length,na_value = 0)) %>%
  mutate(PH_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = ph_std,func = min,na_value = 0)) %>%
  mutate(PH_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = ph_std,func = max,na_value = 0)) %>%
  mutate(PH_MISSING = ifelse(PH_COUNT == 0,1,0)) %>%
  mutate(PH_X1_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  ph_x1 %>% mutate(unadjusted_value = VALUE),func = max,na_value = 0)) %>%
  mutate(PH_X2_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  ph_x2 %>% mutate(unadjusted_value = VALUE),func = max,na_value = 0)) %>%
  mutate(PH_X1_MAX = ifelse(PH_X1_MAX == pmax(PH_X1_MAX,PH_X2_MAX),PH_X1_MAX,0)) %>%
  mutate(PH_X2_MAX = ifelse(PH_X2_MAX == pmax(PH_X1_MAX,PH_X2_MAX),PH_X2_MAX,0)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PH_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = ph_std)) %>%
  mutate(PH_DAILY_CHANGE_MISSING = is.na(PH_DAILY_CHANGE)) %>%
  mutate(PH_DAILY_CHANGE = ifelse(PH_DAILY_CHANGE_MISSING == 1,0,PH_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PH_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = ph_std,func = mean,na_value = 0)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(SODIUM_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = sodium_std,func = mean,na_value = 0)) %>%
  mutate(SODIUM_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =sodium_std,func = sd,na_value = 1)) %>%
  mutate(SODIUM_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = sodium_std,func = length,na_value = 0)) %>%
  mutate(SODIUM_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = sodium_std)) %>%
  mutate(SODIUM_MISSING = ifelse(SODIUM_COUNT == 0,1,0)) %>%
  mutate(SODIUM_SLOPE_MISSING = ifelse(SODIUM_COUNT < 2,1,0)) %>%
  mutate(SODIUM_SLOPE = ifelse(SODIUM_SLOPE_MISSING == 1,0,SODIUM_SLOPE)) %>%
  mutate(SODIUM_SD = ifelse(SODIUM_SLOPE_MISSING == 1,1,SODIUM_SD)) %>%
  mutate(SODIUM_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "SODIUM",marker_mat = sodium_std)) %>%
  mutate(SODIUM_LOCF = ifelse(SODIUM_MISSING == 1,0,SODIUM_LOCF))  

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(SODIUM_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = sodium_std)) %>%
  mutate(SODIUM_DAILY_CHANGE_MISSING = is.na(SODIUM_DAILY_CHANGE)) %>%
  mutate(SODIUM_DAILY_CHANGE = ifelse(SODIUM_DAILY_CHANGE_MISSING == 1,0,SODIUM_DAILY_CHANGE))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(POTASSIUM_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = potassium_std,func = mean,na_value = 0)) %>%
  mutate(POTASSIUM_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =potassium_std,func = sd,na_value = 1)) %>%
  mutate(POTASSIUM_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = potassium_std,func = length,na_value = 0)) %>%
  mutate(POTASSIUM_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = potassium_std)) %>%
  mutate(POTASSIUM_MISSING = ifelse(POTASSIUM_COUNT == 0,1,0)) %>%
  mutate(POTASSIUM_SLOPE_MISSING = ifelse(POTASSIUM_COUNT < 2,1,0)) %>%
  mutate(POTASSIUM_SLOPE = ifelse(POTASSIUM_SLOPE_MISSING == 1,0,POTASSIUM_SLOPE)) %>%
  mutate(POTASSIUM_SD = ifelse(POTASSIUM_SLOPE_MISSING == 1,1,POTASSIUM_SD)) %>%
  mutate(POTASSIUM_LOCF = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "POTASSIUM",marker_mat = potassium_std)) %>%
  mutate(POTASSIUM_LOCF = ifelse(POTASSIUM_MISSING == 1,0,POTASSIUM_LOCF)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(POTASSIUM_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = potassium_std)) %>%
  mutate(POTASSIUM_DAILY_CHANGE_MISSING = is.na(POTASSIUM_DAILY_CHANGE)) %>%
  mutate(POTASSIUM_DAILY_CHANGE = ifelse(POTASSIUM_DAILY_CHANGE_MISSING == 1,0,POTASSIUM_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BP_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bp_std,func = mean,na_value = 0)) %>%
  mutate(BP_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bp_std,func = min,na_value = 0)) %>%
  mutate(BP_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bp_std,func = max,na_value = 0)) %>%
  mutate(BP_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =bp_std,func = sd,na_value = 1)) %>%
  mutate(BP_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bp_std,func = length,na_value = 0)) %>%
  mutate(BP_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = bp_std)) %>%
  mutate(BP_MISSING = ifelse(BP_COUNT == 0,1,0)) %>%
  mutate(BP_SLOPE_MISSING = ifelse(BP_COUNT < 2,1,0)) %>%
  mutate(BP_SLOPE = ifelse(BP_SLOPE_MISSING == 1,0,BP_SLOPE)) %>%
  mutate(BP_SD = ifelse(BP_SLOPE_MISSING == 1,1,BP_SD))  

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BP_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = bp_std)) %>%
  mutate(BP_DAILY_CHANGE_MISSING = is.na(BP_DAILY_CHANGE)) %>%
  mutate(BP_DAILY_CHANGE = ifelse(BP_DAILY_CHANGE_MISSING == 1,0,BP_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(TEMP_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = temp_std,func = mean,na_value = 0)) %>%
  mutate(TEMP_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = temp_std,func = max,na_value = 0)) %>%
  mutate(TEMP_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = temp_std,func = min,na_value = 0)) %>%
  mutate(TEMP_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =temp_std,func = sd,na_value = 1)) %>%
  mutate(TEMP_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = temp_std,func = length,na_value = 0)) %>%
  mutate(TEMP_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = temp_std)) %>%
  mutate(TEMP_MISSING = ifelse(TEMP_COUNT == 0,1,0)) %>%
  mutate(TEMP_SLOPE_MISSING = ifelse(TEMP_COUNT < 2,1,0)) %>%
  mutate(TEMP_SLOPE = ifelse(TEMP_SLOPE_MISSING == 1,0,TEMP_SLOPE)) %>%
  mutate(TEMP_SD = ifelse(TEMP_SLOPE_MISSING == 1,1,TEMP_SD))  

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(TEMP_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = temp_std)) %>%
  mutate(TEMP_DAILY_CHANGE_MISSING = is.na(TEMP_DAILY_CHANGE)) %>%
  mutate(TEMP_DAILY_CHANGE = ifelse(TEMP_DAILY_CHANGE_MISSING == 1,0,TEMP_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(SF_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  sf_ratio_std,func = mean,na_value = 0)) %>%
  mutate(SF_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  sf_ratio_std,func = max,na_value = 0)) %>%
  mutate(SF_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  sf_ratio_std,func = min,na_value = 0)) %>%
  mutate(SF_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = sf_ratio_std,func = sd,na_value = 1)) %>%
  mutate(SF_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  sf_ratio_std,func = length,na_value = 0)) %>%
  mutate(SF_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  sf_ratio_std)) %>%
  mutate(SF_MISSING = ifelse(SF_COUNT == 0,1,0)) %>%
  mutate(SF_SLOPE_MISSING = ifelse(SF_COUNT < 2,1,0)) %>%
  mutate(SF_SLOPE = ifelse(SF_SLOPE_MISSING == 1,0,SF_SLOPE)) %>%
  mutate(SF_SD = ifelse(SF_SLOPE_MISSING == 1,1,SF_SD)) 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(SF_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = sf_ratio_std)) %>%
  mutate(SF_DAILY_CHANGE_MISSING = is.na(SF_DAILY_CHANGE)) %>%
  mutate(SF_DAILY_CHANGE = ifelse(SF_DAILY_CHANGE_MISSING == 1,0,SF_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PF_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  pf_ratio_std,func = mean,na_value = 0)) %>%
  mutate(PF_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  pf_ratio_std,func = max,na_value = 0)) %>%
  mutate(PF_MIN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  pf_ratio_std,func = min,na_value = 0)) %>%
  mutate(PF_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = pf_ratio_std,func = sd,na_value = 1)) %>%
  mutate(PF_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  pf_ratio_std,func = length,na_value = 0)) %>%
  mutate(PF_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  pf_ratio_std)) %>%
  mutate(PF_MISSING = ifelse(PF_COUNT == 0,1,0)) %>%
  mutate(PF_SLOPE_MISSING = ifelse(PF_COUNT < 2,1,0)) %>%
  mutate(PF_SLOPE = ifelse(PF_SLOPE_MISSING == 1,0,PF_SLOPE)) %>%
  mutate(PF_SD = ifelse(PF_SLOPE_MISSING == 1,1,PF_SD))  

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(PF_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = pf_ratio_std)) %>%
  mutate(PF_DAILY_CHANGE_MISSING = is.na(PF_DAILY_CHANGE)) %>%
  mutate(PF_DAILY_CHANGE = ifelse(PF_DAILY_CHANGE_MISSING == 1,0,PF_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(NEWS2_MEAN = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  news2_std,func = mean,na_value = 0)) %>%
  mutate(NEWS2_MAX = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  news2_std,func = max,na_value = 0)) %>%
  mutate(NEWS2_SD = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data = news2_std,func = sd,na_value = 1)) %>%
  mutate(NEWS2_COUNT = add_meas(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  news2_std,func = length,na_value = 0)) %>%
  mutate(NEWS2_SLOPE = add_slope(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,meas_data =  news2_std)) %>%
  mutate(NEWS2_MISSING = ifelse(NEWS2_COUNT == 0,1,0)) %>%
  mutate(NEWS2_SLOPE_MISSING = ifelse(NEWS2_COUNT < 2,1,0)) %>%
  mutate(NEWS2_SLOPE = ifelse(NEWS2_SLOPE_MISSING == 1,0,NEWS2_SLOPE)) %>%
  mutate(NEWS2_SD = ifelse(NEWS2_SLOPE_MISSING == 1,1,NEWS2_SD))  


marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(NEWS2_DAILY_CHANGE = add_daily_diff(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,data = news2_std)) %>%
  mutate(NEWS2_DAILY_CHANGE_MISSING = is.na(NEWS2_DAILY_CHANGE)) %>%
  mutate(NEWS2_DAILY_CHANGE = ifelse(NEWS2_DAILY_CHANGE_MISSING == 1,0,NEWS2_DAILY_CHANGE))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(DNAR = add_dnar(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "DNAR")) %>%
  mutate(PALLIATIVE = add_pall(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,var = "PALLIATIVE"))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(VASOPRESSORS = add_medication(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,med_data = vasopressors)) %>%
  mutate(INOTROPES = add_medication(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,med_data = inotropes)) %>%
  mutate(STEROIDS = add_steroids()) %>%
  mutate(CARDIO_VASCULAR_SUPPORT = ifelse(VASOPRESSORS + INOTROPES > 0,1,0)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(SODIUM_X1_MAX = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,marker_mat =  sodium_x1 %>% mutate(unadjusted_value = VALUE))) %>%
  mutate(SODIUM_X2_MAX = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,marker_mat =  sodium_x2 %>% mutate(unadjusted_value = VALUE))) %>%
  mutate(SODIUM_X1_MAX = ifelse(SODIUM_X1_MAX == pmax(SODIUM_X1_MAX,SODIUM_X2_MAX),SODIUM_X1_MAX,0)) %>%
  mutate(SODIUM_X2_MAX = ifelse(SODIUM_X2_MAX == pmax(SODIUM_X1_MAX,SODIUM_X2_MAX),SODIUM_X2_MAX,0))
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(BMI_X1_MAX = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,marker_mat =  bmi_x1%>% mutate(unadjusted_value = VALUE))) %>%
  mutate(BMI_X2_MAX = add_marker(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX,marker_mat =  bmi_x2%>% mutate(unadjusted_value = VALUE))) %>%
  mutate(BMI_X1_MAX = ifelse(BMI_X1_MAX == pmax(BMI_X1_MAX,BMI_X2_MAX),BMI_X1_MAX,0)) %>%
  mutate(BMI_X2_MAX = ifelse(BMI_X2_MAX == pmax(BMI_X1_MAX,BMI_X2_MAX),BMI_X2_MAX,0)) 
  
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(RRT = add_rrt(ID = STUDY_SUBJECT_DIGEST,STAY = STAY,time1 = INDEX))


saveRDS(marker_matrix_lm_cr_2,rds_file("lm_matrix_intermediate_12_04_2021"))
marker_matrix_lm_cr_2 = readRDS(rds_file("lm_matrix_intermediate_12_04_2021"))



# Post adjusting covariates
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(GCS_BELOW_9 = (GCS < 9)) %>%
  mutate(GCS_BELOW_12 = (GCS < 12)) %>%
  select(-GCS) %>%
  mutate(GENDER_DESC = (GENDER_DESC == "Male")) %>%
  mutate(WHITE = (ETHNIC_GROUP_GROUPED == "White\n"))%>%
  mutate(ASIAN = (ETHNIC_GROUP_GROUPED == "Asian\n"))%>%
  mutate(BLACK = (ETHNIC_GROUP_GROUPED == "Black\n"))%>%
  mutate(OTHER = (ETHNIC_GROUP_GROUPED %in% c("Other Ethnic Group\n","Other, less than 5","Mixed\n"))) %>%
  select(-ETHNIC_GROUP_GROUPED) %>%
  mutate(ACIDOSIS = (PH_MIN < 7.35)) %>%
  mutate(ALKALOSIS = (PH_MAX > 7.45)) %>%
  select(-PH_MIN,-PH_MAX)

# Frailty
cfs_rel <- cfs %>% filter(!STUDY_SUBJECT_DIGEST %in% frailty_raw$STUDY_SUBJECT_DIGEST)
adt_data_crop <- adt_data_rel %>% select(STUDY_SUBJECT_DIGEST,TIME_0,IN_DTTM) %>%  group_by(STUDY_SUBJECT_DIGEST) %>% slice(1)

cfs_after_admission <- 
cfs_rel %>% left_join(adt_data_crop,by = c("STUDY_SUBJECT_DIGEST")) %>%
  filter(TIME_0 > MEASURE_TIME) %>%
  mutate(distance = difftime(MEASURE_TIME,IN_DTTM,unit = "days")) %>%
  group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(distance) %>% 
  slice(1)


marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>% 
  left_join(frailty_raw %>% select(STUDY_SUBJECT_DIGEST,Clinical.Frailty.Scale.score) %>% 
              rename(FRAILTY = Clinical.Frailty.Scale.score),by = "STUDY_SUBJECT_DIGEST") %>%
  left_join(cfs_after_admission %>% select(STUDY_SUBJECT_DIGEST,VALUE) %>% rename(FRAILTY_AUTO = VALUE),by = "STUDY_SUBJECT_DIGEST") 

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  mutate(FRAILTY = case_when(is.na(FRAILTY) & !is.na(FRAILTY_AUTO) ~ as.numeric(FRAILTY_AUTO),
                             !is.na(FRAILTY) ~ as.numeric(FRAILTY),
                             is.na(FRAILTY) & is.na(FRAILTY_AUTO) ~ 0))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>% 
  mutate(FRAILTY_MISSING = ifelse(FRAILTY == 0,1,0))

marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  select(-FRAILTY_AUTO)


### ECMO

ECMO <- read.csv(csv_in_file("2021-04-13-ECMO"))
ECMO <- ECMO %>% filter(as.character(ECMO.confirmed.from.notes) == "Yes")

marker_matrix_lm_cr_2 %>% filter(STUDY_SUBJECT_DIGEST %in% ECMO$STUDY_SUBJECT_DIGEST)

# ID is enough, as single stay per patient
marker_matrix_lm_cr_2 = 
marker_matrix_lm_cr_2 %>% mutate(STATUS_WITHIN_W_OF_LM = case_when(STUDY_SUBJECT_DIGEST %in% ECMO$STUDY_SUBJECT_DIGEST & STATUS_WITHIN_W_OF_LM != 0 ~ 3,
                                                                 !(STUDY_SUBJECT_DIGEST %in% ECMO$STUDY_SUBJECT_DIGEST & STATUS_WITHIN_W_OF_LM != 0) ~ STATUS_WITHIN_W_OF_LM))


marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>% select(STUDY_SUBJECT_DIGEST,STAY,INDEX,INDEX_W,REAL_TIME,IN_DTTM,TIME_0,STATUS_TIME_FROM_LM,STATUS_WITHIN_W_OF_LM,everything())

# Add NIV
niv = readRDS(rds_file("niv_raw"))

marker_matrix_lm_cr_2_wave_1 = marker_matrix_lm_cr_2_wave_1 %>%
  mutate(NIV = add_niv(ID = STUDY_SUBJECT_DIGEST,time1 = REAL_TIME))

marker_matrix_lm_cr_2_wave_2 = marker_matrix_lm_cr_2_wave_2 %>%
  mutate(NIV = add_niv(ID = STUDY_SUBJECT_DIGEST,time1 = REAL_TIME))

# Add 3 day results
marker_matrix_lm_cr_2 <- marker_matrix_lm_cr_2 %>%
  left_join(base_matrix_lm_cr_3 %>% select(STUDY_SUBJECT_DIGEST,STAY,INDEX,STATUS_TIME_FROM_LM,STATUS_WITHIN_W_OF_LM) %>%
              rename(STATUS_TIME_FROM_LM_3 = STATUS_TIME_FROM_LM) %>%
              rename(STATUS_WITHIN_W_OF_LM_3 = STATUS_WITHIN_W_OF_LM),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) 

# Add diagnosis mode
marker_matrix_lm_cr_2_wave_1 = marker_matrix_lm_cr_2_wave_1 %>% 
  mutate(TESTED = paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(covid_pos$STUDY_SUBJECT_DIGEST,covid_pos$STAY)) %>% 
  mutate(DIAGNOSED = paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(covid_diag$STUDY_SUBJECT_DIGEST,covid_diag$STAY)) 

marker_matrix_lm_cr_2_wave_2 = marker_matrix_lm_cr_2_wave_2 %>% 
  mutate(TESTED = paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(covid_pos$STUDY_SUBJECT_DIGEST,covid_pos$STAY)) %>% 
  mutate(DIAGNOSED = paste0(STUDY_SUBJECT_DIGEST,STAY) %in% paste0(covid_diag$STUDY_SUBJECT_DIGEST,covid_diag$STAY)) 


# Save results
saveRDS(marker_matrix_lm_cr_2 %>% filter(IN_DTTM >= as.Date("2020-03-01")) ,file = rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_14_04_2021"))



### SPLIT BY WAVE
# Wave 1
marker_matrix_lm_cr_2_wave_1 = marker_matrix_lm_cr_2 %>% filter(IN_DTTM <= as.Date("2020-09-18") & IN_DTTM >= as.Date("2020-03-01"))
saveRDS(marker_matrix_lm_cr_2_wave_1,rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_01_11_2021_wave_1"))

# Wave 2
marker_matrix_lm_cr_2_wave_2 = marker_matrix_lm_cr_2 %>% filter(IN_DTTM > as.Date("2020-09-18"))
saveRDS(marker_matrix_lm_cr_2_wave_2,rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_01_11_2021_wave_2"))







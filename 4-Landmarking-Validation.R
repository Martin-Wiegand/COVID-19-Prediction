# Load libraries
library("dplyr")
library("tidyr")
library("cmprsk")
library("survival")
library("crrp")
library("parallel")
library("pROC")
library("ROCR")
library("precrec")
library("caret")
library("plotROC")

# load gcrrp_mod
source(filepath("0-gcrrp-mod.R"))
source(filepath("0-ROC-Functions.R"))
source(filepath("0-common-binom.R"))

adm_data = readRDS(rds_file("adm"))

# Load raw landmarking data frames
marker_matrix_lm_wave_1 = readRDS(rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_01_11_2021_wave_1"))
marker_matrix_lm_wave_2 = readRDS(rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_01_11_2021_wave_2"))


marker_matrix_lm_wave_1 = marker_matrix_lm_wave_1 %>%
  left_join(adm_data %>% select(STUDY_SUBJECT_DIGEST,STAY,ETHNIC_GROUP_GROUPED),by = c("STUDY_SUBJECT_DIGEST","STAY")) %>%
  mutate(WHITE = (ETHNIC_GROUP_GROUPED == "White\n"))%>%
  mutate(ASIAN = (ETHNIC_GROUP_GROUPED == "Asian\n"))%>%
  mutate(BLACK = (ETHNIC_GROUP_GROUPED == "Black\n"))%>%
  mutate(OTHER = (ETHNIC_GROUP_GROUPED %in% c("Other Ethnic Group\n","Other, less than 5","Mixed\n")))%>%
  mutate(ETHNICITY_MISSING = !BLACK & !WHITE & !OTHER & !ASIAN)

marker_matrix_lm_wave_2 = marker_matrix_lm_wave_2 %>% mutate(ETHNICITY_MISSING = !BLACK & !WHITE & !OTHER & !ASIAN)

# Adjust Steroids treatment dose

marker_matrix_lm_wave_1 = marker_matrix_lm_wave_1 %>%
  mutate(STEROIDS = DEXAMETHASONE|HYDROCORTISONE|METHYLPREDNISALONE|PREDNISALONE) %>%
  select(-c(DEXAMETHASONE,HYDROCORTISONE,METHYLPREDNISALONE,PREDNISALONE))

marker_matrix_lm_wave_2 = marker_matrix_lm_wave_2 %>%
  mutate(STEROIDS = DEXAMETHASONE|HYDROCORTISONE|METHYLPREDNISALONE|PREDNISALONE) %>%
  select(-c(DEXAMETHASONE,HYDROCORTISONE,METHYLPREDNISALONE,PREDNISALONE))

# Function wrapper for the grouping process,
# adding duplicates and assigning group IDs
Create_Grouped_Matrix <- function(marker_matrix_lm){
  
  # Create duplicates
  marker_matrix_lm_prime <-
    marker_matrix_lm %>%
    mutate(CRP_MISSING_PRIME = CRP_MISSING) %>%
    mutate(WCC_MISSING_PRIME = WCC_MISSING) %>%
    mutate(CRP_DAILY_CHANGE_MISSING_PRIME = CRP_DAILY_CHANGE_MISSING) %>%
    mutate(WCC_DAILY_CHANGE_MISSING_PRIME = WCC_DAILY_CHANGE_MISSING) %>%
    mutate(RDW_MISSING_PRIME = RDW_MISSING) %>%
    mutate(FERRITIN_MISSING_PRIME = FERRITIN_MISSING) %>%
    mutate(HB_MISSING_PRIME = HB_MISSING) %>%
    mutate(HB_DAILY_CHANGE_MISSING_PRIME = HB_DAILY_CHANGE_MISSING) %>%
    mutate(IL1_MISSING_PRIME = IL1_MISSING) %>%
    mutate(IG_MISSING_PRIME = IG_MISSING) %>%
    mutate(PCT_MISSING_PRIME = PCT_MISSING) %>%
    mutate(DDIMER_MISSING_PRIME = DDIMER_MISSING) %>%
    mutate(EOSINOPHILS_MISSING_PRIME = EOSINOPHILS_MISSING) %>%
    mutate(IL_RATIO_MISSING_PRIME = IL_RATIO_MISSING) %>%
    mutate(IL10_MISSING_PRIME = IL10_MISSING) %>%
    mutate(IL6_MISSING_PRIME = IL6_MISSING) %>%
    mutate(LYMPHOCYTES_MISSING_PRIME = LYMPHOCYTES_MISSING) %>%
    mutate(MONOCYTES_MISSING_PRIME = MONOCYTES_MISSING) %>%
    mutate(NEUTROPHILS_MISSING_PRIME = NEUTROPHILS_MISSING) %>%
    mutate(NL_RATIO_MISSING_PRIME = NL_RATIO_MISSING) %>%
    mutate(PLATELETS_MISSING_PRIME = PLATELETS_MISSING) %>%
    mutate(PLATELETS_DAILY_CHANGE_MISSING_PRIME = PLATELETS_DAILY_CHANGE_MISSING) %>%
    mutate(TNFA_MISSING_PRIME = TNFA_MISSING) %>%
    mutate(LDH_MISSING_PRIME = LDH_MISSING) %>%
    mutate(TROPONIN_MISSING_PRIME = TROPONIN_MISSING) %>%
    mutate(ASPARTATE_MISSING_PRIME = ASPARTATE_MISSING) %>%
    mutate(APTT_MISSING_PRIME = APTT_MISSING) %>%
    mutate(PT_MISSING_PRIME = PT_MISSING) %>%
    mutate(MAGNESIUM_MISSING_PRIME = MAGNESIUM_MISSING) %>%
    mutate(PHOSPHATE_MISSING_PRIME = PHOSPHATE_MISSING) %>%
    mutate(ALKALINE_PHOSPHATASE_MISSING_PRIME = ALKALINE_PHOSPHATASE_MISSING) %>%
    mutate(LACTATE_MISSING_PRIME = LACTATE_MISSING) %>%
    # ALBUMIN
    mutate(ALBUMIN_MISSING_PRIME = ALBUMIN_MISSING)%>%
    # ALANINE
    mutate(ALANINE_MISSING_PRIME = ALANINE_MISSING)%>%
    # BILIRUBIN
    mutate(BILIRUBIN_MISSING_PRIME = BILIRUBIN_MISSING)%>%
    # CHLORIDE 
    mutate(CHLORIDE_MISSING_PRIME = CHLORIDE_MISSING)%>%
    # CORTISOL
    mutate(CORTISOL_MISSING_PRIME = CORTISOL_MISSING)%>%
    # CREATININE
    mutate(CREATININE_MISSING_PRIME = CREATININE_MISSING)%>%
    mutate(CREATININE_DAILY_CHANGE_MISSING_PRIME = CREATININE_DAILY_CHANGE_MISSING)%>%
    # UREA
    mutate(UREA_MISSING_PRIME = UREA_MISSING)%>%
    # PULSE
    mutate(PULSE_MEAN_MISSING = PULSE_MISSING)%>%  
    mutate(PULSE_MAX_MISSING = PULSE_MISSING)%>% 
    mutate(PULSE_MIN_MISSING = PULSE_MISSING)%>% 
    mutate(PULSE_SD_MISSING = PULSE_MISSING)%>%
    mutate(PULSE_DAILY_CHANGE_MISSING_PRIME = PULSE_DAILY_CHANGE_MISSING) %>%
    # RR
    mutate(RR_MEAN_MISSING = RR_MISSING)%>%  
    mutate(RR_MAX_MISSING = RR_MISSING)%>% 
    mutate(RR_MIN_MISSING = RR_MISSING)%>% 
    mutate(RR_SD_MISSING = RR_MISSING)%>%
    mutate(RR_DAILY_CHANGE_MISSING_PRIME = RR_DAILY_CHANGE_MISSING) %>%
    # SODIUM
    mutate(SODIUM_MEAN_MISSING = SODIUM_MISSING)%>%  
    mutate(SODIUM_SD_MISSING = SODIUM_MISSING)%>%
    mutate(SODIUM_DAILY_CHANGE_MISSING_PRIME = SODIUM_DAILY_CHANGE_MISSING)%>%
    # POTASSIUM
    mutate(POTASSIUM_MISSING_PRIME = POTASSIUM_MISSING)%>%  
    mutate(POTASSIUM_DAILY_CHANGE_MISSING_PRIME = POTASSIUM_DAILY_CHANGE_MISSING) %>%
    # BP
    mutate(BP_MEAN_MISSING = BP_MISSING)%>%  
    mutate(BP_MIN_MISSING = BP_MISSING) %>%
    mutate(BP_MAX_MISSING = BP_MISSING) %>%
    mutate(BP_SD_MISSING = BP_MISSING)%>%
    mutate(BP_DAILY_CHANGE_MISSING_PRIME = BP_DAILY_CHANGE_MISSING) %>%
    
    # TEMP
    mutate(TEMP_MEAN_MISSING = TEMP_MISSING)%>%  
    mutate(TEMP_SD_MISSING = TEMP_MISSING)%>%
    mutate(TEMP_MAX_MISSING = TEMP_MISSING)%>%
    mutate(TEMP_MIN_MISSING = TEMP_MISSING)%>%
    mutate(TEMP_DAILY_CHANGE_MISSING_PRIME = TEMP_DAILY_CHANGE_MISSING) %>%
    
    # NEWS2
    mutate(NEWS2_MEAN_MISSING = NEWS2_MISSING)%>%  
    mutate(NEWS2_SD_MISSING = NEWS2_MISSING)%>%
    mutate(NEWS2_MAX_MISSING = NEWS2_MISSING)%>%
    # PH
    mutate(ALKALOSIS_MISSING = PH_MISSING)%>%
    mutate(ACIDOSIS_MISSING = PH_MISSING) %>%
    # SF RATIO
    mutate(SF_MEAN_MISSING = SF_MISSING)%>%  
    mutate(SF_SD_MISSING = SF_MISSING)%>%
    mutate(SF_MAX_MISSING = SF_MISSING)%>%
    mutate(SF_MIN_MISSING = SF_MISSING)%>%
    mutate(SF_DAILY_CHANGE_MISSING_PRIME = SF_DAILY_CHANGE_MISSING) %>%
    
    # PF RATIO
    mutate(PF_MEAN_MISSING = PF_MISSING)%>%  
    mutate(PF_SD_MISSING = PF_MISSING)%>%
    mutate(PF_MAX_MISSING = PF_MISSING)%>%
    mutate(PF_MIN_MISSING = PF_MISSING)%>%
    mutate(PF_DAILY_CHANGE_MISSING_PRIME = PF_DAILY_CHANGE_MISSING) %>%
    
    # VENTIALTION
    mutate(VENTILATION = VENTILATON)
  
  # GROUP PERMUTATIONS
  add_permutations <- function(testdata,add_mat,count_vec,arg_name){
    mat_names <- add_mat %>% colnames
    n <- max(base_group)
    
    selection <- function(x){
      c_mat <- t(combn(1:ncol(add_mat),x))
      cbind(c_mat,matrix(0,ncol = ncol(add_mat) - ncol(c_mat),nrow = nrow(c_mat)))
    }
    
    selection_columns <- do.call(rbind,sapply(X = 1:ncol(add_mat),FUN = selection))
    
    
    for(l in 1:nrow(selection_columns)){
      submat <- add_mat[,selection_columns[l,]]
      colnames(submat) <- paste(mat_names[selection_columns[l,]],l,sep = "_")
      testdata <- add_column(testdata,submat)
      
      count_mat = tibble(count_vec)
      colnames(count_mat) <- paste0(arg_name,"_MISSING_",l)
      testdata <- add_column(testdata,count_mat)
      
      base_group <<- c(base_group,rep(n + l,ncol(submat)+ 1))
      
    }
    
    count_mat = tibble(count_vec)
    colnames(count_mat) <- paste0(arg_name,"_MISSING_",0)
    testdata <- add_column(testdata,count_mat)
    
    base_group <<- c(base_group,n + nrow(selection_columns) + 1)
    
    return(testdata)
  }
  
  
  
  add_permutations_var <- function(testdata,add_mat,count_vec,arg_name){
    mat_names <- add_mat %>% colnames
    n <- max(base_group)
    
    selection <- function(x){
      c_mat <- t(combn(1:ncol(add_mat),x))
      cbind(c_mat,matrix(0,ncol = ncol(add_mat) - ncol(c_mat),nrow = nrow(c_mat)))
    }
    
    selection_columns <- do.call(rbind,sapply(X = 1:ncol(add_mat),FUN = selection))
    
    for(l in 1:nrow(selection_columns)){
      submat <- add_mat[,selection_columns[l,]]
      colnames(submat) <- paste(mat_names[selection_columns[l,]],l,sep = "_")
      testdata <- add_column(testdata,submat)
      
      count_mat = tibble(count_vec)
      colnames(count_mat) <- paste0(arg_name,"_VAR_MISSING_",l)
      testdata <- add_column(testdata,count_mat)
      
      base_group <<- c(base_group,rep(n + l,ncol(submat)+ 1))
      
    }
    
    count_mat = tibble(count_vec)
    colnames(count_mat) <- paste0(arg_name,"_VAR_MISSING_",0)
    testdata <- add_column(testdata,count_mat)
    
    base_group <<- c(base_group,n + nrow(selection_columns) + 1)
    
    return(testdata)
  }
  
  
  ##
  marker_matrix_lm_prime <-
    marker_matrix_lm_prime %>% 
    select(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM,
           # Blood markers
           CRP_LOCF,CRP_MISSING,CRP_MISSING_PRIME,
           CRP_DAILY_CHANGE,CRP_DAILY_CHANGE_MISSING,CRP_DAILY_CHANGE_MISSING_PRIME,
           
           RDW_LOCF,RDW_MISSING,RDW_MISSING_PRIME,
           
           FERRITIN_LOCF,FERRITIN_MISSING,FERRITIN_MISSING_PRIME,
           
           HB_LOCF,HB_MISSING,HB_MISSING_PRIME,
           
           IL6_LOCF,IL6_MISSING,IL6_MISSING_PRIME,
           
           PCT_LOCF,PCT_MISSING,PCT_MISSING_PRIME,
           
           DDIMER_LOCF,DDIMER_MISSING,DDIMER_MISSING_PRIME,
           
           EOSINOPHILS_LOCF,EOSINOPHILS_MISSING,EOSINOPHILS_MISSING_PRIME,
           
           MONOCYTES_LOCF,MONOCYTES_MISSING,MONOCYTES_MISSING_PRIME,
           
           LDH_LOCF,LDH_MISSING,LDH_MISSING_PRIME,
           
           TROPONIN_LOCF,TROPONIN_MISSING,TROPONIN_MISSING_PRIME,
           
           APTT_LOCF,APTT_MISSING,APTT_MISSING_PRIME,
           
           PT_LOCF,PT_MISSING,PT_MISSING_PRIME,
           
           LACTATE_LOCF,LACTATE_MISSING,LACTATE_MISSING_PRIME,
           
           ALKALINE_PHOSPHATASE_LOCF,ALKALINE_PHOSPHATASE_MISSING,ALKALINE_PHOSPHATASE_MISSING_PRIME,
           
           # Other biomarkers
           GCS_BELOW_9,GCS_BELOW_12,
           
           ALBUMIN_LOCF,ALBUMIN_MISSING,ALBUMIN_MISSING_PRIME,
           
           ALANINE_LOCF,ALANINE_MISSING,ALANINE_MISSING_PRIME,
           
           BILIRUBIN_LOCF,BILIRUBIN_MISSING,BILIRUBIN_MISSING_PRIME,
           
           CREATININE_LOCF,CREATININE_MISSING,CREATININE_MISSING_PRIME,
           CREATININE_DAILY_CHANGE,CREATININE_DAILY_CHANGE_MISSING,CREATININE_DAILY_CHANGE_MISSING_PRIME,
           
           UREA_LOCF,UREA_MISSING,UREA_MISSING_PRIME,
           
           POTASSIUM_LOCF,POTASSIUM_MISSING,POTASSIUM_MISSING_PRIME,
           POTASSIUM_DAILY_CHANGE,POTASSIUM_DAILY_CHANGE_MISSING,POTASSIUM_DAILY_CHANGE_MISSING_PRIME,
           
           # Individual markers
           ICU,VENTILATION,AGE_AT_ADM,GENDER_DESC,
           ASTHMA,DEMENTIA,DIABETES,HEART_DISEASE,HYPERTENSION,
           IMMUNOCOMPROMISED,LIVER_DISEASE,MALIGNANCY_H,MALIGNANCY_NH,
           RENAL_DISEASE,RESPIRATORY_DISEASE,STROKE,
           DNAR,CARDIO_VASCULAR_SUPPORT,STEROIDS,FRAILTY,FRAILTY_MISSING,RRT,
           
           PF_DAILY_CHANGE,PF_DAILY_CHANGE_MISSING,PF_DAILY_CHANGE_MISSING_PRIME,
           SF_DAILY_CHANGE,SF_DAILY_CHANGE_MISSING,SF_DAILY_CHANGE_MISSING_PRIME,
           PULSE_DAILY_CHANGE,PULSE_DAILY_CHANGE_MISSING,PULSE_DAILY_CHANGE_MISSING_PRIME,
           BP_DAILY_CHANGE,BP_DAILY_CHANGE_MISSING,BP_DAILY_CHANGE_MISSING_PRIME,
           TEMP_DAILY_CHANGE,TEMP_DAILY_CHANGE_MISSING,TEMP_DAILY_CHANGE_MISSING_PRIME,
           RR_DAILY_CHANGE,RR_DAILY_CHANGE_MISSING,RR_DAILY_CHANGE_MISSING_PRIME,
           
           WCC_DAILY_CHANGE,WCC_DAILY_CHANGE_MISSING,WCC_DAILY_CHANGE_MISSING_PRIME,
           SODIUM_DAILY_CHANGE,SODIUM_DAILY_CHANGE_MISSING,SODIUM_DAILY_CHANGE_MISSING_PRIME,
           HB_DAILY_CHANGE,HB_DAILY_CHANGE_MISSING,HB_DAILY_CHANGE_MISSING_PRIME,
           PLATELETS_DAILY_CHANGE,PLATELETS_DAILY_CHANGE_MISSING,PLATELETS_DAILY_CHANGE_MISSING_PRIME,
           
           PULSE_MIN,PULSE_MEAN,PULSE_MAX,
           RR_MIN,RR_MEAN,RR_MAX,
           BP_MIN,BP_MEAN,BP_MAX,
           RR_MIN,RR_MEAN,RR_MAX,
           TEMP_MIN,TEMP_MEAN,TEMP_MAX,
           SF_MIN,SF_MEAN,SF_MAX,
           
           BLACK,ASIAN,OTHER,ETHNICITY_MISSING
    )
  
  # Data frame for combinations
  data_base <-
    marker_matrix_lm_prime %>% 
    mutate(BELOW_95 = (AGE_AT_ADM < 95)) %>%
    mutate(BELOW_90 = (AGE_AT_ADM < 90)) %>%
    mutate(BELOW_85 = (AGE_AT_ADM < 85)) %>%
    mutate(BELOW_80 = (AGE_AT_ADM < 80)) %>%
    mutate(BELOW_75 = (AGE_AT_ADM < 75)) %>%
    mutate(BELOW_70 = (AGE_AT_ADM < 70)) %>%
    mutate(BELOW_65 = (AGE_AT_ADM < 65)) %>%
    mutate(BELOW_60 = (AGE_AT_ADM < 60)) %>%
    mutate(BELOW_55 = (AGE_AT_ADM < 55)) %>%
    mutate(BELOW_50 = (AGE_AT_ADM < 50)) %>%
    mutate(BELOW_45 = (AGE_AT_ADM < 45)) %>%
    select(-AGE_AT_ADM,-DNAR)
  
  
  base_group <- c(1,1,2,3,3,4, # CRP
                  5,5,6, # RDW
                  7,7,8, # FERRITIN
                  9,9,10, # HB
                  11,11,12, # IL6
                  13,13,14, # PCT
                  15,15,16, # DDIMER
                  17,17,18, # EOSINOPHILS
                  19,19,20, # MONOCYTES
                  21,21,22, # LDH
                  23,23,24, # TROPONIN
                  25,25,26, # APTT
                  27,27,28, # PT
                  29,29,30, # LACTATE
                  31,31,32, # ALKALINE PHOSPHATASE
                  33,33, # GCS
                  34,34,35, # ALBUMIN
                  36,36,37, # ALANINE
                  38,38,39, # BILIRUBIN
                  40,40,41,42,42,43, # CREATININE
                  44,44,45, # UREA
                  46,46,47,48,48,49, # POTASSIUM
                  50, # ICU
                  51, # VENTILATION
                  52, # GENDER
                  53, # ASTHMA
                  54, # DEMENTIA
                  55, # DIABETES
                  56, # HEART
                  57, # HYPERTENSION
                  58, # IMMUNOCOMPROMISED
                  59, # LIVER
                  60, # MALIGNANCY H
                  61, # MALIGNANCY
                  62, # RENAL
                  63, # RESPIRATORY
                  64, # STROKE
                  65, # CARDIOVASCULAR
                  66, # STEROIDS
                  67,67, # FRAILTY
                  68, # RRT
                  69,69,70, # PF SLOPES
                  71,71,72, # SF SLOPES
                  73,73,74, # PULSE SLOPES
                  75,75,76, # BP SLOPES
                  77,77,78, # TEMP SLOPES
                  79,79,80, # RR SLOPES
                  81,81,82, # WCC SLOPES
                  83,83,84, # SODIUM SLOPES
                  85,85,86, # HB SLOPES
                  87,87,88, # PLATELETS SLOPES
                  89:91, # PULSE
                  92:94, # RR
                  95:97, # BP
                  98:100, # TEMP
                  101:103, # SF
                  104:107, # Ethnicity
                  108:118 # AGE BANDS
  )
  

  # Line up groups and data
  cbind(names(data_base)[-(1:9)],base_group)
  
  # Check that values are increasing/adjacent
  length(base_group)
  ncol(data_base %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)))
  base_group %>% unique %>% length
  base_group %>% max
  # Add PERMUTATIONS for frequently collected data variations
  
  # NL
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(NEUTROPHILS_LOCF,LYMPHOCYTES_LOCF,NL_RATIO_LOCF),count_vec = marker_matrix_lm$NEUTROPHILS_MISSING,arg_name = "N_L")
  
  # WCC/PLATELETS
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(WCC_LOCF,PLATELETS_LOCF),count_vec = marker_matrix_lm$WCC_MISSING,arg_name = "WCC_PLATELETS")
  
  # IG/TNFA/IL1/IL10/IL RAtio
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(IG_LOCF,TNFA_LOCF,IL1_LOCF,IL10_LOCF,IL_RATIO_LOCF),count_vec = marker_matrix_lm$IL_RATIO_MISSING,arg_name = "IL1_IL10_IG_TNFA")
  
  # PF
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(PF_MAX,PF_MIN,PF_MEAN),count_vec = marker_matrix_lm$PF_MISSING,arg_name = "PF")
  
  # SODIUM
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>%
                                  select(SODIUM_X1_MAX,SODIUM_X2_MAX),count_vec = marker_matrix_lm$SODIUM_MISSING,arg_name = "SODIUM")
  
  # PH
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(PH_X1_MAX,PH_X2_MAX),count_vec = marker_matrix_lm$PH_MISSING,arg_name = "PH")
  
  # BMI
  data_base <- add_permutations(testdata = data_base,add_mat = marker_matrix_lm %>% ungroup %>% 
                                  select(BMI_X1_MAX,BMI_X2_MAX),count_vec = marker_matrix_lm$BMI_MISSING,arg_name = "BMI")
  
  

  
  
  return(list(data_base = data_base,
              base_group = base_group))
}


##### -----------------
# Due to the various groups, and correlations in missingness base group & data_base are a bit of a mess
# of manual groups & automatic suffixes. Given this only has to be run once to actually build a model,
# and we are checking the results (plus all the exceptions that are necessary) I don't think coming up with an
# automatic way to do this is worth the time.
Create_Grouped_Matrix_Output_wave_1 <- Create_Grouped_Matrix(marker_matrix_lm = marker_matrix_lm_wave_1)
Create_Grouped_Matrix_Output_wave_2 <- Create_Grouped_Matrix(marker_matrix_lm = marker_matrix_lm_wave_2)



# Wave 1
cbind(names(Create_Grouped_Matrix_Output_wave_1$data_base)[-(1:9)],Create_Grouped_Matrix_Output_wave_1$base_group)
# Check group vector and data matrix coincide
length(Create_Grouped_Matrix_Output_wave_1$base_group)
ncol(Create_Grouped_Matrix_Output_wave_1$data_base %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)))

# Permutations data frame
saveRDS(Create_Grouped_Matrix_Output_wave_1$data_base,rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave1"))
marker_matrix_group_permutations_wave_1 <- readRDS(rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave1"))

# Group vector permutations
saveRDS(Create_Grouped_Matrix_Output_wave_1$base_group,rds_file("base_group_2_6h_uniform_2021_11_11_wave1"))
base_group_1 <- readRDS(rds_file("base_group_2_6h_uniform_2021_11_11_wave1"))



# Wave 2
cbind(names(Create_Grouped_Matrix_Output_wave_2$data_base)[-(1:9)],Create_Grouped_Matrix_Output_wave_2$base_group)
# Check group vector and data matrix coincide
length(Create_Grouped_Matrix_Output_wave_2$base_group)
ncol(Create_Grouped_Matrix_Output_wave_2$data_base %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)))

saveRDS(Create_Grouped_Matrix_Output_wave_2$data_base,rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave2"))
marker_matrix_group_permutations_wave_2 <- readRDS(rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave2"))

saveRDS(Create_Grouped_Matrix_Output_wave_2$base_group,rds_file("base_group_2_6h_uniform_2021_11_11_wave2"))
base_group_2 <- readRDS(rds_file("base_group_2_6h_uniform_2021_11_11_wave2"))









################ BUILD CRR INPUT
w = 2

nuffield_id = "A0247AB474BF912629FAEE03649022A9AB7D1596EE2B47B83F7E396809D54602"
nuffield_discharge = as.POSIXct("2020-07-22 22:32:00")

fill_lag <- function(x){
  ifelse(x == 0,lag(x),x) %>% return
}

# WAVE 1
marker_matrix_group_permutations_wave_1 <- readRDS(rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave1"))

data_non_palliative_wave_1 <- marker_matrix_group_permutations_wave_1 %>% 
  left_join(marker_matrix_lm_wave_1 %>% 
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,PALLIATIVE),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
  filter(!(STATUS_TIME_FROM_LM < w & STATUS_WITHIN_W_OF_LM == 0)) %>%   # Remove the 2 active patients last landmarks
  filter(cumsum(PALLIATIVE) == 0) %>% # Never been palliative up to that LM
  select(-PALLIATIVE) %>% # Remove palliative marker
  filter(STATUS_TIME_FROM_LM >= 0)  # Remove < 6h stays (~35 Ids/Stays) 
  
# Change nuffield patient to discharge once transferred to Nuffield
data_non_palliative_wave_1 = data_non_palliative_wave_1 %>% filter(STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME < nuffield_discharge) %>%
  mutate(STATUS_TIME_FROM_LM = case_when(STUDY_SUBJECT_DIGEST == nuffield_id & REAL_TIME + ddays(w) > nuffield_discharge ~ difftime(nuffield_discharge,REAL_TIME,unit = "days"),
                                         STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME + ddays(w) <= nuffield_discharge ~ STATUS_TIME_FROM_LM)) %>%
  mutate(STATUS_WITHIN_W_OF_LM = case_when(STUDY_SUBJECT_DIGEST == nuffield_id & REAL_TIME + ddays(w) > nuffield_discharge ~ 1,
                                         STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME + ddays(w) <= nuffield_discharge ~ STATUS_WITHIN_W_OF_LM))


# LOCF for exception cases with missing vitals
data_non_palliative_wave_1 = data_non_palliative_wave_1 %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  mutate_at(vars("SF_MIN","SF_MEAN","SF_MAX","BP_MIN","BP_MEAN","BP_MAX","PULSE_MIN",
                 "PULSE_MEAN","PULSE_MAX","BP_MIN","BP_MEAN","BP_MAX","RR_MIN",
                 "RR_MEAN","RR_MAX","TEMP_MIN","TEMP_MEAN","TEMP_MAX"),fill_lag)

# Print current selection
write.csv(x = cbind(names(data_non_palliative_wave_1)[-(1:9)],base_group_1),file = csv_out_file("data_non_palliative_predictor_options_uniform_2021_11_11_wave1_ethnicity"))

saveRDS(data_non_palliative_wave_1,rds_file("data_non_palliative_2021_11_11_6h_shift_prediction_2days_uniform_wave1"))
saveRDS(base_group_1,rds_file("base_group_2021_11_11_6h_shift_prediction_2days_uniform_wave1"))




# WAVE 2
marker_matrix_group_permutations_wave_2 <- readRDS(rds_file("marker_matrix_lm_cr_2_6h_group_permutations_uniform_2021_11_11_wave2"))

base_group_2 <- readRDS(rds_file("base_group_2_6h_uniform_2021_11_11_wave2"))

data_non_palliative_wave_2 <- marker_matrix_group_permutations_wave_2 %>% 
  left_join(marker_matrix_lm_wave_2 %>% 
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,PALLIATIVE),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
  filter(!(STATUS_TIME_FROM_LM < w & STATUS_WITHIN_W_OF_LM == 0)) %>%   # Remove the 2 active patients last landmarks
  filter(cumsum(PALLIATIVE) == 0) %>% # Never been palliative up to that LM
  select(-PALLIATIVE) %>% # Remove palliative marker
  filter(STATUS_TIME_FROM_LM >= 0)  # Remove < 6h stays (~30 Ids/Stays) 

# Change nuffield patient to discharge once transferred to Nuffield
data_non_palliative_wave_2 = data_non_palliative_wave_2 %>% filter(STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME < nuffield_discharge) %>%
  mutate(STATUS_TIME_FROM_LM = case_when(STUDY_SUBJECT_DIGEST == nuffield_id & REAL_TIME + ddays(w) > nuffield_discharge ~ difftime(nuffield_discharge,REAL_TIME,unit = "days"),
                                         STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME + ddays(w) <= nuffield_discharge ~ STATUS_TIME_FROM_LM)) %>%
  mutate(STATUS_WITHIN_W_OF_LM = case_when(STUDY_SUBJECT_DIGEST == nuffield_id & REAL_TIME + ddays(w) > nuffield_discharge ~ 1,
                                           STUDY_SUBJECT_DIGEST != nuffield_id | REAL_TIME + ddays(w) <= nuffield_discharge ~ STATUS_WITHIN_W_OF_LM))


# LOCF for exception cases with missing vitals
# One case of missingness
data_non_palliative_wave_2 = data_non_palliative_wave_2 %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  mutate_at(vars("SF_MIN","SF_MEAN","SF_MAX","BP_MIN","BP_MEAN","BP_MAX","PULSE_MIN",
                 "PULSE_MEAN","PULSE_MAX","BP_MIN","BP_MEAN","BP_MAX","RR_MIN",
                 "RR_MEAN","RR_MAX","TEMP_MIN","TEMP_MEAN","TEMP_MAX"),fill_lag)

data_non_palliative_wave_2 = data_non_palliative_wave_2 %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  filter_at(vars("SF_MIN","SF_MEAN","SF_MAX","BP_MIN","BP_MEAN","BP_MAX","PULSE_MIN",
                 "PULSE_MEAN","PULSE_MAX","BP_MIN","BP_MEAN","BP_MAX","RR_MIN",
                 "RR_MEAN","RR_MAX","TEMP_MIN","TEMP_MEAN","TEMP_MAX"),all_vars(!is.na(.)))

# Remove immunocompromised, as no diagnoses in wave 2
rm_immuno = which(data_non_palliative_wave_2 %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM)) %>% names == "IMMUNOCOMPROMISED")

immuno_group = base_group_2[rm_immuno]
base_group_2 = base_group_2[-rm_immuno]
base_group_2 = ifelse(base_group_2 > immuno_group,base_group_2 -1 ,base_group_2)

data_non_palliative_wave_2 = data_non_palliative_wave_2 %>% select(-IMMUNOCOMPROMISED)

cbind(names(data_non_palliative_wave_2 %>% ungroup %>% select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),base_group_2)

# Print current selection
write.csv(x = cbind(names(data_non_palliative_wave_2)[-(1:9)],base_group_2),file = csv_out_file("data_non_palliative_predictor_options_uniform_2021_11_11_wave2_ethnicity"))

saveRDS(data_non_palliative_wave_2,rds_file("data_non_palliative_2021_11_11_6h_shift_prediction_2days_uniform_wave2"))
saveRDS(base_group_2,rds_file("base_group_2021_11_11_6h_shift_prediction_2days_uniform_wave2"))




# LOAD DATA
# November 11
base_group_1  = readRDS(rds_file("base_group_2021_11_11_6h_shift_prediction_2days_uniform_wave1"))
base_group_2  = readRDS(rds_file("base_group_2021_11_11_6h_shift_prediction_2days_uniform_wave2"))

data_non_palliative_wave_1 = readRDS(rds_file("data_non_palliative_2021_11_11_6h_shift_prediction_2days_uniform_wave1"))
data_non_palliative_wave_2 = readRDS(rds_file("data_non_palliative_2021_11_11_6h_shift_prediction_2days_uniform_wave2"))

# Load vaccination status document
vaccination_status = read.csv(csv_out_file("Vaccination_Notes_SingleExtract_Annotated"))

vaccination_status = as_tibble(vaccination_status)
vaccination_status = vaccination_status %>%
  select(STUDY_SUBJECT_DIGEST,STAY,Vaccinated,DATETIME_1)

# Check frequency of vaccination status
vaccination_status %>% filter(Vaccinated %in% c("Yes","1 dose","2 doses"))

# Add vaccination status to dataframe
data_non_palliative_wave_2 = data_non_palliative_wave_2 %>% left_join(vaccination_status,by = c("STUDY_SUBJECT_DIGEST","STAY")) 

data_non_palliative_wave_2 = data_non_palliative_wave_2 %>% 
  mutate(Vaccinated = Vaccinated %in% c("Yes","1 dose","2 doses")) %>%
  select(-DATETIME_1) 

saveRDS(data_non_palliative_wave_2,rds_file("data_non_palliative_wave_2_ethnicity_new_steroids_vaccinated"))
data_non_palliative_wave_2_vaccinated = readRDS(rds_file("data_non_palliative_wave_2_ethnicity_new_steroids_vaccinated"))

# Save final data frame
# saveRDS(data_non_palliative_wave_2,rds_file("data_non_palliative_wave_2_ethnicity_new_steroids"))
# saveRDS(data_non_palliative_wave_1,rds_file("data_non_palliative_wave_1_ethnicity_new_steroids"))


# Rejoin 3 day horizon outcome
data_non_palliative_wave_1_w_3 = data_non_palliative_wave_1 %>%
  left_join(marker_matrix_lm_wave_1 %>%
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,STATUS_TIME_FROM_LM_3,STATUS_WITHIN_W_OF_LM_3),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
  select(-STATUS_TIME_FROM_LM,-STATUS_WITHIN_W_OF_LM) %>%
  rename(STATUS_TIME_FROM_LM = STATUS_TIME_FROM_LM_3,
         STATUS_WITHIN_W_OF_LM = STATUS_WITHIN_W_OF_LM_3) %>%
  select(STUDY_SUBJECT_DIGEST:TIME_0,STATUS_TIME_FROM_LM,STATUS_WITHIN_W_OF_LM,everything())

data_non_palliative_wave_2_w_3 = data_non_palliative_wave_2 %>%
  left_join(marker_matrix_lm_wave_2 %>%
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,STATUS_TIME_FROM_LM_3,STATUS_WITHIN_W_OF_LM_3),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
  select(-STATUS_TIME_FROM_LM,-STATUS_WITHIN_W_OF_LM) %>%
  rename(STATUS_TIME_FROM_LM = STATUS_TIME_FROM_LM_3,
         STATUS_WITHIN_W_OF_LM = STATUS_WITHIN_W_OF_LM_3) %>%
  select(STUDY_SUBJECT_DIGEST:TIME_0,STATUS_TIME_FROM_LM,STATUS_WITHIN_W_OF_LM,everything())



################################ MODELS ON FULL DATA

data_non_palliative_wave_1 = readRDS(rds_file("data_non_palliative_wave_1_ethnicity_new_steroids"))
data_non_palliative_wave_2 = readRDS(rds_file("data_non_palliative_wave_2_ethnicity_new_steroids"))

base_group = readRDS(rds_file("base_group_2021_11_11_6h_shift_prediction_2days_uniform_wave1"))
lambda = seq(0.0000708101,0.0234459433,by = 0.0002361125)


model <- gcrrp_mod(time = data_non_palliative_wave_1$STATUS_TIME_FROM_LM,
                                 fstatus = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM,
                                 X = as.matrix(data_non_palliative_wave_1 %>% 
                                                 ungroup %>% 
                                                 select(-(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),
                                 failcode = 2,
                                 cencode = 0,
                                 group = base_group,
                                 penalty = "gSCAD",
                                 lambda = lambda,
                                 eps = 0.0001,
                                 max.iter = 1000,
                                 weighted = F)

saveRDS(model,rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity"))
model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity"))


model_w3 <- gcrrp_mod(time = data_non_palliative_wave_1_w_3$STATUS_TIME_FROM_LM,
                   fstatus = data_non_palliative_wave_1_w_3$STATUS_WITHIN_W_OF_LM,
                   X = as.matrix(data_non_palliative_wave_1_w_3 %>% 
                                   ungroup %>% 
                                   select(-c(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),
                   failcode = 2,
                   cencode = 0,
                   group = base_group,
                   penalty = "gSCAD",
                   lambda = lambda,
                   eps = 0.0001,
                   max.iter = 1000,
                   weighted = F)

saveRDS(model_w3,rds_file("model_mort_6h_gSCAD_2021_04_14_uniform_wave_1_w3"))



# LOS < 14 Days
data_non_palliative_wave_1_14 = data_non_palliative_wave_1 %>%
  drop_na() %>%
  filter(INDEX <= 14)

model_extra_14 <- gcrrp_mod(time = data_non_palliative_wave_1_14 %>%
                              pull(STATUS_TIME_FROM_LM),
                            fstatus = data_non_palliative_wave_1_14 %>%
                              pull(STATUS_WITHIN_W_OF_LM),
                            X = as.matrix(data_non_palliative_wave_1_14 %>% 
                                            ungroup %>% 
                                            select(-c(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),
                            failcode = 2,
                            cencode = 0,
                            group = base_group,
                            penalty = "gSCAD",
                            lambda.min = 0.001,
                            nlambda = 100,
                            eps = 0.0001,
                            max.iter = 1000,
                            weighted = F)


# LOS < 21 Days
data_non_palliative_wave_1_21 = data_non_palliative_wave_1 %>%
  drop_na() %>%
  filter(INDEX <= 21)

model_extra_21 <- gcrrp_mod(time = data_non_palliative_wave_1_21 %>%
                              pull(STATUS_TIME_FROM_LM),
                            fstatus = data_non_palliative_wave_1_21 %>%
                              pull(STATUS_WITHIN_W_OF_LM),
                            X = as.matrix(data_non_palliative_wave_1_21 %>% 
                                            ungroup %>% 
                                            select(-c(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),
                            failcode = 2,
                            cencode = 0,
                            group = base_group,
                            penalty = "gSCAD",
                            lambda.min = 0.001,
                            nlambda = 100,
                            eps = 0.0001,
                            max.iter = 1000,
                            weighted = F)


# LOS < 28 DAys
data_non_palliative_wave_1_28 = data_non_palliative_wave_1 %>%
  drop_na() %>%
  filter(INDEX <= 28)

model_extra_28 <- gcrrp_mod(time = data_non_palliative_wave_1_28 %>%
                              pull(STATUS_TIME_FROM_LM),
                            fstatus = data_non_palliative_wave_1_28 %>%
                              pull(STATUS_WITHIN_W_OF_LM),
                            X = as.matrix(data_non_palliative_wave_1_28 %>% 
                                            ungroup %>% 
                                            select(-c(STUDY_SUBJECT_DIGEST:STATUS_WITHIN_W_OF_LM))),
                            failcode = 2,
                            cencode = 0,
                            group = base_group,
                            penalty = "gSCAD",
                            lambda.min = 0.001,
                            nlambda = 100,
                            eps = 0.0001,
                            max.iter = 1000,
                            weighted = F)



# **************************** PICK OPTIMAL MODEL

# Pick model after adjusting BIC for duplicates, rebuild beta and matrices according to correlations,
# build crr model and evaluate ROC inside

ROC_model_object_SCAD_train_1_test_1 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),test = data_non_palliative_wave_1_ethnicity_new_steroids,train = data_non_palliative_wave_1_ethnicity_new_steroids,base_group = base_group)
ROC_model_object_SCAD_train_1_test_2 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),test = data_non_palliative_wave_2_ethnicity_new_steroids,train = data_non_palliative_wave_1_ethnicity_new_steroids,base_group = base_group)

ROC_model_object_SCAD_train_1_test_1_w3 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_04_14_uniform_wave_1_w3")),test = data_non_palliative_wave_1_w_3%>% drop_na(),train = data_non_palliative_wave_1_w_3 %>% drop_na(),base_group = base_group,bic.min = 73)
ROC_model_object_SCAD_train_1_test_2_w3 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_04_14_uniform_wave_1_w3")),test = data_non_palliative_wave_2_w_3%>% drop_na(),train = data_non_palliative_wave_1_w_3%>% drop_na(),base_group = base_group,bic.min = 73)





# ***************************** SENSITIVITY ANALYSIS
# Stratify by vaccination status
ROC_model_extra_2_vacc = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                        test = data_non_palliative_wave_2_vaccinated %>% filter(Vaccinated) %>% select(-Vaccinated),
                                        train = data_non_palliative_wave_1,
                                        base_group = base_group)

ROC_model_extra_2_unvacc = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                        test = data_non_palliative_wave_2_vaccinated %>% filter(!Vaccinated) %>% select(-Vaccinated),
                                        train = data_non_palliative_wave_1,
                                        base_group = base_group)


ci.auc(ROC_model_extra_2_vacc$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_extra_2_vacc$retmat_test$Probability)
ci.auc(ROC_model_extra_2_unvacc$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_extra_2_unvacc$retmat_test$Probability)



# Stratify by LOS
ROC_model_object_14 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                                      test = data_non_palliative_wave_1 %>%
                                                        filter(INDEX < 14),
                                                      train = data_non_palliative_wave_1,
                                                      base_group = base_group)

ROC_model_object_21 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                     test = data_non_palliative_wave_1 %>%
                                       filter(INDEX < 21),
                                     train = data_non_palliative_wave_1,
                                     base_group = base_group)

ROC_model_object_28 = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                     test = data_non_palliative_wave_1 %>%
                                       filter(INDEX < 28),
                                     train = data_non_palliative_wave_1,
                                     base_group = base_group)

ROC_model_object_14$ROC$AUC
ROC_model_object_21$ROC$AUC
ROC_model_object_28$ROC$AUC

ci.auc(ROC_model_object_14$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_14$retmat_test$Probability)
ci.auc(ROC_model_object_21$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_21$retmat_test$Probability)
ci.auc(ROC_model_object_28$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_28$retmat_test$Probability)

# Evaluate model selection for different LOS
ROC_model_object_14_test = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                     test = data_non_palliative_wave_2 %>%
                                       filter(INDEX < 14),
                                     train = data_non_palliative_wave_1,
                                     base_group = base_group)

ROC_model_object_21_test = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                     test = data_non_palliative_wave_2 %>%
                                       filter(INDEX < 21),
                                     train = data_non_palliative_wave_1,
                                     base_group = base_group)

ROC_model_object_28_test = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                     test = data_non_palliative_wave_2 %>%
                                       filter(INDEX < 28),
                                     train = data_non_palliative_wave_1,
                                     base_group = base_group)

# AUROCs
ROC_model_object_14_test$ROC$AUC
ROC_model_object_21_test$ROC$AUC
ROC_model_object_28_test$ROC$AUC

# 95% CIs
ci.auc(ROC_model_object_14_test$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_14_test$retmat_test$Probability)
ci.auc(ROC_model_object_21_test$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_21_test$retmat_test$Probability)
ci.auc(ROC_model_object_28_test$retmat_test$STATUS_WITHIN_W_OF_LM == 2,ROC_model_object_28_test$retmat_test$Probability)

# Stratify by inclusion mode (tested vs diagnosed)
data_non_palliatiave_1_tested = 
data_non_palliative_wave_1 %>%
  left_join(marker_matrix_lm_wave_1 %>%
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,TESTED),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
  mutate(TESTED = ifelse(is.na(TESTED),FALSE,TESTED))

data_non_palliatiave_2_tested = 
  data_non_palliative_wave_2 %>%
  left_join(marker_matrix_lm_wave_2 %>%
              select(STUDY_SUBJECT_DIGEST,STAY,INDEX,TESTED),
            by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX"))%>%
  mutate(TESTED = ifelse(is.na(TESTED),FALSE,TESTED))


ROC_model_object_tested = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                          test = data_non_palliatiave_1_tested %>%
                                            filter(TESTED),
                                          train = data_non_palliative_wave_1,
                                          base_group = base_group)


ROC_model_object_tested_test = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                          test = data_non_palliatiave_2_tested %>%
                                            filter(TESTED),
                                          train = data_non_palliative_wave_1,
                                          base_group = base_group)



ROC_model_object_tested$ROC$AUC
ROC_model_object_tested_test$ROC$AUC


ROC_model_object_untested = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                         test = data_non_palliatiave_1_tested %>%
                                           filter(!TESTED),
                                         train = data_non_palliative_wave_1,
                                         base_group = base_group)


ROC_model_object_untested_test = evaluate_model(model = readRDS(rds_file("model_mort_6h_gSCAD_2021_01_15_uniform_wave_1_ethnicity")),
                                              test = data_non_palliatiave_2_tested %>%
                                                filter(!TESTED),
                                              train = data_non_palliative_wave_1,
                                              base_group = base_group)


ROC_model_object_untested$ROC$AUC
ROC_model_object_untested_test$ROC$AUC


ci.auc(ROC_model_object_tested$retmat_test$STATUS_WITHIN_W_OF_LM == 2,
       ROC_model_object_tested$retmat_test$Probability)
ci.auc(ROC_model_object_tested_test$retmat_test$STATUS_WITHIN_W_OF_LM == 2,
       ROC_model_object_tested_test$retmat_test$Probability)
ci.auc(ROC_model_object_untested$retmat_test$STATUS_WITHIN_W_OF_LM == 2,
       ROC_model_object_untested$retmat_test$Probability)
ci.auc(ROC_model_object_untested_test$retmat_test$STATUS_WITHIN_W_OF_LM == 2,
       ROC_model_object_untested_test$retmat_test$Probability)



############################ Plot Performance measures

library(pROC)
library(precrec)
library(caret)
library(plotROC)
source(filepath("common-binom.R"))


nne <- function(ROC_model_object){
  
  ROC_model_object$retmat <- ROC_model_object$retmat %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  ROC_model_object$retmat_test <- ROC_model_object$retmat_test %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  
  N = nrow(ROC_model_object$retmat_test)
  
  TPF <- function(p){return(ROC_model_object$retmat_test %>% filter(Probability > p & STATUS_WITHIN_W_OF_LM == 2) %>% nrow)}
  FPF <- function(p){return(ROC_model_object$retmat_test %>% filter(Probability > p & STATUS_WITHIN_W_OF_LM != 2) %>% nrow)}

  FP = sapply(X = ROC_model_object$retmat_test$Probability,FUN = FPF)
  TP = sapply(X = ROC_model_object$retmat_test$Probability,FUN = TPF)
  
  return(tibble(NNE = (TP+FP)/TP,
                TP = TP,
                FP = FP,
                NNE2 = N/TP,
                Sensitivity = ROC_model_object$ROC$Sens))
}


Do_Plots <- function(ROC_model_object,run_date,setup,test_data,train_data){
  auc_value = ROC_model_object$ROC$AUC
  
  # Draw ROC
  pdf(pdf_file(paste0("/Landmarking/ROC_",run_date,"_",setup)),height = cm(5),width = cm(5))
  ggplot() +
    geom_line(aes(x = 1 - ROC_model_object$ROC$Spec,
                  y = ROC_model_object$ROC$Sens)) +
    labs(x = "False positive rate (FPR)",
         y = "True positive Rate (TPR)",
         title = paste0("Receiver operating characteristic (AUC ",round(ROC_model_object$ROC$AUC,2),")")) +
    geom_abline(slope = 1,intercept = 0,col = "red") %>% 
    print
  dev.off()
  
  ROC_model_object$retmat <- ROC_model_object$retmat %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  
  ROC_model_object$retmat_test <- ROC_model_object$retmat_test %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  
  logit <- function(p){
    log(p/(1-p))
  }
  
  # Calibration intercept + slope
  sink(file = txt_file(paste0("/Output_",run_date,"_",setup)),split = T)
  calib_intercept <- glm(mortality == 1 ~ offset(logit(Probability)),
                         data = ROC_model_object$retmat_test,
                         family = "binomial")
  summary(calib_intercept) %>% print
  confint(calib_intercept) %>% print
  
  calib_slope <- glm(mortality == 1 ~ logit(Probability),
                     data = ROC_model_object$retmat_test,
                     family = "binomial")
  summary(calib_slope) %>% print
  confint(calib_slope) %>% print
  sink(NULL)
  
  # Stratify by week
  ROC_model_object$retmat_test %>%
    mutate(week = case_when(INDEX < 7 ~ 1,
                            INDEX < 14 ~ 2,
                            INDEX < 21 ~ 3)) %>%
    group_by(week) %>%
    do(auc = unclass(pROC::auc(mortality ~ Probability,
                               data = .,
                               percent = FALSE))[1]) %>%
    unnest_wider(auc)
  
  #AUC
  pROC::auc(mortality ~ Probability,
            data = ROC_model_object$retmat,
            percent = FALSE) %>%
    print
  
  pROC::auc(mortality ~ Probability,
            data = ROC_model_object$retmat_test,
            percent = FALSE) %>%
    print
  
  sscurves <- evalmod(scores = ROC_model_object$retmat_test$Probability,
                      labels = ROC_model_object$retmat_test$mortality)
  
  pdf(pdf_file(paste0("/Landmarking/ROC_PR_",run_date,"_",setup)),height = cm(5),width = cm(10))
  autoplot(sscurves)
  dev.off()
  
  sink(file = txt_file(paste0("CONFUSIONMATRIX_",run_date,"_",setup)),split = T)
  print("MORTALITY_RETMAT")
  mean(ROC_model_object$retmat$mortality) %>% print 
  mean(ROC_model_object$retmat_test$mortality) %>% print
  
  confusionMatrix(data = as.factor(ROC_model_object$retmat$Probability > 
                                     mean(ROC_model_object$retmat$mortality)),
                  reference = as.factor(ROC_model_object$retmat$mortality == 1),
                  positive = "TRUE")%>% print
  sink(NULL)
  
  # Add mortality marker (ever)
  ROC_model_object$retmat <- ROC_model_object$retmat %>%
    group_by(STUDY_SUBJECT_DIGEST) %>%
    mutate(ever_mortality = any(mortality == 1))
  
  pdf(pdf_file(paste0("/Landmarking/Timeline_Probability_",run_date,"_",setup)),height = cm(10),width = cm(5))
  ggplot(ROC_model_object$retmat_test,
         aes(x = INDEX,
             y = Probability,
             group = STUDY_SUBJECT_DIGEST)) +
    geom_line() +
    facet_grid(ever_mortality ~ .)%>% 
    print
  dev.off()
  
  pdf(pdf_file(paste0("/Landmarking/Probability_Histogramm",run_date,"_",setup)),height = cm(10),width = cm(5))
  ggplot(ROC_model_object$retmat_test,
         aes(x = Probability)) +
    geom_histogram(boundary = 0, binwidth = 0.005) +
    facet_wrap( mortality ~ ., scales = "free_y", ncol = 1)%>% 
    print
  dev.off()
  
  
  
  plot_roc <- ggplot(ROC_model_object$retmat_test,
                     aes(d = mortality,
                         m  = Probability)) +
    geom_roc(labelround = 3,
             labelsize = 2,
             cutoffs.at = seq(from = 0.01, to = 0.06, by = 0.01),
             size = 0.5,
             hjust = -0.4,
             vjust = 1.75) +
    geom_abline(slope = 1,
                linetype = "dashed") +
    annotate("text",
             label = paste0("AUROC=", sprintf("%.2f", round(ROC_model_object$ROC$AUC, 2))),
             x = 0.7,
             y = 0.1) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE) +
    theme_classic() +
    xlab("FPR (1 - Specificity)") +
    ylab("TPR (Sensitivity)")+
    labs(title = "A")
  
  plot_roc_comp <- ggplot() +
    geom_roc(data = ROC_model_object$retmat_test,
             mapping = aes(d = mortality,
                 m  = Probability,
                 color = "Wave 2"),
             labelround = 3,
             labelsize = 3,
             cutoffs.at = seq(from = 0.01, to = 0.06, by = 0.01),
             size = 0.5,
             hjust = -0.4,
             vjust = 1.75) +
    geom_roc(data = ROC_model_object$retmat,
             mapping = aes(d = mortality,
                           m  = Probability,
                           color = "Wave 1"),
             labelround = 3,
             labelsize = 3,
             cutoffs.at = seq(from = 0.01, to = 0.06, by = 0.01),
             size = 0.5,
             hjust = -0.4,
             vjust = 1.75) +
    geom_abline(slope = 1,
                linetype = "dashed") +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE) +
    theme_classic() +
    labs(colour = "Test data") +
    xlab("FPR (1 - Specificity)") +
    ylab("TPR (Sensitivity)")
  
  pdf(pdf_file(paste0("/Landmarking/Plot_ROC_",run_date,"_",setup)),height = cm(10),width = cm(5))
  plot_roc %>% print
  dev.off()
  
  # Deciles
  ROC_model_object$retmat_decile <- ROC_model_object$retmat_test %>%
    mutate(Probability_decile = cut(Probability, seq(0, 1, by = 0.1)),
           Probability_decile_lower =
             as.numeric(sub("\\((.+),.*", "\\1", Probability_decile)),
           Probability_decile_upper =
             as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", Probability_decile))) %>%
    group_by(Probability_decile) %>%
    summarise(Probability_decile_lower = unique(Probability_decile_lower),
              Probability_decile_upper = unique(Probability_decile_upper),
              prop_mortality = mean(mortality),
              prop_mortality_min = binom.wilson(x = sum(mortality),
                                                n = n())$lower,
              prop_mortality_max = binom.wilson(x = sum(mortality),
                                                n = n())$upper,
              n = n())
  
  
  # 25-ile
  ROC_model_object$retmat_25ile <- ROC_model_object$retmat_test %>%
    mutate(Probability_decile = cut(Probability, seq(0, 1, by = 0.025)),
           Probability_decile_lower =
             as.numeric(sub("\\((.+),.*", "\\1", Probability_decile)),
           Probability_decile_upper =
             as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", Probability_decile))) %>%
    group_by(Probability_decile) %>%
    summarise(Probability_decile_lower = unique(Probability_decile_lower),
              Probability_decile_upper = unique(Probability_decile_upper),
              prop_mortality = mean(mortality),
              prop_mortality_min = binom.wilson(x = sum(mortality),
                                                n = n())$lower,
              prop_mortality_max = binom.wilson(x = sum(mortality),
                                                n = n())$upper,
              n = n())
  
  # 100-ile
  ROC_model_object$retmat_100ile <- ROC_model_object$retmat_test %>%
    mutate(Probability_decile = cut(Probability, seq(0, 1, by = 0.01)),
           Probability_decile_lower =
             as.numeric(sub("\\((.+),.*", "\\1", Probability_decile)),
           Probability_decile_upper =
             as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", Probability_decile))) %>%
    group_by(Probability_decile) %>%
    summarise(Probability_decile_lower = unique(Probability_decile_lower),
              Probability_decile_upper = unique(Probability_decile_upper),
              prop_mortality = mean(mortality),
              prop_mortality_min = binom.wilson(x = sum(mortality),
                                                n = n())$lower,
              prop_mortality_max = binom.wilson(x = sum(mortality),
                                                n = n())$upper,
              n = n())
  
  plot_calib <- ggplot() +
    geom_smooth(
                aes(x = as.numeric(ROC_model_object$retmat_test$Probability),
                    y = as.numeric(ROC_model_object$retmat_test$STATUS_WITHIN_W_OF_LM == 2)),
                method = "loess",
                se = T,
                colour = "darkgrey") +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_pointrange(aes(x = (ROC_model_object$retmat_decile$Probability_decile_lower +
                               ROC_model_object$retmat_decile$Probability_decile_upper)/2,
                        y  = ROC_model_object$retmat_decile$prop_mortality,
                        ymin = ROC_model_object$retmat_decile$prop_mortality_min,
                        ymax = ROC_model_object$retmat_decile$prop_mortality_max)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE,
                xlim = c(0, 1.02),
                ylim = c(0, 1.02)) +
    theme(aspect.ratio = 1) +
    xlab("Predicted risk of mortality") +
    ylab("Observed mortality rate") +
    theme_classic() +
    theme(legend.position = c(0.99, 0.01),
          legend.justification = c("right", "bottom"))+
    labs(title = "D") 
  
  ggplot() +
    geom_smooth(aes(x = as.numeric(ROC_model_object$retmat$Probability),
                    y = as.numeric(ROC_model_object$retmat$STATUS_WITHIN_W_OF_LM == 2)),
                method = "loess",
                se = F)
  
  # Print calibration
  pdf(pdf_file(paste0("/Landmarking/Plot_calib_",run_date,"_",setup)),height = cm(5),width = cm(5))
  plot_calib %>% print
  dev.off()
  
  # Print 25 ile calibration
  ggplot(ROC_model_object$retmat_25ile,
         aes(x = (Probability_decile_lower +
                    Probability_decile_upper)/2,
             y  = prop_mortality,
             ymin = prop_mortality_min,
             ymax = prop_mortality_max)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_pointrange() +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE,
                xlim = c(0, 1.02),
                ylim = c(0, 1.02)) +
    theme(aspect.ratio = 1) +
    xlab("Predicted risk of mortality") +
    ylab("Observed mortality rate") +
    theme_classic() +
    theme(legend.position = c(0.99, 0.01),
          legend.justification = c("right", "bottom"))%>% 
    print
  ggsave(pdf_file(paste0("/Landmarking/Plot_calib_25ile_",run_date,"_",setup)),height = cm(5),width = cm(5))
  
  
 
  ggplot(ROC_model_object$retmat_100ile,
         aes(x = (Probability_decile_lower +
                    Probability_decile_upper)/2,
             y  = prop_mortality,
             ymin = prop_mortality_min,
             ymax = prop_mortality_max)) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "dashed") +
    geom_pointrange() +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE,
                xlim = c(0, 1.02),
                ylim = c(0, 1.02)) +
    theme(aspect.ratio = 1) +
    xlab("Predicted risk of mortality") +
    ylab("Observed mortality rate") +
    theme_classic() +
    theme(legend.position = c(0.99, 0.01),
          legend.justification = c("right", "bottom"))%>% 
    print
  ggsave(pdf_file(paste0("/Landmarking/Plot_calib_100ile_",run_date,"_",setup)),height = cm(5),width = cm(5))
  
  
  combined <- ROC_model_object$retmat_test %>%
    ungroup %>%
    left_join(test_data %>% 
                select(ROC_model_object$beta$COVARIATE, INDEX),
              by = c("STUDY_SUBJECT_DIGEST","STAY","INDEX")) %>%
    filter(mortality == 0 &
             Probability > 0.2) %>%
    arrange(desc(Probability))
  
  print("BASELINE")
  prc_baseline <- mean(ROC_model_object$retmat_test$mortality)
  print(prc_baseline)
  
  aucs <- attr(sscurves, "aucs")
  auprc <- aucs[aucs$curvetypes == "PRC", "aucs"]
  
  # Precision recall curve
  plot_prc <- ggplot(as.data.frame(sscurves) %>%
                       filter(type == "PRC"),
                     aes(x = x,
                         y = y)) +
    geom_line() +
    geom_hline(yintercept = prc_baseline,
               linetype = "dashed") +
    annotate("text",
             label = paste0("AUPRC=", sprintf("%.2f", round(auprc, 2))),
             x = 0.65,
             y = 0.25) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0", "", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    coord_fixed(expand = FALSE) +
    theme_classic() +
    xlab("Recall (sensitivity)") +
    ylab("Precision (PPV)") +
    labs(title = "B")
  

  
  ## NNE
  
  nne_plot =
    ggplot(as.data.frame(sscurves) %>%
             filter(type == "PRC"),
           aes(x = x,
               y = 1/y)) +
    geom_line() +
    scale_y_continuous(breaks = c(0,seq(5,55,by = 5)),
                       labels = c("0","","10","","20",
                                  "","30","","40",
                                  "","50","")) +
    scale_x_continuous(limits = c(0,1),
                       breaks = seq(0, 1, by = 0.1),
                       labels = c("0.0","", "0.2", "", "0.4", "",
                                  "0.6", "", "0.8", "", "1.0")) +
    theme_classic() +
    labs(title = "C") +
    xlab("Recall (sensitivity)") +
    ylab("Number needed to evaluate (NNE)")
  
  library(patchwork)
  
  
  pdf(pdf_file(paste0("/Landmarking/Performance_GCP_combined_",run_date,"_",setup)),
      width = 20/cm(1),
      height = 15/cm(1))
  print(plot_roc + plot_prc + nne_plot + plot_calib)
  dev.off()
  
  png(png_file(paste0("/Landmarking/Performance_GCP_combined_1",run_date,"_",setup)),
      width = cm(7.874016),
      height = cm(5.905512))
  print(plot_roc + plot_prc + nne_plot + plot_calib)
  ggsave(png_file(paste0("/Landmarking/Performance_GCP_combined_2",run_date,"_",setup)),
          width = cm(3),
          height = cm(3.5))
  dev.off()

  # CAlibration
  # Actual deciles
  ROC_model_object$calibration_plot
  
  # Emp deciles
  ROC_model_object$calibration_plot_emp
  
  prc = ROCR::performance(prediction.obj = ROCR::prediction(predictions = ROC_model_object$retmat_test$Probability,
                                                            labels = ROC_model_object$retmat_test$mortality),measure = "prec",measure.x = "rec")
  
  aucpr = ROCR::performance(prediction.obj = ROCR::prediction(predictions = ROC_model_object$retmat_test$Probability,
                                                      labels = ROC_model_object$retmat_test$mortality),measure = "aucpr")
  
  pdf(pdf_file("La"))
  plot(prc)
  abline(h = prc_baseline,lty = 2)
  dev.off()
  
  print("AUC CI")
  proc_roc =
  pROC::roc(ROC_model_object$retmat_test$mortality,
            ROC_model_object$retmat_test$Probability)
  AUC_CI = pROC::ci.auc(proc_roc,method = "bootstrap")
  print(AUC_CI)
  
  auprc_list = list(labels = ROC_model_object$retmat_test$mortality,
                    scores = ROC_model_object$retmat_test$Probability)
  
  # print("AUC CI")
  return(list(auc_value = auc_value,
              calib_intercept_summary = summary(calib_intercept),
              calib_intercept_confint = confint(calib_intercept),
              calib_slope_summary = summary(calib_slope),
              calib_slope_confint = confint(calib_slope),
              ROC_model_object= ROC_model_object,
              aucs = aucs,
              auprc = auprc,
              plot_roc = plot_roc,
              plot_prc = plot_prc,
              plot_calib = plot_calib,
              plot_roc_comp = plot_roc_comp,
              AUC_CI = AUC_CI,
              baseline = prc_baseline))
}

# Run composite performance plots for wave 1 & 2 for both the 48 and 72 hour windows
Plot_return_train_1_test_2 <-
Do_Plots(ROC_model_object = ROC_model_object_SCAD_train_1_test_2,
         run_date = "2021_11_29",
         setup = "train_wave_1_test_wave_2",
         test_data = data_non_palliative_wave_2_ethnicity_new_steroids,
         train_data = data_non_palliative_wave_1_ethnicity_new_steroids)

Plot_return_train_1_test_1 <-
  Do_Plots(ROC_model_object = ROC_model_object_SCAD_train_1_test_1,
           run_date = "2021_11_29",
           setup = "train_wave_1_test_wave_1",
           test_data = data_non_palliative_wave_1_ethnicity_new_steroids,
           train_data = data_non_palliative_wave_1_ethnicity_new_steroids)

Plot_return_train_1_test_1_w3 <-
  Do_Plots(ROC_model_object = ROC_model_object_SCAD_train_1_test_1_w3,
           run_date = "2021_12_02",
           setup = "train_wave_1_test_wave_1_w3",
           test_data = data_non_palliative_wave_1_w_3,
           train_data = data_non_palliative_wave_1_w_3)

Plot_return_train_1_test_2_w3 <-
  Do_Plots(ROC_model_object = ROC_model_object_SCAD_train_1_test_2_w3,
           run_date = "2021_12_02",
           setup = "train_wave_1_test_wave_2_w3",
           test_data = data_non_palliative_wave_2_w_3,
           train_data = data_non_palliative_wave_1_w_3)





#### NET BENEFIT

net_benefit <- function(ROC_model_object,thresholds){
  
  ROC_model_object$retmat <- ROC_model_object$retmat %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  ROC_model_object$retmat_test <- ROC_model_object$retmat_test %>%
    mutate(mortality = if_else(STATUS_WITHIN_W_OF_LM == 2, 1, 0))
  
  N = nrow(ROC_model_object$retmat_test)
  
  NB <- function(p){
    TP = ROC_model_object$retmat_test %>% filter(Probability > p & STATUS_WITHIN_W_OF_LM == 2) %>% nrow
    FP = ROC_model_object$retmat_test %>% filter(Probability > p & STATUS_WITHIN_W_OF_LM != 2) %>% nrow
    
    return(TP/N - FP/N*p/(1-p))
  }
  
  
  TP_all = ROC_model_object$retmat_test %>% filter(STATUS_WITHIN_W_OF_LM == 2) %>% nrow
  FP_all = ROC_model_object$retmat_test %>% filter(STATUS_WITHIN_W_OF_LM != 2) %>% nrow
  
  NB_all = TP_all/N - FP_all/N*thresholds/(1-thresholds)
  
  return(tibble(Threshold = thresholds*100,
         NetBenefit = sapply(X = thresholds,FUN = NB),
         NB_all = NB_all))
}

at_risk = function(threshold,ROC){
  return(sum(ROC$retmat_test$Probability > threshold))
}

# Compute net benefit for threshold 0.01-1 for both waves  
nb_train1_test1 = net_benefit(ROC_model_object = ROC_model_1,
                              thresholds = seq(0,1,by = 0.01))

nb_train1_test1 = nb_train1_test1 %>% 
  rowwise() %>%
  mutate(At_Risk = at_risk(threshold = Threshold/100,ROC = ROC_model_1))

# Bootstrap sampler for 95% CIs. Subsample 80% of data, and calculate Netbenefit at each threshold.
# Empirical 95% CIs are returned 
bootstrap = function(threshold,data,perc = 0.8,n = 1000){
  NNE = numeric(1)
  
  for(i in 1:n){
    data_sel = data[sample(1:nrow(data),floor(nrow(data)*perc)),]
    N = nrow(data_sel)
    
    TP = data_sel %>% filter(Probability > threshold & STATUS_WITHIN_W_OF_LM == 2) %>% nrow
    FP = data_sel %>% filter(Probability > threshold & STATUS_WITHIN_W_OF_LM != 2) %>% nrow
    
    NNE[i] = (TP/N - FP/N*threshold/(1-threshold))
  }
  
  return(list(lower = quantile(NNE,probs = 0.05,na.rm = T),
              upper = quantile(NNE,probs = 0.95,na.rm = T)))
}

CIs_Wave_1 = sapply(X = seq(0.01,0.99,0.01),FUN =  bootstrap,data = ROC_model_1$retmat_test)
bootstrap(threshold = 0, data =ROC_model_1$retmat_test)
bootstrap(threshold = 1, data =ROC_model_1$retmat_test)

nb_train1_test1 =
add_column(nb_train1_test1,
           matrix(rbind(c(0.01607012 ,0.01862673),
                        CIs_Wave_1 %>% t,
                        c(NA_real_,NA_real_)) %>% unlist %>% as.numeric,ncol =  2) %>% as_tibble())

# Wave 2

nb_train1_test2 = net_benefit(ROC_model_object = ROC_model_2,
                              thresholds = seq(0,1,by = 0.01))

nb_train1_test2 = nb_train1_test2 %>% 
  rowwise() %>%
  mutate(At_Risk = at_risk(threshold = Threshold/100,ROC = ROC_model_2))

ggsave(pdf_file("Landmarking/NNE"),height = cm(4),width = cm(4))

ggplot(data = nne_train1_test2 %>% 
         filter(is.finite(NNE))) +
  geom_line(aes(x = Sensitivity,
                y = NNE))

ggplot(data = nne_train1_test1) +
  geom_line(aes(x = Sensitivity,
                y = NNE))


CIs_Wave_2 = sapply(X = seq(0.01,0.99,0.01),FUN =  bootstrap,data = ROC_model_2$retmat_test)
bootstrap(threshold = 0, data =ROC_model_2$retmat_test)
bootstrap(threshold = 1, data =ROC_model_2$retmat_test)

nb_train1_test2 =
  add_column(nb_train1_test2,
             matrix(rbind(c(0.01832662 ,0.02043754),
                          CIs_Wave_2 %>% t,
                          c(NA_real_,NA_real_)) %>% unlist %>% as.numeric,ncol =  2) %>% as_tibble())

### NET BENEFIT

# PLOT
library(patchwork)

# sample_size = ggplot(data = nb_train1_test1) +
#   geom_line(aes(x = Threshold,
#                 y = At_Risk)) +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(0,100,by = 2)) +
#   labs(x = "",
#        y = "N") +
#   coord_cartesian(xlim = c(0,20),expand = FALSE) 

nb_wave_1_plot = ggplot(data = nb_train1_test1) +
  geom_line(aes(x = Threshold,
                y = NetBenefit,
                color = "Classifying by proposed model")) +
  geom_abline(intercept = 0,slope = 0,linetype = "dashed") +
  labs(x = "Threshold probability (%)",
       y = "Net benefit",
       colour = "") +
  scale_x_continuous(breaks = seq(0,100,by = 2)) +
  geom_line(aes(x = Threshold,
                y = NB_all,
                color = "Classifying everyone as severe")) +
  geom_line(aes(x = Threshold,
                y = 0,
                color = "Classifying no-one as severe")) +
  geom_line(aes(x = Threshold,
                y = V2),
            colour = "red",
            linetype = "dashed") +
  geom_line(aes(x = Threshold,
                y = V1),
            colour = "red",
            linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  +
  guides(color=guide_legend(ncol=2)) +
  coord_cartesian(xlim = c(0,20),ylim = c(-0.0005,0.02),expand = FALSE) +
  scale_colour_manual(values = c("red","black","gray","black","black")) 

# Plot net benefit plot
nb_wave_1_plot %>% print

ggsave(png_file("Landmarking/Net_Benefit_Curve_W1_update_CI"),height = cm(2),width = cm(3.5))

# sample_size = ggplot(data = nb_train1_test2) +
#   geom_line(aes(x = Threshold,
#                 y = At_Risk)) +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(0,100,by = 2)) +
#   labs(x = "",
#        y = "N") +
#   scale_y_continuous(breaks = c(0,5000,10000),
#                      minor_breaks = seq(from = 0,to = 12500,by = 2500)) +
#   coord_cartesian(xlim = c(0,20),expand = FALSE) 

nb_wave_2_plot = 
ggplot(data = nb_train1_test2) +
  geom_line(aes(x = Threshold,
                y = NetBenefit,
                color = "Classifying by model")) +
  geom_abline(intercept = 0,slope = 0,linetype = "dashed") +
  labs(x = "Threshold probability (%)",
       y = "Net benefit",
       colour = "") +
  scale_x_continuous(breaks = seq(0,100,by = 2)) +
  geom_line(aes(x = Threshold,
                y = NB_all,
                color = "Classifying everyone as severe")) +
  geom_line(aes(x = Threshold,
                y = 0,
                color = "Classifying no-one as severe")) +
  geom_line(aes(x = Threshold,
                y = V2),
            colour = "red",
            linetype = "dashed") +
  geom_line(aes(x = Threshold,
                y = V1),
            colour = "red",
            linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  +
  guides(color=guide_legend(ncol=2)) +
  coord_cartesian(xlim = c(0,20),ylim = c(-0.0015,0.02),expand = FALSE) +
  scale_colour_manual(values = c("red","black","gray","black","black"))

nb_wave_2_plot %>% print

ggsave(png_file("Landmarking/Net_Benefit_Curve_W2_update_CI"),height = cm(2),width = cm(3.5))



#### CHECKING IL6 REPLACEMENT BY CRP

crr_crp =
  crr(ftime = data_non_palliative_wave_1$STATUS_TIME_FROM_LM,
      fstatus = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2,
      cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                         select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                                CRP_LOCF,CRP_MISSING,PH_X1_MAX_1,PH_MISSING_1,WCC_LOCF_1,WCC_PLATELETS_MISSING_0,
                                PULSE_MEAN,RR_MIN,SF_MIN)),
      init = rep(0,7))

crr_eval(crr_model = crr_crp,
         cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                            select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                                   PULSE_MEAN,RR_MIN,SF_MIN)),
         results = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2)

crr_eval(crr_model = crr_crp,
         cov1 = as.matrix(data_non_palliative_wave_2 %>% ungroup %>%
                            select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                                   PULSE_MEAN,RR_MIN,SF_MIN)),
         results = data_non_palliative_wave_2$STATUS_WITHIN_W_OF_LM == 2)


# No Labs
crr_none =
  crr(ftime = data_non_palliative_wave_1$STATUS_TIME_FROM_LM,
      fstatus = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2,
      cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                         select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                                PULSE_MEAN,RR_MIN,SF_MIN)),
      init = rep(0,7))

crr_eval(crr_model = crr_none,
         cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                     select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                            PULSE_MEAN,RR_MIN,SF_MIN)),
         results = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2)

crr_eval(crr_model = crr_none,
         cov1 = as.matrix(data_non_palliative_wave_2 %>% ungroup %>%
                            select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING,
                                   PULSE_MEAN,RR_MIN,SF_MIN)),
         results = data_non_palliative_wave_2$STATUS_WITHIN_W_OF_LM == 2)


# Only age and CFS
crr_age_cfs =
  crr(ftime = data_non_palliative_wave_1$STATUS_TIME_FROM_LM,
      fstatus = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2,
      cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                         select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING)),
      init = rep(0,4))

crr_eval(crr_model = crr_age_cfs,
         cov1 = as.matrix(data_non_palliative_wave_2 %>% ungroup %>%
                            select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING)),
         results = data_non_palliative_wave_2$STATUS_WITHIN_W_OF_LM == 2)

crr_eval(crr_model = crr_age_cfs,
         cov1 = as.matrix(data_non_palliative_wave_1 %>% ungroup %>%
                            select(BELOW_75,BELOW_80,FRAILTY,FRAILTY_MISSING)),
         results = data_non_palliative_wave_1$STATUS_WITHIN_W_OF_LM == 2)


# Create cumulative hazard from CRR model object
cumhaz_function = function(crr_model){
  cumhaz = predict(crr_model,cov1 = rep(0,length(crr_model$coef)))
  cumhaz[,2] = -log(1-cumhaz[,2])

  cumhaz_approx = approxfun(x = cumhaz[,1],y = cumhaz[,2])
  cumhaz = cbind(hours = 0:48,cumhazard = c(0,cumhaz_approx(seq(0,2,by = 1/24))[-c(1,49)],max(cumhaz[,2])))
  colnames(cumhaz) = c("Times","Cumulative Baseline Hazard")
  
  return(cumhaz)
}


write.csv(x = cumhaz_function(crr_age_cfs),
          file = csv_out_file("Cumhaz_hourly_age_cfs"))

write.csv(x = cumhaz_function(crr_none),
          file = csv_out_file("Cumhaz_hourly_no_blood_tests"))

write.csv(x = cumhaz_function(cumhaz_function(ROC_model_object_SCAD_train_1_test_1$crr_model)),
          file = csv_out_file("Cumhaz_hourly"))

write.csv(x = cumhaz_function(crr_crp),
          file = csv_out_file("Cumhaz_hourly_crp"))

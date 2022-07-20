marker_matrix_lm <- readRDS(rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_14_01_2021"))
marker_matrix_lm_wave_1 <- readRDS(rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_14_01_2021_wave_1"))
marker_matrix_lm_wave_2 <- readRDS(rds_file("cox_time_varying_markers_lm_cr_2_6h_slopes_14_01_2021_wave_2"))


data_non_palliative_wave_1 <- readRDS(rds_file("data_non_palliative_2021_01_15_6h_shift_prediction_2days_uniform_wave1"))
data_non_palliative_wave_2 <- readRDS(rds_file("data_non_palliative_2021_01_15_6h_shift_prediction_2days_uniform_wave2"))

marker_matrix_lm_wave_1 <- marker_matrix_lm_wave_1 %>%
  filter(paste0(STUDY_SUBJECT_DIGEST,STAY,INDEX) %in% paste0(data_non_palliative_wave_1$STUDY_SUBJECT_DIGEST,data_non_palliative_wave_1$STAY,data_non_palliative_wave_1$INDEX))

marker_matrix_lm_wave_2 <- marker_matrix_lm_wave_2 %>%
  filter(paste0(STUDY_SUBJECT_DIGEST,STAY,INDEX) %in% paste0(data_non_palliative_wave_2$STUDY_SUBJECT_DIGEST,data_non_palliative_wave_2$STAY,data_non_palliative_wave_2$INDEX))


##### VARIABLES

is_zero <- function(x){x == 0}

variable_matrix =
  rbind(c("UREA","LOCF"),
        c("UREA_DAILY_CHANGE",""),
        c("UREA_SLOPE",""),
        c("CREATININE","LOCF"),
        c("CREATININE_DAILY_CHANGE",""),
        c("CREATININE_SLOPE",""),
        c("SODIUM","X1_MAX"),
        c("SODIUM","X2_MAX"),
        c("SODIUM","LOCF"),
        c("SODIUM_DAILY_CHANGE",""),
        c("POTASSIUM","LOCF"),
        c("POTASSIUM_DAILY_CHANGE",""),
        c("POTASSIUM_SLOPE",""),
        c("ALBUMIN","LOCF"),
        c("ALBUMIN_DAILY_CHANGE",""),
        c("ALBUMIN_SLOPE",""),
        c("ALANINE","LOCF"),
        c("ALANINE_DAILY_CHANGE",""),
        c("ALANINE_SLOPE",""),
        c("ALKALINE_PHOSPHATASE","LOCF"),
        c("ALKALINE_PHOSPHATASE_DAILY_CHANGE",""),
        c("ALKALINE_PHOSPHATASE_SLOPE",""),
        c("BILIRUBIN","LOCF"),
        c("BILIRUBIN_SLOPE",""),
        c("BILIRUBIN_DAILY_CHANGE",""),
        c("LDH","LOCF"),
        c("LDH_DAILY_CHANGE",""),
        c("LDH_SLOPE",""),
        c("CRP","LOCF"),
        c("CRP_DAILY_CHANGE",""),
        c("CRP_SLOPE",""),
        c("PCT","LOCF"),
        c("PCT_DAILY_CHANGE",""),
        c("PCT_SLOPE",""),
        c("FERRITIN","LOCF"),
        c("FERRITIN_DAILY_CHANGE",""),
        c("FERRITIN_SLOPE",""),
        c("HB","LOCF"),
        c("HB_DAILY_CHANGE",""),
        c("HB_SLOPE",""),
        c("WCC","LOCF"),
        c("WCC_DAILY_CHANGE",""),
        c("WCC_SLOPE",""),
        c("NEUTROPHILS","LOCF"),
        c("NEUTROPHILS_DAILY_CHANGE",""),
        c("NEUTROPHILS_SLOPE",""),
        c("LYMPHOCYTES","LOCF"),
        c("LYMPHOCYTES_DAILY_CHANGE",""),
        c("LYMPHOCYTES_SLOPE",""),
        c("NL_RATIO","LOCF"),
        c("NL_RATIO_DAILY_CHANGE",""),
        c("NL_RATIO_SLOPE",""),
        c("EOSINOPHILS","LOCF"),
        c("EOSINOPHILS_DAILY_CHANGE",""),
        c("EOSINOPHILS_SLOPE",""),
        c("MONOCYTES","LOCF"),
        c("MONOCYTES_DAILY_CHANGE",""),
        c("MONOCYTES_SLOPE",""),
        c("PLATELETS","LOCF"),
        c("PLATELETS_DAILY_CHANGE",""),
        c("PLATELETS_SLOPE",""),
        c("RDW","LOCF"),
        c("RDW_DAILY_CHANGE",""),
        c("RDW_SLOPE",""),
        c("PT","LOCF"),
        c("PT_DAILY_CHANGE",""),
        c("PT_SLOPE",""),
        c("APTT","LOCF"),
        c("APTT_DAILY_CHANGE",""),
        c("APTT_SLOPE",""),
        c("DDIMER","LOCF"),
        c("DDIMER_DAILY_CHANGE",""),
        c("DDIMER_SLOPE",""),
        c("TROPONIN","LOCF"),
        c("TROPONIN_DAILY_CHANGE",""),
        c("TROPONIN_SLOPE",""),
        c("LACTATE","LOCF"),
        c("LACTATE_DAILY_CHANGE",""),
        c("LACTATE_SLOPE",""),
        c("IG","LOCF"),
        c("IG_DAILY_CHANGE",""),
        c("IG_SLOPE",""),
        c("TNFA","LOCF"),
        c("TNFA_DAILY_CHANGE",""),
        c("TNFA_SLOPE",""),
        c("IL1","LOCF"),
        c("IL1_DAILY_CHANGE",""),
        c("IL1_SLOPE",""),
        c("IL6","LOCF"),
        c("IL6_DAILY_CHANGE",""),
        c("IL6_SLOPE",""),
        c("IL10","LOCF"),
        c("IL10_DAILY_CHANGE",""),
        c("IL10_SLOPE",""),
        c("IL_RATIO","LOCF"),
        c("IL_RATIO_DAILY_CHANGE",""),
        c("IL_RATIO_SLOPE",""),
        c("PH","X1_MAX"),
        c("PH","X2_MAX"),
        c("PH_DAILY_CHANGE",""))


measurements_matrix <- rbind(c("PULSE","MEAN"),
                             c("PULSE","MIN"),
                             c("PULSE","MAX"),
                             c("PULSE_SLOPE",""),
                             c("PULSE_DAILY_CHANGE",""),
                             c("TEMP","MEAN"),
                             c("TEMP","MIN"),
                             c("TEMP","MAX"),
                             c("TEMP_SLOPE",""),
                             c("TEMP_DAILY_CHANGE",""),
                             c("RR","MEAN"),
                             c("RR","MIN"),
                             c("RR","MAX"),
                             c("RR_SLOPE",""),
                             c("RR_DAILY_CHANGE",""),
                             c("SF","MEAN"),
                             c("SF","MIN"),
                             c("SF","MAX"),
                             c("SF_SLOPE",""),
                             c("SF_DAILY_CHANGE",""),
                             c("PF","MEAN"),
                             c("PF","MIN"),
                             c("PF","MAX"),
                             c("PF_SLOPE",""),
                             c("PF_DAILY_CHANGE",""),
                             c("BP","MEAN"),
                             c("BP","MIN"),
                             c("BP","MAX"),
                             c("BP_SLOPE",""),
                             c("BP_DAILY_CHANGE",""))

## 

print_summary = function(x,var_variant){
  
  # MISSINGNESS
  MEAN_DAYS_MISSING = landmark_data %>% pull(paste0(x,"_MISSING")) %>% mean
  if(grepl("SLOPE|CHANGE",x)){
    MEAN_COUNT_PER_DAY = NA
  }else{
    MEAN_COUNT_PER_DAY = landmark_data %>% pull(paste0(x,"_COUNT")) %>% mean
  }
    
  # VALUES PER DAY
  if(var_variant != "" ){
    DAYS_SUMMARY = landmark_data %>% filter_at(paste0(x,"_MISSING"),all_vars(is_zero(.))) %>% pull(paste0(x,"_",var_variant)) %>% summary
  }else{
    DAYS_SUMMARY = landmark_data %>% filter_at(paste0(x,"_MISSING"),all_vars(is_zero(.))) %>% pull(paste0(x,var_variant)) %>% summary
  }
  
  DAYS_SUMMARY = round(DAYS_SUMMARY,5)
  return(c(paste0(DAYS_SUMMARY[3]," [",DAYS_SUMMARY[2],",",DAYS_SUMMARY[5],"] "),
           paste0(round(MEAN_COUNT_PER_DAY,5)," (",round(MEAN_DAYS_MISSING*100,5),"%)"))
         )
}

#print_summary(x = "UREA_SLOPE",var_variant = "",data = landmark_data)



summary_comorbidities <- function(x){
  avrg =
  x %>% group_by(STUDY_SUBJECT_DIGEST) %>%
    #select(HYPERTENSION:DEMENTIA) %>%
    summarise_at(vars(ICU,VENTILATON,CARDIO_VASCULAR_SUPPORT,RRT,STEROIDS,INOTROPES,VASOPRESSORS,HYPERTENSION:DEMENTIA),max) %>%
    select(ICU,VENTILATON,CARDIO_VASCULAR_SUPPORT,RRT,STEROIDS,INOTROPES,VASOPRESSORS,HYPERTENSION:DEMENTIA) %>%
    colMeans()
  
  retmat = cbind(round(avrg*100,2))
  colnames(retmat) = "% of COHORT"
  return(retmat)
}

build_matrix <- function(x){
  summary_matrix =
    mapply(FUN = print_summary,
           x = x[,1],
           var_variant = x[,2]) %>% t
  
  colnames(summary_matrix) <- c("SUMMARY - MEDIAN [IQR] ","MEASUREMENTS P. LM (% DAYS NO MEASUREMENT)")
  rownames(summary_matrix) <- apply(X = x,MARGIN = 1,FUN = paste,collapse = "_")
  
  return(summary_matrix)
}

demographic_data <- function(x){
  AGE_SUM = x %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% summarise(AGE = unique(AGE_AT_ADM)) %>% pull(AGE) %>% as.numeric %>% summary %>% round(digits = 3)
  GENDER_SUM = x %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% summarise(GENDER = unique(GENDER_DESC)) %>% pull(GENDER) %>% mean
  FIRST_BMI_SUM = x %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(BMI_MISSING == 0) %>% arrange(INDEX) %>% slice(1) %>% pull(BMI) %>% summary%>% round(digits = 3)
  FRAILTY_SUM = x %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(FRAILTY_MISSING == 0) %>% summarise(FRAILTY = unique(FRAILTY)) %>% pull(FRAILTY) %>% summary %>% round(digits = 3)
  BMI_X1 = x %>% filter(BMI_MISSING == 0) %>% pull(BMI_X1_MAX) %>% summary %>% round(digits = 3)
  BMI_X2 = x %>% filter(BMI_MISSING == 0) %>% pull(BMI_X2_MAX) %>% summary %>% round(digits = 3)
  BMI_MISSING = x %>% pull(BMI_MISSING) %>% mean%>% round(digits = 4) * 100 
  FRAILTY_MISSING = x %>% pull(FRAILTY_MISSING) %>% mean%>% round(digits = 4) *100 
  
  return(
    rbind(c("AGE",paste0(AGE_SUM[3]," [",AGE_SUM[2],",",AGE_SUM[5],"] "),""),
          c("GENDER FEMALE/MALE",paste0(round((1-GENDER_SUM)*100,2),"%/ ",round(GENDER_SUM*100,2),"%"),""),
          c("FIRST BMI AFTER ADMISSION",paste0(FIRST_BMI_SUM[4]," [",FIRST_BMI_SUM[2],",",FIRST_BMI_SUM[5],"] "),BMI_MISSING),
          c("BMI X1",paste0(BMI_X1[3]," [",BMI_X1[2],",",BMI_X1[5],"] "),BMI_MISSING),
          c("BMI X2",paste0(BMI_X2[3]," [",BMI_X2[2],",",BMI_X2[5],"] "),BMI_MISSING),
          c("FRAILTY",paste0(FRAILTY_SUM[3]," [",FRAILTY_SUM[2],",",FRAILTY_SUM[5],"] "),FRAILTY_MISSING))
         )
}

LATEST_ENTRY = function(x,data){
   data %>% filter_at(paste0(x,"_MISSING"),all_vars(is_zero(.))) %>% pull(REAL_TIME) %>% summary %>% print
  return()
}

##### ENTIRETY
# landmark_data = marker_matrix_lm
# 
# write.csv(x = build_matrix(x = variable_matrix),file = csv_out_file("S1_TESTS_WAVE_1_AND_2_DAILY_CHANGE_08_01_2021_redo"))
# write.csv(x = build_matrix(x = measurements_matrix),file = csv_out_file("S1_VITALS_WAVE_1_AND_2_DAILY_CHANGE_08_01_2021_redo"))
# write.csv(x = summary_comorbidities(x = landmark_data),file = csv_out_file("S1_TREATMENTS_AND_COMORBIDITIES_WAVE_1_AND_2_08_01_2021_redo"))
# write.csv(x = demographic_data(landmark_data),file = csv_out_file("S1_DEMOGRAPHICS_WAVE_1_AND_2_08_01_2021_redo"))


landmark_data = marker_matrix_lm_wave_1

write.csv(x = build_matrix(x = variable_matrix),file = csv_out_file("S1_TESTS_WAVE_1_DAILY_CHANGE_15_01_2021_MEDIAN_IQR"))
write.csv(x = build_matrix(x = measurements_matrix),file = csv_out_file("S1_VITALS_WAVE_1_DAILY_CHANGE_15_01_2021_MEDIAN_IQR"))
write.csv(x = summary_comorbidities(x = landmark_data),file = csv_out_file("S1_TREATMENTS_AND_COMORBIDITIES_WAVE_1_15_01_2021_MEDIAN_IQR"))
write.csv(x = demographic_data(landmark_data),file = csv_out_file("S1_DEMOGRAPHICS_WAVE_1_15_01_2021_MEDIAN_IQR"))



landmark_data = marker_matrix_lm_wave_2

write.csv(x = build_matrix(x = variable_matrix),file = csv_out_file("S1_TESTS_WAVE_2_DAILY_CHANGE_15_01_2021_MEDIAN_IQR"))
write.csv(x = build_matrix(x = measurements_matrix),file = csv_out_file("S1_VITALS_WAVE_2_DAILY_CHANGE_15_01_2021_MEDIAN_IQR"))
write.csv(x = summary_comorbidities(x = landmark_data),file = csv_out_file("S1_TREATMENTS_AND_COMORBIDITIES_WAVE_2_15_01_2021_MEDIAN_IQR"))
write.csv(x = demographic_data(landmark_data),file = csv_out_file("S1_DEMOGRAPHICS_WAVE_2_15_01_2021_MEDIAN_IQR"))



# BMI lower end

png(png_file("Cohort/BMI_low_wave_1_2"),width = 600,height = 400)
ggplot(data = marker_matrix_lm %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(BMI_MISSING == 0) %>% arrange(INDEX) %>% slice(1),aes(x = BMI)) +
  geom_histogram(binwidth = 1,colour = "black",fill = "lightgray") +
  coord_cartesian(xlim = c(0,20),ylim = c(0,45)) +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = seq(0,60,by = 5)) +
  labs(x = "BMI (20 and below,first after admission)",
       y = "Patients")
dev.off()

png(png_file("Cohort/BMI_low_wave_1"),width = 600,height = 400)
ggplot(data = marker_matrix_lm_cr_2_wave_1 %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(BMI_MISSING == 0) %>% arrange(INDEX) %>% slice(1),aes(x = BMI)) +
  geom_histogram(binwidth = 1,colour = "black",fill = "lightgray") +
  coord_cartesian(xlim = c(0,20),ylim = c(0,45)) +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = seq(0,60,by = 5)) +
  labs(x = "BMI (20 and below,first after admission)",
       y = "Patients")
dev.off()

png(png_file("Cohort/BMI_low_wave_2"),width = 600,height = 400)
ggplot(data = marker_matrix_lm_cr_2_wave_2 %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(BMI_MISSING == 0) %>% arrange(INDEX) %>% slice(1),aes(x = BMI)) +
  geom_histogram(binwidth = 1,colour = "black",fill = "lightgray") +
  coord_cartesian(xlim = c(0,20),ylim = c(0,45)) +
  scale_x_continuous(breaks = 0:20) +
  scale_y_continuous(breaks = seq(0,60,by = 5)) +
  labs(x = "BMI (20 and below,first after admission)",
       y = "Patients")
dev.off()

sink(file = txt_file("LATEST_LMS_BY_TEST_14_01_2021"))
for(i in 1:length(variable_matrix[,1])){
  print(variable_matrix[i,1])
  LATEST_ENTRY(x = variable_matrix[i,1],data = marker_matrix_lm_cr_2)
}
sink(NULL)

sink(file = txt_file("LATEST_LMS_BY_VITAL_14_01_2021"))
for(i in 1:length(measurements_matrix[,1])){
  print(measurements_matrix[i,1])
  LATEST_ENTRY(x = measurements_matrix[i,1],data = marker_matrix_lm_cr_2)
}
sink(NULL)


##### TEST VARIANTS AND DATES
# Make sure to load correct "_std" test files
# before running this

write.csv(x = 
crp_std %>% 
  bind_rows(wcc_std)%>% 
  bind_rows(ferritin_std)%>% 
  bind_rows(eosinophils_std)%>% 
  bind_rows(neutrophils_std)%>% 
  bind_rows(lymphocytes_std)%>% 
  bind_rows(platelets_std)%>% 
  bind_rows(monocytes_std)%>% 
  bind_rows(nl_ratio_std)%>% 
  bind_rows(il1_std)%>% 
  bind_rows(il6_std)%>% 
  bind_rows(il10_std)%>% 
  bind_rows(il_ratio_std)%>% 
  bind_rows(albumin_std)%>% 
  bind_rows(alanine_std)%>% 
  bind_rows(bilirubin_std)%>% 
  bind_rows(chloride_std)%>% 
  bind_rows(cortisol_std)%>% 
  bind_rows(creatinine_std)%>% 
  bind_rows(urea_std)%>% 
  bind_rows(sodium_std)%>% 
  bind_rows(potassium_std)%>% 
  bind_rows(rdw_std)%>% 
  bind_rows(ferritin_std)%>% 
  bind_rows(hb_std)%>% 
  bind_rows(ig_std)%>% 
  bind_rows(pct_std)%>% 
  bind_rows(ddimer_std)%>% 
  bind_rows(tnfa_std)%>% 
  bind_rows(aspartate_std)%>% 
  bind_rows(ldh_std)%>% 
  bind_rows(troponin_std)%>% 
  bind_rows(aptt_std)%>% 
  bind_rows(pt_std)%>% 
  bind_rows(magnesium_std)%>% 
  bind_rows(alkaline_phosphatase_std)%>% 
  bind_rows(lactate_std) %>% 
  group_by(TestName) %>%
  summarise(NUMBER_TESTS = n(),MIN_DATE = min(ResultDate),MAX_DATE = max(ResultDate)),
file = csv_out_file("Test_Variants_Dates_14_01_2021")) 

# GCS


data_non_palliative_wave_1 %>% filter(GCS_BELOW_9) %>% nrow
data_non_palliative_wave_1 %>% filter(GCS_BELOW_12) %>% nrow

data_non_palliative_wave_2 %>% filter(GCS_BELOW_9) %>% nrow
data_non_palliative_wave_2 %>% filter(GCS_BELOW_12) %>% nrow

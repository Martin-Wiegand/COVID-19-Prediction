fsheet_raw <- readRDS(rds_file("fsheet_raw"))
adt_data <- readRDS(rds_file("adt"))
adm_data <- readRDS(rds_file("adm"))

# Overview of measurements
#fsheet_raw$ disp_name %>% table

fsheet_disp_name <- fsheet_raw %>%
  group_by(disp_name) %>%
  tally

write.csv(fsheet_disp_name,
          file = csv_out_file("fsheet_disp_name"))

##### RJBG O2 Device
fsheet_o2device_values <- fsheet_raw %>%
  filter(disp_name == "O2 Device") %>%
  group_by(measured_value) %>%
  tally %>%
  arrange(desc(n))

write.csv(fsheet_o2device_values,
          file = csv_out_file("2021-01-12-fsheet_o2device_values"),
          row.names = FALSE)

######

# ----------- O2 DEVICE
height_raw <- fsheet_raw %>% filter(grepl("height",disp_name,ignore.case = T))

height <- height_raw %>%
  mutate(measured_value = convert.numeric(measured_value)*2.54) %>%
  filter(!is.na(measured_value))
  
saveRDS(height,rds_file("height"))

# ----------- O2 DEVICE
O2_device_raw <- fsheet_raw %>% filter(disp_name == "O2 Device")

saveRDS(O2_device_raw,rds_file("O2_device_raw"))



#------------ NIV
niv_raw <- fsheet_raw %>%
  filter(disp_name == "O2 Device") %>%
  filter(measured_value %in% c("Bi-PAP","CPAP"))

saveRDS(niv_raw,rds_file("niv_raw"))

# ----------- ACVPU

fsheet_raw %>%
  filter(str_detect(disp_name, "NEWS")) %>%
  group_by(disp_name) %>%
  tally

acvpu_raw <- fsheet_raw %>% filter(disp_name == "NEWS2/MEOWS: ACVPU")
saveRDS(acvpu_raw,rds_file("acvpu_raw"))

# ----------- BLOOD PRESSURE

fsheet_raw %>%
  filter(str_detect(disp_name, "BP|MAP|Blood Pressure")) %>%
  group_by(disp_name) %>%
  tally %>%
  print(n = Inf)

# Note "MAP (cmH2O)" looks like airway pressure
# "MAP High (cm H2O)" is presumably also

BP_raw <- fsheet_raw %>%
  filter(disp_name %in% c("BP",
                          "Blood Pressure",
                          "Mean BP (mmHg)",
                          "Arterial Line BP",
                          "Arterial Line MAP (mmHg)"))

BP_raw <- BP_raw %>% 
  mutate(TYPE = case_when(disp_name == "Blood Pressure" ~ "SYST/DIAS",
                          disp_name == "Mean BP (mmHg)" ~ "MAP",
                          disp_name == "BP" ~ "SYST/DIAS",
                          disp_name == "Arterial Line BP" ~ "SYST/DIAS",
                          disp_name == "Arterial Line MAP (mmHg)" ~ "MAP"))

BP_mod <-
BP_raw %>% 
  mutate(SYST = case_when(TYPE == "SYST/DIAS" ~ get.syst(measured_value),
                          TYPE == "MAP" ~ Inf)) %>%
  mutate(DIAS = case_when(TYPE == "SYST/DIAS" ~ get.dias(measured_value),
                          TYPE == "MAP" ~ Inf)) %>%
  mutate(MAP = case_when(TYPE == "MAP" ~ convert.numeric(measured_value),
                         TYPE == "SYST/DIAS" ~ SYST/3+2*DIAS/3))

BP_mod <- BP_mod %>%
  filter(MAP <= 160 & 30 <= MAP)

saveRDS(BP_mod, file = rds_file("bp"))

png(png_file("Fsheet/MAP"),width = 1200,height = 800)
ggplot(data = BP_mod) +
  geom_histogram(aes(x = MAP), fill = "green",alpha = 0.5) +
  geom_histogram(aes(x = SYST),fill = "red",alpha = 0.5) +
  geom_histogram(aes(x = DIAS),fill = "blue",alpha = 0.5)
#   BP_mod$SYST %>% hist(col = rgb(1,0,0,alpha = 0.25),xlim = c(0,250),ylim = c(0,20000),breaks = 100,main = "MAP Measurements (Computed)")
# BP_mod$DIAS %>% hist(col = rgb(0,0,1,alpha = 0.25),add = T,breaks = 100)
# BP_mod$MAP %>% hist(col = rgb(0,1,0,alpha = 0.5),add = T,breaks = 50)
# legend("topright",legend = c("Systolic","Diastolic","MAP"),pch = 15,col = c("red","blue","green"))
dev.off()



# -----------  TEMPERATURE

fsheet_raw %>%
  filter(str_detect(disp_name, "Temp|temp")) %>%
  group_by(disp_name) %>%
  tally

# "Inspiratory Temp." is different
# "Skin Temperature" is different

temp_raw <- fsheet_raw %>%
  filter(disp_name %in% c("Temp", "Temp (in Celsius)", "Temperature"))

ggplot(temp_raw,
       aes(x = as.numeric(measured_value))) +
  geom_histogram() +
  facet_wrap(disp_name ~ .)

# Check parameter range
# png(png_file("Cohort/TemperatureRAW"),width = 1200,height = 800)
# temp_raw %>% pull(measured_value) %>% as.numeric %>% hist(breaks = 100,main = "Temperature measurement overview")
# dev.off()

temp_raw %>% pull(measured_value) %>% as.numeric %>% summary

# Mostly Fahrenheit, but needs adjusting.
temp_mod <- temp_raw %>%
  mutate(measured_value = as.numeric(measured_value),
         measured_value =
           case_when(disp_name == "Temp (in Celsius)" ~ measured_value,
                     TRUE ~ convert.to.celcius(measured_value)))

ggplot(temp_mod,
       aes(x = as.numeric(measured_value))) +
  geom_histogram() +
  facet_wrap(disp_name ~ .)

# png(png_file("Cohort/TempCelcius"),width = 1200,height = 800)
# temp_mod$measured_value %>% hist(main = "Temperature in Celcius",xlab = "Temperature",breaks = 20)
# dev.off()

# Check for different measurements at the same time
temp_mod %>% group_by(STUDY_SUBJECT_DIGEST,MEASURE_TIME) %>%
  filter(n() > 1) %>%
  filter(length(unique(measured_value)) > 1) %>%
  arrange(STUDY_SUBJECT_DIGEST,MEASURE_TIME)

# Pivot into table with measurements from the same time in a single row
temp_mod_wider <- temp_mod %>%
  select(STUDY_SUBJECT_DIGEST, MEASURE_TIME, measured_value, disp_name) %>%
  distinct %>%
  group_by(STUDY_SUBJECT_DIGEST, MEASURE_TIME) %>%
  pivot_wider(names_from = disp_name, values_from = measured_value)

# Explore differences between the measurements at the same time

# Disparity here!
ggplot(temp_mod_wider,
       aes(x = Temp, y = `Temp (in Celsius)`)) +
  geom_point() +
  geom_abline(slope = 1)

ggplot(temp_mod_wider,
       aes(y = Temp, x = `Temp (in Celsius)`)) +
  geom_point() +
  geom_abline(slope = 1)

ggplot(temp_mod_wider,
       aes(x = Temp, y = `Temperature`)) +
  geom_point() +
  geom_abline(slope = 1)

ggplot(temp_mod_wider,
       aes(x = `Temp (in Celsius)`, y = `Temperature`)) +
  geom_point() +
  geom_abline(slope = 1)

# Comparing "Temp" and "Temp (in Celsius)" shows there is a constant disparity
# of 1 between SOME (NOT ALL) of the measurements at the same time
temp_mod_disparity <- temp_mod_wider %>%
  filter(near(abs(Temp - `Temp (in Celsius)`), 1)) %>%
  pivot_longer(c(`Temp`, `Temp (in Celsius)`, `Temperature`),
               names_to = "disp_name",
               values_to = "measured_value")

# Looking at the histogram suggests "Temp (in Celsius)" is too high
ggplot(temp_mod_disparity, aes(x = measured_value)) +
  geom_histogram() +
  facet_wrap(disp_name ~ .)

# So use "Temp (in Celsius)" as a last resort. This will get rid of the
# higher "Temp (in Celsius)" measurements when there is a "Temp" at the same
# time
temp_mod_wider <- temp_mod_wider %>%
  mutate(measured_value = coalesce(Temp, Temperature, `Temp (in Celsius)`))

temp_mod <-
  temp_mod_wider %>%
  select(STUDY_SUBJECT_DIGEST, MEASURE_TIME, measured_value)

# Save modified value
saveRDS(temp_mod, file = rds_file("temp_mod"))


# -----------  POSITIONING
posit_raw <-
  fsheet_raw %>% filter(disp_name == "Positioning")

posit_table <-
  posit_raw$measured_value %>% table

# png(png_file("Cohort/Positioning"),width = 1200,height = 800)
# par(mar = c(10,4,4,4))
# posit_table %>% barplot(las = 2)
# dev.off()

saveRDS(posit_raw,file = rds_file("posit"))


# ----------- glasgow coma scale

gcs_raw <- fsheet_raw %>% filter(disp_name == "Glasgow Coma Scale Score")

gcs_raw %>%
  group_by(measured_value) %>%
  tally

saveRDS(gcs_raw,file = rds_file("gcs_raw"))

# -----------  RR
fsheet_raw %>%
  filter(str_detect(disp_name, "RR|Resp")) %>%
  group_by(disp_name) %>%
  tally

rr_raw <-
  fsheet_raw %>%
  filter(disp_name %in% c("Resp", "Resp Rate", "RR Spont", "Resp Rate (Set)"))

rr_raw <- rr_raw %>% 
  mutate(measured_value = convert.numeric(measured_value)) %>% 
  filter(measured_value <= 60 & 4 <= measured_value)

pdf(pdf_file("Fsheet/rr"))
rr_raw$measured_value %>% as.numeric %>%
  hist(breaks = 30,main = "Resp rate")
graphics.off()

saveRDS(rr_raw,file = rds_file("rr_raw"))

# -----------  VENT MODE
ventmode_raw <-
  fsheet_raw %>%
  filter(disp_name == "Vent Mode")

# START RJBG added 2020-04-29
covid_ventmode_values <- ventmode_raw %>% group_by(measured_value) %>% tally

write.csv(covid_ventmode_values,
          file = csv_out_file("2021-01-12-covid_ventmode_values"),
          row.names = FALSE)
# END RJBG added 2020-04-29

ventmode_raw <-
  ventmode_raw %>%
  left_join(mechanical_ventilation_list %>%
              select(measured_value,mechanical_ventilation),
            by = "measured_value")

# Check for missingness
mechanical_ventilation_unlabelled <- ventmode_raw %>% filter(is.na(mechanical_ventilation))

if (nrow(mechanical_ventilation_unlabelled) > 0){
  cat("********** possible unlabelled mechanical vent modes **********")
}

O2_device_raw <-
  O2_device_raw %>% left_join(O2_device_list, by = "measured_value")

# unlabelled
O2_device_raw_unlabelled <- O2_device_raw %>%
  filter(is.na(mechanical_ventilation))

if (nrow(O2_device_raw_unlabelled) > 0){
  cat("********** possible unlabelled vent modes **********")
}

saveRDS(ventmode_raw, file = rds_file("ventmode"))

# Join O2 & Ventmode
ventmode_join <- bind_rows(ventmode_raw, O2_device_raw)
ventmode_join <- ventmode_join %>% arrange(STUDY_SUBJECT_DIGEST,MEASURE_TIME)

saveRDS(ventmode_join,rds_file("ventmode_join"))

# -----------  VENTILATOR PATIENT
ventpatient_raw <-
  fsheet_raw %>% filter(disp_name == "Ventilator Patient")

ventpatient_mod <- ventpatient_raw %>% rename(DATETIME = MEASURE_TIME)
ventpatient_mod <- adt_data %>%
  do(Hosp_Match(.,data = ventpatient_mod))

# --- checking Ventilated Patient vs Ventmode reveals that the definition
# --- in the data is different from ICUDEL definition -> use previous definition

ventmode_values <- mechanical_ventilation_list %>%
  as_tibble %>%
  rename(vent_mode = measured_value)


# FIO2
fsheet_raw %>%
  filter(str_detect(disp_name, coll("fio2", ignore_case = TRUE))) %>%
  group_by(disp_name) %>%
  tally

# Check fio2 set is OK
fio2_set_data <- fsheet_raw %>%
  filter(disp_name == "FIO2 (%) set")

fio2_set_data %>% filter(as.numeric(measured_value) < 1)
fio2_set_data %>% filter(as.numeric(measured_value) > 100)

pdf(pdf_file("Fsheet/fio2_set"))
ggplot(fio2_set_data, aes(x = as.numeric(measured_value))) +
  geom_histogram(binwidth = 1, boundary = 0)
graphics.off()

# Use both FiO2
fio2_data <- fsheet_raw %>% filter(disp_name %in% c("FiO2", "FIO2 (%) set"))

# CHeck flo meas id vs measured value
ggplot(data = fio2_data) + geom_histogram(aes(x = convert.numeric(measured_value))) + facet_wrap(~ flo.meas_id)

fio2_data <- fio2_data %>%
  mutate(measured_value = ifelse(flo.meas_id == 3047001100,
                                 convert.numeric(measured_value) * 100,
                                 convert.numeric(measured_value))) %>%
  filter(measured_value >= 21 & measured_value <= 100)

png(png_file("Fsheet/fio2"),width = 1200,height = 800)
fio2_data$measured_value %>% as.numeric %>% hist(main = "FiO2")
dev.off()

fio2_tally <-
fio2_data %>% 
  distinct %>%
  mutate(MEASURE_TIME = as.Date(MEASURE_TIME)) %>% 
  group_by(STUDY_SUBJECT_DIGEST,MEASURE_TIME) %>% 
  tally %>% pull(n) %>% hist(xlab = "Measurements per Day per Patient",main = "FiO2")

saveRDS(fio2_data,file = rds_file("fio2"))

### SpO2
fsheet_raw %>%
  filter(str_detect(disp_name, coll("spO2", ignore_case = TRUE))) %>%
  group_by(disp_name) %>%
  tally

# SPO2 Heart Rate is a pulse measurement rather than SPO2 measurement
spo2_data <- fsheet_raw %>% filter(disp_name == "SpO2") %>%
  filter(as.numeric(measured_value) >= 50)


png(png_file("Fsheet/spo2"),width = 1200,height = 800)
spo2_data$measured_value %>% as.numeric %>% hist(main = "SpO2")
dev.off()

saveRDS(spo2_data,file = rds_file("spo2"))

#### Approximate FIO2
flow_rate_data <- fsheet_raw %>%
  filter(disp_name == "O2 Flow Rate (L/min)") %>%
  mutate(measured_value = as.numeric(measured_value))
o2device_data <- fsheet_raw %>%
  filter(disp_name == "O2 Device")
write.csv(x = o2device_data$measured_value %>% table,
          file = csv_out_file("02_device_table"))

o2device_data %>%
  group_by(measured_value) %>%
  tally

Cannula <- c("Nasal cannula")
Mask <- c("Humidified mask (heated)", "Humidified mask(cold)", "Simple mask")
MaskRes <- c("Non-rebreather mask")

fio2_app_cannula <- approxfun(x = 0:6,
                              y = c(0.21, 0.24, 0.28, 0.32, 0.36, 0.40, 0.44),
                              method = "linear",
                              rule = 2)
fio2_app_mask <- approxfun(x = c(0, 5, 6.5, 7.5),
                           y = c(0.21, 0.4, 0.5, 0.6),
                           method = "linear",
                           rule = 2)
fio2_app_maskres <- approxfun(x = c(0, 6, 7, 8, 9, 10),
                              y = c(0.21, 0.6, 0.7, 0.8, 0.9, 0.95),
                              method = "linear",
                              rule = 2)

fio2_app_venturi <- function(flow_rate){
  case_when(flow_rate == 0 ~ 0.21,
            flow_rate == 2 ~ 0.24,
            flow_rate == 4 ~ 0.28,
            flow_rate == 6 ~ 0.31,
            flow_rate == 8 ~ 0.35,
            flow_rate == 10 ~ 0.4,
            flow_rate == 15 ~ 0.6,
            TRUE ~ NA_real_)
}

# When multiple measurements are available at the same measure time, choose FiO2 over FiO2 (Set). If only one
# type of measurements is available, take what is there (Set or FiO2). When multiple FiO2 or FiO2 (Set) are available
# at the same time, take first element.
fio2_unique <-
fio2_data %>% group_by(STUDY_SUBJECT_DIGEST,MEASURE_TIME) %>% filter(n() == 1 | disp_name == "FiO2") %>%
  mutate(measured_value = measured_value/100) %>%
  slice(1)

fio2_matrix <-
  fio2_unique %>% 
  full_join(o2device_data %>% select(STUDY_SUBJECT_DIGEST,MEASURE_TIME,measured_value) %>% rename(o2_device = measured_value),by = c("STUDY_SUBJECT_DIGEST","MEASURE_TIME")) %>%
  full_join(flow_rate_data %>% select(STUDY_SUBJECT_DIGEST,MEASURE_TIME,measured_value) %>% rename(flow_rate = measured_value),by = c("STUDY_SUBJECT_DIGEST","MEASURE_TIME"))

fio2_matrix %>%
  mutate(has_fio2 = !is.na(measured_value)) %>%
  group_by(has_fio2, o2_device) %>%
  tally %>%
  pivot_wider(names_from = has_fio2, values_from = n)

feasible_venturi_fio2 <- c(0.21, 0.24, 0.28, 0.31, 0.35, 0.4, 0.6)

harmonise_fio2_flow_rate_o2_device <- function(o2_device,
                                               measured_value,
                                               flow_rate){
  case_when(
    # Venturi mask
    # (a) if recorded fio2 is feasible, then use it
    o2_device == "Venturi Mask" &
      !is.na(measured_value) &
      measured_value %in% feasible_venturi_fio2 ~ measured_value,

    # (b) if recorded fio2 not feasible, and then convert flow rate
    o2_device == "Venturi Mask"  ~ fio2_app_venturi(flow_rate),

    # Except for Venturi (since only 7 feasible values), use recorded fio2
    # wherever available
    o2_device != "Venturi Mask" & !is.na(measured_value) ~ measured_value,

    # Convert various masks using
    # https://www.intensive.org/epic2/Documents/Estimation%20of%20PO2%20and%20FiO2.pdf
    is.na(measured_value) & o2_device %in% Cannula ~ fio2_app_cannula(flow_rate),
    is.na(measured_value) & o2_device %in% Mask ~ fio2_app_mask(flow_rate),
    is.na(measured_value) & o2_device %in% MaskRes ~ fio2_app_maskres(flow_rate),
    is.na(measured_value) & o2_device == "None (Room air)" ~ 0.21,

    # If not o2_device data, then assume cannula if plausible flow_rate
    # (this will not overestimate fio2)
    is.na(measured_value) & is.na(o2_device) &
      flow_rate <= 6 ~ fio2_app_cannula(flow_rate),

    # Otherwise discard, since unclear what it means
    is.na(measured_value) & is.na(o2_device) &
      flow_rate > 6 ~ NA_real_
  )
}

# o2_device = "Other (Comment)" is largely speaking valves and nebulisers, for
# which the recorded fio2 should be OK

fio2_matrix_joint <-
  fio2_matrix %>%
  mutate(fio2 =
           harmonise_fio2_flow_rate_o2_device(o2_device = o2_device,
                                              measured_value = measured_value,
                                              flow_rate = flow_rate))


fio2_matrix_joint %>%
  filter(o2_device == "Nasal cannula") %>%
  filter(fio2 > 0.44)



# SpO2_fio2
# Summarise Duplicates - Spo2 values at same measure time. Relatively small differences - average between values at same time
spo2_data_deduplicated <- spo2_data %>% group_by(STUDY_SUBJECT_DIGEST,MEASURE_TIME) %>%
  summarise(measured_value = mean(as.numeric(measured_value)))

# Join 
spo2_fio2_data <- spo2_data_deduplicated %>% select(STUDY_SUBJECT_DIGEST,MEASURE_TIME,measured_value) %>% 
  rename(spo2 = measured_value) %>% 
  left_join(fio2_matrix_joint,by = c("STUDY_SUBJECT_DIGEST","MEASURE_TIME"))

# Spo2 without match not discarded - in line 800 additional rule on missing fio2 with room-air assumption follows
spo2_data_without_match <- spo2_data_deduplicated %>%
  anti_join(fio2_matrix_joint,by = c("STUDY_SUBJECT_DIGEST","MEASURE_TIME"))

png(png_file("Effrossyni/Spo2_without_match"))
ggplot(spo2_data_without_match,aes(x = measured_value)) +
  geom_histogram(binwidth = 1, closed = "right") +
  coord_cartesian(xlim = c(85,100)) +
  scale_x_continuous(breaks = 85:100) +
  labs(x = "SpO2",
       y = "",
       title = "SpO2 without (exact-) matching FiO2")
dev.off()


spo2_fio2_data <- spo2_fio2_data %>% 
  mutate(fio2 = case_when(!is.na(fio2) ~ fio2,
                          is.na(fio2) & spo2 >= 88 ~ 0.21)) %>%
  filter(!is.na(fio2) & !is.na(spo2))

spo2_fio2_data <- spo2_fio2_data %>%
  mutate(ratio = as.numeric(spo2)/as.numeric(fio2)) %>%
  select(STUDY_SUBJECT_DIGEST,MEASURE_TIME,ratio)

png(png_file("Fsheet/spo2_fio2_ratio_joint"),width = 1200,height = 800)
ggplot(data = spo2_fio2_data,aes(x = ratio)) +
  geom_histogram(bindwidth = 10, boundary = 100) +
  labs(title = "Spo2/FiO2",xlab = "Ratio Value")
dev.off()

saveRDS(spo2_fio2_data,file = rds_file("spo2_fio2"))

# CHECK COMPLETENESS AND MEASUREMENT DISTRIBUTION

adt_mod <- adt_data %>% mutate(ICU_EVER = any(ICU))
adm_mod <- adm_data %>% mutate(OUTCOME = case_when(!is.na(DATE_OF_DEATH) ~ "DECEASED",
                                                   !is.na(HOSP_DISCH_TIME) & is.na(DATE_OF_DEATH) ~ "DISCHARGED",
                                                   is.na(HOSP_DISCH_TIME) ~ "ONGOING")) %>%
  mutate(LOS = case_when(!is.na(HOSP_DISCH_TIME) ~ difftime(HOSP_DISCH_TIME,IN_DTTM,unit = "days") %>% ceiling,
                         is.na(HOSP_DISCH_TIME) ~ difftime(pull_date,IN_DTTM,unit = "days") %>% ceiling))

spo2_fio2_data_hosp <-
  adt_data %>% 
  do(Hosp_Match(.,data = spo2_fio2_data %>% rename(DATETIME = MEASURE_TIME),default_out_date = pull_date))
  
ratio_tally <-
spo2_fio2_data_hosp %>%
  group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  mutate(MEASURE_DATE = as.Date(DATETIME)) %>%
  summarise(count = n(),days = length(unique(MEASURE_DATE))) %>%
  inner_join(adt_mod  %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% slice(1) %>% select(STUDY_SUBJECT_DIGEST,STAY,ICU_EVER) ,by = c("STUDY_SUBJECT_DIGEST","STAY")) %>%
  inner_join(adm_mod  %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% slice(1) %>% select(STUDY_SUBJECT_DIGEST,STAY,OUTCOME,LOS) %>% mutate(LOS = ceiling(LOS)),by = c("STUDY_SUBJECT_DIGEST","STAY"))

ratio_tally <- ratio_tally %>% mutate(completeness = min(1,days/as.numeric(LOS))*100)

outcome.tally <- ratio_tally %>% group_by(OUTCOME) %>%
  tally

icu.tally <- ratio_tally %>% group_by(ICU_EVER) %>%
  tally  


png(png_file("Cohort/Spo2_Fio2_Outcome_Hist"),height = 800, width = 1200)
ggplot(as.data.frame(ratio_tally),mapping = aes(x = count,fill = OUTCOME)) +
  geom_histogram() +
  facet_wrap(~OUTCOME) +
  labs(x = " ",y = "Number of Spo2/Fio2 ratios per hospital stay") +
  theme(legend.position = "none") +
  geom_label(aes(label=paste('n=', n)), x=1000, y=125, vjust=0.5, hjust=1, data=outcome.tally) 
dev.off()

png(png_file("Cohort/Spo2_Fio2_ICU_Hist"),height = 800, width = 1200)
ggplot(as.data.frame(ratio_tally),mapping = aes(x = count,fill = ICU_EVER)) +
  geom_histogram() +
  facet_wrap(~ICU_EVER) +
  labs(x = "ICU admission during hospital stay",y = "Number of Spo2/Fio2 ratios per hospital stay") +
  theme(legend.position = "none")+
  geom_label(aes(label=paste('n=', n)), x=1000, y=125, vjust=0.5, hjust=1, data=icu.tally) 
dev.off()

png(png_file("Cohort/Spo2_Fio2_ICU_Completeness"),height = 800, width = 1200)
ggplot(as.data.frame(ratio_tally),mapping = aes(x= " ",y = completeness,fill = ICU_EVER)) +
  geom_boxplot() +
  facet_wrap(~ICU_EVER) +
  labs(x = "ICU admission during hospital stay",y = "Completeness - days with Spo2/Fio2 Ration of all hospital days") +
  theme(legend.position = "none") +
  geom_label(aes(label=paste('n=', n)), x=1, y=10, vjust=0.5, hjust=1, data=icu.tally) 
dev.off()

png(png_file("Cohort/Spo2_Fio2_OUTCOME_Completeness"),height = 800, width = 1200)
ggplot(ratio_tally,mapping = aes(x= " ",y = completeness,fill = OUTCOME)) +
  geom_boxplot() +
  facet_wrap(~OUTCOME) +
  labs(x = "ICU admission during hospital stay",y = "Completeness - days with Spo2/Fio2 Ration of all hospital days") +
  theme(legend.position = "none") +
  geom_label(aes(label=paste('n=', n)), x=1, y=10, vjust=0.5, hjust=1, data=outcome.tally) 
dev.off()




# PaO2 FIO2 RATIO
bloodgas_data_arterial <- readRDS(rds_file("bloodgas_data_arterial"))

make_o2_fio2_ratio <- function(bg,fio2,buffer = 1){
  ID = bg %>% pull(STUDY_SUBJECT_DIGEST) %>% unique
  fio2_rel <- fio2 %>% filter(STUDY_SUBJECT_DIGEST == ID) %>% filter(!is.na(measured_value))

  find_closest <- function(x,pull.which = 1){
    ret_val <- fio2_rel %>%
      mutate(TimeDiff = difftime(x,MEASURE_TIME,unit = "hours")) %>%
      mutate(MEANDATE = MEASURE_TIME + TimeDiff/2) %>%
      mutate(TimeDiff = abs(as.numeric(TimeDiff))) %>%
      filter(TimeDiff <= buffer) %>%
      arrange(TimeDiff) %>%
      slice(1) %>%
      pull(measured_value)
      # select(measured_value,MEANDATE,MEASURETIME) %>%
      # pull(pull.which)
      #
    if(length(ret_val) == 0){return(NA)
    }else{return(ret_val)}
  }

  bg_mod = bg %>% group_by(ResultDate) %>% mutate(fio2 = find_closest(ResultDate)/100)
  # %>%
  #   mutate(MEANDATE = find_closest(ResultDate,pull.which = 2)) %>%
  #   mutate(FIO2_DATE = find_closest(ResultDate,pull.which = 3))

  bg_mod = bg_mod %>% mutate(pf_ratio = arterial_po2_temp*7.5/fio2)

  return(bg_mod)
}

fio2_po2_ratio_data <-
bloodgas_data_arterial %>% group_by(STUDY_SUBJECT_DIGEST) %>%
  do(make_o2_fio2_ratio(.,fio2 = fio2_data))

saveRDS(fio2_po2_ratio_data,rds_file("bloodgas_data_arterial_pfratio"))

pdf(pdf_file("Cohort/PFratio"),height = 800,width = 1200)
fio2_po2_ratio_data$pf_ratio %>% hist(breaks = 100,main = "PF Ratio - PaO2*7.5/Fio2")
dev.off()


# Weight
fsheet_raw %>%
  filter(str_detect(disp_name, coll("weight", ignore_case = TRUE))) %>%
  group_by(disp_name) %>%
  tally

weight_data <- fsheet_raw %>% filter(disp_name == "Weight") %>%
  mutate(measured_value = as.numeric(measured_value)*28.35/1000)

weight_data_ids <- weight_data %>%
  pull(STUDY_SUBJECT_DIGEST) %>%
  unique

# Height
fsheet_raw %>%
  filter(str_detect(disp_name, coll("Height", ignore_case = TRUE))) %>%
  group_by(disp_name) %>%
  tally

height_data <- fsheet_raw %>% filter(disp_name == "Height") %>%
  mutate(measured_value = as.numeric(measured_value)*2.54/100) %>%
  filter(measured_value >= 1)

png(png_file("Cohort/Height"),height = 800,width = 1200)
height_data$measured_value %>% hist(breaks = 50,main = "Height (m)")
dev.off()

saveRDS(height_data,rds_file("height_raw"))

# BMI
bmi_data <- fsheet_raw %>% filter(disp_name == "BMI (Calculated)") %>%
  mutate(measured_value = as.numeric(measured_value))

saveRDS(bmi_data,rds_file("bmi"))

png(png_file("Cohort/BMI"),height = 800,width = 1200)
bmi_data$measured_value %>% hist(breaks = 50,main = "BMI")
dev.off()



# Check if we need to include entered patient weight
# This looks like strange hourly data, so only include if neccesary
weight_entered_patients <- fsheet_raw %>%
  filter(disp_name == "Entered Patient Weight")

weight_entered_patients %>%
  pull(measured_value) %>%
  as.numeric %>%
  summary

pdf(pdf_file("Fsheet/weight_entered_patients_hist"))
ggplot(weight_entered_patients, aes(x = as.numeric(measured_value))) +
  geom_histogram(boundary = 0, binwidth = 1)
graphics.off()

patients_with_entered_weight_but_not_standard_weight <-
  weight_entered_patients %>%
  filter(!STUDY_SUBJECT_DIGEST %in% weight_data_ids) %>%
  group_by(STUDY_SUBJECT_DIGEST)

if (nrow(patients_with_entered_weight_but_not_standard_weight) > 0){
  cat("******* warning: possible adjustment to weight data needed ****")
}

png(png_file("Cohort/Weight"),width = 1200,height = 800)
weight_data$measured_value %>% hist(main = "Weight in KG")
dev.off()

pdf(pdf_file("Cohort/Weight_by_person"),
    height = 20/cm(1),
    width = 20/cm(1))
ggplot(weight_data,
       aes(x = MEASURE_TIME, y = measured_value, group = STUDY_SUBJECT_DIGEST)) +
  geom_point() +
  geom_line()
graphics.off()

saveRDS(weight_data,rds_file("weight"))

bmi_manual <- function(ID,Time,Weight){
  height <- height_data %>% filter(STUDY_SUBJECT_DIGEST == unique(ID)) %>%
    # arrange(abs(difftime(MEASURE_TIME,Time,unit = "days"))) %>%
    slice(1) %>%
    pull(measured_value)
  BMI <- Weight/height^2
  
  if(length(height) == 0) return(NA)
  else return(BMI)
}

bmi_manual_data <-
weight_data %>% group_by(STUDY_SUBJECT_DIGEST) %>% mutate(measured_value = bmi_manual(ID = STUDY_SUBJECT_DIGEST,
                                                                                    Time = MEASURE_TIME,
                                                                                    Weight = measured_value)) 

bmi_combined <- bind_rows(bmi_manual_data,bmi_data)

saveRDS(bmi_combined,rds_file("bmi_combined"))

# BMI CLASSES
bmi_classes <- fsheet_raw %>% filter(disp_name == "BMI ASSESSMENT") 
bmi_classes$measured_value %>% table

bmi_classes_from_calculated <- bmi_combined %>% mutate(measured_value = case_when(measured_value <= 18.5 ~ "Underweight",
                                                                                  measured_value > 18.5 & measured_value < 25 ~ "Normal Range",
                                                                                  measured_value >= 25 & measured_value < 30 ~ "Overweight",
                                                                                  measured_value >= 30 & measured_value < 40 ~ "Obese",
                                                                                  measured_value >= 40 ~ "Morbidly Obese"))

bmi_classes_combined <-bind_rows(bmi_classes_from_calculated,bmi_classes)
bmi_classes_combined$measured_value %>% table
saveRDS(bmi_classes_combined,rds_file("bmi_classes"))

# RRT
########

fsheet_raw %>% group_by(disp_name) %>% tally

rrt <- fsheet_raw %>%
  filter(disp_name == "Renal replacement therapy?")

saveRDS(rrt,rds_file("rrt"))

# NEWS2
#######

news2 <- fsheet_raw %>%
  filter(disp_name == "NEWS2 score") %>%
  filter(measured_value != " ") %>%
  mutate(measured_value = case_when(measured_value == "3mm" ~ "3",

                                    # Clearly a typo
                                    measured_value == "E" ~ NA_character_,
                                    TRUE ~ measured_value))

saveRDS(news2, rds_file("news2"))

# Clinical Frailty Scale
#######

cfs_data <- fsheet_raw %>%
  filter(disp_name == "Clinical Frailty Scale* ")

saveRDS(cfs_data, rds_file("cfs"))

# SPO2 Heart Rate
#######

hr_data <- fsheet_raw %>%
  filter(disp_name %in% c("SPO2 Heart Rate", "Pulse"))

saveRDS(hr_data, rds_file("hr_data"))


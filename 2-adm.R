adm_raw <- readRDS(rds_file("adm_raw"))
# make sure to run adt first
adt_data <- readRDS(rds_file("adt"))

adm_raw <- adm_raw %>% group_by(STUDY_SUBJECT_DIGEST) %>% 
  arrange(IN_DTTM) %>% 
  mutate(STAY = 1:n()) %>%
  select(STUDY_SUBJECT_DIGEST,STAY,everything())

adm_raw <-
adm_raw %>% inner_join(adt_data %>% 
                         group_by(STUDY_SUBJECT_DIGEST,STAY) %>% 
                         slice(1) %>% 
                         select(STUDY_SUBJECT_DIGEST,STAY),by = c("STUDY_SUBJECT_DIGEST","STAY"))

pdf(pdf_file("Cohort/age-admission"))
adm_raw %>%
  .$AGE_AT_ADM %>%
  hist(main = "Age at Admission",breaks = 30)
graphics.off()

# Add status - DECEASED,ACTIVE,DISCHARGED
adm_raw <-
adm_raw %>% mutate(STATUS = ifelse(!is.na(DATE_OF_DEATH),
                                   "DECEASED",
                                   ifelse(is.na(HOSP_DISCH_TIME),
                                          "ACTIVE",
                                          "DISCHARGED")))

# Add Los

adm_raw <-
adm_raw %>% mutate(LOS = ifelse(is.na(HOSP_DISCH_TIME),
                                NA,
                                difftime(HOSP_DISCH_TIME,IN_DTTM,unit = "d")))

adm_fatalities <-
adm_raw %>% filter(!is.na(DATE_OF_DEATH)) %>%
  mutate(DATE_OF_DEATH = as.Date(DATE_OF_DEATH))

png(png_file(paste("Cohort\\Fatalities_",Sys.Date(),sep="")),height = 800,width = 1200)
ggplot(data = adm_fatalities,aes(x = DATE_OF_DEATH)) +
  geom_histogram(binwidth =  0.5) +
  ylab("Frequency") +
  xlab("Date of death") +
  scale_x_date(date_breaks = "days" ,
               date_labels = "%d-%m") +
  theme(axis.text.x = element_text(angle = 90))
dev.off()

# Fatalities summary (uo to )
adm_fatalities$AGE_AT_ADM %>% summary
fatalities <- adm_fatalities$STUDY_SUBJECT_DIGEST
adm_fatalities$GENDER_DESC %>% table
difftime(adm_fatalities$HOSP_DISCH_TIME,adm_fatalities$IN_DTTM,unit = "days") %>% as.numeric %>% summary

# All Admissions Gender Split
png(png_file("Cohort\\Gender.png"),height = 800,width = 1200)
adm_raw %>% pull(GENDER_DESC) %>% table %>% barplot
dev.off()

# Save modified file
saveRDS(object = adm_raw,file = rds_file("adm"))



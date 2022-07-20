filename <- "frailty_mod.csv"
frailty_raw <-
  read.epic.csv(filename,
                colClasses = c("STUDY_SUBJECT_DIGEST" = "character",	
                               "Date of Admission (1st or Index Episode)"	= "character",
                               "Positive swab" = "character",
                               "Absolute number of co-morbid conditions listed in admission" = "character",
                               "Absolute number of regular medications prescribed (excluding PRN" = "character",
                               "Known Dementia"= "character",
                               "Admission from Care Home"= "character",
                               "Mobility status on admission" = "character",
                               "Clinical Frailty Scale done"= "numeric",
                               "Clinical Frailty Scale score"= "numeric",
                               "CXR on admission" = "character",
                               "Fever" = "character",
                               "Cough" = "character",
                               "Shortness of Breath" = "character",
                               "Fatigue or Lethargy"= "character",
                               "Myalgia"= "character",
                               "New or worsening Confusion"= "character",
                               "Fall" = "character",
                               "Nausea or vomiting"= "character",
                               "Diarrhoea"= "character",
                               "Abdominal pain" = "character",
                               "Loss of sense of smell" = "character",
                               "Loss of sense of taste" = "character",
                               "Non-specifically unwell"= "character",
                               "Other (free text)"= "character",
                               "4AT score on admission"= "character",
                               "New/ Worsening Confusion noted during admisison episode"= "character",
                               "Delirium diagnosed during admission episode"= "character",
                               "Mobility Status on Discharge"= "character",
                               "Collector"= "character",
                               "Hosp Acquired Covid"= "character",
                               "F33"= "character"))
frailty_raw %>% summary

saveRDS(frailty_raw, file = rds_file("frailty_raw"))



                                                   
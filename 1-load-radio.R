radio_raw <- read.epic.csv(radio_file,
                           colClasses = c(STUDY_SUBJECT_DIGEST = "character",
                                          Proc_Date = "iso8601space",
                                          Proc_Code = "character",
                                          Proc_Name = "character",
                                          Proc_Narrative = "character",
                                          Proc_Impression = "character",
                                          Proc_Addenda = "character",
                                          Proc_Assessment = "character"))


radio_raw %>% summary

radio_raw %>%
  group_by(Proc_Name) %>%
  tally %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# echo
radio_raw %>%
  filter(str_detect(Proc_Name, coll("Echo", ignore_case = TRUE))) %>%
  pull(Proc_Impression)

saveRDS(radio_raw, file = rds_file("radio_raw"))


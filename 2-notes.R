notes = readRDS(rds_file("notes_raw"))


# ECMO
ECMO_notes = notes_raw %>% filter(grepl("ECMO ",NOTE_TEXT,ignore.case = T)) %>%
  filter(NOTE_TYPE_NAME == "Disch Summ")

write.csv(ECMO_notes,file = csv_out_file("ECMO_notes"))

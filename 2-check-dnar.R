dnar <- readRDS(rds_file("dnar_raw")) %>%
  filter(PROC_START_TIME <= pull_date)
notes <- readRDS(rds_file("notes_raw")) %>%
  filter(CONTACT_DATE <= pull_date)

triggers_ind <- c("end of life",
              "end-of-life",
              "supportive care",
              "EOL ",
              "anticipatory med",
              "palliative",
              "comfort care",
              "end of his life",
              "end of her life",
              "terminal wean")

triggers <- c("end of life|end-of-life|supportive care|EOL |anticipatory med|palliative|comfort care|end of his life|end of her life|terminal wean")

return_trigger <- function(x){
    paste(triggers_ind[sapply(X = triggers_ind,FUN = function(y)grepl(y,x,ignore.case = T))],sep = ",",collapse = ", ") %>%
    return
}

notes_dnar_no_palliatives <-
notes %>% filter(STUDY_SUBJECT_DIGEST %in% (dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(all(DESCRIPTION != "SYMPTOM CONTROL,DNACPR")) %>% pull(STUDY_SUBJECT_DIGEST))) %>% 
  group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(CONTACT_DATE) %>%
  filter(grepl(triggers,NOTE_TEXT,ignore.case = T)) %>%
  ungroup %>%
  rowwise %>%
  mutate(TRIGGERS = return_trigger(NOTE_TEXT))

notes_dnar <-
  notes %>% filter(STUDY_SUBJECT_DIGEST %in% dnar$STUDY_SUBJECT_DIGEST) %>% 
  group_by(STUDY_SUBJECT_DIGEST) %>%
  arrange(CONTACT_DATE) %>%
  filter(grepl(triggers,NOTE_TEXT,ignore.case = T)) %>%
  ungroup %>%
  rowwise %>%
  mutate(TRIGGERS = return_trigger(NOTE_TEXT))


write.csv(x = notes_dnar_no_palliatives,file = csv_out_file("DNAR_but_never_palliative_notes"))


# Venn

manual_dnar <- read.csv(csv_out_file("DNAR_palliative_notes_annotated"))
manual_dnar <- as_tibble(manual_dnar) %>% select(STUDY_SUBJECT_DIGEST,CONTACT_DATE,NOTE_TEXT,NOTE_TYPE_NAME,TRIGGERS,End_of_life)

dnar_palliative <- dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(DESCRIPTION == "SYMPTOM CONTROL,DNACPR") %>% distinct() %>% arrange(PROC_START_TIME)
manual_palliative <- manual_dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(End_of_life == "yes") %>% arrange(CONTACT_DATE)

a = (dnar_palliative %>% filter(PROC_START_TIME <= pull_date) %>% slice(1))$STUDY_SUBJECT_DIGEST
b = (manual_palliative %>% filter(as.Date(CONTACT_DATE) <= as.Date(pull_date)) %>% slice(1))$STUDY_SUBJECT_DIGEST %>% as.character

VennDiagram::draw.pairwise.venn(area1 = length(a),area2 = length(b),cross.area = length(intersect(a,b)),fill = c("red","blue"),
                                category = list("DNAR palliative","Manual palliative"))



##
dnar_not_in_manul <- dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(DESCRIPTION == "SYMPTOM CONTROL,DNACPR") %>% filter(!(STUDY_SUBJECT_DIGEST %in% manual_palliative$STUDY_SUBJECT_DIGEST))
dnar_not_in_manul_notes <- notes_dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(!(STUDY_SUBJECT_DIGEST %in% manual_palliative$STUDY_SUBJECT_DIGEST)) %>% arrange(STUDY_SUBJECT_DIGEST,CONTACT_DATE)
write.csv(x = dnar_not_in_manul,file = csv_out_file("Palliative_in_DNAR_not_in_manual"))

manual_not_in_dnar <- manual_dnar %>% group_by(STUDY_SUBJECT_DIGEST) %>% filter(End_of_life == "yes") %>% filter(!(STUDY_SUBJECT_DIGEST %in% dnar_palliative$STUDY_SUBJECT_DIGEST))
write.csv(x = manual_not_in_dnar,file = csv_out_file("Palliative_in_manual_not_in_DNAR"))


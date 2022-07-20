tests_raw <- readRDS(rds_file("tests_raw"))

adt_data <- readRDS(rds_file("adt"))
adm_data <- readRDS(rds_file("adm"))

#  -------------- Look for COVID tests
possible_covid_tests <- tests_raw %>%
  filter(str_detect(TestName, coll("COV", ignore_case = TRUE))) %>%
  group_by(TestName) %>%
  tally

if (nrow(possible_covid_tests) > 13){
  cat("********** possible new covid tests **********")
}

# Treat POC and RNA test as interchangeable for now
covid_raw <- tests_raw %>%
  filter(TestName %in% c("2019-NCOV RNA",
                         "2019 NCOV RNA, POC",
                         "2019 NCOV PCR POC"))

# Missing Collection Date
print("COVID test fraction without collection date")
covid_raw$COLLECTED_DATETIME %>%is.na %>% mean %>% print
print("COVID test fraction without order date")
covid_raw$ORDERED_DATETIME %>%is.na %>% mean %>% print
print("COVID test fraction without collection & order date")
covid_raw$ResultDate %>%is.na %>% mean %>% print
print("COVID test fraction without collection date")
covid_raw %>% filter(is.na(COLLECTED_DATETIME) & is.na(ORDERED_DATETIME)) %>% nrow/nrow(covid_raw) %>% print
  
# Instances of COLLECTED time missing, ordered date time should be close though
covid_raw <- covid_raw %>%
  mutate(DATETIME =
           case_when(!is.na(COLLECTED_DATETIME) ~ COLLECTED_DATETIME,
                     !is.na(ORDERED_DATETIME) ~ ORDERED_DATETIME,
                     !is.na(ResultDate) ~ ResultDate))

saveRDS(covid_raw, file = rds_file("covid_raw"))

covid_pos <- covid_raw %>%
  filter((ResultValue == "Detected" & TestName == "2019-NCOV RNA") |
           (ResultValue == "POSITIVE" & TestName == "2019 NCOV RNA, POC") |
           (ResultValue == "Positive" & TestName == "2019 NCOV PCR POC"))



saveRDS(covid_pos, file = rds_file("covid_pos"))

covid_pos_hosp <-
adt_data %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
  do(Hosp_Match(adt_data_id = .,data = covid_pos,buffer.out = hours(24),buffer.in = hours(24))) 

write.csv(x = covid_pos_hosp,file = csv_out_file("Positive_Tests_DURING_stay"))
write.csv(x = adm_data %>% anti_join(covid_pos_hosp,by = c("STUDY_SUBJECT_DIGEST","STAY")),file = csv_out_file("IDs_No_Positive_Test_DURING_stay"))
write.csv(x = adm_data %>% anti_join(covid_pos,by = c("STUDY_SUBJECT_DIGEST")),file = csv_out_file("IDs_never_positive_test"))

firsts_no_test <- adm_data %>% anti_join(covid_pos_hosp,by = c("STUDY_SUBJECT_DIGEST","STAY")) %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>% filter(STAY == 1)

png(png_file("Time_difference_COVID_to_admission.png"),height = 500,width = 700)
firsts_no_test %>% left_join(covid_pos%>% distinct(),by = "STUDY_SUBJECT_DIGEST") %>% group_by(STUDY_SUBJECT_DIGEST) %>% arrange(DATETIME) %>% select(STUDY_SUBJECT_DIGEST,STAY,IN_DTTM,HOSP_DISCH_TIME,DATETIME) %>% 
  mutate(TIMEDIFF = difftime(DATETIME,IN_DTTM,unit = "days")) %>% pull(TIMEDIFF) %>% as.numeric %>% hist(main = "Time difference (d) between test and first admission \n for admissions without positive test during stay \n (2 positive times are afterdischarge)")
dev.off()


covid_first <-
adt_data %>% group_by(STUDY_SUBJECT_DIGEST,STAY) %>%
do(Hosp_Match(adt_data_id = .,data = covid_pos,buffer.in = hours(72))) %>%
  arrange(DATETIME) %>%
  slice(1) %>%
  select(STUDY_SUBJECT_DIGEST,STAY,STATUS,DATETIME)


# Save file
saveRDS(covid_first, file = rds_file("covid_first"))




#  -------------- AUTOMATED VANILLA TESTS

# Search Term csv storage
# SearchTermFile <- csv_out_file(paste0("SearchTermsTest_",Sys.Date()))
# file.create(SearchTermFile)
# 
save.vals <- T      # Save results as RDS
plot.vals <- T      # Plot results as png in \\Plots\\
print.terms <- T    # Print the search terms in csv
omit.na.times <- 3  # 0 - Do nothing
                    # 1 - Omit tests with missing collection time
                    # 2 - If colletion time is missing, use ordered date, omit if still NA
                    # 3 - If collection time is missing, use ordered date, if OD is missing, use ResultDate, omit if still missing



# -----------  MAGNESIUM


tests_pull(
  tests_raw,
  symbol = "magnesium",
  title = "Magnesium",
  names_cuh = c("MAGNESIUM"),
  names_external = "EXT MAGNESIUM",
  search_pattern = c("magnesium", "mg"),
  search_exclude = c("ETOH MG/100ML",
                     "RETIRED-ETOH MG/100ML",
                     "RIST 0.2MG/ML",
                     "RIST 0.77MG/ML",
                     "RISTOCETIN 0.5MG/ML",
                     "RISTOCETIN 1.5MG/ML"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.29" ~ "left", # very low
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.29" ~ 0.29,
                            TRUE ~ value_numeric),
  expect = (unit == "mmol/L"),
  range_mainly_low = 0.4,
  range_mainly_high = 1.2,
  range_discard_below = 0.32,
  range_discard_above = 1.8)


# -----------  PHOSPHATE

tests_pull(
  tests_raw,
  symbol = "phosphate",
  title = "Phosphate",
  names_cuh = c("PHOSPHATE"),
  search_pattern = c("phosphate", "po4", "po 4"),
  search_exclude = c("EXT I PHOSPHATE", "EXT PHOSPHATE"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "mmol/L"),
  range_mainly_low = 0.4,
  range_mainly_high = 2.5,
  range_discard_below = 0.2)

# -----------  ALKALINE PHOSPHATASE

tests_pull(
  tests_raw,
  symbol = "alkaline_phosphatase",
  title = "Alkaline Phosphatase",
  names_cuh = c("POC ED ALP (ALKALINE PHOSPHATASE)", "ALKALINE PHOSPHATASE"),
  names_external = "EXT ALK PHOS",
  search_pattern = c("PHOSPHATASE", "ALP", "ALKP", "ALPase", "Alk Phos"),
  search_exclude = c("ALPHA-1 ANTITRYPSIN PHENOTYPE",
                     "ALPHA-2-ANTIPLASMIN",
                     "ALPHA-GLUCOSIDASE (REFERENCE ENZYME)",
                     "CD3 TCR ALPHA/BETA",
                     "FAECAL CALPROTECTIN",
                     "MI-2 ALPHA ANTIBODY",
                     "TNF ALPHA",
                     "VALPROATE",
                     "BLOOD SPOT ALPHA-GALACTOSIDASE",
                     "BLOOD SPOT ALPHA-GALACTOSIDASE (FEMALE)"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "U/L"),
  range_mainly_low = 40,
  range_mainly_high = 1000)

# -----------  BASOPHIL

tests_pull(
  tests_raw,
  symbol = "basophil",
  title = "Basophil",
  names_cuh = c("BASOPHIL COUNT POC", "BA#", "BA# (DIFF)"),
  names_external = "EXT BASOPHILS",
  search_pattern = c("basophil", "ba#"),
  search_exclude = c("BASOPHIL PERCENTAGE POC", "BASOPHILIC STIPPLING"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 1)

# -----------  LACTATE

# Is blood gas
# Venous and arterial blood gas are "close enough", so don't worry
tests_pull(
  tests_raw,
  symbol = "lactate",
  title = "Lactate",
  names_cuh = c("POC LACTATE BG", "PLASMALACTATE"),
  search_pattern = "lactate",
  search_exclude = NA,
  silently_exclude_na_when = (name == "POC LACTATE BG"),
  censoring_fn = case_when(value == "<" ~ "left",
                           value == ">" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(# Use minimum recorded value within test name
    # since this case_when is run group_by
    value == "<" ~ min(value_numeric, na.rm = TRUE),
    value == ">" ~ max(value_numeric, na.rm = TRUE),
    TRUE ~ value_numeric),
  expect = ((name == "PLASMALACTATE" & unit == "mmol/L") |
              (name == "POC LACTATE BG" & unit %in% c("mmol/L","mmol/l"))),
  range_mainly_low = 0,
  range_mainly_high = 25)

# -----------  CRP

tests_pull(
  tests_raw,
  symbol = "crp",
  title = "C-Reactive Protein",
  names_cuh = c("CRP", "POC ED CRP (C-REACTIVE PROTEIN)", "POC ED CRP DILUTION"),
  names_external = "EXT CRP",
  search_pattern = c("crp", "reactive"),
  search_exclude = "REACTIVE LYMPHOCYTES",
  silently_exclude_na_when = (name == "POC ED CRP (C-REACTIVE PROTEIN)"),
  censoring_fn = case_when(value == "<1" ~ "left",
                           value == "<4" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<1" ~ 1,
                            value == "<4" ~ 4,
                            TRUE ~ value_numeric),
  expect = (unit == "mg/L"),
  range_mainly_low = 1,
  range_mainly_high = 700)

# -----------  PCT

# TestName == "PCT" with TestGroupName == "FULL BLOOD COUNT" looks to be
# Plateletcrit rather than Procalcitonin
tests_pull(
  tests_raw,
  symbol = "pct",
  title = "Procalcitonin",
  names_cuh = c("PROCALCITONIN"),
  search_pattern = c("pct", "procalcitonin"),
  search_exclude = NA,
  search_exclude_group = "FULL BLOOD COUNT",
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.01" ~ "left",
                           value == "<0.05" ~ "left",
                           value == "<0.06" ~ "left",
                           value == ">75.00" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.01" ~ 0.01,
                            value == "<0.05" ~ 0.05,
                            value == "<0.06" ~ 0.06,
                            value == ">75.00" ~ 75.00,
                            TRUE ~ value_numeric),
  expect = (unit == "ng/ml"),
  range_mainly_low = 0,
  range_mainly_high = 30)

# -----------  WCC

# NOTE ON INCLUSION
# 1. TestName == "WBC, POC" - Emegency Department have a POC test for WCC
#
# TO REVIEW
# 1. TestName == "WBC WHOLE BLOOD" - not clear what this is. Don't include for
#    now, but should review occasionally
# 2. TestName == "HODS WHITE CELL COUNT" - doesn't look like it is a standard
#    WCC test, and has no reference range, so exclude for now
#
# NOTES ON EXCLUSIONS
# 1. "WBC COMMENTS (DM96)" is another test done often on the same sample - looks
#    at the sample under a microscope. This is a comment, so is relevant to
#    White Cell Count, but clearly not a numeric value that can be used
#    interchangeably with the main automated results.
# 2. "CSF WBC" and "HODS CSF WHITE COUNT" are cerebrospinal fluid, which is a
#    completely different thing
# 3. "HODS BMA WBC" = Haemato-Oncology Diagnostic Service, Bone Marrow
#    Aspiration. This is not a blood test, so should not be included.
# 4. "STERILE FLUID WHITE CELL COUNT", "URINE WHITE BLOOD CELLS" and
#    "URINE LEUKOCYTE, POC" are not the same sample type, so
#    completely different
tests_pull(
  tests_raw,
  symbol = "wbc",
  title = "White Cell Count",
  names_cuh = c("WBC", "WBC, POC"),
  names_external = "EXT WBC",
  search_pattern = c("wbc", "wcc", "white", "leukocyte", "leucocyte"),
  search_exclude = c("CSF WBC:RBC BOTTLE 1",
                     "CSF WBC:RBC BOTTLE 2",
                     "CSF WBC:RBC BOTTLE 3",
                     "HODS BMA WBC",
                     "WBC COMMENTS (DM96)",
                     "URINE WHITE BLOOD CELLS",
                     "STERILE FLUID WHITE CELL COUNT",
                     "HODS CSF WHITE COUNT"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 100)


# -----------  FERRITIN

tests_pull(
  tests_raw,
  symbol = "ferritin",
  title = "Ferritin",
  names_cuh = c("FERRITIN"),
  names_external = "EXT FERRITIN",
  search_pattern = c("ferritin", "ftl"),
  search_exclude = NA,
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == ">1650.0" ~ "right",
                           value == ">16500.0" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == ">1650.0" ~ 1650.0,
                            value == ">16500.0" ~ 16500.0,
                            TRUE ~ value_numeric),
  expect = (unit == "ug/L"),
  range_mainly_low = 8,
  range_mainly_high = 1000)

# -----------  ALDOSTERONE

tests_pull(
  tests_raw,
  symbol = "aldosterone",
  title = "Aldosterone",
  names_cuh = c("ALDOSTERONE"),
  search_pattern = "aldosterone",
  search_exclude = c("ALDOSTERONE RENIN RATIO",
                     "RETIRED-ALDOSTERONE RENIN RATIO"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<70" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<70" ~ 70,
                            TRUE ~ value_numeric),
  expect = (unit == "pmol/L"),
  range_mainly_low = 100,
  range_mainly_high = 500)

# -----------  CORTISOL

# REVIEW
# 1. c("CORTISOL 0", "CORTISOL 09:00", "CORTISOL 30", "CORTISOL 60") are all
# standard cortisol tests, but are done as part of a SYNACTHEN (Synthethic ACTH)
# test, which involves giving ATCH to see if a cortisol response is elicited and
# testing cortisol levels at time 0, 30 and 60. So depends on use whether these
# should be included
# 2. "CORTISOL(DEXAMETHASONE SUPP)" is a measurement after dex, which again
# reflects post-intervention levels.
#
# EXCLUDE
# 1. "SALIVARY CORTISOL" is not a blood test
tests_pull(
  tests_raw,
  symbol = "cortisol",
  title = "Cortisol",
  names_cuh = c("CORTISOL"),
  search_pattern = "cortisol",
  search_exclude = "SALIVARY CORTISOL",
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<25" ~ "left",
                           value == ">2070" ~ "right",
                           value == ">4140" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<25" ~ 25,
                            value == ">2070" ~ 2070,
                            value == ">4140" ~ 4140,
                            TRUE ~ value_numeric),
  expect = (unit == "nmol/L"),
  range_mainly_low = 10,
  range_mainly_high = 1000)


# -----------  NEUTROPHILS

# EXCLDUE
# 1. "NEUTROPHIL PERCENTAGE POC" - percentage that is neutrophils out of
#    neutrophil, lyphocytes etc. So not the same test
# 2. "VACUOLATED NEUTROPHILS", "NEUTROPHIL LEFT SHIFT" and
#    "HYPERSEGMENTED NEUTROPHILS" are qualitative results from blood films,
#    not quantitative results
tests_pull(
  tests_raw,
  symbol = "neutrophils",
  title = "Neutrophils",
  names_cuh = c("NE#", "NE# (DIFF)", "NEUTROPHIL COUNT POC"),
  names_external = "EXT NEUTROPHILS ",
  search_pattern = c("neutrophil", "ne#", "neutrocytes", "heterophils"),
  search_exclude = c("NEUTROPHIL PERCENTAGE POC",
                     "VACUOLATED NEUTROPHILS",
                     "NEUTROPHIL LEFT SHIFT",
                     "HYPERSEGMENTED NEUTROPHILS"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 50)

# -----------  LYMPHOCYTES

# EXCLUDE
# 1. Cerebrospinal fluid are a completely different thing
# 2. "HODS LYMPHOCYTE MORPHOLOGY" and "REACTIVE LYMPHOCYTES" are qualitative
#    results
tests_pull(
  tests_raw,
  title = "Lymphocytes",
  symbol = "lymphocytes",
  names_cuh = c("LY#", "LY# (DIFF)", "LYMPHOCYTE COUNT", "LYMPH COUNT, POC"),
  names_external = "EXT LYMPHOCYTES",
  search_pattern = c("lymphocyte", "ly#"),
  search_exclude = c("CSF LYMPHOCYTES BOTTLE 1",
                     "CSF LYMPHOCYTES BOTTLE 2",
                     "CSF LYMPHOCYTES BOTTLE 3",
                     "HODS LYMPHOCYTE MORPHOLOGY",
                     "REACTIVE LYMPHOCYTES"),
  silently_exclude_na_when = FALSE,
  expect = (unit %in% c("10*9/L", "x 10*9/l")),
  range_mainly_low = 0,
  range_mainly_high = 100)

# -----------  EOSINOPHILS

tests_pull(
  tests_raw,
  symbol = "eosinophils",
  title = "Eosinophils",
  names_cuh = c("EO#", "EO# (DIFF)", "EOSINOPHIL COUNT POC"),
  names_external = "EXT EOSINOPHILS",
  search_pattern = c("eosinoph", "eosinophiles", "acidophils", "eo#"),
  search_exclude = "EOSINOPHIL PERCENTAGE POC",
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 5)

# -----------  MONOCYTES

tests_pull(
  tests_raw,
  symbol = "monocytes",
  title = "Monocytes",
  names_cuh = c("MO#", "MO# (DIFF)", "MONOCYTE COUNT POC"),
  names_external = "EXT MONOCYTES",
  search_pattern = c("monocyte", "mo#"),
  search_exclude = "MONOCYTE PERCENTAGE POC",
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 3)

# -----------  PLATELETS

# REVIEW
# 1. "THROMBOEXACT PLT" may be the same? Not sure
# 2. "PLATELET COUNT, WHOLE BLOOD" unsure
# 3. "PLATELET COUNT, PRP" probably not the same
# 4. "HODS PLT (IMPEDENCE)", "HODS PLT (IMMUNO)" not sure
#
# INCLUDE
# 1. Note "CITRATED PLATELET COUNT" is used when haematology spot "clumped
#    platelets". The result on the FBC may be unreliable in this case, and this
#    results should be used instead. TODO spot these cases
#
# EXCLUDE
# 1. "HODS BMA PLTS" is Bone Marrow Aspiration, so not a blood test
# 2. "PLT COMMENTS (DM96)", "LARGE PLATELETS", "PLATELET REPORT" are
#     qualitative results, not quantitative
# 3. "EXT MEAN PLT VOLUME" is not platelet count, so not the same
tests_pull(
  tests_raw,
  symbol = "platelets",
  title = "Platelets",
  names_cuh = c("PLT", "PLT, POC", "CITRATED PLATELET COUNT"),
  names_external = "EXT PLT",
  search_pattern = c("platelet", "plt", "thrombocytes"),
  search_exclude = c("HODS BMA PLTS",
                     "PLT COMMENTS (DM96)",
                     "EXT MEAN PLT VOLUME",
                     "LARGE PLATELETS",
                     "PLATELET REPORT"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "10*9/L"),
  range_mainly_low = 0,
  range_mainly_high = 1000)

# -----------  INTERLEUKIN-6 aka IL-6

tests_pull(
  tests_raw,
  symbol = "il6",
  title = "Interleukin 6",
  names_cuh = c("INTERLEUKIN-6"),
  search_pattern = c("INTERLEUKIN", "IL-6", "IL6"),
  search_exclude = "ANTI IL-6 ANTIBODY",
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.2100" ~ "left",
                           value == "<2.7" ~ "left",
                           value == ">5500.0" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.2100" ~ 0.21,
                            value == "<2.7" ~ 2.7,
                            value == ">5500.0" ~ 5500,
                            TRUE ~ value_numeric),
  expect = (unit %in% c("pg/ml", "pg/mL")),
  range_mainly_low = 0,
  range_mainly_high = 1000)

# Manually limit to BIO RAD LUMINEX method
il6 = readRDS(rds_file("il6"))
il6 = il6 %>% filter(Method == "BIO RAD LUMINEX")

saveRDS(il6,rds_file("il6"))

# -----------  D-DIMER

# REVIEW
# 1. "POC D-DIMER" has a different range, and has different values. Would need
#    work to integrate these. Not for now.
tests_pull(
  tests_raw,
  symbol = "ddimer",
  title = "D-Dimer",
  names_cuh = c("D-DIMER"),
  search_pattern = c("dimer", "fdp"),
  search_exclude = NA,
  silently_exclude_na_when = FALSE,
  expect = (unit == "ng/mL"),
  range_mainly_low = 25,
  range_mainly_high = 5000)

# -----------  APTT


# REVIEW
# 1. "LUPUS INSENSITIVE APTT" - not certain if this is comparable
tests_pull(
  tests_raw,
  symbol = "aptt",
  title = "APTT",
  names_cuh = c("APTT"),
  names_external = "EXT APTT",
  search_pattern = c("aptt", "partial thrombo", "kcct", "kaolin"),
  search_exclude = c("APTT RATIO", "LUPUS ANTICOAGULANT APTT RATIO"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == ">400.0" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == ">400.0" ~ 400,
                            TRUE ~ value_numeric),
  expect = (unit %in% c("sec", "Seconds")),
  range_mainly_low = 10,
  range_mainly_high = 100,
  range_discard_above = 300)
# -----------  LDH

tests_pull(
  tests_raw,
  symbol = "ldh",
  title = "Lactate Dehydrogenase",
  names_cuh = c("LD"),
  search_pattern = c("LD", "lactate", "dehydrogenase", "ldh"),
  search_exclude = c("ALDOSTERONE",
                     "ALDOSTERONE RENIN RATIO",
                     "EXT LDL",
                     "FLUID LD",
                     "LDL CHOLO",
                     "PLASMALACTATE",
                     "POC LACTATE BG",
                     "RETIRED-ALDOSTERONE RENIN RATIO"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<20" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<20" ~ 20,
                            TRUE ~ value_numeric),
  expect = (unit == "U/L"),
  range_mainly_low = 0,
  range_mainly_high = 1000)


# -----------  PT

# REVIEW
# 1. "POC ED PROTHROMBIN TIME S" - not sure, could be the same but no reference
#    range
# 2. "NORMAL PLASMA PT" and "PATIENT : NORMAL PLASMA PT" - probably the same as
#    PT, but not sure. No reference range.
tests_pull(
  tests_raw,
  symbol = "pt",
  title = "PT",
  names_cuh = c("PT"),
  search_pattern = c("pt", "prothro"),
  names_external = "EXT PT ",
  search_exclude = c("6-METHYLMERCAPTOPURINE NUCLEOTIDES",
                     "AB DEFINED UNACCEPTABLES",
                     "ACETYL CHOLINE RECEPTOR ANTIBODY",
                     "AMPA 1 RECEPTOR AB",
                     "AMPA 2 RECEPTOR AB",
                     "ANTI-STREPTOLYSIN O",
                     "APTT",
                     "APTT RATIO",
                     "C-PEPTIDE",
                     "CRYPTOCOCCAL ANTIGEN REF",
                     "CRYPTOSPORIDIUM STAIN",
                     "CYCLIC CITRULLINATD PEPTIDE (CCP) AB",
                     "ELIZABETHKINGIA MENINGOSEPTICA #1 CT",
                     "ELIZABETHKINGIA MENINGOSEPTICA DNA",
                     "ELLIPTOCYTES",
                     "EXT APTT",
                     "GABA B RECEPTOR AB",
                     "GLYCINE RECEPTOR ANTIBODY",
                     "HAPTOGLOBIN",
                     "HODS PTPN11 MUTATION ANALYSIS",
                     "LUPUS ANTICOAGULANT APTT RATIO",
                     "LUPUS INSENSITIVE APTT",
                     "MAST CELL TRYPTASE",
                     "NMDA RECEPTOR ANTIBODY FIXED CELL ASSAY",
                     "OTHER UNACCEPTABLES",
                     "POC APPT",
                     "STREPTOCOCCUS #1 CT",
                     "STREPTOCOCCUS #2 CT",
                     "STREPTOCOCCUS DNA",
                     "STREPTOCOCCUS PNEUMONIAE #2 CT",
                     "STREPTOCOCCUS PNEUMONIAE CT",
                     "STREPTOCOCCUS PNEUMONIAE DNA",
                     "STREPTOCOCCUS PYOGENES"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == ">320.0" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == ">320.0" ~ 320.0,
                            TRUE ~ value_numeric),
  expect = (unit %in% c("sec", "Seconds")),
  range_mainly_low = 9,
  range_mainly_high = 50)

# -----------  TROPONIN

tests_pull(
  tests_raw,
  symbol = "troponin",
  title = "TROPONIN",
  names_cuh = c("HIGH SENSITIVITY TROPONIN"),
  search_pattern = c("troponin", "trop"),
  search_exclude = c("ELECTROPHORESIS COMMENTS",
                     "EXT NEUTROPHILS ",
                     "HYPERSEGMENTED NEUTROPHILS",
                     "NEUTROPHIL COUNT POC",
                     "NEUTROPHIL LEFT SHIFT",
                     "NEUTROPHIL PERCENTAGE POC",
                     "SERUM PROTEIN ELECTROPHORESIS",
                     "STENOTROPHOMONAS MALTOPHILIA CT",
                     "STENOTROPHOMONAS MALTOPHILIA DNA",
                     "URINE ELECTROPHORESIS(BENCE JONES PROTEIN)",
                     "VACUOLATED NEUTROPHILS"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<2.5" ~ "left",
                           value == "<3" ~ "left",
                           value == ">25000" ~ "right",
                           value == ">25000.0" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<2.5" ~ 2.5,
                            value == "<3" ~ 3,
                            value == ">25000" ~ 25000,
                            value == ">25000.0" ~ 25000,
                            TRUE ~ value_numeric),
  expect = (unit == "ng/L"),
  range_mainly_low = 0,
  range_mainly_high = 10000)

# -----------  INTERLEUKIN-10 aka IL-10

tests_pull(
  tests_raw,
  symbol = "il10",
  title = "Interleukin 10",
  names_cuh = c("IL10"),
  search_pattern = c("INTERLEUKIN",
                     "IL-10",
                     "IL10",
                     "human cytokine synthesis inhibitory factor",
                     "CSIF"),
  search_exclude = "INTERLEUKIN-6",
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.1100" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.1100" ~ 0.11,
                            TRUE ~ value_numeric),
  expect = (unit == "pg/ml"),
  range_mainly_low = 0,
  range_mainly_high = 25)


# -----------  IL-1 BETA

tests_pull(
  tests_raw,
  symbol = "il1_beta",
  title = "Interleukin 1 Beta",
  names_cuh = c("IL1 BETA"),
  search_pattern = c("INTERLEUKIN", "IL-1", "IL1"),
  search_exclude = c("INTERLEUKIN-6", "IL10"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.0900" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.0900" ~ 0.09,
                            TRUE ~ value_numeric),
  expect = (unit == "pg/ml"),
  range_mainly_low = 0,
  range_mainly_high = 25)


# -----------  TNF ALPHA

tests_pull(
  tests_raw,
  symbol = "tnf_alpha",
  title = "TNF alpha",
  names_cuh = c("TNF ALPHA"),
  search_pattern = c("TNF", "cachexin", "Tumor necrosis factor", "cachectin",
                     "tumor necrosis factor alpha"),
  search_exclude = NA,
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.23" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.23" ~ 0.23,
                            TRUE ~ value_numeric),
  expect = (unit == "pg/ml"),
  range_mainly_low = 0,
  range_mainly_high = 100)

# -----------  INTERFERON GAMMA

tests_pull(
  tests_raw,
  symbol = "interferon_gamma",
  title = c("Interferon gamma"),
  names_cuh = c("INTERFERON GAMMA"),
  search_pattern = c("INTERFEON", "IFN"),
  search_exclude = "QUANTIFERON TB IFN GAMMA",
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<0.49" ~ "left",
                           value == "<0.94" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<0.49" ~ 0.49,
                            value == "<0.94" ~ 0.94,
                            TRUE ~ value_numeric),
  expect = (unit == "pg/ml"),
  range_mainly_low = 0,
  range_mainly_high = 100)


# -----------  RDW

tests_pull(
  tests_raw,
  symbol = "rdw",
  title = "Red Blood Cell Distribution Width",
  names_cuh = c("RDW"),
  names_external = "EXT RDW",
  search_pattern = c("rdw", "distribution", "width", "rcdw"),
  search_exclude = NA,
  silently_exclude_na_when = FALSE,
  expect = (unit == "%"),
  range_mainly_low = 5,
  range_mainly_high = 25)

# -----------  ALT

# REVIEW
# 1. "POC ED ALT DILUTION" - maybe diulted results when off the scale, not
#    clear.
#
# INCLUDE
# 1. "POC ED ALT (ALANINE AMINOTRANSFERASE)" has a somewhat different reference
#    range, but is probably similar enough for most purposes
#
# EXCLUDE
# 1. "ALANINE" is a different thing
tests_pull(
  tests_raw,
  symbol = "ALT",
  title = "Alanine Transaminase",
  names_cuh = c("ALANINE TRANSAMINASE", "POC ED ALT (ALANINE AMINOTRANSFERASE)"),
  names_external = "EXT ALT",
  search_pattern = c("alt", "alanine", "alat", "transaminase",
                     "aminotransferase"),
  search_exclude = c("ALANINE",
                     "ASPARTATE TRANSAMINASE",
                     "COBALT (JOINT REPLACEMENT)",
                     "COBALT (JOINT REPLACEMENT) (PPB)",
                     "COMPLEMENT ALTERNATIVE PATHWAY AP100",
                     "PHENYLALANINE",
                     "STENOTROPHOMONAS MALTOPHILIA CT",
                     "STENOTROPHOMONAS MALTOPHILIA DNA"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<8" ~ "left",
                           value == "<2" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<8" ~ 8,
                            value == "<2" ~ 2,
                            TRUE ~ value_numeric),
  expect = (unit == "U/L"),
  range_mainly_low = 10,
  range_mainly_high = 10000)

# -----------  ALBUMIN

# EXCLUDE
# 1. "FLUID ALBUMIN" - not a blood test
# 2. "RETIRED-URINE ALBUMIN/CREATININE RATIO", "URINE ALBUMIN" and
#    "URINE ALBUMIN/CREATININE RATIO" are not blood tests
# 3. "PHENYTOIN - ALBUMIN ADJUSTED" - PHENYTOIN is a drug, so this is where you
#    have to adjust for albumin, so not the same
tests_pull(
  tests_raw,
  symbol = "albumin",
  title = "Albumine",
  names_cuh = c("ALBUMIN"),
  names_external = "EXT ALBUMIN",
  search_pattern = c("albumin"),
  search_exclude = c("FLUID ALBUMIN",
                     "PHENYTOIN - ALBUMIN ADJUSTED",
                     "RETIRED-URINE ALBUMIN/CREATININE RATIO",
                     "URINE ALBUMIN",
                     "URINE ALBUMIN/CREATININE RATIO"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<6" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<6" ~ 6,
                            TRUE ~ value_numeric),
  expect = (unit == "g/L"),
  range_mainly_low = 10,
  range_mainly_high = 45,
  range_discard_above = 55)

# -----------  AST

# EXCLUDE
# 1. "ASPARTATE" is the amino acid, not the liver function test
tests_pull(
  tests_raw,
  symbol = "AST",
  title = "Aspartate Transminase",
  names_cuh = c("ASPARTATE TRANSAMINASE"),
  search_pattern = c("aspartate", "ast", "AspAT", "got", "sgot", "oxaloacetic"),
  search_exclude = c("ASPARTATE",
                     "ABSOLUTE BLAST COUNT",
                     "ACTIVATED  B CELLS/PLASMABLASTS %",
                     "BACTERIAL VAGINOSIS YEASTS",
                     "BLAST CELLS # (DM96)",
                     "CD38 +++  IGM - CLASS SWITCHED PLASMABLASTS %",
                     "FAECAL ELASTASE",
                     "FASTING PLASMA GLUCOSE",
                     "GASTRIC PARIETAL CELL ANTIBODY",
                     "GENITAL YEAST",
                     "HODS BMA BLAST COUNT",
                     "HODS FLOW CYTOMETRY MASTER SUMMARY",
                     "MAST CELL TRYPTASE",
                     "PH (GASTRIC)"),
  silently_exclude_na_when = FALSE,
  expect = (unit == "U/L"),
  range_mainly_low = 0,
  range_mainly_high = 250)

# -----------  BILIRUBIN

# INCLUDE
# 1. "POC BILIRUBIN", "TOTAL BILIRUBIN" and "POC ED TOTAL BILIRUBIN" have
#    similar reference ranges
#
# EXCLUDE
# 1. "CONJUGATED BILIRUBIN" is a subset of billirubin, so not the same.
# 2. "BILIRUBIN, POC" comes from TestGroupName == "URINALYSIS DIPSTICK, POC"
#    so not the same.
tests_pull(
  tests_raw,
  symbol = "bilirubin",
  title = "Bilirubin",
  names_cuh = c("POC BILIRUBIN", "TOTAL BILIRUBIN", "POC ED TOTAL BILIRUBIN"),
  names_external = "EXT T. BILIRUBIN",
  search_pattern = c("bilirubin", "bili"),
  search_exclude = c("CONJUGATED BILIRUBIN",
                     "FLUID BILIRUBIN",
                     "MATCHABILITY SCORE",
                     "NET BILIRUBIN ABSORBANCE",
                     "PORPHOBILINOGEN CREATININE RATIO",
                     "UROBILINOGEN, POC"),
  search_exclude_group = "URINALYSIS DIPSTICK, POC",
  silently_exclude_na_when = (name %in% "POC BILIRUBIN"),
  silently_exclude_when = (value == "n/a" | value == "-" | value == "N/A" | value == "Negative" | value == "Positive"),
  censoring_fn = case_when(value == "<" ~ "left",
                           value == "<2" ~ "left",
                           value == "<2.5" ~ "left",
                           value == ">=131" ~ "right",
                           value == "<34" ~ "left",
                           value == "Trace" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(# Use minimum recorded value within test name
    # since this case_when is run group_by
    value == "<" ~ min(value_numeric, na.rm = TRUE),
    value == "<2" ~ 2,
    value == "<2.5" ~ 2.5,
    value == ">= 131" ~ 131,
    value == "<34" ~ 34,
    value == "Trace" ~ 0,
    TRUE ~ value_numeric),
  expect = (unit %in% c("umol/L", "?mol/L")),
  range_mainly_low = 1,
  range_mainly_high = 400)

# -----------  PH

# This comes from a blood gas
#
# EXCLUDE
# 1. "FLUID PH", "PH (GASTRIC)", "URINE PH, POC" are not the same
tests_pull(
  tests_raw,
  symbol = "ph",
  title = "PH Value (POC)",
  names_cuh = c("POC PH"),
  search_pattern = c("ph"),
  search_exclude = c("FLUID PH",
                     "PH (GASTRIC)",
                     "URINE PH, POC",
                     "ALKALINE PHOSPHATASE",
                     "ALPHA-1 ANTITRYPSIN PHENOTYPE",
                     "ALPHA-2-ANTIPLASMIN",
                     "ALPHA-GLUCOSIDASE (REFERENCE ENZYME)",
                     "AMPHIPHYSIN ANTIBODY",
                     "AR PHT",
                     "BASOPHIL COUNT POC",
                     "BASOPHIL PERCENTAGE POC",
                     "BASOPHILIC STIPPLING",
                     "BLOOD SPOT ALPHA-GALACTOSIDASE",
                     "BLOOD SPOT ALPHA-GALACTOSIDASE (FEMALE)",
                     "C BURNETII (QFEVER) PHASE 2 CFT REF",
                     "CD3 TCR ALPHA/BETA",
                     "COAGULASE NEGATIVE STAPHYLOCOCCUS CT",
                     "COAGULASE NEGATIVE STAPHYLOCOCCUS DNA",
                     "CSF LYMPHOCYTES BOTTLE 1",
                     "CSF LYMPHOCYTES BOTTLE 2",
                     "CSF LYMPHOCYTES BOTTLE 3",
                     "CSF POLYMORPHS BOTTLE 1",
                     "CSF POLYMORPHS BOTTLE 2",
                     "CSF POLYMORPHS BOTTLE 3",
                     "DRVVT PHOSPHOLIPID CORRECTION",
                     "ELECTROPHORESIS COMMENTS",
                     "EOSINOPHIL COUNT POC",
                     "EOSINOPHIL PERCENTAGE POC",
                     "EXT ALK PHOS",
                     "EXT BASOPHILS",
                     "EXT EOSINOPHILS",
                     "EXT I PHOSPHATE",
                     "EXT LYMPHOCYTES",
                     "EXT NEUTROPHILS ",
                     "EXT PHOSPHATE",
                     "HAEMOPHILUS DNA",
                     "HAEMOPHILUS INFLUENZA B(HIB) ANTIBODY",
                     "HAEMOPHILUS INFLUENZAE #2 CT",
                     "HAEMOPHILUS INFLUENZAE CT",
                     "HAEMOPHILUS INFLUENZAE DNA",
                     "HB PHENOTYPE",
                     "HODS BMA LYMPH COUNT",
                     "HODS BONE MARROW TREPHINE SUMMARY",
                     "HODS CLL LYMPH COUNT",
                     "HODS LYMPHOCYTE MORPHOLOGY",
                     "HODS PDF LYMPHS %",
                     "HODS PERIPHERAL BLOOD - NO. OF LOCI",
                     "HODS PERIPHERAL BLOOD DONOR CHIMERISM (%)",
                     "HYPERSEGMENTED NEUTROPHILS",
                     "LEGIONELLA PNEUMOPHILA DNA",
                     "LYMPH # (DM96)",
                     "LYMPH COUNT, POC",
                     "LYMPH PERCENTAGE, POC",
                     "LYMPHOCYTE COUNT",
                     "MI-2 ALPHA ANTIBODY",
                     "MV PHT",
                     "MVA BY PHT",
                     "NEUTROPHIL COUNT POC",
                     "NEUTROPHIL LEFT SHIFT",
                     "NEUTROPHIL PERCENTAGE POC",
                     "PEMPHIGOID ANTIBODY",
                     "PEMPHIGUS ANTIBODY",
                     "PHENOBARBITONE",
                     "PHENYLALANINE",
                     "PHENYTOIN",
                     "PHENYTOIN - ALBUMIN ADJUSTED",
                     "PHOSPHATE",
                     "POC AMPHETAMINE",
                     "POC ED ALP (ALKALINE PHOSPHATASE)",
                     "POC METHAMPHETAMINE",
                     "POC MORPHINE",
                     "POC PH TEMP",
                     "PORPHOBILINOGEN CREATININE RATIO",
                     "REACTIVE LYMPHOCYTES",
                     "RED CELL PHENOTYPE",
                     "SERUM PROTEIN ELECTROPHORESIS",
                     "SPHEROCYTES",
                     "SPHEROCYTES (DM96)",
                     "STAPH NUC #2 CT",
                     "STAPH NUC CT",
                     "STAPHYLOCOCCUS AUREUS",
                     "STAPHYLOCOCCUS EPIDERMIDIS CT",
                     "STAPHYLOCOCCUS EPIDERMIDIS DNA",
                     "STAPHYLOCOCCUS MECA CT",
                     "STAPHYLOCOCCUS MECA GENE",
                     "STAPHYLOCOCCUS PVL GENE",
                     "STENOTROPHOMONAS MALTOPHILIA CT",
                     "STENOTROPHOMONAS MALTOPHILIA DNA",
                     "SULPHAMETHOXAZOLE POST LEVEL",
                     "SULPHAMETHOXAZOLE PRE LEVEL",
                     "SYPHILIS ANTIBODY CENTAUR",
                     "THEOPHYLLINE",
                     "TNF ALPHA",
                     "URINE ELECTROPHORESIS(BENCE JONES PROTEIN)",
                     "VACUOLATED NEUTROPHILS"),
  silently_exclude_na_when = (name == "POC PH"),
  expect = (unit == ""),
  range_mainly_low = 6.7,
  range_mainly_high = 7.6,
  range_discard_above = 7.7)

# -----------  CHLORIDE


tests_pull(
  tests_raw %>% filter(ResultUnit != "n/a"),
  symbol = "chloride",
  title = "Chloride",
  names_cuh = c("CHLORIDE", "POC CHLORIDE"),
  names_external = "EXT CHLORIDE",
  search_pattern = c("chloride", "cl"),
  search_exclude = c("6-METHYLMERCAPTOPURINE NUCLEOTIDES",
                     "6-THIOGUANINE NUCLEOTIDES",
                     "ANTI NUCLEAR ANTIBODY (HEP2)",
                     "CD38 +++  IGM - CLASS SWITCHED PLASMABLASTS %",
                     "CICLOSPORIN",
                     "CLARITY, POC",
                     "CLOT FORM.TIME (EXTEM)",
                     "CLOT FORM.TIME (NATEM)",
                     "CLOT TIME (EXTEM)",
                     "CLOT TIME (FIBTEM)",
                     "CLOT TIME (NATEM)",
                     "COMPLEMENT  CLASSICAL PATHWAY CH100",
                     "CSF OLIGOCLONAL BANDING",
                     "CSF OLIGOCLONAL BANDING (PAIRED SERUM)",
                     "CYCLIC CITRULLINATD PEPTIDE (CCP) AB",
                     "ENA(RNP SM RO LA SCL JO1)CENTROMERE",
                     "ENTEROBACTER CLOACAE DNA",
                     "GANCICLOVIR POST LEVEL",
                     "HEPATITIS BSAG (POLYCLONAL) VIDAS",
                     "HLA CLASS I",
                     "HLA CLASS I AB",
                     "HLA CLASS I RATIO",
                     "HLA CLASS II AB",
                     "HLA CLASS II RATIO",
                     "HODS BMA CONCLUSION",
                     "HODS CLINICAL INFORMATION",
                     "HODS CLL LYMPH COUNT",
                     "HODS CONCLUSIONS",
                     "HODS OVERALL CONCLUSION",
                     "HODS PARTICLES",
                     "IGD-CD27+ CLASS SWITCHED MEMORY B CELLS %",
                     "IGM+IGD+CD27+ MARGINAL ZONE LIKE B CELLS(NON-CLASS SWIT",
                     "MAX CLOT FIRM (EXTEM)",
                     "MAX CLOT FIRM (NATEM)",
                     "MAX. CLOT FIRM. (FIBTEM)",
                     "NEURONAL NUCLEAR ANTIBODY 1 HU",
                     "NEURONAL NUCLEAR ANTIBODY 2 RI",
                     "NOT CLASSED # (DM96)",
                     "PARAPROTEIN(MONOCLONAL BAND) QUANTITATION",
                     "PM-SCL100 ANTIBODY",
                     "PM SCL75 ANTIBODY",
                     "POC TRICYCLICANTIDEPRESSANTS",
                     "SCL-70 ANTIBODY",
                     "SCLERODERMA LINE BLOT",
                     "SCREENING TEST DECLINED",
                     "SMOOTH MUSCLE ANTIBODY",
                     "TOTAL NUCLEATED CELLS"),
  silently_exclude_na_when = (name %in% c("POC CHLORIDE", "CHLORIDE")),
  silently_exclude_when = (value == "n/a" | value == "-" | value == "N/A"),
  expect = (unit == "mmol/l"),
  range_mainly_low = 90,
  range_mainly_high = 115)

# -----------  CREATININE SERUM

# REVIEW
# 1. "POC CREATININE" not clear what these are
#
# INCLUDE
# 1. "SERUM CREATININE" and "POC ED CREATININE" have very similar reference
#    ranges
#
# EXCLUDE
# 1. "FLUID CREATININE " not the same
tests_pull(
  tests_raw,
  symbol = "creatinine",
  title = "Creatinine",
  names_cuh = c("SERUM CREATININE", "POC ED CREATININE"),
  names_external = "EXT CREATININE",
  search_pattern = c("creatinine"),
  search_exclude = c("FLUID CREATININE ",
                     "PORPHOBILINOGEN CREATININE RATIO",
                     "RETIRED-URINE ALBUMIN/CREATININE RATIO",
                     "URINE ALBUMIN/CREATININE RATIO",
                     "URINE CALCIUM/CREATININE RATIO",
                     "URINE CREATININE MMOL",
                     "URINE POTASSIUM/CREATININE RATIO",
                     "URINE PROTEIN/CREATININE RATIO",
                     "URINE SODIUM/CREATININE RATIO"),
  silently_exclude_na_when = FALSE,
  censoring_fn = case_when(value == "<18" ~ "left",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<18" ~ 18,
                            TRUE ~ value_numeric),
  expect = (unit == "umol/L"),
  range_mainly_low = 10,
  range_mainly_high = 1200)

# -----------  POTASSIUM

# REVIEW
# 1. "PLASMA POTASSIUM (LITH HEP)" - not certain if this is the same.
#    somwwhat different reference range though
#
# INCLUDE
# 1. "SERUM POTASSIUM" and "POC POTASSIUM" have similar reference ranges
tests_pull(
  tests_raw,
  symbol = "potassium",
  title = "Potassium",
  names_cuh = c("SERUM POTASSIUM", "POC POTASSIUM"),
  names_external = "EXT POTASSIUM",
  search_pattern = c("potassium"),
  search_exclude = c("URINE POTASSIUM", "URINE POTASSIUM/CREATININE RATIO"),
  silently_exclude_na_when = (name == "POC POTASSIUM"),
  censoring_fn = case_when(value == ">" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(# Use maximum recorded value within test name
    # since this case_when is run group_by
    value == ">" ~ max(value_numeric, na.rm = TRUE),
    TRUE ~ value_numeric),
  expect = (unit %in% c("mmol/L", "mmol/l")),
  range_mainly_low = 2,
  range_mainly_high = 6.5,
  range_discard_below = 1.2,
  range_discard_above = 8.5)
# discard <1.2 and >8.5 since these are probably incompatible with life

# -----------  SODIUM

# INCLUDE
# 1. "SERUM SODIUM" and "POC SODIUM" have similar reference ranges
tests_pull(
  tests_raw,
  symbol = "sodium",
  title = "Sodium",
  names_cuh = c("SERUM SODIUM", "POC SODIUM"),
  names_external = "EXT SODIUM",
  search_pattern = c("sodium"),
  search_exclude = c("FLUID SODIUM",
                     "URINE SODIUM",
                     "URINE SODIUM 24H",
                     "URINE SODIUM/CREATININE RATIO"),
  silently_exclude_na_when = (name == "POC SODIUM"),
  expect = (unit %in% c("mmol/L", "mmol/l")),
  range_mainly_low = 115,
  range_mainly_high = 165)

# -----------  UREA

# INCLUDE
# 1. "SERUM UREA" and "POC UREA" have similar reference ranges
tests_pull(
  tests_raw,
  symbol = "urea",
  title = "Urea",
  names_cuh = c("POC UREA", "SERUM UREA"),
  names_external = "EXT UREA",
  search_pattern = c("urea", "bun", "carbamide"),
  search_exclude = c("FLUID UREA", "FREE BETA SUBUNIT", "URINE UREA"),
  silently_exclude_na_when = (name == "POC UREA"),
  silently_exclude_when = (value == "0.0"),
  censoring_fn = case_when(value == "<1.8" ~ "left",
                           value == ">" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(value == "<1.8" ~ 1.8,
                            # Use maximum recorded value within test name
                            # since this case_when is run group_by
                            value == ">" ~ max(value_numeric, na.rm = TRUE),
                            TRUE ~ value_numeric),
  expect = (unit %in% c("mmol/L", "mmol/l")),
  range_mainly_low = 1,
  range_mainly_high = 60)


# -----------  LYMPHOCYTE/ NEUTROPHIL RATIO

lymphocytes_data <- readRDS(rds_file("lymphocytes"))
neutrophils_data <- readRDS(rds_file("neutrophils"))

nl_ratio <-
  lymphocytes_data %>%
  distinct %>% 
  group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
  arrange(COLLECTED_DATETIME,ResultDate) %>% 
  slice(1) %>%
  left_join(neutrophils_data %>% select(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME,ResultValue,ResultDate) %>% 
              distinct  %>% 
              group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
              arrange(COLLECTED_DATETIME,ResultDate) %>% 
              slice(1)  ,
            c("STUDY_SUBJECT_DIGEST","COLLECTED_DATETIME","ResultDate")) %>%
  arrange(COLLECTED_DATETIME) %>% group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
  mutate(ResultValue = as.numeric(ResultValue.y)/as.numeric(ResultValue.x)) %>%
  select(-ResultValue.x,-ResultValue.y) %>%
  select(STUDY_SUBJECT_DIGEST,TestGroupName,TestName,ResultValue,everything()) %>%
  mutate(TestName = "NL_RATIO")

saveRDS(nl_ratio, file = rds_file("nl_ratio"))

# -----------  MCV

# INCLUDE
# 1. "MCV, POC" doesn't have a reference range, but we think it should be
#    equivalent
#
# EXCLUDE
# 1. "HODS BMA MCV" = Haemato-Oncology Diagnostic Service, Bone Marrow
#    Aspiration. This is not a blood test, so should not be included.
# 2. "MCV (SYNC)" comes from TestGroupName == "HAEMOGLOBIN VARIANT SCREEN", so
#    is a different thing
tests_pull(
  tests_raw,
  symbol = "mcv",
  title = "Mean Corpuscular Volume",
  names_cuh = c("MCV", "MCV, POC"),
  names_external = "EXT MCV",
  search_pattern = c("mcv", "corpuscular", "mean cell volume"),
  search_exclude = c("HODS BMA MCV"),
  search_exclude_group = "HAEMOGLOBIN VARIANT SCREEN",
  silently_exclude_na_when = FALSE,
  expect = (unit == "fL"),
  range_mainly_low = 60,
  range_mainly_high = 120)

# -----------  HBC

# INCLUDE
# 1. "HGB, POC" - looks like it is probably the ED POC test
# 2. "POC HAEMOGLOBIN" is the blood gas looks to similar range to "HB"
#
# EXCLUDE
# 1. "FREE HAEMOGLOBIN" is to do with haemolysis, not the same
# 2. "HAEMOGLOBIN A2 LEVEL" not the same
# 3. "HB (SYNC)" comes from TestGroupName == "HAEMOGLOBIN VARIANT SCREEN", so
#    is a different thing
# 4. "HB PHENOTYPE" is not the same
# 5. "HODS BMA HB" = Haemato-Oncology Diagnostic Service, Bone Marrow
#    Aspiration. This is not a blood test, so should not be included.
#
# CENSORED VALUES
# There are a fair number of censored values, and the usual rule of plugging
# in the minimum value of the measured values leads to a large spike at a very
# low, rather unlikely value. So we are excluding them instead.
tests_pull(
  tests_raw,
  symbol = "hb",
  title = "HB",
  names_cuh = c("HB", "HGB, POC", "POC HAEMOGLOBIN"),
  names_external = "EXT HB",
  search_pattern = c("hb", "Hemoglobin", "haemoglobin", "hgb"),
  search_exclude = c("FREE HAEMOGLOBIN",
                     "HAEMOGLOBIN A2 LEVEL",
                     "HB PHENOTYPE",
                     "HODS BMA HB",
                     "ANTI-HBC CENTAUR",
                     "ANTI-HBC IGM",
                     "ANTI-HBC VIDAS",
                     "ANTI-HBE (CENT)",
                     "ANTI-HBS",
                     "EXT HBA1C",
                     "HAEMOGLOBIN A1C, POC",
                     "HBA1C (IFCC) MMOL/MOL",
                     "HBSAG",
                     "HBSAG TITRE",
                     "HBV DNA",
                     "HBV VIRAL LOAD",
                     "HBV VIRAL LOAD LOG",
                     "NET OXYHAEMOGLOBIN",
                     "POC CARBOXYHAEMOGLOBIN",
                     "POC DEOXYHAEMOGLOBIN",
                     "POC OXYHAEMOGLOBIN"),
  search_exclude_group = "HAEMOGLOBIN VARIANT SCREEN",
  silently_exclude_na_when = (name == "POC HAEMOGLOBIN"),
  silently_exclude_when = (value == "<"),
  expect = (unit == "g/L"),
  range_mainly_low = 40,
  range_mainly_high = 200,
  range_discard_below = 30)


# -----------  HCT

# REVIEW
# 1. "HAEMATOCRIT WHOLE BLOOD" - not sure if this is the same or not
# 2. "POC HCT C" looks to be probably HCTc ie calculated HCT, but not certain
#
#
# CENSORED VALUES
# There are a fair number of censored values, and the usual rule of plugging
# in the minimum value of the measured values leads to a large spike at a very
# low, rather unlikely value. So we are excluding them instead.
tests_pull(
  tests_raw,
  symbol = "hct",
  title = "HCT",
  names_cuh = c("HCT", "HCT, POC", "POC HCT"),
  names_external = "EXT HCT",
  search_pattern = c("hct", "hematocrit", "haematocrit", "ht",
                     "packed cell volume", "pcv",
                     "volume of packed red cells", "vprc",
                     "erythrocyte volume fraction", "evf"),
  search_exclude = c("AR PHT",
                     "HTLV 1 AND 2 ANTIBODY",
                     "MV PHT",
                     "MVA BY PHT",
                     "SCHISTOSOMA AB EIA (REF HTD) REF",
                     "SERUM KAPPA FREE LIGHT CHAIN",
                     "SERUM KAPPA/LAMBDA FREE LIGHT CHAIN RATIO",
                     "SERUM LAMBDA FREE LIGHT CHAIN",
                     "STONE WEIGHT"),
  silently_exclude_na_when = (name == "POC HCT"),
  silently_exclude_when = (value == "<"),
  censoring_fn = case_when(value == ">" ~ "right",
                           TRUE ~ NA_character_),
  nonnumeric_fn = case_when(
    # Use maximum recorded value within test name
    # since this case_when is run group_by
    value == ">" ~ max(value_numeric, na.rm = TRUE),
    TRUE ~ value_numeric),
  unit_rescale_fn = case_when(name == "HCT" ~ value * 100,
                              name == "HCT, POC" ~ value * 100,
                              TRUE ~ value),
  unit_relabel_fn = case_when(name == "HCT" ~ "%",
                              name == "HCT, POC" ~ "%",
                              TRUE ~ unit),
  expect = (unit == "%"),
  range_mainly_low = 15,
  range_mainly_high = 60)

# -----------  MCHC

hct <- readRDS(rds_file("hct"))
hb <- readRDS(rds_file("hb"))

mchc <-
hct %>% select(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME,ResultDate,ResultValue) %>% 
  distinct() %>%
  rename(HCT = ResultValue) %>%
  inner_join(hb %>% rename(HB = ResultValue) %>% select(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME,ResultDate,HB) %>% 
               distinct(),
            by = c("STUDY_SUBJECT_DIGEST","COLLECTED_DATETIME")) %>%
  mutate(MCHC = convert.numeric(HB)/convert.numeric(HCT))

saveRDS(mchc,rds_file("mchc"))

# ----------- IL6 / IL10L RATIO

il6_data <- readRDS(rds_file("il6"))
il10_data <- readRDS(rds_file("il10"))

il6_il10_ratio <-
  il6_data %>% 
  distinct %>% 
  group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
  arrange(COLLECTED_DATETIME,ResultDate) %>% 
  slice(1) %>%
  left_join(il10_data %>% select(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME,ResultValue,ResultDate) %>% 
              distinct  %>% 
              group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
              arrange(COLLECTED_DATETIME,ResultDate) %>% 
              slice(1)  ,
            c("STUDY_SUBJECT_DIGEST","COLLECTED_DATETIME","ResultDate")) %>%
  arrange(COLLECTED_DATETIME) %>% group_by(STUDY_SUBJECT_DIGEST,COLLECTED_DATETIME) %>% 
  mutate(ResultValue = convert.numeric(ResultValue.x)/convert.numeric(ResultValue.y)) %>%
  select(-ResultValue.x,-ResultValue.y) %>%
  select(STUDY_SUBJECT_DIGEST,TestGroupName,TestName,ResultValue,everything()) %>%
  mutate(TestName = "IL_RATIO")

saveRDS(il6_il10_ratio, file = rds_file("il6_il10_ratio"))

###############################################################################
# Blood Gas POC - need to reshape to link each test with blood specimen type
#
# In the raw data, the various measures from a single blood sample are in
# different rows, and another row has POC BLOOD SPECIMEN TYPE. The rows
# have a common OrderProcId
#
# Want to reshape so that each blood sample is in a single row, with a column
# to identify the blood specimen type
###############################################################################


bloodgas_data_by_result <- tests_raw %>%
  filter(TestGroupName == "BLOOD GAS, POC")

bloodgas_data_by_result_testnames <- bloodgas_data_by_result %>% count(TestName)

write.csv(bloodgas_data_by_result_testnames,
          file = csv_out_file("2021-04-11-bloodgas_data_by_result_testnames"))


bloodgas_data_by_result_values <- bloodgas_data_by_result %>%
  pull(ResultValue) %>%
  unique

bloodgas_data_by_result_values[is.na(as.numeric(bloodgas_data_by_result_values))]
dput(bloodgas_data_by_result_values[is.na(as.numeric(bloodgas_data_by_result_values))])

# check only Blood Specimen Type has these values
blood_values <- c("Arterial blood", "Venous blood", "Arterial Blood", "Blood",
                  "Mixed blood", "Capillary blood", "Venous Blood",
                  "Capillary Blood")

all(bloodgas_data_by_result %>%
      filter(ResultValue %in% blood_values) %>%
      pull(TestName) %>%
      unique == "POC BLOOD SPECIMEN TYPE")


bloodgas_data_by_result %>%
  filter(ResultValue == "<") %>%
  pull(TestName) %>%
  unique

bloodgas_data_by_result %>%
  filter(ResultValue == ">") %>%
  pull(TestName) %>%
  unique

bloodgas_data_by_result %>%
  filter(ResultValue == "out of range")


bloodgas_data_by_result %>%
  filter(ResultValue == "n/a") %>%
  pull(TestName) %>%
  unique

bloodgas_data_by_result %>%
  filter(ResultValue == "- out of range")

bloodgas_data_by_result %>%
  filter(ResultValue == "-")

# Not all machines have all tests, so decent number of NAs is not too surprising
bloodgas_data_by_result %>%
  filter(is.na(ResultValue))

values_to_drop <- c("out of range", "- out of range", "-", "0..8", "n/a")

bloodgas_data_by_result <- bloodgas_data_by_result %>%
  filter(!ResultValue %in% values_to_drop)

# Check the Units are consistent
################################
bloodgas_data_by_result_testnames_resultunit <- bloodgas_data_by_result %>%
  count(TestName, ResultUnit)

write.csv(bloodgas_data_by_result_testnames_resultunit,
          file = csv_out_file("2021-04-11-bloodgas_data_by_result_testnames_resultunit"))

# HAEMOGLOBIN looks fine now, unlike delirium

bloodgas_data_by_result <- bloodgas_data_by_result %>%
  select(-ResultUnit)



# Reshape the bloodgas_data_by_result so that each row corresponds to a single order
##############################################################################

unique_rows <- bloodgas_data_by_result %>% n_distinct
total_rows <- bloodgas_data_by_result %>% nrow
total_rows - unique_rows

# delete duplicate rows
bloodgas_data_by_result <- bloodgas_data_by_result %>%
  distinct

# Convert the measurement names to be suitable as column names
# then reshape
bloodgas_data <- bloodgas_data_by_result %>%
  mutate(TestName =
           fct_recode(TestName,
                      "poc_po2" = "PO2, POC",
                      "poc_be" = "POC BE",
                      "poc_bilirubin" = "POC BILIRUBIN",
                      "blood_specimen_type" = "POC BLOOD SPECIMEN TYPE",
                      "poc_carboxyhaemoglobin" = "POC CARBOXYHAEMOGLOBIN",
                      "poc_deoxyhaemoglobin" = "POC DEOXYHAEMOGLOBIN",
                      "poc_glucose" = "POC GLUCOSE",
                      "poc_haemoglobin" = "POC HAEMOGLOBIN",
                      "poc_fio2" = "POC FIO2",
                      "poc_methaaemoglobin" = "POC METHAAEMOGLOBIN",
                      "poc_chloride" = "POC CHLORIDE",
                      "poc_hco3" = "POC HCO3",
                      "poc_hydrogen" = "POC HYDROGEN",
                      "poc_hct" = "POC HCT",
                      "poc_hct_c" = "POC HCT C",
                      "poc_ca" = "POC IONISED CALCIUM",
                      "poc_lactate_bg" = "POC LACTATE BG",
                      "poc_oxyhaemoglobin" = "POC OXYHAEMOGLOBIN",
                      "poc_pco2" = "POC PCO2",
                      "poc_so2" = "POC SO2",
                      "poc_pco2_temp" = "POC PCO2 TEMP",
                      "poc_ph_temp" = "POC PH TEMP",
                      "poc_ph" = "POC PH",
                      "poc_po2_temp" = "POC PO2 TEMP",
                      "poc_potassium" = "POC POTASSIUM",
                      "poc_sodium" = "POC SODIUM",
                      "poc_urea" = "POC UREA",
                      "poc_temperature" = "POC TEMPERATURE")) %>%
  select(STUDY_SUBJECT_DIGEST, OrderProcId, TestName, ResultValue) %>%
  spread(TestName, ResultValue)

# actually all blood gas is poc, so drop that from the column names
# except for those that are measured elsewhere, so might get confused
bloodgas_data <- bloodgas_data %>%
  rename("po2" = "poc_po2",
         "be" = "poc_be",
         "bg_glucose" = "poc_glucose",
         "lactate" = "poc_lactate_bg",
         "pco2" = "poc_pco2",
         "pco2_temp" = "poc_pco2_temp",
         "ph_temp" = "poc_ph_temp",
         "po2_temp" = "poc_po2_temp",
         "methaaemoglobin" = "poc_methaaemoglobin",
         "hco3" = "poc_hco3",
         "hydrogen" = "poc_hydrogen",
         "ph" = "poc_ph")


# Associate each order with its ResultDate
##########################################

# Check that each OrderProcId has a unique ResultDate
bloodgas_data_by_result %>%
  group_by(OrderProcId) %>%
  summarise(n = length(unique(ResultDate))) %>%
  filter(n != 1) %>%
  nrow == 0

bloodgas_data_by_result_resultdate <- bloodgas_data_by_result %>%
  select(OrderProcId, ResultDate) %>%
  distinct

bloodgas_data <- bloodgas_data %>%
  left_join(bloodgas_data_by_result_resultdate, by = "OrderProcId")


# Check the reshaping is correct
################################
# Check a couple to verify reshaping
bloodgas_data_by_result %>% filter(OrderProcId == "148381021")
bloodgas_data %>% filter(OrderProcId == "148381021")

# ??
bloodgas_data %>%
  filter(blood_specimen_type == "Capillary blood")

# Check number of rows
order_prod_id_unique <- bloodgas_data_by_result %>%
  pull(OrderProcId) %>%
  unique %>%
  length

nrow(bloodgas_data) == order_prod_id_unique


blood_specimen_types <- bloodgas_data %>%
  count(blood_specimen_type)

write.csv(blood_specimen_types,
          file = csv_out_file("2021-01-12-blood_specimen_types"))

# There are missing values in blood specimen type
# Note there is no POC BLOOD SPECIMEN TYPE row for this OrderProcId
# we will label these as "Non-arterial blood"
bloodgas_data %>%
  filter(is.na(blood_specimen_type))


bloodgas_data <- bloodgas_data %>%
  mutate(blood_specimen_type =
           fct_recode(blood_specimen_type,
                      "Arterial blood" = "Arterial blood",
                      "Arterial blood" = "Arterial Blood",
                      "Blood" = "Blood",
                      "Capillary blood" = "Capillary blood",
                      #"Capillary blood" = "Capillary Blood",
                      "Mixed blood" = "Mixed blood",
                      "Venous blood" = "Venous blood",
                      "Venous blood" = "Venous Blood"),
         blood_specimen_type_simplified =
           if_else(blood_specimen_type == "Arterial blood",
                   true = "Arterial blood",
                   false = "Non-arterial blood",
                   missing = "Non-arterial blood"))

# Blood specimen type sanity checking
bloodgas_data %>%
  count(blood_specimen_type)

bloodgas_data %>%
  count(blood_specimen_type_simplified)




# Add columns to indicate censoring type for all the censored values
####################################################################

meas_cols <- c("po2", "be", "poc_bilirubin", "poc_carboxyhaemoglobin",
               "poc_deoxyhaemoglobin", "bg_glucose", "poc_haemoglobin",
               "poc_hct", "poc_hct_c", "poc_ca",
               "lactate", "poc_oxyhaemoglobin", "pco2",
               "pco2_temp", "ph_temp", "po2_temp", "poc_potassium",
               "poc_sodium", "poc_urea", "poc_fio2", "methaaemoglobin",
               "poc_chloride", "hco3", "hydrogen", "poc_so2",
               "ph", "poc_temperature")

numeric_value_or_na <- function(x){
  case_when(x == "<" ~ NA_character_,
            x == ">" ~ NA_character_,
            TRUE ~ x)
}

censoring_type <- function(x){
  case_when(x == "<" ~ "left",
            x == ">" ~ "right",
            TRUE ~ NA_character_)
}

bloodgas_data <- bloodgas_data %>%
  mutate_at(meas_cols, list(censoring_type = censoring_type)) %>%
  mutate_at(meas_cols, numeric_value_or_na) %>%
  mutate_at(meas_cols, as.numeric)


bloodgas_meas_cols_values <- bloodgas_data %>%
  select(all_of(meas_cols)) %>%
  unlist %>%
  unique

sort(bloodgas_meas_cols_values) %>% head
sort(bloodgas_meas_cols_values) %>% tail

# the arterial less than 6.5 we reckon are actually venous, but
# PO2 TEMP for venous is not interesting, so just make NA for simplicity
bloodgas_data <- bloodgas_data %>%
  mutate(po2_temp =
           case_when(blood_specimen_type_simplified == "Arterial blood" &
                       po2_temp < 6.5 ~ NA_real_,
                     TRUE ~ po2_temp))

bloodgas_data %>%
  filter(blood_specimen_type_simplified == "Non-arterial blood") %>%
  select(po2_temp)


###### NOTE ONLY PO2_TEMP IS TIDIED UP IN THIS!!!
saveRDS(bloodgas_data, file = rds_file("bloodgas_data"))


# Subset to just arterial, and rename columns to highlight this
bloodgas_data_arterial <- bloodgas_data %>%
  filter(blood_specimen_type_simplified == "Arterial blood") %>%
  select(STUDY_SUBJECT_DIGEST,
         OrderProcId,
         ResultDate,
         "arterial_po2" = "po2",
         "arterial_be" =  "be",
         "arterial_pco2" =  "pco2",
         "arterial_pco2_temp" =  "pco2_temp",
         "arterial_ph_temp" =  "ph_temp",
         "arterial_ph" =  "ph",
         "arterial_po2_temp" =  "po2_temp",
         "arterial_hco3" = "hco3",
         "arterial_hydrogen" = "hydrogen",
         "arterial_poc_fio2" = "poc_fio2",
         "arterial_poc_so2" = "poc_so2",

         "arterial_po2_censoring_type" = "po2_censoring_type",
         "arterial_be_censoring_type" =  "be_censoring_type",
         "arterial_pco2_censoring_type" =  "pco2_censoring_type",
         "arterial_pco2_temp_censoring_type" =  "pco2_temp_censoring_type",
         "arterial_ph_temp_censoring_type" =  "ph_temp_censoring_type",
         "arterial_ph_censoring_type" =  "ph_censoring_type",
         "arterial_po2_temp_censoring_type" =  "po2_temp_censoring_type",
         "arterial_hco3_censoring_type" =  "hco3_censoring_type",
         "arterial_hydrogen_censoring_type" =  "hydrogen_censoring_type",
         "arterial_poc_fio2_censoring_type" =  "poc_fio2_censoring_type",
         "arterial_poc_so2_censoring_type" =  "poc_so2_censoring_type")

bloodgas_data_arterial %>% pull(arterial_po2_temp) %>% summary

## ONLY PO2_TEMP IS TIDIED IN THIS
saveRDS(bloodgas_data_arterial,
        file = rds_file("bloodgas_data_arterial"))

### 

# Arterial PCO2 (aka PaCO2)
paco2 <- bloodgas_data_arterial %>%
  group_by(STUDY_SUBJECT_DIGEST, ResultDate) %>%
  select(STUDY_SUBJECT_DIGEST, ResultDate, arterial_pco2) %>%
  rename(paco2_time = ResultDate,
         paco2 = arterial_pco2)

saveRDS(paco2, file = rds_file("paco2"))


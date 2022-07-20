fsheet_io <- readRDS(rds_file("fsheetio_raw"))
fsheet_io <- fsheet_io %>%
  group_by(STUDY_SUBJECT_DIGEST, MEASURE_TIME, disp_name, measured_value, FLO_MEAS_NAME) %>%
  distinct %>%
  filter(disp_name != "Post Void Cath Residual (ml)")


##### INPUT & OUTPUT (ALDOSTERONE)
fsheet_io$measured_value %>% hist
fsheet_io$form %>% table()
fsheet_io$disp_name %>% table()
fsheet_io$FLO_MEAS_NAME %>% table()
fsheet_io$template %>% table()



input <- c("Clinician override bolus dose",
           "Enteral Feed Volume (ml)",
           "Enteral Water/Flushes (ml)",
           "I.V.",
           "Saline Flush (ml)",
           "Volume (ml)",
           "Volume (mL)",
           "Volume (mL) ",
           "Volume (mL) Infused ",
           "Volume infused (mL)",
           "Volume Infused (mL)",
           "Supplement Volume mL (taken PO)",
           "Supplement Volume mL (taken via enteral tube)",
           "Cell Saver",
           "P.O.",
           "Post Void Cath Residual (ml)")

output <- c("Catheter output (ml)",
            "Drainage bag volume (mL) (if applicable)",
            "Drainage since last check (ml)",
            "Emesis",
            "Estimated Blood Loss",
            "Blood loss",
            "Rectal Tube Output",
            "Fluid removed",
            "Free Drainage (ml)",
            "Gastric Aspirate (discarded)",
            "Intermittent venting (ml)",
            "Intermittent/Straight Cath (ml)",
            "Measured output volume (mL)",
            "Mixed Urine/Stool Output (mls)",
            "NG/OG Tube Output",
            "Output (ml)",
            "Stool Volume (mls)",
            "Subglottic secretions",
            "Urine",
            "Urine",
            "VAC Output (mL)")


fsheet_io <- fsheet_io %>% 
  filter(!is.na(measured_value)) %>% 
  mutate(signed_value = case_when(disp_name %in% input ~ abs(measured_value),
                                  disp_name %in% output ~ -abs(measured_value),
                                  disp_name == "Other" & FLO_MEAS_NAME == "OTHER INTAKE" ~ abs(measured_value),
                                  disp_name == "Other" & FLO_MEAS_NAME == "OTHER OUTPUT" ~ -abs(measured_value))) %>%
  mutate(sign = case_when(disp_name %in% input ~ "input",
                          disp_name %in% output ~ "output",
                          disp_name == "Other" & FLO_MEAS_NAME == "OTHER INTAKE" ~ "input",
                          disp_name == "Other" & FLO_MEAS_NAME == "OTHER OUTPUT" ~ "output"))


# Unassigned remainder seems to be positive
fsheet_io <- fsheet_io %>% 
  mutate(signed_value = ifelse(is.na(signed_value),abs(signed_value),signed_value)) %>%
  mutate(sign = ifelse(is.na(sign),"input",sign))

pdf(pdf_file("FLUID_BALANCE"),height = cm(3),width = cm(3))
ggplot(data = fsheet_io) +
  geom_histogram(aes(x = signed_value,
                     fill = sign),
                 binwidth = 100)+ 
  coord_cartesian(xlim = c(-1000,1000)) +
  labs(x = "mL",
       fill = "Type")
dev.off()

#SAVE
saveRDS(fsheet_io,rds_file("fsheet_io"))

# Any measured value for these flo_meas_name means that the patient was on
# RRT at the indicated time
rrt_io <- fsheet_io %>%
  filter(FLO_MEAS_NAME %in% c("R CUH RRT FLUID REMOVED",
                              "R CUH IP HEPARIN RRT CIRCUIT VOLUME",
                              "R CUH IP EPOPROSTENOL RRT CIRCUIT VOLUME"))

saveRDS(rrt_io, file = rds_file("rrt_io"))



mod.file = "McAllister_HBM_m23.txt"
run.name = "HBM Skeena Pinkut,Fulton and Kitwanga (m23)"
basin = "Skeena"
inits.ver = 15
description = "Sensitivity run #25, removing Pinkut, Fultong and vague prior on Kitwanga"
out.file = "result_HBM_Skeena_m23_pinkut_fulton_kitwanga.rds"


# PREPARE Skeena or Nass Specific analysis
source(file.path(script.dir, paste0("PREP_", basin, "_HBM.R")))



smax.dat <- smax.dat %>%
  filter((Stock %in% c("Pinkut", "Fulton")) == FALSE) %>%
  mutate(
    prCV = ifelse(Stock == "Kitwanga", 2, prCV),
    ID = seq_along(Stock)
  )

sr.dat <- sr.dat %>% 
  filter((Stock %in% c("Pinkut", "Fulton")) == FALSE) %>%
  select(-ID) %>%
  left_join(., smax.dat %>% select(Stock, ID), by = "Stock") %>%
  mutate(CU = ID)


sr.dat %>% count(Stock, CU, ID)


diagnostics <- c("gelman-rubin", "traceplots")
source(file.path(script.dir, "FIT_HBM.R"))

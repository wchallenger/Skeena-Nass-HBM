
rm(list=ls())
library(tidyverse)
library(R2jags)
library(modeest)
library(coda)
library(mcmcplots)
library(MCMCvis)


setwd("~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub")

# Settings ----------------------------------------------------------------
# base.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox"

base.dir <- "."  # assumed working directory is set to project root.

data.dir <- file.path(base.dir, "DATA")
script.dir <- file.path(base.dir, "HBM", "SCRIPTS")
jags.dir <- file.path(base.dir, "HBM", "JAGS")
save.dir <- file.path(base.dir, "HBM", "RESULTS", Sys.Date())

smax.script <- "Smax_prep_mcallister_v2.R"


sr.scale <- 1
# sr.scale <- 1e6

if (!dir.exists(save.dir)) dir.create(save.dir)




# Runs Dec 14 -------------------------------------------------------------


runs <- list()

# Korman and English using McAllister Dec 14 Smax
# 
# runs[['KormanEnglish']] <- list(
#   mod.file = "KormanEnglish_HBM.txt",
#   run.name = "Korman and English 2013",
#   smax.script = "Smax_prep_mcallister.R",
#   description ="Korman and English with Dec 14 Smax priors",
#   out.file = "result_HBM_KormanEnglish.rds"
# )
#
# # McAllister 2021 model without year effects
# runs[['McAllister M6']] <- list(
#   mod.file = "McAllister_HBM_m6.txt",
#   run.name = "McAllister 2021 (m6)",
#   smax.script = "Smax_prep_mcallister.R",
#   description ="McAllister 2021 (m6) model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m6.rds"
# )
# # McAllister 2021 model with year effects
# runs[['McAllister M7']] <- list(
#   mod.file = "McAllister_HBM_m7.txt",
#   run.name = "McAllister 2021 (m7)",
#   smax.script = "Smax_prep_mcallister.R",
#   description ="McAllister 2021 (m7) model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m7.rds"
# )
# 
# # McAllister 2021 model with year effects
# runs[['McAllister M8']] <- list(
#   mod.file = "McAllister_HBM_m8.txt",
#   run.name = "McAllister 2021 (m8)",
#   smax.script = "Smax_prep_mcallister.R",
#   description ="McAllister 2021 (m8) year effects model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m8.rds"
# )

# Run m23 ------------------------------------------------------------ 

# McAllister 2021 model with year effects
runs[['McAllister m23 Skeena']] <- list(
  mod.file = "McAllister_HBM_m23.txt",
  run.name = "HBM Skeena Basecase (m23)",
  basin = "Skeena",
  inits.ver = 15,
  description = "HBM base case ",
  out.file = "result_HBM_Skeena_m23.rds"
)

runs[['McAllister m23 Nass']] <- list(
  mod.file = "McAllister_HBM_m23.txt",
  run.name = "HBM Nass Basecase (m23)",
  basin = "Nass",
  inits.ver = 15,
  description = "HBM base case ",
  out.file = "result_HBM_Nass_m23.rds"
)




# Run m24 ------------------------------------------------------------
# McAllister 2021 model with year effects
runs[['McAllister m24 Skeena']] <- list(
  mod.file = "McAllister_HBM_m24.txt",
  run.name = "HBM Skeena Basecase (m24)",
  basin = "Skeena",
  inits.ver = 15,
  description ="HBM base case but including Korman & English’s (2013) error in the code for precision and sigma in the likelihood function",
  out.file = "result_HBM_Skeena_m24.rds"
)


# Run m25 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m25 Skeena']] <- list(
  mod.file = "McAllister_HBM_m25.txt",
  run.name = "HBM Skeena Basecase (m25)",
  basin = "Skeena",
  inits.ver = 15,
  description ="Same as HBM base case but with no upper bounds on Smax",
  out.file = "result_HBM_Skeena_m25.rds"
)

# Run m26 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m26 Skeena']] <- list(
  mod.file = "McAllister_HBM_m26.txt",
  run.name = "HBM Skeena Basecase (m26)",
  basin = "Skeena",
  description ="Same as HBM base case but leaving out common shared year effects",
  out.file = "result_HBM_Skeena_m26.rds"
)

# Run m27 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m27 Skeena']] <- list(
  mod.file = "McAllister_HBM_m27.txt",
  run.name = "HBM Skeena Basecase (m27)",
  basin = "Skeena",
  description ="Non-hierarchical model run with no common shared year effect but including the same Smax prior information as in the base case HBM",
  out.file = "result_HBM_Skeena_m27.rds"
)

# Run m28 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m28 Skeena']] <- list(
  mod.file = "McAllister_HBM_m28.txt",
  run.name = "HBM Skeena Basecase (m28)",
  basin = "Skeena",
  description ="Same as HBM base case but normal priors on Smax instead of the base case lognormal prior on Ricker b.",
  out.file = "result_HBM_Skeena_m28.rds"
)

# Run m29 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m29 Skeena']] <- list(
  mod.file = "McAllister_HBM_m23.txt",
  run.name = "HBM Skeena Basecase (m29)",
  basin = "Skeena",
  description ="Same as HBM base case but with vague Ricker b priors, but including the upper bounds on Smax.",
  out.file = "result_HBM_Skeena_m29.rds"
)


# Shrinkage ---------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m23 Skeena Shrinkage']] <- list(
  mod.file = "McAllister_HBM_m23_nonHBM.txt",
  run.name = "HBM Skeena non HBM Basecase (m23*)",
  basin = "Skeena",
  inits.ver = 15,
  description = "nonHBM base case",
  out.file = "result_HBM_Skeena_m23_nonHBM.rds"
)





# PREP: Smax Data ------------------------------------------------------------
# Late meta data

lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble

# smax.dat %>% select(ID, Basin, Stock, prSmax, pr)

# creates an smax.tab object containing all the Smax values for Skeena and Nass
# analyses
source(file.path(script.dir, smax.script))

# Adjust scaling
smax.tab <- smax.tab %>% mutate(
  prSmax = prSmax/sr.scale,
  Smaxmax = Smaxmax/sr.scale
)

# stop("DEV STOPPAGE L182", call. = F)

  
# VALIDATE: Smax  --------------------------------------------------------------


# message("WARNING: Using Mcalliser smax")
# smax.old <-  smax.dat
# smax.dat <- cbind(smax.alt, smax.old %>% select(ID, Basin,Stock))
# if (names(runs)[r] == "McAllister m29") smax.dat$prCV <- 2
# 

alt.datdir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena/2022-01-06"
# 
smax.alt <- read.table(file.path(alt.datdir, "data_v2h.txt"), skip = 4, nrows = 18) %>%
  rownames_to_column(var="ID") %>%
  mutate(ID = as.numeric(ID))
colnames(smax.alt) <- c("ID",  "prSmax", "prCV", "Smaxmax")


smax.compare <- full_join(
  x = smax.tab %>% filter(Basin == "Skeena") %>% select(ID,Stock, prSmax, prCV, Smaxmax),
  y = smax.alt %>% rename(prSmaxCheck = prSmax, prCVCheck=prCV, SmaxmaxCheck=Smaxmax),
  by = "ID"
) %>%
  mutate(
    prSmax_DIFF = prSmax - prSmaxCheck,
    prCV_DIFF = prCV - prCVCheck,
    Smaxmax_DIFF = Smaxmax - SmaxmaxCheck 
  ) 

check <- smax.compare %>% filter(abs(prSmax_DIFF) > 1 | abs(prCV_DIFF) > 0 | abs(Smaxmax_DIFF) > 5)


if (nrow(check)>0) message("Smax priors have changed for ", paste(check$Stock, collapse = ","))





# PREP: SR Data ----------------------------------------------------
# Stock Recruit Data

stock.info <- read.csv(file.path(data.dir, "StockInfo_Main.csv")) %>% as_tibble

sr.tab <-  left_join(
  x = read.csv(file.path(data.dir, "StockData_Main.csv")),
  y = stock.info %>% select(Stock, Basin, LifeHistory),
  by = "Stock"
) %>% as_tibble %>%
  # filter(Basin == "Skeena") %>%
  filter(!is.na(Rec) & !is.na(Spn)) %>%
  group_by(Basin) %>%
  mutate(year = Year - min(Year) + 1) %>%
  ungroup %>%
  mutate(
    # CU = factor(Stock) %>% as.numeric,
    lnRSobs = log(Rec/Spn)
  ) %>%
  left_join(
    .,
    y = smax.tab %>% select(ID, Stock) %>% rename(CU = ID),
    by = "Stock"
  ) %>%
  # rename(Spawn = Spn) %>%
  mutate(Spawn = Spn / sr.scale) %>%
  select(Basin, Stock, CU,Spawn, lnRSobs, year, Year) %>%
  arrange(desc(Basin), CU, year)


# Filter out extreme
warning("log Recruit/Spawner values over 4 excluded")
sr.tab <- sr.tab %>% filter(lnRSobs < 4)

# # Extract CU id to stock name
# stock.lookup <-  sr.dat %>% select(CU, Stock) %>% unique
# 



# Run Analyses ------------------------------------------------------------
diagnostics = TRUE
for (r in seq_along(runs)) {
  
  message("Fitting run '", names(runs)[r],"'")
  
  # attach(runs[[r]])
  for (parm in names(runs[[r]])) assign(parm, runs[[r]][[parm]])
  
  # PREPARE Skeena or Nass Specific analysis
  source(file.path(script.dir, paste0("PREP_", basin, "_HBM.R")))
  
  # RUN HBM 
  source(file.path(script.dir, "RUN_HBM.R"))
  
  rm(list = names(runs[[r]]))
  # detach(runs[[r]])

}


stop("Dev Stop 286")

# Post Processing ---------------------------------------------------------
# @TODO


# SAVE --------------------------------------------------------------------

# RES DOC appendix
smax.appendix <- smax.tab %>% select(Basin, Stock, Lake, prSmax, prCV) %>%
  arrange(desc(Basin), Stock)


library(openxlsx)
WinBUG.runs <- loadWorkbook("/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/appendix_results_WinBUGS.xlsx")

app.result <- list(
  "Smax" = smax.appendix,
  "Shrinkage" = read.xlsx(WinBUG.runs, sheet = "Shrinkage"),
  "Run 1" = read.xlsx(WinBUG.runs, sheet = "Run 1"),
  "Run 2" = read.xlsx(WinBUG.runs, sheet = "Run 2"),
  "Run 3" = read.xlsx(WinBUG.runs, sheet = "Run 3"),
  "Run 4" = read.xlsx(WinBUG.runs, sheet = "Run 4"),
  "Run 5" = read.xlsx(WinBUG.runs, sheet = "Run 5"),
  "Run 6" = read.xlsx(WinBUG.runs, sheet = "Run 6")
)


saveRDS(app.result, "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-Sk-Model-Report/REPORT/data/HBM/HBM_appendix_tables.rds")
saveRDS(app.result, "/Users/wchallenger/GitHub/Fisheries/Skeena-TEST/data/HBM/HBM_appendix_tables.rds")

app.result$Shrinkage %>%
  select(Stock, nonHBM.median, HBM.median) %>%
  gather(key=Model, value = Median, ends_with("median")) %>%
  mutate(
    Model = str_replace(Model, "\\.median", "") %>% factor(levels = c("nonHBM", "HBM"))
  ) %>%
  ggplot(., aes(x=Model, y = Median)) +
  geom_point(aes(color = Stock)) +
  geom_line(aes(group = Stock, color = Stock)) +
  theme_classic(14) +
  theme(legend.title = element_blank()) +
  labs(
    y = "Median Ricker a parameter"
  )

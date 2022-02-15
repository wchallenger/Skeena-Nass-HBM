
rm(list=ls())
library(tidyverse)
library(R2jags)
library(modeest)
library(coda)
library(mcmcplots)


# Settings ----------------------------------------------------------------
# base.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox"

base.dir <- "."  # assumed working directory is set to project root.

data.dir <- file.path(base.dir, "DATA")
script.dir <- file.path(base.dir, "HBM", "Scripts")
jags.dir <- file.path(base.dir, "HBM", "JAGS")
save.dir <- file.path(base.dir, "HBM")


runs <- list()


# WORKAROUND --------------------------------------------------------------


alt.datdir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena/2022-round3"

smax.alt <- read.table(file.path(alt.datdir, "data_v2h.txt"), skip = 4, nrows = 18)
colnames(smax.alt) <- c("prSmax", "prCV", "Smaxmax")
# Runs Dec 14 -------------------------------------------------------------


# Korman and English using McAllister Dec 14 Smax
# 
# runs[['KormanEnglish']] <- list(
#   mod.file = "KormanEnglish_HBM.txt",
#   model.name = "Korman and English 2013",
#   smax.script = "Smax_prep_mcallister.R",
#   run.name ="Korman and English with Dec 14 Smax priors",
#   out.file = "result_HBM_KormanEnglish.rds"
# )
# 
# # McAllister 2021 model without year effects
# runs[['McAllister M6']] <- list(
#   mod.file = "McAllister_HBM_m6.txt",
#   model.name = "McAllister 2021 (m6)",
#   smax.script = "Smax_prep_mcallister.R",
#   run.name ="McAllister 2021 (m6) model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m6.rds"
# )
# # McAllister 2021 model with year effects
# runs[['McAllister M7']] <- list(
#   mod.file = "McAllister_HBM_m7.txt",
#   model.name = "McAllister 2021 (m7)",
#   smax.script = "Smax_prep_mcallister.R",
#   run.name ="McAllister 2021 (m7) model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m7.rds"
# )
# 
# # McAllister 2021 model with year effects
# runs[['McAllister M8']] <- list(
#   mod.file = "McAllister_HBM_m8.txt",
#   model.name = "McAllister 2021 (m8)",
#   smax.script = "Smax_prep_mcallister.R",
#   run.name ="McAllister 2021 (m8) year effects model with Dec 14 Smax priors",
#   out.file = "result_HBM_McAllister_m8.rds"
# )


# Run m24 ------------------------------------------------------------ 
# McAllister 2021 model with year effects
runs[['McAllister m24']] <- list(
  mod.file = "McAllister_HBM_m24.txt",
  model.name = "McAllister 2022 (m24)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="HBM base case but including Korman & English’s (2013) error in the code for precision and sigma in the likelihood function",
  out.file = "result_HBM_McAllister_m24.rds"
)


# Run m25 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m25']] <- list(
  mod.file = "McAllister_HBM_m25.txt",
  model.name = "McAllister 2022 (m25)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Same as HBM base case but with no upper bounds on Smax",
  out.file = "result_HBM_McAllister_m25.rds"
)

# Run m26 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m26']] <- list(
  mod.file = "McAllister_HBM_m26.txt",
  model.name = "McAllister 2022 (m26)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Same as HBM base case but leaving out common shared year effects",
  out.file = "result_HBM_McAllister_m26.rds"
)

# Run m27 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m27']] <- list(
  mod.file = "McAllister_HBM_m27.txt",
  model.name = "McAllister 2022 (m27)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Non-hierarchical model run with no common shared year effect but including the same Smax prior information as in the base case HBM",
  out.file = "result_HBM_McAllister_m27.rds"
)

# Run m28 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m28']] <- list(
  mod.file = "McAllister_HBM_m28.txt",
  model.name = "McAllister 2022 (m28)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Same as HBM base case but normal priors on Smax instead of the base case lognormal prior on Ricker b.",
  out.file = "result_HBM_McAllister_m28.rds"
)

# Run m29 ------------------------------------------------------------

# McAllister 2021 model with year effects
runs[['McAllister m29']] <- list(
  mod.file = "McAllister_HBM_m23.txt",
  model.name = "McAllister 2022 (m29)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Same as HBM base case but with vague Ricker b priors, but including the upper bounds on Smax.",
  out.file = "result_HBM_McAllister_m29.rds"
)





# Run Analyses ------------------------------------------------------------

for (r in seq_along(runs)) {
  message("Fitting run '", names(runs)[r],"'")
  attach(runs[[r]])
  source(file.path(script.dir, "fit_HBM.R"))
  detach(runs[[r]])
}


# Post Processing ---------------------------------------------------------
# @TODO


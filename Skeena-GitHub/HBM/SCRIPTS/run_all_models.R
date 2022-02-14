
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
# Korman and English using McAllister Dec 14 Smax

runs[['KormanEnglish']] <- list(
  mod.file = "KormanEnglish_HBM.txt",
  model.name = "Korman and English 2013",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="Korman and English with Dec 14 Smax priors",
  out.file = "result_HBM_KormanEnglish.rds"
)

# McAllister 2021 model without year effects
runs[['McAllister M6']] <- list(
  mod.file = "McAllister_HBM_m6.txt",
  model.name = "McAllister 2021 (m6)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="McAllister 2021 (m6) model with Dec 14 Smax priors",
  out.file = "result_HBM_McAllister_m6.rds"
)
# McAllister 2021 model with year effects
runs[['McAllister M7']] <- list(
  mod.file = "McAllister_HBM_m7.txt",
  model.name = "McAllister 2021 (m7)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="McAllister 2021 (m7) model with Dec 14 Smax priors",
  out.file = "result_HBM_McAllister_m7.rds"
)

# McAllister 2021 model with year effects
runs[['McAllister M8']] <- list(
  mod.file = "McAllister_HBM_m8.txt",
  model.name = "McAllister 2021 (m8)",
  smax.script = "Smax_prep_mcallister.R",
  run.name ="McAllister 2021 (m8) year effects model with Dec 14 Smax priors",
  out.file = "result_HBM_McAllister_m8.rds"
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


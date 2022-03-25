# RUn all analyses

rm(list=ls())
library(tidyverse)
library(R2jags)
library(modeest)
library(coda)
library(mcmcplots)
library(MCMCvis)

# base.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox"
setwd("~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub")
base.dir <- "."  # assumed working directory is set to project root.

base.dir <- "~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub"

# Settings ----------------------------------------------------------------
ver <- as.character(Sys.Date())
# ver <- "2022-03-24"


# data.dir <- file.path(base.dir, "DATA")
data.dir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox/1_DATA"
script.dir <- file.path(base.dir, "HBM", "SCRIPTS")
jags.dir <- file.path(base.dir, "HBM", "JAGS")
fun.dir <- file.path(base.dir, "HBM", "FUNCTIONS")
save.dir <- file.path(base.dir, "HBM", "RESULTS", ver) # where to save run specific results

output.dir <- "~/GitHub/Fisheries/Skeena-TEST/data/HBM" # where to save final output


smax.script <- "Smax_prep_mcallister_v2.R"


sr.scale <- 1
# sr.scale <- 1e6


# Analyses  ------------------------------------------------------------
if (!dir.exists(save.dir)) {
  
  dir.create(save.dir)
  
  # Prepare main analysis data set used across all analyses
  source(file.path(script.dir, "PREP-analysis_data.R"))
  
  # Determine convergence of basecase for fundamental parameters
  source(file.path(script.dir, "RUN-basecase_diagnostics.R"))
  
  # Run basecase, the main sensitivity analyese, and nonHBM model for shrinkage
  source(file.path(script.dir, "RUN-basecase_nonHBM_runs1-6.R"))
  
  # Run drop 1 or more stocks jackknife 
  source(file.path(script.dir, "RUN-jackknife_runs.R"))
  
} else {
  message("Reloading previous run")
}

# Post processing: appendix Tables  ----------------------------------------------
message("Generating appendix figures and tables")
app.plots <- list()
app.tables <- list()

app.tables[['Smax']] <- smax.tab %>% select(Basin, Stock, Lake, prSmax, prCV) %>%
  arrange(desc(Basin), Stock)

# Create shrinkage table and plot
source(file.path(script.dir, "POST-shrinkage.R"))

# Create sensitivity of reference points to high and low productivity
source(file.path(script.dir, "POST-productivity_periods.R"))

# Create comparison tables for sensitivity runs 1-6
source(file.path(script.dir, "POST-sensitivity_comparisons.R"))

# Save a copy of appendix tables to Excel
source(file.path(script.dir, "SAVE-appendix_tables_excel.R"))

# Save RDS for report integration
for (table in names(app.tables)) {
  colnames(app.tables[[table]]) <- str_replace_all(colnames(app.tables[[table]]), "[:space:]", "\\.")
}
saveRDS(app.tables, file.path(output.dir, "HBM_appendix_tables.rds"))  # Tables


# Generate jackknife figures using sensitivity runs 7-25
source(file.path(script.dir, "POST-jackknife_plots.R"))

# Generate year effect plots
source(file.path(script.dir, "POST-shared_yr_effect.R"))

# saveRDS(app.plots, file.path(output.dir, "HBM_appendix_figures.rds")) # Figures

# Post Processing: Estimates ----------------------------------------------
# Create a nested list of parameter estimates and time varying alpha
# estimates by 

message("Generating parameter estimate summaries for main report")
source(file.path(script.dir, "POST-parm-estimates.R"))




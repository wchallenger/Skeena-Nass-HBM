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


# Settings ----------------------------------------------------------------
ver <- as.character(Sys.Date())

base.dir <- "."  # assumed working directory is set to project root.

data.dir <- file.path(base.dir, "DATA")
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

# Create comparison tables for sensitivity runs 1-6
source(file.path(script.dir, "POST-sensitivity_comparisons.R"))

# Save to file
for (table in names(app.tables)) {
  colnames(app.tables[[table]]) <- str_replace_all(colnames(app.tables[[table]]), "[:space:]", "\\.")
}
saveRDS(app.tables, file.path(output.dir, "HBM_appendix_tables.rds"))  # Tables


# Create jackknife figures using sensitivity runs 7-25
source(file.path(script.dir, "POST-jackknife_plots.R"))

# Create year effect plots
source(file.path(script.dir, "POST-shared_yr_effect.R"))


# saveRDS(app.plots, file.path(output.dir, "HBM_appendix_figures.rds")) # Figures
# Post Processing: Estimates ----------------------------------------------


message("Generating parameter estimate summaries for main report")
source(file.path(script.dir, "POST-parm-estimates.R"))


# Save Excel Copy -------------------------------------------------------------
wb <- createWorkbook()

for (run in names(app.tables)) {
  
  tab <- app.tables[[run]]
  
  addWorksheet(wb, run)
  writeData(wb, run, tab)
  
  # header
  style <- createStyle(
    border= c("Bottom", "Top"), 
    fgFill = rgb(208,208,208, maxColorValue = 255),
    textDecoration = "bold",
    wrapText = TRUE
  )  
  addStyle(wb, run, style, rows= 1, cols = 1:ncol(tab), gridExpand = TRUE)
  
  if (str_detect(run, "Shrinkage")) {
    style <- createStyle(numFmt = "0.000")
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(2:5), gridExpand = TRUE)
    style <- createStyle(numFmt = "0.0%")
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(6:7), gridExpand = TRUE)
  }
  
  if (str_detect(run, "Run")) {
  
    # Percent change column
    check1 <- run %in% c( "Run 2 Smsy", "Run 3 Smsy")
    check2 <-  str_detect(run, "Umsy")
    if (check1 | check2) {
      style <- createStyle(numFmt = "0.0%")
    } else {
      style <- createStyle(numFmt = "0%")
    }
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(4,7), gridExpand = TRUE)
    # Format estimates
    if (run == "Run 1" | str_detect(run, "Umsy")) {
      style <- createStyle(numFmt = "0.000")
    } else {
      style <- createStyle(numFmt = "#,##0")
    }
    addStyle(wb, run, style, rows= 2:(nrow(tab) + 1), cols = c(2:3,5:6), gridExpand = TRUE)
    
    
    setColWidths(wb, run, cols = 1:7, widths = 16.5) 
  }
  
  
}

out.file <- file.path(output.dir, paste0("appendix_results_JAGS--", ver, ".xlsx"))
saveWorkbook(wb, file = out.file, overwrite = TRUE)

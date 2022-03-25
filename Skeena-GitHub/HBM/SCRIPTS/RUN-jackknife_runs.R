# rm(list=ls())
# library(tidyverse)
# library(R2jags)
# library(modeest)
# library(coda)
# library(mcmcplots)


# Settings ----------------------------------------------------------------
# base.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox"

# base.dir <- "."  # assumed working directory is set to project root.
# 
# data.dir <- file.path(base.dir, "DATA")
# script.dir <- file.path(base.dir, "HBM", "SCRIPTS")
# jags.dir <- file.path(base.dir, "HBM", "JAGS")

save.dir.orig <- save.dir
save.dir <- file.path(save.dir, "jackknife")

offset = 6

 
# smax.script = "Smax_prep_mcallister.R"

mod.file="McAllister_HBM_m23.txt"

basin = "Skeena"


# Preamble ----------------------------------------------------------------
if (!dir.exists(save.dir)) dir.create(save.dir)

# retain a copy of the original analysis data
data.orig <- list(sr.dat = sr.dat, smax.dat=smax.dat)

# sr.dat <-  sr.tab %>% filter(Basin == basin)
# smax.dat <- smax.tab %>% filter(Basin == basin)

run.name <- "Jackknife"
# PREPARE Skeena or Nass Specific analysis
source(file.path(script.dir, paste0("PREP_", basin, "_HBM.R")))


# Jack Knife --------------------------------------------------------------
# master list of stocks
stock.dat <- sr.dat  %>% count(Stock, CU, name='nYear')

#@TODO - improve
if (any(stock.dat$Stock != smax.dat$Stock)) stop("Stock out of sequence")

# drop one of each stock
j <-  as.list(seq_along(stock.dat$Stock))
       
# drop both Pinkut and Fulton (enhanced)
j[[length(j) + 1]] <- which(stock.dat$Stock %in% c("Pinkut", "Fulton"))
   
# for (i in 12:length(j)) {
for (i in seq_along(j)) {

  message("Jacknife run #", i, ", excluding stocks: ", stock.dat[j[[i]], ] %>% select(Stock) %>% unlist %>% paste(collapse=", ") )
  # Create new stock meta data with a new CU index
  meta <- stock.dat[-j[[i]], ] %>%
    rename(CU.orig = CU) %>%
    mutate(CU = seq_along(Stock)) # %>%  select(Stock, CU)
  
  # Stock Recruitment Data
  sr.dat <- data.orig$sr.dat %>% 
    filter(CU %in% j[[i]] == FALSE) %>%
    rename(CU.orig = CU) %>%
    left_join(., y =  meta, by ="Stock")
   
  smax.dat <- data.orig$smax.dat %>% 
    filter(ID %in% j[[i]] == FALSE) %>%
    rename(ID.orig = ID) %>%
    left_join(., y =  meta %>% rename(ID = CU), by ="Stock")
  
  if (any(meta$Stock != smax.dat$Stock)) stop("Stock smax out of sequence")
  
  source(file.path(script.dir, "FIT_HBM_jackknife.R"))
  
}


# Reassign workspace var --------------------------------------------------


assign('save.dir', save.dir.orig)


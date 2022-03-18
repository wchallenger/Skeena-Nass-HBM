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
script.dir <- file.path(base.dir, "HBM", "SCRIPTS")
jags.dir <- file.path(base.dir, "HBM", "JAGS")
save.dir <- file.path(base.dir, "HBM", "jackknife")

smax.script = "Smax_prep_mcallister.R"

mod.file="McAllister_HBM_m23.txt"

# Preamble ----------------------------------------------------------------
if (!dir.exists(save.dir)) dir.create(save.dir)


# Generate base data ------------------------------------------------------


# Late meta data
lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble


source(file.path(script.dir, smax.script))

alt.datdir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena/2022-01-06"

smax.alt <- read.table(file.path(alt.datdir, "data_v2h.txt"), skip = 4, nrows = 18)
colnames(smax.alt) <- c("prSmax", "prCV", "Smaxmax")


message("WARNING: Using McaAlliser smax")
smax.old <-  smax.dat
smax.dat <- cbind(smax.alt, smax.old %>% select(ID, Basin,Stock))


# PREP: SR Data ----------------------------------------------------
# Stock Recruit Data

stock.info <- read.csv(file.path(data.dir, "StockInfo_Main.csv")) %>% as_tibble

sr.dat <-  left_join(
  x = read.csv(file.path(data.dir, "StockData_Main.csv")),
  y = stock.info %>% select(Stock, Basin, LifeHistory),
  by = "Stock"
) %>% as_tibble %>%
  filter(Basin == "Skeena") %>%
  filter(!is.na(Rec) & !is.na(Spn)) %>%
  mutate(
    year = Year - min(Year) + 1,
    # CU = factor(Stock) %>% as.numeric,
    lnRSobs = log(Rec/Spn)
  ) %>%
  left_join(
    .,
    y = smax.dat %>% select(ID, Stock) %>% rename(CU = ID),
    by = "Stock"
  ) %>%
  rename(Spawn = Spn) %>%
  select(Stock, CU,Spawn, lnRSobs, year, Year) %>%
  arrange(CU, year)

# Check to ensure all the Smax database and SR database match.
if (any(is.na(sr.dat$CU)))  stop("Missing CU number in SR data")

# Filter out extreme
warning("log Recruit/Spawner values over 4 excluded")
sr.dat <- sr.dat %>% filter(lnRSobs < 4)

# Extract CU id to stock name
stock.lookup <-  sr.dat %>% select(CU, Stock) %>% unique


data.orig <- list(sr.dat = sr.dat, smax.dat=smax.dat)


# Jack Knife --------------------------------------------------------------
stock.dat <- sr.dat %>% count(Stock, CU, name='nYear')

#@TODO - improve
if (any(stock.dat$Stock != smax.dat$Stock)) stop("Stock out of sequence")

# drop one of each stock
j <-  as.list(seq_along(stock.dat$Stock))
       
# drop both Pinkut and Fulton (enhanced)
j[[length(j) + 1]] <- which(stock.dat$Stock %in% c("Pinkut", "Fulton"))
   

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
  
  source("HBM/SCRIPTS/fit_jags.R")
  
}


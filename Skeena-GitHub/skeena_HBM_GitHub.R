
rm(list=ls())
library(tidyverse)
library(openxlsx)
library(rjags)
library(R2jags) 
library(modeest)   
library(coda)
library(mcmcplots)

# library(scales)
# library(ggrepel)
# library(openxlsx)

 

# Settings ----------------------------------------------------------------

data.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox/DATA"
save.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox/DATA/HBM"

mod.file <- "KormanEnglish_HBM.txt"
mod.file <- "McAllister_HBM_m8.txt"

model.name <- str_replace(basename(mod.file), "\\.txt", "")
run.name <- "Dec 14 Smax"

# Load Data ---------------------------------------------------------------



# PREP: Smax --------------------------------------------------------------

# Late meta data
lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble


source("Smax_prep_mcallister.R")


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
  select(Stock, CU,Spawn, lnRSobs, year) %>%
  arrange(CU, year)


# Filter out extreme
sr.dat <- sr.dat %>% filter(lnRSobs < 4)

# Extract CU id to stock name
stock.lookup <-  sr.dat %>% select(CU, Stock) %>% unique



# Prepare JAGS Data -------------------------------------------------------
if (exists('jags.dat')) rm(jags.dat)

if (str_detect(mod.file, "^KormanEnglish")) {
  
  jags.dat <- list(
    nstock = sr.dat$CU %>% unique %>% length,
    ndata = nrow(sr.dat),
    stock = sr.dat$CU,
    lnRS_obs = sr.dat$lnRSobs,
    Spawn = sr.dat$Spawn,
    prmub = log(1/smax.dat$prSmax),
    prtaub = 1/(smax.dat$prCV^2)
  )
  
  parms <-  c("a", "b",  "CC", "Smsy", "Umsy")
  jags.settings <- list(
    n.thin = 10,
    n.iter=100000,
    n.burnin = 20000
  )
  
} else if (str_detect(mod.file, "^McAllister")) {

  jags.dat <- list(
    Nstocks = sr.dat$CU %>% unique %>% length,
    ndata = nrow(sr.dat),
    CU = sr.dat$CU,
    lnRSobs = sr.dat$lnRSobs,
    Spawn = sr.dat$Spawn,
    prSmax = smax.dat$prSmax,
    prCV = smax.dat$prCV
  )
  
  if (str_extract(basename(mod.file), "_m[:digit:]") %in% paste0("_m", 7:8)){
    jags.dat$year = sr.dat$year
    jags.dat$Nyear = max(sr.dat$year)
  }
  
  parms <-  c("intercept", "slope", "intercmn", "intercsd", "CC", "Smsy", "Umsy")
}
# OPTION A: Fit Stepwise ------------------------------------------------------------
# The first option lets you hand fit each step from compiling, burn-in, to 
# sampling from the posterior.  This can be useful for debugging.
# ptm = proc.time()
# 
# jags.m <- jags.model( file = mod.file, data=jags.dat,  n.chains=3, n.adapt=1000)
# # Burnin
# update(jags.m, n.iter=10000) 
# 
#
# samps <- coda.samples(jags.m, parms, n.iter = 20000, thin = 10 )
# 
# (endtime = proc.time()-ptm)


# Option B:  Parallel Processing ---------------------------------------------------------------



ptm = proc.time()
jagsfit.p <- jags.parallel(data=jags.dat,  parameters.to.save=parms, n.thin=10,
                           n.iter=100000, model.file= mod.file, n.burnin = 20000, n.chains=6)
(endtime = proc.time()-ptm)

samps <- as.mcmc(jagsfit.p)


# Diagnostics -------------------------------------------------------------

# Gelman and Rubin's convergence diagnostic
# gelman.diag(samps, multivariate = TRUE)

# Posterior densities, trace plots and auto-correlation diagnostics
# Showing only derived variables
# mcmcplot(samps,  parms = c("CC","Smsy", "Umsy"), dir = "diagnostics")


# Summaries Statistics ------------------------------------------------

x <- summary(samps)


# reorganize estimates for queries
est <- cbind(
  as.data.frame(x$statistics),
  as.data.frame(x$quantiles)
) %>%
  rownames_to_column("Node") %>%
  mutate(
    Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
    StkID = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", ""))
  )%>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock),
    by = c("StkID" = "ID")
  ) %>%
  arrange(Parm, StkID) %>%
  select( StkID, Stock, Node, Parm, Mean, SD, `Time-series SE`, `2.5%`, `50%`, `97.5%`) %>%
  mutate(
    CV = SD/Mean,
    Model = model.name,
    Run = run.name
  )



# Display estimates by type

# Fundamental parameters
# est %>% filter(Parm %in% c("intercept", "slope", "intercmn", "intercsd"))
est %>% filter(Parm == "intercept")
est %>% filter(Parm == "slope")
est %>% filter(Parm %in% c("intercmn", "intercsd"))

# Derived Parameters
est %>% filter(Parm == "CC")
est %>% filter(Parm == "Smsy")
est %>% filter(Parm == "Umsy")

# Save  R data object
out.file <- file.path(save.dir, )
saveRDS(est, file = "Skeena/mcallister-hsm-result-rev_smax_cv.rds")


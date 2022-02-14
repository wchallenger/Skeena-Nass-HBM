rm(list=ls())
library(tidyverse)
library(openxlsx)
library(rjags)
library(coda)
library(mcmcplots)
library(scales)
library(openxlsx)

# Settings ----------------------------------------------------------------
base.dir <- "~/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena"


xlsx.file <- file.path(base.dir, "McAllister_data.xlsx")

# mod.file <- file.path(base.dir, "McAllister_HBM_m6.txt")
# mod.file <- file.path(base.dir, "McAllister_HBM_m7.txt")
mod.file <- file.path(base.dir, "McAllister_HBM_m8.txt")

# Load Data ---------------------------------------------------------------

# Load excel workbook
wb <- loadWorkbook(xlsx.file)

dat <- read.xlsx(wb, "Data v2")
meta <- read.xlsx(wb, "Meta")

# Prepare JAGS Data -------------------------------------------------------

jags.dat <- list(
  Nstocks = dat$CU %>% unique %>% length,
  ndata = nrow(dat),
  CU = dat$CU,
  lnRSobs = dat$lnRSobs,
  Spawn = dat$Spawn,
  prSmax = meta$prSmax,
  prCV = meta$prCV
)

if (str_extract(basename(mod.file), "_m[:digit:]") %in% paste0("_m", 7:8)){
  jags.dat$year = dat$year
  jags.dat$Nyear = max(dat$year)
}


parms <-  c("intercept", "slope", "intercmn", "intercsd", "CC", "Smsy", "Umsy")

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

library(R2jags) 
library(modeest)    

ptm = proc.time()
jagsfit.p <- jags.parallel(data=jags.dat,  parameters.to.save=parms, n.thin=10,
                           n.iter=100000, model.file= mod.file, n.burnin = 20000, n.chains=6)
(endtime = proc.time()-ptm)

samps <- as.mcmc(jagsfit.p)


# Diagnostics -------------------------------------------------------------

# Gelman and Rubin's convergence diagnostic
gelman.diag(samps, multivariate = TRUE)

# Posterior densities, trace plots and auto-correlation diagnostics
# Showing only derived variables
mcmcplot(samps, parms = c("CC","Smsy", "Umsy"))


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
    y = meta %>% select(ID, Stock),
    by = c("StkID" = "ID")
  ) %>%
  arrange(Parm, StkID) %>%
  select( StkID, Stock, Node, Parm, Mean, SD, `Time-series SE`, `2.5%`, `50%`, `97.5%`) %>%
  mutate(CV = SD/Mean)
  

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



# Save --------------------------------------------------------------------

# Save  R data object
saveRDS(est, file = "Skeena/mcallister-hsm-result.rds")

# Save as excel
sheet <- str_replace(basename(mod.file), "\\.txt","")
addWorksheet(wb, sheet)
writeData(wb, sheet, est)
saveWorkbook(wb, file = "Skeena/mcallister-results.xlsx", overwrite = TRUE)


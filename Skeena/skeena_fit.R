library(tidyverse)
library(openxlsx)
library(rjags)
library(coda)
library(mcmcplots)
library(scales)
library(openxlsx)

# Settings ----------------------------------------------------------------
xlsx.file <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena/McAllister_data.xlsx"
mod.file <- "~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena/Korman_HBM.txt"
# Load Data ---------------------------------------------------------------

# Load excel workbook
wb <- loadWorkbook(xlsx.file)

dat <- read.xlsx(wb, "Data")
smax <- read.xlsx(wb, "Meta")





# Fit JAGS ----------------------------------------------------------------



jags.dat <- list(
  nstock = dat$CU %>% unique %>% length,
  ndata = nrow(dat),
  stock = dat$CU,
  lnRS_obs = dat$lnRSobs,
  Spawn = dat$Spawn,
  prmub = log(1/smax$prSmax),
  prtaub = 1/smax$prCV^2
)


jags.m <- jags.model( file = mod.file, data=jags.dat,  n.chains=3, n.adapt=1000)
update(jags.m, n.iter=10000) 

parms <-  c("a", "b", "CC")
samps <- coda.samples(jags.m, parms, n.iter = 20000, thin = 10 )

# Gelman diagnostic
gelman.diag(samps, multivariate = TRUE)

# Trace plots and lags
mcmcplot(samps, parms = "CC")


# Summaries Stats -------------------------


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
    y = smax %>% select(ID, Stock),
    by = c("StkID" = "ID")
  ) %>%
  arrange(Parm, StkID) %>%
  select( StkID, Stock, Node, Parm, Mean, SD, `Time-series SE`, `2.5%`, `50%`, `97.5%`)
  


est %>% filter(Parm == "CC")


# Using Parrelle=al Processing ---------------------------------------------------------------


library(R2jags) 
library(modeest)    
ptm = proc.time()
jagsfit.p <- jags.parallel(data=jags.dat,  parameters.to.save=parms, n.thin=10,
                           n.iter=100000, model.file= mod.file,n.burnin = 5000, n.chains=6)
endtime = proc.time()-ptm

samps2 <- as.mcmc(jagsfit.p)
x <- summary(samps2)
# reorganize estimates for queries
est2 <- cbind(
  as.data.frame(x$statistics),
  as.data.frame(x$quantiles)
)


rm(list=ls())
library(tidyverse)
library(MCMCvis)

setwd("~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub")

# result.ver <- "2022-03-16"
# result.ver <- "2022-03-16-trial"
# result.ver <- "2022-03-17"
# sr.scale = 1e6
# mod.result <- readRDS(file.path("HBM", "RESULTS", result.ver, "result_HBM_McAllister_m23.rds"))
result.ver <- "2022-03-26"
sr.scale = 1
mod.result <- readRDS(file.path("HBM", "RESULTS", result.ver, "result_HBM_Skeena_m23.rds"))


input.dir <- "Report_Integration"
out.dir <- file.path("Report_Integration", "output", result.ver)

view.results <- FALSE


# Preamble ----------------------------------------------------------------

if (!dir.exists(out.dir)) dir.create(out.dir)
out.dir2 <- file.path(out.dir, "updated_tables")
if (!dir.exists(out.dir2)) dir.create(out.dir2)
# est <- MCMCsummary(mod.result$samples, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
#   rownames_to_column(var = "Node") %>%
#   mutate(
#     Parm = str_extract(Node, "[[:alpha:]_]+"),
#     Idx = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
#   ) 

est <- mod.result$estimates
sr.dat <- mod.result$sr.data
smax.dat <- mod.result$smax.data
samps <- mod.result$samples

report.tables <- list()



# Model Specs -------------------------------------------------------------

# Stock	ReportSeq	ModelLabel	ReportLabel	ModelType	ReportDescription	n.chains	n.burnin	n.thin	n.samples	Kept


report.tables[['ModelSpecs']] <- est %>% select(Stock) %>%
  mutate(
    ReportSeq	= NA,
    ModelLabel = "HBM",
    ReportLabel = "HBM",
    ModelType	= "HBM",
    ReportDescription = "Hierarchical Bayesian Model",
    n.chains	= mod.result$fit$BUGSoutput$n.chains,
    n.burnin	= mod.result$fit$BUGSoutput$n.burnin, 
    n.thin	  =  mod.result$fit$BUGSoutput$n.thin, 
    n.samples = mod.result$fit$BUGSoutput$n.sims * mod.result$fit$BUGSoutput$n.thin,
    Kept      = mod.result$fit$BUGSoutput$n.sims
  )

if (view.results) View(report.tables[['ModelSpecs']], title = "ModelSpecs")
# CapacityPar -------------------------------------------------------------
# Capacity parameter was what Gottfried was using to refer to Smax
#

report.tables[['CapacityPar']] <- sr.dat  %>% 
  filter(lnRSobs < 4) %>% 
  group_by(Stock) %>%
  summarise(
    NumSRObs = n(),
    FirstBY = min(Year),
    LastBY = max(Year),
    SpnContr = max(Spawn)/min(Spawn),
    .groups = "drop"
  ) %>%
  left_join(
   x =. ,
   y = est %>% filter(Parm == "Smax"),
   by = "Stock"
) %>%
  mutate(
    Model = "HBM",
    ReportLabel = "HBM",
    ModelType = "HBM",
    ReportSeq = NA,
    cv = sd/mean
  ) %>%
  rename(
    median = `50%`,
    p2.5 = `2.5%`,
    p25 = `25%`,
    p75 = `75%`,
    p97.5 = `97.5%`
  ) %>%
  select(
    Stock, Model, ReportLabel, ReportSeq, ModelType, NumSRObs, FirstBY, LastBY,
    SpnContr,	median,	mean,	cv,	p2.5,	p25,	p75,	p97.5,	Rhat,	n.eff
  )

if (view.results) View(report.tables[['CapacityPar']], title = "CapacityPar")



# Model Fit ------------------------------------------------------------
# TODO 
# - [x] SpnContr
# - [ ] scaling of sigma seems very different - they have negative values! I think they are reporting the wrong scaling as rickers is ln(R/S) ~ Normal
# - [x] ln.alpha.num  (not clear how to do the parameter count because of hierarhcial shared year effect)
# - [x] ln.alpha.max.cv,
# - [x] ln.alpha.min.n.eff,	
# - [x] ln.alpha.med.n.eff,	
# - [x] ln.alpha.max.n.eff


report.tables[['ModelFits']] <- sr.dat  %>% 
  filter(lnRSobs < 4) %>% 
  group_by(Stock) %>%
  summarise(
    NumSRObs = n(),
    FirstBY = min(Year),
    LastBY = max(Year),
    SpnContr = max(Spawn)/min(Spawn),
    .groups = "drop"
  ) %>%
  mutate(
    Model = "HBM",
    ReportLabel = "HBM",
    ReportSeq = NA, 
    ModelType = "HBM",
    pD = mod.result$fit$BUGSoutput$pD,
    DIC = mod.result$fit$BUGSoutput$DIC
  ) %>%
  select(Stock,	Model,	ReportLabel,	ReportSeq,	ModelType,	NumSRObs,	FirstBY,	LastBY,	SpnContr,	pD,	DIC) %>%
  # Add max Rhat value by stock
  left_join(
    x =., 
    y = est %>% filter(Parm %in% c("intercept", "slope", "se")) %>% group_by(Stock) %>% summarise(max.Rhat = max(Rhat), .groups = "drop"),
    by = "Stock"
  ) %>%
  # Add median sigma
  left_join(
    x =., 
    y = est %>% filter(Parm %in% c("se")) %>% 
      select(Stock, `50%`) %>% 
      rename(med.sigma = `50%`),
    by = "Stock"
  ) %>%
  # Add median deviance
  mutate(
    med.deviance = est %>% filter(Parm %in% c("deviance")) %>% select(`50%`) %>% unlist
  )  %>%
  # Productivity parameter
mutate(
  ln.alpha.num = 1
) %>%
  left_join(
    x = .,
    y = est %>% 
      filter(Parm %in% c("intercept")) %>% 
      select(Stock, CV, n.eff) %>% 
      mutate(ln.alpha.max.cv = CV,	ln.alpha.min.n.eff = n.eff,	ln.alpha.med.n.eff = n.eff,	ln.alpha.max.n.eff = n.eff) %>% 
      select(-c(CV, n.eff)),
    by = "Stock"
  ) %>%
  #add: beta.cv	beta.n.eff	
  left_join(
    x = .,
    y =   est %>% filter(Parm == "slope")  %>% select(Stock,CV, n.eff) %>% rename(beta.cv = CV, beta.n.eff = n.eff),
    by = "Stock"
  ) %>%
  # Add:  sigma.cv	sigma.n.eff
  left_join(
    x = .,
    y =   est %>% filter(Parm == "se")  %>% select(Stock,CV, n.eff) %>% rename(sigma.cv = CV, sigma.n.eff = n.eff),
    by = "Stock"
  ) 



if (view.results) View(report.tables[['ModelFits']], title = "ModelFits")



# Productivity Parameter --------------------------------------------------

report.tables[['ProductivityPar']] <- sr.dat  %>% 
  filter(lnRSobs < 4) %>% 
  count(Stock, name = "NumSRObs") %>%
  left_join(
    x =. ,
    y = est %>% filter(Parm == "intercept.c"),
    by = "Stock"
  ) %>%
  mutate(
    Model = "HBM",
    ReportLabel = "HBM",
    ModelType = "HBM",
    ReportSeq = NA,
    YrIdx = NA,
    cv = sd/mean
  ) %>%
  rename(
    median = `50%`,
    p2.5 = `2.5%`,
    p25 = `25%`,
    p75 = `75%`,
    p97.5 = `97.5%`
  ) %>%
  select(
    Stock, Model, ReportLabel, ReportSeq, ModelType, NumSRObs, YrIdx, Year, 
    median, mean, cv, p2.5, p25, p75, p97.5, Rhat, n.eff
  )


if (view.results) View(report.tables[['ProductivityPar']], title = "ProductivityPar")



# ProductivityPar_RpSVersion ----------------------------------------------


all.samps <- NULL
for (chain in 1:length(samps)) {
  n.it <- nrow(samps[[chain]])
  all.samps <- rbind(
    all.samps, 
    as.data.frame(samps[[chain]]) %>%
      mutate(
        chain=chain,  
        it = seq_len(n.it),
        id = paste(chain, it, sep="-")
      )
  )
}

# sr.dat %>%> group_by()
# 
# scenarios <- data.frame(
#   
# )
# for (i in nrow(scenarios))

# Compute recurits per spawner for a benchmark of 1000 spawners
RS.1000 <- left_join(
  x = all.samps %>%
    # head(n=1) %>%
    select(c(id, starts_with("intercept["))) %>%
    gather(key=Node, value=Value, -c(id)) %>%
    mutate(
      Parm = str_extract(Node, "[[:alpha:]_]+"),
      StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
    ) %>%
    filter(Parm == "intercept") %>%
    select(id, StkId, Value) %>%
    rename(Intercept = Value),
  y = all.samps %>%
    # head(n=1) %>%
    select(c(id, starts_with("slope["))) %>%
    gather(key=Node, value=Value, -c(id)) %>%
    mutate(
      Parm = str_extract(Node, "[[:alpha:]_]+"),
      StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
    ) %>%
    filter(Parm == "slope") %>% 
    select(id, StkId, Value) %>%
    rename(Slope = Value), 
  by = c("id", "StkId")
) %>% 
  # Recruits per 
  mutate(
    lnRS = (Intercept - Slope/sr.scale * 1000),
    Value = exp(lnRS)
  ) %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.025),
    p25 = quantile(Value, prob = 0.25),
    Median = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.975),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  )

# RS.1000

report.tables[['ProductivityPar_RpSVersion']]  <- left_join(
  x = RS.1000,
  y = sr.dat  %>% 
    filter(lnRSobs < 4) %>% 
    count(Stock, name = "NumSRObs"),
  by = "Stock"
) %>% 
  mutate(
    Model = "HBM",
    ReportLabel = "HBM",	
    ReportSeq = NA,	
    YrIdx = NA,	
    Year = NA,
    SpnLabel = 	"At1k",
    Spn = 1000,
    SpnSeq = 3
  )  %>%
  select(
    Stock, Model, ReportLabel, ReportSeq, YrIdx, Year, SpnLabel, 
    Spn, Median, Mean, p10, p25, p75, p90, SpnSeq
  )

if (view.results) View(report.tables[['ProductivityPar_RpSVersion']], title = "ProductivityPar")




# Update Files ------------------------------------------------------------



saveRDS(report.tables, file.path(out.dir, "ReportTablesHBM.rds"))

for (type in c("ModelSpecs", "ModelFits", "CapacityPar", "ProductivityPar", "ProductivityPar_RpSVersion")) {
  # Just the HBM records
  out.file <-file.path(out.dir, paste0("ReportTableHBM_", type, ".csv"))
  write_csv(report.tables[[type]], out.file)
  
  # Updating t
  temp <- bind_rows(
    read.csv(file.path(input.dir, paste0("ReportTable_", type, ".csv"))),
    report.tables[[type]]
  )
  out.file <- file.path(out.dir2, paste0("ReportTable_", type, ".csv"))
  write_csv(temp, out.file)
}







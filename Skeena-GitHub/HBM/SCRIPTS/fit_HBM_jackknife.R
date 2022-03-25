# Used for jackknife fitting

mod.ver <- str_extract(basename(mod.file), "m[:digit:]+")

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
    n.iter =100000,
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
  
  if (mod.ver %in% paste0("m", c(7:8, 23:24, 25, 28))) {
    jags.dat$year = sr.dat$year
    jags.dat$Nyear = max(sr.dat$year)
  }
  # Include Smax - maximum value
  if (mod.ver %in% paste0("m", c(23:24, 26:28))) {
    jags.dat$Smaxmax = smax.dat$Smaxmax
  }
  
  
  parms <-  c("intercept_new", "intercept", "intercept_c",  "slope", "CC", "Smsy", "Smax","Umsy")
  if (mod.ver %in%  paste0("m",c(7:8, 23:24, 25, 28))) {
    parms = c(parms, "TE")
  }
}



# Run JAGS (Parallel Processing) ------------------------------------------
# n.thin = mcmc.set$thin,
# n.iter = mcmc.set$iter,
# n.burnin = mcmc.set$burnin,
# n.chains = mcmc.set$chains
message("Fitting JAGS model...")

ptm = proc.time()
jagsfit.p <- jags.parallel(
  data = jags.dat,
  model.file =  file.path(jags.dir, mod.file),
  parameters.to.save = parms,
  n.thin = 10,
  n.iter = 100000,
  n.burnin = 20000,
  n.chains = 6
)
endtime = proc.time()-ptm
message("Process time: ", endtime[['elapsed']])


samps <- as.mcmc(jagsfit.p)




# Summaries Statistics ------------------------------------------------
message("Summarizing and saving output...")

# MCMCsummary(jagsfit.p, round = 2) %>% as_tibble
# x <- summary(samps)
# 
# # reorganize estimates for queries
# est <- cbind(
#   as.data.frame(x$statistics),
#   as.data.frame(x$quantiles)
# ) %>%
#   rownames_to_column("Node") %>%
#   mutate(
#     Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
#     StkID = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", ""))
#   )%>%
#   left_join(
#     x =.,
#     y = smax.dat %>% select(ID, Stock, CU.orig ) %>% rename(CU = CU.orig),
#     by = c("StkID" = "ID")
#   ) %>%
#   arrange(Parm, StkID) %>%
#   mutate(
#     CV = SD/Mean,
#     Model = str_replace(mod.file, "\\.txt", ""),
#     # Run = stock.dat[j[[i]], ] %>% select(Stock) %>% unlist %>% paste(collapse=", ") %>% paste("Excluded:",.),
#     Run = i,
#     Excluded = stock.dat[j[[i]], ] %>% select(Stock) %>% unlist %>% paste(collapse=", ")
#   ) %>%
#   mutate( Description = paste("Jacknife run excluding:", Excluded)) %>% 
#   select(CU, StkID, Stock, Node, Parm, Run,Excluded, Mean, SD, `Time-series SE`, `2.5%`, `50%`, `97.5%`, CV, Model, Description) %>%
#   as_tibble

est <- MCMCsummary(samps, probs = c(0.025, 0.10, 0.25, 0.5, 0.75, 0.90, 0.975)) %>%
  rownames_to_column("Node") %>%
  as_tibble %>%
  mutate(
    Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
    Idx = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", "")),
    StkIdx = ifelse(Parm %in% c("CC", "intercept", "intercept_x", "intercept_c", "slope", "slope_x", "se", "Smax") | str_detect(Parm, "msy"), Idx, NA),
    YrIdx = ifelse(Parm %in% "TE", Idx, NA)
  ) %>% 
  
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Basin, Stock),
    by = c("StkIdx" = "ID")
  ) %>%
  # Left join Year
  left_join(
    x =.,
    y = sr.dat %>% select(year, Year) %>% unique,
    by = c("YrIdx" = "year")
  ) %>% 
  arrange(Parm, StkIdx) %>%
  select(Node,Parm, Basin, Stock, Year,  mean:n.eff) %>%
  mutate(
    CV = sd/mean,
    Model = str_replace(mod.file, "\\.txt", ""),
    Run = i+offset,
    Excluded = stock.dat[j[[i]], ] %>% select(Stock) %>% unlist %>% paste(collapse=", ")
  )


# Save  R data object
out.file <- paste0("jacknife_",i,"_est.rds")
saveRDS(est, file = file.path(save.dir, out.file))


# Posterior Sample ---------------------------------------------------



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

all.samps %>% count(chain)


all.samps <- all.samps %>%
  # head %>%
  # select(c(id, starts_with("psimu["))) %>%
  select(-c(chain,it)) %>%   # chain number included in contained in ID
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    ParmClass = str_extract(Node, "[[:alpha:]_]+"),
    StkID = str_extract(Node, "\\[[:digit:]+") %>% 
      str_replace(., pattern ="\\[", replacement =  "" ) %>% 
      as.numeric
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock, CU.orig ) %>% rename(CU = CU.orig),
    by = c("StkID" = "ID")
  ) 



# Save  R data object
out.file <- paste0("jacknife_",i,"_samps.rds")
saveRDS(all.samps, file = file.path(save.dir, out.file))
# 


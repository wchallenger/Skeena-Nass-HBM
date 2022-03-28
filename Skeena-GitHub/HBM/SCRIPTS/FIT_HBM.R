message("Running HBM JAGS model...")



mod.ver <- str_extract(basename(mod.file), "m[:digit:]+")
run.ver <- str_extract(basename(run.name), "m[:digit:]+")


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
  if (!exists('parms')) {
    parms <-  c("tau_a", "tau_a_x", "intercept", "intercept_x", "intercept_c", "intercept_new", "slope", "slope_x", "se",  "CC", "Smax", "Smsy","Umsy")
  }
  if (mod.ver %in%  paste0("m",c(7:8, 23:24, 25, 28))) {
    parms = c(parms, "TE")
  }
  
}


# stop()
# DEV: Fit Stepwise ------------------------------------------------------------
# The first option lets you hand fit each step from compiling, burn-in, to 
# sampling from the posterior.  This can be useful for debugging.
# ptm = proc.time()
# 
# if (exists('inits')) {
#   jags.m <- jags.model(
#     file = file.path(jags.dir, mod.file),
#     inits = chain_inits,
#     data=jags.dat,
#     n.chains=2,
#     n.adapt=1000
#   )
# } else {
#   jags.m <- jags.model(
#     file = file.path(jags.dir, mod.file),
#     data=jags.dat,
#     n.chains=2,
#     n.adapt=1000
#   )
# }
# # Burnin
# update(jags.m, n.iter=10000) 
# 
#
# samps <- coda.samples(jags.m, parms, n.iter = 20000, thin = 10 )
# 
# (endtime = proc.time()-ptm)




# Run JAGS (Parallel Processing) ------------------------------------------
# n.thin = mcmc.set$thin,
# n.iter = mcmc.set$iter,
# n.burnin = mcmc.set$burnin,
# n.chains = mcmc.set$chains


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

# stop()
# Diagnostics -------------------------------------------------------------
if (exists('diagnostics')) {
  
  # Gelman and Rubin's convergence diagnostic
  if ("gelman-rubin" %in% diagnostics) {
    gelman.rubin.diag <- gelman.diag(samps, multivariate = TRUE)
    print(gelman.rubin.diag)
  }
 
  if ("gelman-rubin plot" %in% diagnostics) {
    save.name <- str_replace_all(run.name, "[[:space:](]+","_")
    save.name <-  str_replace(save.name, "[)]", "")
    save.name <- paste0("gelman-rubin--",save.name, ".pdf")
    
    out.dir <- file.path(save.dir, "diagnostics")
    if (!dir.exists(out.dir)) dir.create(out.dir, recursive = T)
    
    pdf(file = file.path(out.dir, save.name), width=8, height = 11)
      par(mfrow = c(5,4))
      gelman.plot(samps, ylim = c(0.9, 1.1), auto.layout = FALSE)
    dev.off()
  }
  # Posterior densities, trace plots and auto-correlation diagnostics
  # Showing only derived variables
  # mcmcplot(samps,  parms = c("CC","Smsy", "Umsy"), dir = "diagnostics")
  
  if ("traceplots" %in% diagnostics ) {
    out.dir <- file.path(save.dir, "traceplots", run.name )
  
    if (!dir.exists(out.dir)) dir.create(out.dir, recursive = T)
    mcmcplot(samps,  regex = c("intercept\\[", "slope\\[","Smsy"), dir = out.dir)
  }
}
# Summaries Statistics ------------------------------------------------



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
    Run = run.name,
    Description = description
  )


# SAVE --------------------------------------------------------------------


# Save  R data object
results.obj <- list(
  basin = basin,
  sr.data = sr.dat,
  smax.data = smax.dat,
  fit = jagsfit.p, 
  estimates = est, 
  samples = samps
)
if (exists('gelman.rubin.diag')) {
  results.obj$gelman.rubin  <-  gelman.rubin.diag
}

saveRDS(results.obj, file = file.path(save.dir, out.file))
message("Run results save to:\n  ",file.path(save.dir, out.file))


rm(list = c("jagsfit.p", "est", "samps", "parms"))
if (exists("gelman.rubin.diag")) rm(gelman.rubin.diag)

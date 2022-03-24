message("Preparing Skeena analysis data")

# Data Prep ---------------------------------------------------------------
smax.dat <- smax.tab %>% filter(Basin == "Skeena")
sr.dat <- sr.tab %>% filter(Basin == "Skeena")

# Sensitivity run 6 uses base case with vague priors
if (str_detect(run.name,"m29")) {
  message("Using vague priors")
  smax.dat$prCV <- 2
}
# Check to ensure all the Smax database and SR database match.
if (any(is.na(sr.dat$CU)))  stop("Missing CU number in SR data")



# mcmc.set <- list(
#   thin = 10,
#   iter = 100000,
#   burnin = 20000,
#   chains = 6
# )

# 
# mod.ver <- str_extract(basename(mod.file), "m[:digit:]+")
# run.ver <- str_extract(basename(run.name), "m[:digit:]+")
# 
# 
# # Prepare JAGS Data -------------------------------------------------------
# if (exists('jags.dat')) rm(jags.dat)
# 
# if (str_detect(mod.file, "^KormanEnglish")) {
# 
#   jags.dat <- list(
#     nstock = sr.dat$CU %>% unique %>% length,
#     ndata = nrow(sr.dat),
#     stock = sr.dat$CU,
#     lnRS_obs = sr.dat$lnRSobs,
#     Spawn = sr.dat$Spawn,
#     prmub = log(1/smax.dat$prSmax),
#     prtaub = 1/(smax.dat$prCV^2)
#   )
# 
#   parms <-  c("a", "b",  "CC", "Smsy", "Umsy")
#   jags.settings <- list(
#     n.thin = 10,
#     n.iter =100000,
#     n.burnin = 20000
#   )
# 
# } else if (str_detect(mod.file, "^McAllister")) {
# 
#   jags.dat <- list(
#     Nstocks = sr.dat$CU %>% unique %>% length,
#     ndata = nrow(sr.dat),
#     CU = sr.dat$CU,
#     lnRSobs = sr.dat$lnRSobs,
#     Spawn = sr.dat$Spawn,
#     prSmax = smax.dat$prSmax,
#     prCV = smax.dat$prCV
#   )
# 
#   if (mod.ver %in% paste0("m", c(7:8, 23:24, 25, 28))) {
#     jags.dat$year = sr.dat$year
#     jags.dat$Nyear = max(sr.dat$year)
#   }
#  # Include Smax - maximum value
#   if (mod.ver %in% paste0("m", c(23:24, 26:28))) {
#     jags.dat$Smaxmax = smax.dat$Smaxmax
#   }
#   parms <-  c("tau_a", "tau_a_x", "intercept", "intercept_x", "intercept_c", "slope", "slope_x", "se",  "CC", "Smsy", "Smax")
#   
#   if (mod.ver %in%  paste0("m",c(7:8, 23:24, 25, 28))) {
#     parms = c(parms, "TE")
#   }
#   
# }
# 
# 
# 
# # DEV: Fit Stepwise ------------------------------------------------------------
# # The first option lets you hand fit each step from compiling, burn-in, to 
# # sampling from the posterior.  This can be useful for debugging.
# # ptm = proc.time()
# # 
# # if (exists('inits')) {
# #   jags.m <- jags.model(
# #     file = file.path(jags.dir, mod.file),
# #     inits = chain_inits,
# #     data=jags.dat,
# #     n.chains=2,
# #     n.adapt=1000
# #   )
# # } else {
# #   jags.m <- jags.model(
# #     file = file.path(jags.dir, mod.file),
# #     data=jags.dat,
# #     n.chains=2,
# #     n.adapt=1000
# #   )
# # }
# # # Burnin
# # update(jags.m, n.iter=10000) 
# # 
# #
# # samps <- coda.samples(jags.m, parms, n.iter = 20000, thin = 10 )
# # 
# # (endtime = proc.time()-ptm)
# 
# 
# 
# 
# # Run JAGS (Parallel Processing) ------------------------------------------
# # n.thin = mcmc.set$thin,
# # n.iter = mcmc.set$iter,
# # n.burnin = mcmc.set$burnin,
# # n.chains = mcmc.set$chains
# message("Running JAGS model...")
# 
# ptm = proc.time()
# jagsfit.p <- jags.parallel(
#   data = jags.dat,
#   model.file =  file.path(jags.dir, mod.file),
#   parameters.to.save = parms,
#   n.thin = 10,
#   n.iter = 100000,
#   n.burnin = 20000,
#   n.chains = 6
# )
# endtime = proc.time()-ptm
# message("Process time: ", endtime[['elapsed']])
# 
# 
# samps <- as.mcmc(jagsfit.p)
# 
# # Diagnostics -------------------------------------------------------------
# if (exists('diagnostics')) {
#   browser()
# # Gelman and Rubin's convergence diagnostic
# 
#   gelman.diag(samps, multivariate = TRUE)
# 
# # Posterior densities, trace plots and auto-correlation diagnostics
# # Showing only derived variables
# # mcmcplot(samps,  parms = c("CC","Smsy", "Umsy"), dir = "diagnostics")
# 
# 
#   out.dir <- file.path("HBM", "diagnostics", run.ver )
#   
#   if (!dir.exists(out.dir)) dir.create(out.dir)
#   mcmcplot(samps,  parms = c("slope","CC","Smsy", "Umsy"), dir = out.dir)
# }
# # Summaries Statistics ------------------------------------------------
# 
# est <- MCMCsummary(samps, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
#   rownames_to_column("Node") %>%
#   as_tibble %>%
#   mutate(
#     Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
#     Idx = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", "")),
#     StkIdx = ifelse(Parm %in% c("CC", "intercept", "intercept.c", "slope", "se", "Smax") | str_detect(Parm, "msy"), Idx, NA),
#     YrIdx = ifelse(Parm %in% "TE", Idx, NA)
#   ) %>% 
# 
#   left_join(
#     x =.,
#     y = smax.dat %>% select(ID, Basin, Stock),
#     by = c("StkIdx" = "ID")
#   ) %>%
#   # Left join Year
#   left_join(
#     x =.,
#     y = sr.dat %>% select(year, Year) %>% unique,
#     by = c("YrIdx" = "year")
#   ) %>% 
#   arrange(Parm, StkIdx) %>%
#   select(Node,Parm, Basin, Stock, Year,  mean:n.eff) %>%
#   mutate(
#     CV = sd/mean,
#     Model = str_replace(mod.file, "\\.txt", ""),
#     Run = run.name,
#     Description = description
#   )
# 
# 
# 
# 
# # SAVE --------------------------------------------------------------------
# 
# 
# 
# # Save  R data object
# results.obj <- list(
#   basin = basin,
#   sr.data = sr.dat,
#   smax.data = smax.dat,
#   fit = jagsfit.p, 
#   estimates = est, 
#   samples = samps
# )
# saveRDS(results.obj, file = file.path(save.dir, out.file))


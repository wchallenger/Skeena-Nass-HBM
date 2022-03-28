# 
# https://stackoverflow.com/questions/41657817/ggplot-equivalent-of-gelman-plot-mcmc-diagnostic-in-r
# 
diagnostics = c("gelman-rubin plot", "gelman-rubin", "traceplots")
parms <-  c("intercept", "slope")


mod.file = "McAllister_HBM_m23.txt"
run.name = "HBM Skeena Basecase  Diagnostics (m23)"
basin = "Skeena"
inits.ver = 15
description = "HBM base case "
out.file = "result_HBM_Skeena_m23_reduced.rds"


# PREPARE Skeena or Nass Specific analysis
source(file.path(script.dir, paste0("PREP_", basin, "_HBM.R")))


source(file.path(script.dir, "FIT_HBM.R"))

restuls.obj <- readRDS(file.path(save.dir, out.file))
out.file = file.path(save.dir, "gelman-rubin-plots--Skeena_basecase.pdf")
pdf(file = out.file, width=8, height = 11)
par(mfrow = c(5,4))
gelman.plot(results.obj$samples, ylim = c(0.9, 1.1), auto.layout = FALSE)
dev.off()


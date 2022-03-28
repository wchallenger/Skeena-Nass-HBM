# Generate 

source(file.path(fun.dir, "ComputeTimeVaryingAlpha.fn.R"))

# ver <- "2022-03-24"

result.files <- list(
  'Base Case' = "result_HBM_Skeena_m23.rds",
  'nonHBM' = "result_HBM_Skeena_m23_nonHBM.rds",
  'Sensitivity Run 1' = "result_HBM_Skeena_m24.rds",
  'Sensitivity Run 2' = "result_HBM_Skeena_m25.rds",
  'Sensitivity Run 3' = "result_HBM_Skeena_m26.rds",
  'Sensitivity Run 4' = "result_HBM_Skeena_m27.rds",
  'Sensitivity Run 5' = "result_HBM_Skeena_m28.rds",
  'Sensitivity Run 6' = "result_HBM_Skeena_m29.rds",
  'Sensitivity Run 26' = "result_HBM_Skeena_m23_pinkut_fulton_kitwanga.rds"
)


HBM.results <- list()

for (r in seq_along(result.files)) {
  run <- names(result.files)[r]
  message("Processing Run:", run)
  
  mod.result <- readRDS(file.path(save.dir, result.files[r]))
  
  HBM.results[[run]] <- list()
  
  HBM.results[[run]][['estimates']] = mod.result$estimates %>%
    add_column(median = NA, .after = "mean") %>%
    mutate(median = `50%`) %>%
    select(-`50%`) %>%
    mutate(
      Parm = str_replace(Parm, "^intercept$", "ln.alpha"),
      Parm = str_replace(Parm, "^intercept_c$", "ln.alpha.c"),
      Parm = str_replace(Parm, "^slope$", "beta")
    ) %>%
    rename(VarType = Parm) %>%
    filter(VarType %in% c("deviance", "ln.alpha", "ln.alpha.c", "beta", "TE", "Smax", "Smsy", "Umsy" ))
   
    # HBM.results[[run]][['estimates']] %>% count(VarType)
  
  message("  Deriving time varying alpha...")
  HBM.results[[run]][['time.var.alpha']] <- ComputeTimeVaryingAlpha(mod.result)
  
}


HBM.results[['Year Effects RM4']] <- TE.rm4
HBM.results[['Year Effects RM5']] <- TE.rm5


out.file <- file.path(save.dir, "HBM_results.rds")
saveRDS(HBM.results, out.file)


ver <- "2022-03-20"
ver <- "2022-03-23"  # Update Asitka Smax prior

out.dir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM"

library(tidyverse)
library(janitor)
library(openxlsx)


result <- list()
# Sensitivity Run 1 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m24.rds"))$estimates %>% mutate(RunName = "Code Error")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 1']] <- left_join(
  x = est %>% filter(Parm == "se") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    mutate(`Diff mean` = (`Code Error mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "se") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    mutate(`Diff SD` = (`Code Error SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
)

result[['Run 1']]


# Sensitivity Run 2 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m25.rds"))$estimates %>% mutate(RunName = "No Bound")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 2']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`No Bound mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%
    adorn_totals("row") %>%
     mutate(`Diff SD` = (`No Bound SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )

result[['Run 2']]


# Sensitivity Run 3 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m26.rds"))$estimates %>% mutate(RunName = "No TE")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 3']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`No TE mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    adorn_totals("row") %>%
    mutate(`Diff SD` = (`No TE SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )


result[['Run 3']]


# Sensitivity Run 4 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m27.rds"))$estimates %>% mutate(RunName = "No nonHBM")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 4']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`No nonHBM mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    adorn_totals("row") %>%
    mutate(`Diff SD` = (`No nonHBM SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )


result[['Run 4']]


# Sensitivity Run 5 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m28.rds"))$estimates %>% mutate(RunName = "Normal Prior")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 5']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`Normal Prior mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    adorn_totals("row") %>%
    mutate(`Diff SD` = (`Normal Prior SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )


result[['Run 5']]




# Sensitivity Run 6 -------------------------------------------------------

est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m29.rds"))$estimates %>% mutate(RunName = "Vague Prior")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


result[['Run 6']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`Vague Prior mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    adorn_totals("row") %>%
    mutate(`Diff SD` = (`Vague Prior SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )


result[['Run 6']]



# Save Excell -------------------------------------------------------------
wb <- createWorkbook()

for (run in names(result)) {
  
  
  addWorksheet(wb, run)
  writeData(wb, run, result[[run]])
  
  # header
  style <- createStyle(
    border= c("Bottom", "Top"), 
    fgFill = rgb(208,208,208, maxColorValue = 255),
    textDecoration = "bold",
    wrapText = TRUE
  )  
  addStyle(wb, run, style, rows= 1, cols = 1:7, gridExpand = TRUE)
  
  # Percent change column
  if (run %in% c( "Run 2", "Run 3")) {
    style <- createStyle(numFmt = "0.0%")
  } else {
    style <- createStyle(numFmt = "0%")
  }
  addStyle(wb, run, style, rows= 2:(nrow(result[[run]]) + 1), cols = c(4,7), gridExpand = TRUE)
  # Format estimates
  if (run == "Run 1") {
    style <- createStyle(numFmt = "0.000")
  } else {
    style <- createStyle(numFmt = "#,##0")
  }
  addStyle(wb, run, style, rows= 2:(nrow(result[[run]]) + 1), cols = c(2:3,5:6), gridExpand = TRUE)
 
  setColWidths(wb, run, cols = 1:7, widths = 16.5) 
}

out.file <- file.path(out.dir, "appendix_results_JAGS.xlsx")
saveWorkbook(wb, file = out.file, overwrite = TRUE)
system(paste("open", out.file))


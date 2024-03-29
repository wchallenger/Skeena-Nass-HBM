message("Creating comparison tables for sensitivity runs 1-6")

library(tidyverse)
library(janitor)
library(openxlsx)



# Sensitivity Run 1 -------------------------------------------------------


est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m24.rds"))$estimates %>% mutate(RunName = "Code Error")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 1']] <- left_join(
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

# app.tables[['Run 1']]


# Sensitivity Run 2 -------------------------------------------------------


est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m25.rds"))$estimates %>% mutate(RunName = "No Bound")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 2 Smsy']] <- left_join(
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


app.tables[['Run 2 Umsy']] <- left_join(
  x = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%
    # adorn_totals("row") %>%
    mutate(`Diff mean` = (`No Bound mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%
    # adorn_totals("row") %>%
    mutate(`Diff SD` = (`No Bound SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) 



# Sensitivity Run 3 -------------------------------------------------------


est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m26.rds"))$estimates %>% mutate(RunName = "No TE")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 3 Smsy']] <- left_join(
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


app.tables[['Run 3 Umsy']] <- left_join(
  x = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff mean` = (`No TE mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff SD` = (`No TE SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) 


# Sensitivity Run 4 -------------------------------------------------------


est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m27.rds"))$estimates %>% mutate(RunName = "nonHBM")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 4 Smsy']] <- left_join(
  x = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    adorn_totals("row") %>%
    mutate(`Diff mean` = (`nonHBM mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Smsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    adorn_totals("row") %>%
    mutate(`Diff SD` = (`nonHBM SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
) %>%
  mutate( Stock = str_replace(Stock, "^Total$", "Sum Smsy across stocks") )


app.tables[['Run 4 Umsy']] <- left_join(
  x = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff mean` = (`nonHBM mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff SD` = (`nonHBM SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
)

# Sensitivity Run 5 -------------------------------------------------------


est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m28.rds"))$estimates %>% mutate(RunName = "Normal Prior")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 5 Smsy']] <- left_join(
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


app.tables[['Run 5 Umsy']] <- left_join(
  x = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff mean` = (`Normal Prior mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff SD` = (`Normal Prior SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
)


# Sensitivity Run 6 -------------------------------------------------------

est <- rbind(
  readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "Base Case"),
  readRDS(file.path(save.dir, "result_HBM_Skeena_m29.rds"))$estimates %>% mutate(RunName = "Vague Prior")
) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))


app.tables[['Run 6 Smsy']] <- left_join(
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


app.tables[['Run 6 Umsy']] <- left_join(
  x = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff mean` = (`Vague Prior mean` - `Base Case mean`)/`Base Case mean`),
  y = est %>% filter(Parm == "Umsy") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) %>%  
    # adorn_totals("row") %>%
    mutate(`Diff SD` = (`Vague Prior SD` - `Base Case SD`)/`Base Case SD`),
  by = "Stock"
)



# system(paste("open", out.file))


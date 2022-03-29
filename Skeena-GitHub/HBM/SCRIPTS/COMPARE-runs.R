library(tidyverse)
library(scales)
# Settings ----------------------------------------------------------------


# COMPARE RUNS
run1 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-18/result_HBM_McAllister_m23.rds"
run2 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-20/result_HBM_Skeena_m23.rds"



run1 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-26/result_HBM_Skeena_m23.rds"
run1.name <- "All Stocks Included"
run2 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-26-noenhanced/result_HBM_Skeena_m23.rds"
run2.name <- "Excluding Enhanced Stocks"

# IMPORT -------------------------------------------------------------------------

run1 <- readRDS(run1)
run2 <- readRDS(run2)

all.est <- rbind(
  run1$estimates %>% mutate(Run = "Run 1"),
  run2$estimates %>% mutate(Run = "Run 2")
)


# Fundamental Parameters --------------------------------------------------



# run1$estimates %>% filter(Parm == "intercept")
# run2$estimates %>% filter(Parm == "intercept")
# 
left_join(
  x = all.est  %>% 
    filter(Parm %in% c("intercept", "slope", "se", "TE")) %>%
    select(Parm, Year, Stock, mean, Run) %>%
    spread( Run, mean),
  y = all.est  %>% 
    filter(Parm %in% c("intercept", "slope", "se", "TE")) %>%
    select(Parm, Year, Stock, `2.5%`, Run) %>%
    spread( Run, `2.5%`) %>%
    rename(Run1_p2.5 = `Run 1`, Run2_p2.5 = `Run 2`),
  by = c("Parm", 'Year', "Stock")
) %>%
  left_join(
    x = .,
    y = all.est  %>% 
      filter(Parm %in% c("intercept", "slope", "se", "TE")) %>%
      select(Parm, Year, Stock, `97.5%`, Run) %>%
      spread( Run, `97.5%`) %>%
      rename(Run1_p97.5 = `Run 1`, Run2_p97.5 = `Run 2`),
    by = c("Parm", 'Year', "Stock")
  )  %>% 
  mutate(Label = ifelse(is.na(Year), Stock, Year)) %>%
  mutate(Parm = str_replace(Parm, "se", "sigma")) %>%
  ggplot(., aes(x=`Run 1`, y=`Run 2`)) + 
  geom_errorbar(aes( ymin = Run2_p2.5, ymax = Run2_p97.5 ), color = "grey50") +
  geom_errorbarh(aes(xmin = Run1_p2.5, xmax =  Run1_p97.5),  color = "grey50") +
  geom_point() +
  geom_text_repel(aes(label=Label), size=3.2) +
  geom_abline(intercept  =0, slope = 1) +
  facet_wrap(~Parm, scales = "free") +
  theme_bw(14) +
  labs(
    title = "Fundamental Parameter Estimates",
    x = run1.name,
    y = run2.name
  )


library(ggrepel)
left_join(
  x = all.est  %>% 
    filter(Parm %in% c("Smsy", "Umsy", "Smax")) %>%
    # mutate()
    select(Parm, Year, Stock, mean, Run) %>%
    spread( Run, mean),
  y = all.est  %>% 
    filter(Parm %in% c("Smsy", "Umsy", "Smax")) %>%
    select(Parm, Year, Stock, `2.5%`, Run) %>%
    spread( Run, `2.5%`) %>%
    rename(Run1_p2.5 = `Run 1`, Run2_p2.5 = `Run 2`),
  by = c("Parm", 'Year', "Stock")
  ) %>%
  left_join(
    x = .,
    y = all.est  %>% 
      filter(Parm %in% c("Smsy", "Umsy", "Smax")) %>%
      select(Parm, Year, Stock, `97.5%`, Run) %>%
      spread( Run, `97.5%`) %>%
      rename(Run1_p97.5 = `Run 1`, Run2_p97.5 = `Run 2`),
    by = c("Parm", 'Year', "Stock")
  )  %>% 
    ggplot(., aes(x=`Run 1`, y=`Run 2`)) + 
  geom_errorbar(aes( ymin = Run2_p2.5, ymax = Run2_p97.5 ), color = "grey50") +
  geom_errorbarh(aes(xmin = Run1_p2.5, xmax =  Run1_p97.5),  color = "grey50") +
  geom_point() +
  geom_text_repel(aes(label=Stock), size=3.2) +
    geom_abline(intercept  =0, slope = 1) +
    facet_wrap(~Parm, scales = "free") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = comma) +
    theme_bw(14) +
    labs(
      title = "Reference Endpoint Estimates",
      x = run1.name,
      y = run2.name
    )
  
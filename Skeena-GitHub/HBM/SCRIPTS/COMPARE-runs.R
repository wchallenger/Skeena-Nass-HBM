

# Settings ----------------------------------------------------------------


# COMPARE RUNS
run1 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-18/result_HBM_McAllister_m23.rds"
run2 <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-20/result_HBM_Skeena_m23.rds"



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
all.est  %>% 
  filter(Parm %in% c("intercept", "slope", "se", "TE")) %>%
  # mutate()
  select(Parm, Year, Stock, mean, Run) %>%
  spread( Run, mean) %>%
  ggplot(., aes(x=`Run 1`, y=`Run 2`)) + geom_point() +
  geom_abline(intercept  =0, slope = 1) +
  facet_wrap(~Parm, scales = "free") +
  theme_bw(14) +
  labs(
    title = "Fundamental Parameter Estimates"
  )

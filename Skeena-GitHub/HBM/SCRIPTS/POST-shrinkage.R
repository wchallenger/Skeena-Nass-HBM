# create shrinkage table

ver

# Sensitivity Run 1 -------------------------------------------------------


est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23_nonHBM.rds"))$estimates %>% mutate(RunName = "nonHBM"),
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))$estimates %>% mutate(RunName = "HBM")
 ) 



result[['Shrinkage']] <- left_join(
  x = est %>% filter(Parm == "intercept") %>%
    select(RunName, Stock, `50%`) %>%
    mutate(RunName = paste(RunName, "median")) %>%
    spread(RunName, `50%`) ,
  y = est %>% filter(Parm == "intercept") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) ,
  by = "Stock"
) %>%  
  select(Stock, `nonHBM median`, `HBM median`, `nonHBM SD`, `HBM SD`) %>%
  mutate(
    `Shrinkage` = ( `HBM median` - `nonHBM median`)/`nonHBM median`,
    `Diff SD` = ( `HBM SD` - `nonHBM SD`)/`nonHBM SD`
  )
result[['Shrinkage']]



result[['Shrinkage']]  %>%
  select(Stock, `nonHBM median`, `HBM median`) %>%
  gather(key=Model, value = Median, ends_with("median")) %>%
  mutate(
    Model = str_replace(Model, "[\\.[:space:]]median", "") %>% factor(levels = c("nonHBM", "HBM"))
  ) %>%
  ggplot(., aes(x=Model, y = Median)) +
  geom_point(aes(color = Stock)) +
  geom_line(aes(group = Stock, color = Stock)) +
  theme_classic(14) +
  theme(legend.title = element_blank()) +
  labs(
    y = "Posterior Median"
  )



result[['Shrinkage']] <- left_join(
  x = est %>% filter(Parm == "intercept") %>%
    select(RunName, Stock, mean) %>%
    mutate(RunName = paste(RunName, "mean")) %>%
    spread(RunName, mean) ,
  y = est %>% filter(Parm == "intercept") %>%
    select(RunName, Stock, sd) %>%
    mutate(RunName = paste(RunName, "SD")) %>%
    spread(RunName, sd) ,
  by = "Stock"
) %>%  
  select(Stock, `nonHBM mean`, `Base Case mean`, `nonHBM SD`, `Base Case SD`) %>%
  mutate(
    `Shrinkage` = ( `Base Case mean` - `nonHBM mean`)/`nonHBM mean`,
    `Diff SD` = ( `Base Case SD` - `nonHBM SD`)/`nonHBM SD`
  )





post <- read.csv("/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/reporting-comparisons/FullSet_Posterior_Summary.csv")

str(post)



dat <- post %>% filter(Stock == "Alastair")

dat %>% count(Model)


x <- dat %>% 
  mutate(
    Q1 = p25,
    Median = p50,
    Q3 = p75,
    IQR = Q3-Q1,
    Min = Q1 - 1.5*IQR,
    Max = Q3 + 1.5*IQR
) %>%
  filter(VarType == 'beta') %>%
  select(Stock, Model, Min, Q1, Median, Q3, Max)

box.dat <- list(
  stats = x %>% select(Min:Max) %>% t %>% as.matrix,
  names = x$Model
)

par(mar = c(10, 4, 4, 2) + 0.1)
bxp(box.dat, las=2, main =x$Stock %>% unique, ylab = "beta")





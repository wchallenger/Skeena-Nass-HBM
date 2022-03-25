# SAVE --------------------------------------------------------------------

# RES DOC appendix
smax.appendix <- smax.tab %>% select(Basin, Stock, Lake, prSmax, prCV) %>%
  arrange(desc(Basin), Stock)


library(openxlsx)
WinBUG.runs <- loadWorkbook("/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/appendix_results_WinBUGS.xlsx")

app.result <- list(
  "Smax" = smax.appendix,
  "Shrinkage" = read.xlsx(WinBUG.runs, sheet = "Shrinkage"),
  "Run 1" = read.xlsx(WinBUG.runs, sheet = "Run 1"),
  "Run 2" = read.xlsx(WinBUG.runs, sheet = "Run 2"),
  "Run 3" = read.xlsx(WinBUG.runs, sheet = "Run 3"),
  "Run 4" = read.xlsx(WinBUG.runs, sheet = "Run 4"),
  "Run 5" = read.xlsx(WinBUG.runs, sheet = "Run 5"),
  "Run 6" = read.xlsx(WinBUG.runs, sheet = "Run 6")
)


saveRDS(app.result, "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-Sk-Model-Report/REPORT/data/HBM/HBM_appendix_tables.rds")
saveRDS(app.result, "/Users/wchallenger/GitHub/Fisheries/Skeena-TEST/data/HBM/HBM_appendix_tables.rds")

app.result$Shrinkage %>%
  select(Stock, nonHBM.median, HBM.median) %>%
  gather(key=Model, value = Median, ends_with("median")) %>%
  mutate(
    Model = str_replace(Model, "\\.median", "") %>% factor(levels = c("nonHBM", "HBM"))
  ) %>%
  ggplot(., aes(x=Model, y = Median)) +
  geom_point(aes(color = Stock)) +
  geom_line(aes(group = Stock, color = Stock)) +
  theme_classic(14) +
  theme(legend.title = element_blank()) +
  labs(
    y = "Median Ricker a parameter"
  )
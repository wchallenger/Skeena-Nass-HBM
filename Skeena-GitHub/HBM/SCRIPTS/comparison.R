library(scales)

ver <- "2022-03-02"

est <- rbind(
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m24.rds"))$estimates,
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m25.rds"))$estimates,
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m26.rds"))$estimates,
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m27.rds"))$estimates,
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m28.rds"))$estimates,
  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_McAllister_m29.rds"))$estimates
 ) %>%
  mutate(Type = str_extract(Model, "m[:digit:]+"))

est <-   rbind(est, readRDS("HBM/result_HBM_KormanEnglish.rds") %>% mutate(Type="K&E"))

p <-   est %>%
  filter(Parm == "Smsy") %>%
  ggplot(., aes(y=Stock, x=Mean)) + 
  geom_errorbar(aes(xmin=`2.5%`, xmax = `97.5%`, color = Type, group = Type), width=0, position =  position_dodge(0.9))+
  geom_bar(aes(fill = Type), stat="identity", position = position_dodge()) +
  scale_x_continuous(labels=comma) +
  scale_y_discrete(limits=rev)+
  theme_classic(14) +
  # theme(
  #   # 
  #   legend.title = element_blank(),
  #   # legend.position = "top"
  #   legend.justification =  c(1,0.5),
  #   legend.position =  c(1,0.5),
  #   legend.box.margin = margin(1,1,1,1, unit = "pt")
  # ) +
  labs(
    title = "Sustained Maximum Yield",
    x = expression(S[MSY])
  )


p %+% filter(est, Parm %in% c("a", "intercept") ) + theme(legend.position = 'none',axis.title.y = element_blank()) + labs(x = expression(alpha), title = "A) Productivity")
p %+% filter(est, Parm %in% c("b", "slope") )+ theme(axis.title.y = element_blank())+labs(x = expression(beta), title = "B) Density-dependency")
p  + theme(legend.position = 'top') + theme(axis.title.y = element_blank())+ labs(  title = "Spawning Stock Size at Maximum Sustainable Yield")







library(grid)
library(gridExtra)

# # create and displayb arrangement
# grid.arrange(
#   g <- arrangeGrob(
#     p %+% filter(est, Parm %in% c("a", "intercept") ) + 
#       theme(legend.position = 'none', axis.title.y = element_blank()) + labs(x = expression(alpha), title = "A) Productivity"),
#     p %+% filter(est, Parm %in% c("b", "slope") )+ 
#       theme(axis.title.y = element_blank())+
#       labs(x = expression(beta), title = "B) Density-dependency"),
#     p  + 
#       theme(legend.position = 'none', axis.title.y = element_blank()) +
#       labs(  title = "Sustained Maximum Yield"),
#     nrow=3, ncol=1,
#     as.table = F,
#     left = textGrob("Stock", rot=90,  gp=gpar(fontsize=15)),
#     # bottom = textGrob("X-axis Title",  gp=gpar(fontsize=15)), 
#     clip=F
#   ))


p %+% filter(est, Parm %in% c("a", "intercept") ) + theme(legend.position = 'right',axis.title.y = element_blank()) + labs(x = expression(alpha), title = "A) Productivity", fill="Run", color="Run")
p %+% filter(est, Parm %in% c("b", "slope") )+ theme(axis.title.y = element_blank())+labs(x = expression(beta), title = "B) Density-dependency", fill="Run", color="Run")
p  + theme(legend.position = 'right') + theme(axis.title.y = element_blank())+ labs(  title = "C) Spawning Stock Size at Maximum Sustainable Yield", fill="Run", color="Run")





# Gottfried Estimates -----------------------------------------------------
library(openxlsx)
read.xlsx(
  xlsxFile = "/Users/wchallenger/OneDrive - LGL Limited/Projects/Fisheries/Skeena-Nass-SX/Meetings/2021/2021-12-14/TWG_Dec14_PrelimOutputs.xlsx",
  sheet = "TAB 1 - AltSRDataSets"
) %>%
  filter(DataVersion == "Main") %>%
  select(Stock, Smsy)

x <- left_join(
  x = est %>% filter(Parm == "Smsy"),
  y = read.xlsx(
    xlsxFile = "/Users/wchallenger/OneDrive - LGL Limited/Projects/Fisheries/Skeena-Nass-SX/Meetings/2021/2021-12-14/TWG_Dec14_PrelimOutputs.xlsx",
    sheet = "TAB 1 - AltSRDataSets"
  ) %>%
    filter(DataVersion == "Main") %>%
    filter(str_detect(Model, "AllYr$")) %>%
    select(Stock, Smsy),
  by = "Stock"
) %>%
  filter(!is.na(Smsy))%>%
  filter(Type != "Rev1")
#filter(str_detect(Type, "McAl")) 
library(ggrepel)
ggplot(x, aes(x=Smsy, y=Mean)) + 
  geom_errorbar(aes(ymin = `2.5%`, ymax=`97.5%`, color=Type), width=0) +
  geom_point(aes(shape=Type, color=Type), size=2) +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+
  geom_abline(intercept = 0, slope=1) +
  geom_text_repel(data = x %>% filter(Parm == "Smsy", Type == "m24"), aes(label=Stock), size=3.2) + 
  theme_bw(14) +
  scale_y_continuous(labels = comma)+
  theme(
    legend.key.width = unit(2, "lines"),
    legend.position = "right",
    # legend.title = element_blank(),
    # legend.justification =  c(1,1), 
    # legend.position =  c(1,1),
    legend.box.margin = margin(1,1,1,1, unit = "pt")
  ) +
  labs(
    y = expression(paste(S[MSY], " Estimated by HBM model")),
    x = expression(paste(S[MSY], " - Gottfried Preliminary (Main Data, All Yrs)")),
    color = "Run",
    shape = "Run"
  )

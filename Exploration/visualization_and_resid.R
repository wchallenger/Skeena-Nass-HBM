library(tidyverse)
library(ggrepel)

# Load Data ---------------------------------------------------------------
data.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox/DATA"

stock.info <- read.csv(file.path(data.dir, "StockInfo_Main.csv")) %>% as_tibble

stock.dat <-  left_join(
  x = read.csv(file.path(data.dir, "StockData_Main.csv")),
  y = stock.info %>% select(Stock, Basin, LifeHistory),
  by = "Stock"
) %>%as_tibble

ggplot(stock.dat, aes(x=EffSpn/1000, y=RecTotalNA/EffSpn)) +
  geom_point() +
  facet_wrap(~Basin+Stock, scales="free") +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  labs(
   title =  "Spawner/Recruit Relationship",
    x = "Spawners (thousands)"
  )


x <- stock.dat %>% filter(Basin == "Skeena") %>%
  filter(!is.na(Rec), !is.na(Spn) )%>%
  mutate(
    Status = ifelse(log(Rec/Spn) < 4, "Included", "Excluded"),
    Status = ifelse(
      test = Stock %in% c("Pinkut", "Fulton") & Year < 1960,
      yes = "Excluded",
      no = Status
    ),
    Status = factor(
      x = Status,
      levels = c("Included", "Excluded")
    )
  )
  # filter(log(Rec/Spn) < 4) %>%
  ggplot(x, aes(x=Spn/1000, y=Rec/Spn)) +
  # geom_point(
  #   data = filter(x, Status == "Included"),
  #   mapping = aes(shape = Status, color=Status)
  # ) +
  geom_point(aes(shape = Status, color=Status)) +
  facet_wrap(~Stock, scales="free") +
  geom_smooth(data= filter(x, Status == "Included"), mapping =  aes(group = Status),method = "lm") +
  scale_y_log10(labels = comma) +
  scale_color_manual(values = c("Included" = "black", Excluded = "orangered1")) +
  theme_bw(14) +
  theme(
    # legend.title = element_blank(),
    legend.justification =  c(0.9,0),
    legend.position =  c(0.9,0),
    legend.box.margin = margin(1,1,1,1, unit = "pt")
  ) +
  labs(
    # title =  "Spawner/Recruit Relationship",
    title = "Skeena Stock-recruit data",
    # subtitles
    y = "Recruit/Spawner",
    x = "Spawners (thousands)",
    color = "Analysis Data", shape = "Analysis Data"
  )

out.file <- "~/GitHub/Fisheries/Skeena-Nass-HBM/Exploration/recruit-spawner.png"
ggsave(filename = out.file, width=12, height = 8)




# Residual Plots ----------------------------------------------------------


fit <- lm(log(Rec/Spn)~Stock*EffSpn, data = stock.dat)
residuals <- resid(fit)
stock.dat$Resid = NA
stock.dat$Resid [ match(names(residuals),rownames(stock.dat))] <- residuals



hist(stock.dat$Resid,45)

# Outlier years labeled
stock.dat %>%
  mutate(OutlierYr = ifelse(abs(Resid)>2, Year, NA)) %>%
  ggplot(., aes(x=Spn/1000, y=Rec/Spn)) +
  geom_point() +
  geom_text_repel(aes(label=OutlierYr), size=3.2) +
  geom_smooth(method = "lm", formula = y~poly(x,1)) +
  facet_wrap(~Stock, scales="free") +
  scale_y_log10() +
  labs(
    title =  "Spawner/Recruit Relationship",
    x = "Spawners (thousands)"
  )




stock.dat %>%
  filter(!is.na(Basin)) %>%
  # filter(!is.na(Resid)) %>%
  ggplot(., aes(x=Year, y = Resid)) +
  geom_point(aes(Shape = Basin, color = Stock)) +
  # geom_polygon(
  #   data = data.frame(
  #     Year = c(rep(2005,2), rep(2014,2)),
  #     Resid = c(-1,1,1,-1)*4
  #   ),
  #   alpha = 0.21
  # ) +
  geom_line(aes(color=Stock)) +
  facet_wrap(~Basin, ncol=1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1940, 2021, by=5))+
  coord_cartesian(xlim = c(1960,2016)) +
  theme_bw(14) +
  labs(
    title = "Multi-stock Ricker Model Residuals",
    y = "Residual Log Recruits/Spawners"
  )





stock.dat %>%
  filter(!is.na(Basin)) %>%
  group_by(Year, Basin) %>%
  summarise(
    n=sum(!is.na(Resid)),
    Resid.avg = mean(Resid, na.rm=TRUE),
    Resid.SE =  sd(Resid, na.rm = TRUE)/sqrt(sum(!is.na(Resid))),
    .groups = "drop"
  ) %>%
  ggplot(., aes(x=Year, y = Resid.avg)) +
  geom_polygon(
    data = data.frame(
      Year = c(rep(2005,2), rep(2014,2)),
      Resid.avg = c(-1,1,1,-1)*4
    ),
    alpha = 0.21
  ) +
  # geom_errorbar(aes(ymin = Resid.avg - 2*Resid.SE, ymax = Resid.avg + 2*Resid.SE, color=Basin), width=0) +
  # geom_point(aes(shape = Basin, color = Basin)) +
  # geom_line(aes(color=Basin)) +
  geom_errorbar(aes(ymin = Resid.avg - 2*Resid.SE, ymax = Resid.avg + 2*Resid.SE), width=0) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1940, 2021, by=5))+
  coord_cartesian(xlim = c(1960,2016), ylim=c(-1,1)*2.5) +
  facet_wrap(~Basin, ncol = 1) +
  theme_bw(14) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Multi-stock Ricker Residuals",
    y = "Average Residual Log Recruits/Spawners"
  )



x <- stock.dat %>%
  filter(!is.na(Basin)) %>%
  group_by(Year, Basin) %>%
  summarise(
    n=sum(!is.na(Resid)),
    Resid.avg = mean(Resid, na.rm=TRUE),
    Resid.SE =  sd(Resid, na.rm = TRUE)/sqrt(sum(!is.na(Resid))),
    .groups = "drop"
  ) 


x %>% select(Year, Basin, Resid.avg) %>% filter(!is.na(Resid.avg)) %>% spread(key=Basin, value=Resid.avg) %>%
  x %>% mutate(LCL = Resid.avg-2*Resid.SE)  select(Year, Basin, LCL) %>% filter(!is.na(Resid.SE)) %>% spread(key=Basin, value=Resid.SE) 
  ggplot(., aes(x=Skeena, y=Nass)) + 
  coord_cartesian(ylim=c(-1.5,1.5), xlim=c(-1.5,1.5))+
  geom_point() +
  geom_text_repel(aes(label=Year), size=3.2) + 
  geom_smooth(method = "lm", formula = y~poly(x,1)) +
  theme_classic(14) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(
    x = "Skeena Residual Log Recruit/Spawner",
    y = "Nass Residual Log Recruit/Spawner"
  )




x <- stock.dat %>%
  # filter(Year >=2005) %>%
  filter(!is.na(Resid))

acf(x$Resid)


x <- stock.dat %>%
  filter(!is.na(Basin)) %>%
  group_by(Year, Basin) %>%
  summarise(
    n=sum(!is.na(Resid)),
    Resid.avg = mean(Resid, na.rm=TRUE),
    Resid.SE =  sd(Resid, na.rm = TRUE)/sqrt(sum(!is.na(Resid))),
    .groups = "drop"
  ) %>%
  filter(!is.na(Resid.avg)) %>%
  filter(Basin == "Skeena") %>%
  filter(Year >=2005)
acf(x$Resid.avg)
pacf(x$Resid.avg)





stock.dat %>%
  filter(!is.na(Basin)) %>%
  group_by(Year, Basin) %>%
  summarise(
    n=sum(!is.na(Resid)),
    Resid.avg = mean(Resid, na.rm=TRUE),
    Resid.SE =  sd(Resid, na.rm = TRUE)/sqrt(sum(!is.na(Resid))),
    .groups = "drop"
  ) %>%
  filter(Year >=1975) %>%
  ggplot(., aes(x=Year, y = Resid.avg)) +
  geom_errorbar(aes(ymin = Resid.avg - 2*Resid.SE, ymax = Resid.avg + 2*Resid.SE), width=0) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1940, 2021, by=5))+
  coord_cartesian(xlim = c(1975,2016), ylim=c(-1,1)*2.5) +
  facet_wrap(~Basin, ncol = 1) +
  geom_smooth(method = "lm") +
  theme_bw(14) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Multi-stock Ricker Residuals",
    y = "Average Residual Log Recruits/Spawners"
  )



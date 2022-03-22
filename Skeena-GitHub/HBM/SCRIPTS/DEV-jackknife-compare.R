library(tidyverse)
library(scales)


basecase <-  readRDS("../result_HBM_McAllister_m23.rds")
basecase %>% count(Parm)



load.dir <- "_archive"
load.dir <- "."
result <- all.samps <-  NULL
for (r in 1:19){
  message("Loading run ", r)
  load.file <- file.path(load.dir, paste0("jacknife_",r,"_est.rds"))
  est <- readRDS(load.file)
  result <- bind_rows(result, est)
  load.file <- file.path(load.dir, paste0("jacknife_",r,"_samps.rds"))
  samps <-  readRDS(load.file)  %>%
    filter(ParmClass == "intercept_new") %>%
     mutate(Excluded = est$Excluded %>% unique)
  all.samps <- rbind(all.samps, samps)
}


all.samps %>%
  filter(ParmClass == "intercept_new") %>%
  ggplot(., aes(x=Value)) +
  geom_density(aes(color = Excluded, fill=Excluded), alpha=0.1) +
  coord_cartesian(xlim = c(0,5)) +
  theme_classic(14) +
  theme(
    # legend.title = element_blank(),
    legend.justification =  c(1,1), 
    legend.position =  c(1,1),
    legend.box.margin = margin(1,1,1,1, unit = "pt")
  ) +
  labs(
    title = "Productivity Hyper-prior",
    y = "Density",
    x = expression(paste("Intercept (", alpha, ")"))
  )


result %>% filter(Parm == "TE") %>% 
  mutate(Year = StkID + 1960 - 1) %>%
  ggplot(., aes(x=Year, y=Mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(aes(color = Excluded)) +
  geom_line(
    data = basecase %>% filter(Parm == "TE"), 
    mapping = aes(x= StkID + 1960 - 1, y=Mean), 
    size = 0.75
  ) +
  # geom_line(aes(color = Excluded)) +
  scale_x_continuous(breaks = seq(1960, 2020, by=5)) +
  theme_classic(14) +
  labs(
    y = "Shared Year Effect"
  )

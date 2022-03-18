rm(list=ls())
theme_set(theme_classic(14))
library(tidyverse)
library(MCMCvis)
setwd("~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub")

# result.ver <- "2022-03-16"
# result.ver <- "2022-03-16-trial"

result.ver <- "2022-03-18"

mod.result <- readRDS(file.path("HBM", "RESULTS", result.ver, "result_HBM_McAllister_m23.rds"))

# est <- MCMCsummary(mod.result$samples, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
#   rownames_to_column(var = "Node") %>%
#   mutate(
#     Parm = str_extract(Node, "[[:alpha:]_]+"),
#     Idx = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
#   ) 

est <- mod.result$estimates
sr.dat <- mod.result$sr.data
smax.dat <- mod.result$smax.data
samps <- mod.result$samples



# combine chains ----------------------------------------------------------


all.samps <- NULL
for (chain in 1:length(samps)) {
  n.it <- nrow(samps[[chain]])
  all.samps <- rbind(
    all.samps, 
    as.data.frame(samps[[chain]]) %>%
      mutate(
        chain=chain,  
        it = seq_len(n.it),
        id = paste(chain, it, sep="-")
      )
  )
}



# Tau_a -------------------------------------------------------------------


all.samps %>% 
  as_tibble %>%
  select( starts_with("tau_a")) %>%
  gather(key = Parm, value = Value) %>%
  mutate(
    Type = ifelse(str_detect(Parm, "_x$"), "Prior", "Posterior")
  ) %>%
  ggplot(., aes(x=Value)) + geom_density(aes(fill=Type), alpha = 0.8, color=NA)+
  labs(
    x = expression(tau[a])
  )
data.frame(Value = rgamma(1e5, 0.5, 0.5)) %>% ggplot(., aes(x=Value)) + geom_density()



# Productivity Parameter --------------------------------------------------


all.samps %>% 
  # head %>%
  as_tibble %>%
  select( starts_with("intercept")) %>%
  gather(key = Node, value = Value) %>%
  filter(str_detect(Node, "_c") == FALSE) %>%
  mutate(
    Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
    StkIdx = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", "")),
    Type = ifelse(str_detect(Parm, "_x$"), "Prior", "Posterior"),
    Type = factor(Type, levels = c("Prior", "Posterior"))
  ) %>%
  left_join(
    x = .,
    y = smax.dat %>% select(ID, Stock),
    by = c("StkIdx" = "ID")
  ) %>%
  
  ggplot(., aes(x=Value)) + geom_density(aes(fill=Type), alpha = 0.8, color=NA)+
  # facet_wrap(~Stock, scales="free_y") +
  facet_wrap(~Stock) +
  coord_cartesian(xlim = c(0,5)) +
  theme_bw(14) +
  theme(
    legend.title = element_blank(),
    legend.justification =  c(0.75,0), 
    legend.position =  c(0.75,0),
    legend.box.margin = margin(1,1,1,1, unit = "pt"),
    # --- panel ---
    panel.spacing = unit(-.01, "lines"),
    panel.border = element_rect( color="black", size=0.5, fill=NA),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, colour = "grey92"),
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.x = element_line(size=0.5, colour = "grey92"),
    #--- panel strip ---
    strip.background = element_rect(color="black", size=0.5, fill="grey85"),
    strip.placement = "inside"
  ) +
  labs(
    title = "Productivity Parameter",
    y = "Density",
    x = expression(log(alpha))
  )



# Slope -------------------------------------------------------------------
sr.scale <- 1e3

all.samps %>% 
  # head(n=2000) %>%
  as_tibble %>%
  select( starts_with("slope")) %>%
  gather(key = Node, value = Value) %>%
  mutate(
    Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
    StkIdx = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", "")),
    Type = ifelse(str_detect(Parm, "_x$"), "Prior", "Posterior"),
    Type = factor(Type, levels = c("Prior", "Posterior"))
  ) %>%
  left_join(
    x = .,
    y = smax.dat %>% select(ID, Stock),
    by = c("StkIdx" = "ID")
  ) %>%
  # filter(Stock == "Babine Mid Wild") %>%
  # filter(Stock == "Morice") %>%
  filter(Stock %in% c("Babine Mid Wild", "Morice")) %>%
  group_by(Stock) %>% summarize(
    Max = max((1/Value)/sr.scale),
    Mean = mean(1/Value)/sr.scale)
  )

  ggplot(., aes(x=(1/Value)/sr.scale)) + geom_density(aes(fill=Type), alpha = 0.8, color=NA)+
  facet_wrap(~Stock, scales="free") +
  # facet_wrap(~Stock) +
  # coord_cartesian(xlim = c(0,1)*0.5*1e6) +
  theme_bw(14) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    # legend.justification =  c(0.75,0), 
    # legend.position =  c(0.75,0),
    legend.box.margin = margin(1,1,1,1, unit = "pt"),
    # --- panel ---
    panel.spacing = unit(-.01, "lines"),
    panel.border = element_rect( color="black", size=0.5, fill=NA),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size=0.5, colour = "grey92"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_line(size=0.5, colour = "grey92"),
    #--- panel strip ---
    strip.background = element_rect(color="black", size=0.5, fill="grey85"),
    strip.placement = "inside"
  ) +
  labs(
    title = "Capacity Parameter",
    y = "Density",
    x = expression(paste(S[max], " (thousands)")),
  )


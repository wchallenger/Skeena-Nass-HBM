message("Generating jackknife plots")
# rm(list=ls())
library(tidyverse)
library(scales)

# base.run <- "~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/RESULTS/2022-03-21/result_HBM_Skeena_m23.rds"
# jackknife.dir <- "~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub/HBM/jackknife/_archive/2022-02-15-shared"
# save.dir <- "~/GitHub/Fisheries/Skeena-TEST/figure"



# Get Base Case samples -------------------------------------------------------


basecase.run <-  readRDS(file.path("HBM", "RESULTS", ver, "result_HBM_Skeena_m23.rds"))
# basecase.run$estimates %>% count(Parm)

samps <- basecase.run$samples
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
basecase <- all.samps %>%
  # head %>%
  select(id, starts_with("TE"), starts_with("intercept_new")) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    ParmClass = str_extract(Node, "[[:alpha:]_]+"),
    YrIdx = str_extract(Node, "\\[[:digit:]+") %>% 
      str_replace(., pattern ="\\[", replacement =  "" ) %>% 
      as.numeric
  ) %>% 
  left_join(
    x =.,
    y = basecase.run$sr.data %>% select(year, Year),
    by = c("YrIdx" = "year")
  ) 


# Load Jackknife runs -----------------------------------------------------


load.dir <- file.path(save.dir, "jackknife")
result <- all.samps <-  NULL
for (r in 1:19){
  message("Loading jackknife run ", r)
  load.file <- file.path(load.dir, paste0("jacknife_",r,"_est.rds"))
  est <- readRDS(load.file) %>%
    mutate(Excluded = paste("Run", r + 6, "-", Excluded))
  result <- bind_rows(result, est)
  load.file <- file.path(load.dir, paste0("jacknife_",r,"_samps.rds"))
  samps <-  readRDS(load.file)  %>%
    filter(ParmClass == "intercept_new") %>%
     mutate(Excluded =  est$Excluded %>% unique)
  all.samps <- rbind(all.samps, samps)
}

jackknife.samps <- all.samps; rm(all.samps)
jackknife.est <- result; rm(result)



# Determine factor ordering by run number
run.levels <- jackknife.est %>% 
  select(Excluded) %>% 
  unique %>%
  mutate(
    Run = str_extract(Excluded, "[:digit:]+") %>% as.numeric
  ) %>%
  arrange(Run)

jackknife.samps <- jackknife.samps %>%
  mutate(Excluded = factor(Excluded, levels = run.levels$Excluded))

jackknife.est <- jackknife.est %>%
  mutate(Excluded = factor(Excluded, levels = run.levels$Excluded))


# Intercept Plot ----------------------------------------------------------


p.int <- jackknife.samps %>%
  filter(ParmClass == "intercept_new") %>%
  ggplot(., aes(x=Value)) +
  geom_density(
    data=filter(basecase, ParmClass == "intercept_new") %>% mutate(`Base Case` = "Base Case"), 
    mapping = aes(x=Value), 
    fill="grey35"
  ) +
  geom_density(aes(color = Excluded)) +  #fill=Excluded , alpha=0.15
  coord_cartesian(xlim = c(0,5)) +
  theme_classic(14) +
  theme(
    # legend.title = element_blank(),
    legend.justification =  c(1,1), 
    legend.position =  c(1,1),
    legend.box.margin = margin(1,1,1,1, unit = "pt")
  ) +
  labs(
    # title = "Productivity Hyper-prior",
    y = "Density",
    # x = expression(paste("Intercept (", alpha, ")"))
    x = expression(paste("Ricker ", a, ""))
  )

app.plots[['Productivity Jackknife']] <- p.int


out.file <- file.path(output.dir, "Appendix-HBM-productivity-jackknife.pdf")
ggsave(out.file, width=9, height=6.5, plot=p.int)

rm(jackknife.samps)
# Shared Year Effects -----------------------------------------------------



p.TE <- jackknife.est %>% filter(Parm == "TE") %>% 
  ggplot(., aes(x=Year, y=mean)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(aes(color = Excluded)) +
  geom_line(
    data = basecase.run$estimates %>% filter(Parm == "TE"), 
    mapping = aes(x= Year, y=mean), 
    size = 0.75,
    color = "grey35"
  ) +
  # geom_line(aes(color = Excluded)) +
  scale_x_continuous(breaks = seq(1960, 2020, by=5)) +
  theme_classic(14) +
  labs(
    y = "Shared Year Effect"
  )

app.plots[['TE Jackknife']] <- p.TE
out.file <- file.path(output.dir, "Appendix-HBM-yeareffect-jackknife.pdf")
ggsave(out.file, width=9, height=6.5, plot=p.TE)

rm(jackknife.est)

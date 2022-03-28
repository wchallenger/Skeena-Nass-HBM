library(tidyverse)
library(zoo)
message("Creating shared year effect plots")
mod.result <- readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))
samps <- mod.result$samples
sr.dat <- mod.result$sr.data
TE.est <- mod.result$estimates %>% filter(Parm == "TE")


# combine chains
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


TE.samp <-   all.samps %>%
  # head(n=1) %>%
  select(c(id, starts_with("TE"))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    YrIdx = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "TE") %>% 
  rename(TE = Value) %>%
  group_by(id) %>%
  mutate(
    TE.rm4 = rollmean(TE, k=4, na.pad = TRUE, align = "right"),
    TE.rm5 = rollmean(TE, k=5, na.pad = TRUE, align = "right")
  ) %>%
  left_join(
    x =.,
    y = sr.dat %>% select(year, Year) %>% unique,
    by = c("YrIdx" = "year")
  ) %>% 
  select(id, YrIdx, Year, Parm, starts_with("TE")) %>%
  as_tibble()


TE.rm4 <- TE.samp %>% 
  rename(Value = TE.rm4) %>%
  group_by(Year) %>%
  summarize(
    mean = mean(Value, na.rm=TRUE),
    median = quantile(Value, prob = 0.50, na.rm=TRUE),
    p2.5 = quantile(Value, prob = 0.025, na.rm=TRUE), 
    p10 = quantile(Value, prob = 0.10, na.rm=TRUE),
    p25 = quantile(Value, prob = 0.25, na.rm=TRUE),
    # p50 = quantile(Value, prob = 0.50, na.rm=TRUE),
    p75 = quantile(Value, prob = 0.75, na.rm=TRUE),
    p90 = quantile(Value, prob = 0.90, na.rm=TRUE),
    p97.5 = quantile(Value, prob = 0.975, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean)) %>%
  mutate(VarType = "TE.rm4")

TE.rm5 <- TE.samp %>% 
  rename(Value = TE.rm5) %>%
  group_by(Year) %>%
  summarize(
    mean = mean(Value, na.rm=TRUE),
    median = quantile(Value, prob = 0.50, na.rm=TRUE),
    p2.5 = quantile(Value, prob = 0.025, na.rm=TRUE), 
    p10 = quantile(Value, prob = 0.10, na.rm=TRUE),
    p25 = quantile(Value, prob = 0.25, na.rm=TRUE),
    # p50 = quantile(Value, prob = 0.50, na.rm=TRUE),
    p75 = quantile(Value, prob = 0.75, na.rm=TRUE),
    p90 = quantile(Value, prob = 0.90, na.rm=TRUE),
    p97.5 = quantile(Value, prob = 0.975, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean)) %>%
  mutate(VarType = "TE.rm5")



app.plots[['TE RM4']] <- ggplot(TE.rm4, aes(x=Year, y=mean)) + 
  geom_ribbon(aes(ymin = p2.5, ymax = p97.5), alpha = 0.3, fill = "orangered") +
  geom_line(color = "orangered") +
  geom_errorbar(data = TE.est, aes(ymin = `2.5%`, ymax = `97.5%`), width=0, color="grey50") +
  geom_point(data = TE.est, color="grey50") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1960,2022, by=5)) +
  theme_classic(14) +
  labs(
    # title = "Skeena Shared Year Effect (4 year rolling mean)",
    # subtitle = "Error bars indicate 95% Credible Intervals",
    y = "Shared Year Effect Estimate"
  )
out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff_rm4.pdf")
ggsave(out.file, width=9, height=6.5, plot=app.plots[['TE RM4']])
out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff_rm4.png")
ggsave(out.file, width=9, height=6.5, plot=app.plots[['TE RM4']], dpi = 300)



app.plots[['TE RM5']] <- ggplot(TE.rm5, aes(x=Year, y=mean)) + 
  geom_ribbon(aes(ymin = p2.5, ymax = p97.5), alpha = 0.3, fill = "orangered") +
  geom_line(color = "orangered") +
  geom_errorbar(data = TE.est, aes(ymin = `2.5%`, ymax = `97.5%`), width=0, color="grey50") +
  geom_point(data = TE.est, color="grey50") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(1960,2022, by=5)) +
  theme_classic(14) +
  labs(
    # title = "Skeena Shared Year Effect (5 year rolling mean)",
    # subtitle = "Error bars indicate 95% Credible Intervals",
    y = "Shared Year Effect Estimate"
  )

out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff_rm5.pdf")
ggsave(out.file, width=9, height=6.5, plot=app.plots[['TE RM5']])
out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff_rm5.png")
ggsave(out.file, width=9, height=6.5, plot=app.plots[['TE RM5']], dpi = 300)




library(grid)
library(gridExtra)

# create and displayb arrangement
grid.arrange(
  g <- arrangeGrob(
    app.plots[['TE RM4']]  + theme(axis.title = element_blank()) + labs(title = "A) 4-year rolling mean"),
    app.plots[['TE RM5']] + theme(axis.title = element_blank())+ labs(title = "B) 5-year rolling mean"), 
    nrow=2, ncol=1,
    as.table = F,
    left = textGrob("Estimated Common Year Effect", rot=90,  gp=gpar(fontsize=15)),
    bottom = textGrob("Year",  gp=gpar(fontsize=15)), 
    clip=F
  ))
out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff.png")
ggsave(filename = out.file, width = 9, height=9, plot=g, dpi = 300)

out.file <- file.path(output.dir, "Appendix-HBM-common_year_eff.pdf")
ggsave(filename = out.file, width = 9, height=9, plot=g)



 


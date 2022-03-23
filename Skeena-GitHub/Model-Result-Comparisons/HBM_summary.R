library(tidyverse)
library(MCMCvis)

setwd("~/GitHub/Fisheries/Skeena-Nass-HBM/Skeena-GitHub")

readRDS("HBM/RESULTS/2022-03-02/result_HBM_McAllister_m24.rds") %>% str(max.level=1)
samps <- readRDS("HBM/RESULTS/2022-03-03/result_HBM_McAllister_m23.rds")$samples

samps <- readRDS("HBM/RESULTS/2022-03-20/result_HBM_Skeena_m23.rds")$samples


str(samps)


est <- MCMCsummary(samps) %>%
  rownames_to_column("Node") %>%
  mutate(
    Parm = str_replace(Node, "\\[[[:digit:],]+\\]", ""),
    StkID = as.numeric(str_replace(str_extract(Node, "\\[[:digit:]+"), "\\[", ""))
  )
est




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

all.samps %>% count(chain)


# Productivity ------------------------------------------------------------



HBM.int <- all.samps %>%
  select(c(id, starts_with("intercept["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "intercept") %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "ln.alpha"
  )


HBM.int.c <- left_join(
  x = all.samps %>%
    # head(n=1) %>%
    select(c(id, starts_with("intercept["))) %>%
    gather(key=Node, value=Value, -c(id)) %>%
    mutate(
      Parm = str_extract(Node, "[[:alpha:]_]+"),
      StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
    ) %>%
    filter(Parm == "intercept") %>%
    rename(Intercept = Value),
  y = all.samps %>%
    select(c(id, starts_with("se["))) %>%
    gather(key=Node, value=Value, -c(id)) %>%
    mutate(
      Parm = str_extract(Node, "[[:alpha:]_]+"),
      StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
    ) %>%
    filter(Parm == "se") %>% 
    select(id, StkId, Value) %>%
    rename(Sigma = Value), 
  by = c("id", "StkId")
) %>% 
  mutate(
  Value = Intercept + (Sigma * Sigma / 2)
) %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "ln.alpha.c"
  )


# Beta/slope --------------------------------------------------------------

HBM.beta <- all.samps %>%
  select(c(id, starts_with("slope["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "slope") %>%
  mutate(Value = Value * 1e6) %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "beta"
  )

# Sigma -------------------------------------------------------------------

HBM.sigma <- all.samps %>%
  select(c(id, starts_with("se["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(Value = 1/Value) %>%                        # Derive Smax
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "se") %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "sigma"
  )


# SMSY --------------------------------------------------------------------

HBM.Smsy <- all.samps %>%
  select(c(id, starts_with("Smsy["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "Smsy") %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "Smsy"
  )

# Smax (v1 - Derived) ------------------------------------------------------
HBM.Smax <- all.samps %>%
  select(c(id, starts_with("slope["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(Value = 1/Value) %>%                        # Derive Smax
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "slope") %>%
  group_by(StkId) %>%
  summarize(
    Mean = mean(Value),
    p10 = quantile(Value, prob = 0.1),
    p25 = quantile(Value, prob = 0.25),
    p50 = quantile(Value, prob = 0.50),
    p75 = quantile(Value, prob = 0.75),
    p90 = quantile(Value, prob = 0.90),
    .groups = "drop"
  ) %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>%
  mutate(
    VarType = "Smax"
  )




# Combine -----------------------------------------------------------------



HBM.est <- rbind(
  HBM.int,
  HBM.int.c,
  HBM.beta,
  HBM.sigma,
  HBM.Smax,
  HBM.Smsy
)





# # Slope -------------------------------------------------------------------
# 
# 
# all.samps %>% filter(Parm == "slope") %>%
#   group_by(Idx) %>%
# 
#   summarize(
#     Mean = mean(Value),
#     p10 = quantile(Value, prob = 0.1),
#     p25 = quantile(Value, prob = 0.25),
#     p50 = quantile(Value, prob = 0.50),
#     p75 = quantile(Value, prob = 0.75),
#     p90 = quantile(Value, prob = 0.90),
#     .groups = "drop"
#   ) 
# 
# 
# summary.dat <- list(
#   stats = sd$stats
# )
# bxp(sd[-c(2,3,4,5)])

message("Comparing reference points in high and low productivity periods.")
library(tidyverse)
library(zoo)

mod.result <- readRDS(file.path(save.dir, "result_HBM_Skeena_m23.rds"))

basin <- mod.result$basin
# est <- mod.result$estimates

# smax.dat <- mod.result$smax.data
samps <- mod.result$samples
sr.dat <- mod.result$sr.data
stocks <- unique(sr.dat$Stock)

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


intercept.samp <- all.samps %>%
  # head(n=1) %>%
  select(c(id, starts_with("intercept["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "intercept") %>%
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock),
    by = c("StkId" = "ID")
  ) %>% 
  rename(Intercept = Value) %>%
  as_tibble

slope.samp <- all.samps %>%
  # head(n=1) %>%
  select(c(id, starts_with("slope["))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "slope") %>%
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock),
    by = c("StkId" = "ID")
  ) %>% 
  rename(Slope = Value) %>%
  as_tibble

stock.samp <- left_join(
  x = intercept.samp %>% select(id, Stock, Intercept),
  y = slope.samp %>% select(id, Stock, Slope),
  by = c("id", "Stock")
)
# stock.samp

message(" Computing TE rolling mean...")
# Get full posterior sample  of common shared year effects
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

# CC[i] <- intercept[i]/slope[i]  # compute intercept by stock
# Smsy[i] <- CC[i]*(0.5-0.07*intercept[i])   #compute Smsy by stock
# Rmsy[i] <- Smsy[i]*exp(intercept[i]-slope[i]*Smsy[i])
# msy[i]<- Rmsy[i]-Smsy[i]
# Umsy[i] <- msy[i]/Rmsy[i]



result=list()

for (stock in stocks) {
  
  message("  Processing ", stock, "...")
  
  yrs <- sr.dat %>% filter(Stock == stock) %>% select(year) %>% unlist
  
  temp <- left_join(
    x = stock.samp %>% filter(Stock == stock) ,
    y = TE.samp %>% filter(YrIdx %in% yrs), 
    by = "id"
  ) %>%
    mutate(
      # Basin = basin,
      Intercept.yr = Intercept + TE.rm4,
      CC = Intercept.yr/Slope,
      Smsy = CC*(0.5-0.07*Intercept.yr),
      Rmsy = Smsy*exp(Intercept.yr - Slope*Smsy),
      msy = Rmsy - Smsy,
      Umsy = msy/Rmsy
    ) %>%
    mutate(
      Productivity = NA,
      Productivity = case_when( 
        Year %in% 1980:1992 ~ "High",
        Year %in% 1999:2014 ~ "Low",
        TRUE ~ "Other"
      )
    )
  
  
   smsy.prod <- temp %>%
    group_by(id, Stock, Productivity) %>%
    summarise(
      Value = mean(Smsy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(Stock, Productivity) %>%
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
    mutate(VarType = "Smsy") %>%
    add_column(Basin = basin, .before = "Stock")
   result[['Smsy productivity']] <- rbind(result[['Smsy productivity']], smsy.prod)
   
   if (all(c("High", "Low") %in% smsy.prod$Productivity)) {
     smsy.delta <- temp %>%
       # filter(id %in% c("1-1", "1-2")) %>%
       filter(Productivity != "Other") %>%
       group_by(id, Stock, Productivity) %>%
       summarise(
         Value = mean(Smsy, na.rm = TRUE),
         .groups = "drop"
       ) %>%
       spread(Productivity, Value) %>%
       mutate(Value = (Low - High)/Low) %>%
       group_by(Stock) %>%
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
       mutate(VarType = "Smsy Diff") %>%
       add_column(Basin = basin, .before = "Stock")
     result[['Smsy delta']] <- rbind(result[['Smsy delta']], smsy.delta)
   }
   
   umsy.prod <- temp %>%
     group_by(id, Stock, Productivity) %>%
     summarise(
       Value = mean(Umsy, na.rm = TRUE),
       .groups = "drop"
     ) %>%
     group_by(Stock, Productivity) %>%
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
     mutate(VarType = "Smsy") %>%
     add_column(Basin = basin, .before = "Stock")
   result[['Umsy productivity']] <- rbind(result[['Umsy productivity']], umsy.prod)
   
   if (all(c("High", "Low") %in% umsy.prod$Productivity)) {
     umsy.delta <- temp %>%
       # filter(id %in% c("1-1", "1-2")) %>%
       filter(Productivity != "Other") %>%
       group_by(id, Stock, Productivity) %>%
       summarise(
         Value = mean(Umsy, na.rm = TRUE),
         .groups = "drop"
       ) %>%
       spread(Productivity, Value) %>%
       mutate(Value = (Low - High)/Low) %>%
       group_by(Stock) %>%
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
       mutate(VarType = "Umsy Diff") %>%
       add_column(Basin = basin, .before = "Stock")
     result[['Umsy delta']] <- rbind(result[['Umsy delta']], umsy.delta)
   }
  rm(temp)
}

app.tables[['Smsy Productivity']] <- left_join(
  x = result[['Smsy productivity']] %>% 
    filter(Productivity %in% c("Low", "High")) %>%
    select(Basin, Stock,Productivity, mean) %>%
    spread(Productivity, mean),
  y = result[['Smsy delta']] %>% 
    select( Stock, mean, p2.5, p97.5) %>% rename(Diff = mean, Diff_2.5 = p2.5, Diff_97.5=p97.5),
  by = "Stock"
)

app.tables[['Umsy Productivity']] <- left_join(
  x = result[['Umsy productivity']] %>% 
    filter(Productivity %in% c("Low", "High")) %>%
    select(Basin, Stock,Productivity, mean) %>%
    spread(Productivity, mean),
  y = result[['Umsy delta']] %>% 
    select( Stock, mean, p2.5, p97.5) %>% rename(Diff = mean, Diff_2.5 = p2.5, Diff_97.5=p97.5),
  by = "Stock"
  
)
  # 
  # result[['Smsy productivity']]  %>% 
  #   # filter(Productivity %in% c("Low", "High")) %>%
  #   filter(Productivity != "Other") %>%
  #   mutate(Productivity = factor(Productivity, levels = c("Low", "High"))) %>%
  #   ggplot(., aes(x=Productivity, y=mean)) + 
  #   geom_errorbar(aes(ymin = `p2.5`, ymax = p97.5), width=0) +
  #   geom_point(aes(color = Stock)) +
  #   geom_line(aes(color = Stock, group = Stock))
  
  


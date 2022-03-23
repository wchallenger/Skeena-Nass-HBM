# BM.int.c <- left_join(
#   x = all.samps %>%
#     select(c(id, starts_with("intercept["))) %>%
#     gather(key=Node, value=Value, -c(id, chain, id)) %>%
#     mutate(
#       Parm = str_extract(Node, "[[:alpha:]_]+"),
#       StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
#     ) %>%
#     filter(Parm == "intercept") %>%
#     rename(Intercept = Value),
#   y = , 
#   by = c("id", "StkId")
# ) %>% 
#   mutate(
#     Value = Intercept + (Sigma * Sigma / 2)
#   ) %>%
#   group_by(StkId) %>%
#   
  
Int.samp <- all.samps %>%
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
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>% 
  as_tibble
  
TE.samp <-   all.samps %>%
  # head(n=1) %>%
  select(c(id, starts_with("TE"))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    year = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "TE") %>% 
  select(id, year, Parm, Value) %>%
  as_tibble()


sig.samp <-   all.samps %>%
  # head(n=1) %>%
  select(c(id, starts_with("se"))) %>%
  gather(key=Node, value=Value, -c(id)) %>%
  mutate(
    Parm = str_extract(Node, "[[:alpha:]_]+"),
    StkId = str_extract(Node, "\\[[:digit:]+") %>% str_replace(., pattern ="\\[", replacement =  "" ) %>% as.numeric
  ) %>%
  filter(Parm == "se") %>% 
  left_join(
    x =.,
    y = smax.dat %>% select(ID, Stock ),
    by = c("StkId" = "ID")
  ) %>% 
  as_tibble





yr.lookup <- sr.dat %>% select(year, Year) %>% unique

HBM.yearly <- NULL
for (stock in stocks) {
  check1 <- stock %in% unique(HBM.est$Stock)
  check2 <- stock %in% unique(post$Stock)
  if ((check1 & check2) == FALSE) next
  
  yrs <- sr.dat %>% filter(Stock == stock) %>% select(year) %>% unlist
  
  temp1 <- left_join(
    x = Int.samp %>% filter(Stock == stock) %>% rename(Intercept = Value),
    y = TE.samp %>% filter(year %in% yrs)%>% rename(TE = Value), 
    by = "id"
  ) %>%
    mutate(
      Value = Intercept + TE
    ) %>%
    group_by(Stock, year) %>%
    summarize(
      Mean = mean(Value),
      p10 = quantile(Value, prob = 0.1),
      p25 = quantile(Value, prob = 0.25),
      p50 = quantile(Value, prob = 0.50),
      p75 = quantile(Value, prob = 0.75),
      p90 = quantile(Value, prob = 0.90),
      .groups = "drop"
    ) %>%
    left_join(., yr.lookup, by = "year") %>%
    mutate(VarType = "ln.alpha")
  
  temp2 <- left_join(
    x = Int.samp %>% filter(Stock == stock) %>% rename(Intercept = Value),
    y = TE.samp %>% filter(year %in% yrs)%>% rename(TE = Value), 
    by = "id"
  ) %>%
  left_join(
    x = .,
    y = sig.samp %>% filter(Stock == stock) %>% select(id, Value) %>% rename(Sigma = Value),
    by = "id"
  ) %>%
    mutate(
      Value = Intercept + TE + (Sigma * Sigma / 2)
    ) %>%
    group_by(Stock, year) %>%
    summarize(
      Mean = mean(Value),
      p10 = quantile(Value, prob = 0.1),
      p25 = quantile(Value, prob = 0.25),
      p50 = quantile(Value, prob = 0.50),
      p75 = quantile(Value, prob = 0.75),
      p90 = quantile(Value, prob = 0.90),
      .groups = "drop"
    ) %>%
    left_join(., yr.lookup, by = "year") %>%
    mutate(VarType = "ln.alpha.c")
  
  
  
  HBM.yearly <- rbind(HBM.yearly, temp1, temp2)
  
  rm(temp1); rm(temp2)
  
  
}


result  








for (parm in c("ln.alpha",  "ln.alpha.c"))  {
  
  for (yr in yrs) {
    year = yr.lookup %>% filter(year == yr) %>% select(Year) %>% unlist
    out.file <- file.path("kalman", "by-year", paste0("comparison--", parm, "-yr", year, ".pdf"))
    pdf(file = out.file, width = 15, height = 15)
    par(mar = c(10, 4, 4, 2) + 0.1, mfrow = c(4,3))
    for (stock in stocks) {
      check1 <- stock %in% unique(HBM.yearly$Stock)
      check2 <- stock %in% unique(post$Stock)
      if ((check1 & check2) == FALSE) next
      
      dat1 <- HBM.yearly %>% 
        filter(VarType == parm, Stock == stock) %>%
        rename(Q1 = p25, Q3 = p75, Median = p50) %>%
        mutate(
          IQR = Q3-Q1,
          Min = Q1 - 1.5*IQR,
          Max = Q3 + 1.5*IQR,
          Model = "HBM (m23)"
        ) %>%
        select(Stock, Model, year, Year,  Min, Q1, Median, Q3, Max) %>%
        rename(YrIdx = year)
      
      if (nrow(dat1) == 0) next
      
      dat2 <- post %>% 
        filter(Stock == stock) %>%
        filter(VarType == parm) %>%
        filter(str_detect(Model, "Kalman")) %>%
        select(Stock, Model, YrIdx, Yr, Min, Q1, Median, Q3, Max)%>%
        rename(Year = Yr)
      
      if (nrow(dat2) == 0) next
      
      
      x <- rbind(dat1 %>% filter(YrIdx == yr) , dat2 %>% filter(YrIdx == yr))
      if (nrow(x) == 0) next
      
      box.dat <- list(
        stats = x %>% select(Min:Max) %>% t %>% as.matrix,
        names = x$Model
      )
      
      
      bxp(box.dat, 
          las=2, 
          boxfill = c("cornflowerblue", rep("grey85", nrow(dat2))),
          main =x$Stock %>% unique, ylab = parm
      )
      
    }
    dev.off()  
  }
}

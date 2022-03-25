
ComputeTimeVaryingAlpha <- function(mod.result) {
  
  # browser()
  basin <- mod.result$basin
  est <- mod.result$estimates
  sr.dat <- mod.result$sr.data
  smax.dat <- mod.result$smax.data
  samps <- mod.result$samples
  stocks <- unique(sr.dat$Stock)
  
  check <-  "TE" %in% est$Parm 
  if (!check) return(NULL)
  
  if (!setequal(stocks, smax.dat$Stock)) stop("Stock mismatch")
  
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
  
  
  # s -----------------------------------------------------------------------
  # browser()
  # get full posterior samples intercept
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
  
  
  # 
  ln.alpha.yearly <- NULL
  
  for (stock in stocks) {
    
    yrs <- sr.dat %>% filter(Stock == stock) %>% select(year) %>% unlist
    
    temp <- left_join(
      x = intercept.samp %>% filter(Stock == stock) ,
      y = TE.samp %>% filter(YrIdx %in% yrs), 
      by = "id"
    ) %>%
      mutate(
        Value = Intercept + TE.rm4
      ) %>%
      group_by(Stock, YrIdx, Year) %>%
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
      mutate(VarType = "ln.alpha") %>%
      add_column(Basin = basin, .before = "Stock")
    
    ln.alpha.yearly <- rbind(ln.alpha.yearly, temp)
    rm(temp)
  }
  
  
  return(ln.alpha.yearly %>% filter(!is.na(mean)))
}

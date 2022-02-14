
# Derive area to Smax relationship
fit <- lm(log(Smax_Spn) ~ log(Area_km2), lake.meta)
summary(fit)

new.dat = lake.meta %>% 
  # filter(is.na(Smax_Spn)) %>%
  filter(!is.na(Area_km2)) %>% 
  select(LkSeq, Watershed, Lake, Stks, Basin, Area_km2)

new.dat$Smax_Pred <- predict(fit, new.dat) %>% exp

smax.dat <- left_join(
  x = lake.meta,
  y = new.dat %>% select(-Area_km2),
  by = c("LkSeq", "Watershed", "Lake", "Stks", "Basin")
) %>%
  mutate(
    Smax_analysis = ifelse(!is.na(Smax_Spn), Smax_Spn, Smax_Pred),
    Type = ifelse(!is.na(Smax_Spn), "Estimated", "Prediction")
  ) %>%
  group_by(Basin, Stks) %>%
  summarize(
    nLake = n(),
    nCalc = sum(!is.na(Smax_analysis)),
    Type = paste(Type[!is.na(Smax_analysis)] %>% unique, collapse = " + "),
    Area_km2_Total = sum(Area_km2, na.rm = TRUE),
    Smax_Total = ifelse(nCalc>0, sum(Smax_analysis, na.rm=TRUE), NA),
    Smax_Single = ifelse(nCalc>0, max(Smax_analysis, na.rm=TRUE), NA),
    .groups = "drop"
  ) %>%
  mutate(
    Area_km2_Total = ifelse(nCalc == 0,NA, Area_km2_Total),
    Type = ifelse(nCalc == 0,NA, Type)
  ) %>%
  rename( Stock = Stks) 


# Spit out Babine complex... until we h
babine.stocks <- c("Babine Early Wild", "Babine Late Wild", "Babine Mid Wild", "Fulton", "Pinkut")
x <- smax.dat[1:5,]
x[] <- NA
x$Basin = "Skeena"
x$Stock = babine.stocks
x$nLake = 1
x$nCalc = 1
x$Type = "Partitioned"
x$Smax_Total = filter(smax.dat, str_detect(Stock, "Fulton"))$Smax_Total/5

smax.dat <- bind_rows(smax.dat %>% filter(Stock != "Babine Mid Wild"),x) %>% 
  mutate(
    prCV = ifelse(Stock %in% babine.stocks, 0.75, 0.3)
  ) %>%
  arrange(Stock)


smax.dat <- left_join(
  smax.dat,
  y = stock.lookup, 
  by = "Stock"
)

smax.final <- smax.dat %>% filter(!is.na(CU))

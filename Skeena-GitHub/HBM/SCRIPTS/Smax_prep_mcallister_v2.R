# Creates Smax Priors and CVs used in the McAllister Dec 14 TWG presentation

# Load --------------------------------------------------------------------

lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble

# Plot --------------------------------------------------------------------


babine.stocks <- c("Babine Early Wild", "Babine Late Wild", "Babine Mid Wild", "Fulton", "Pinkut")


pooling <- c("Bear", "Mcdonell", "Slamgeesh", "Sustut", "Swan/Stephens", "Kwinageese" )
pooling <- c("Mcdonell", "Swan/Stephens", "Kwinageese" )

smax.tab <-  lake.meta %>%
  rename( Stock = Stks, Lk = Lake) %>%
  # filter(Basin == "Skeena") %>%
  group_by(Basin, Stock) %>%
  summarize(
    nLake = n(),
    nCalc = sum(!is.na(Smax_Spn)),
    prSmax = ifelse(nCalc > 0, sum(Smax_Spn, na.rm=TRUE), NA),
    prSmax = ifelse(nCalc > 1 & !all(Stock %in% pooling), max(Smax_Spn, na.rm=TRUE), prSmax),
    Lake = ifelse(nCalc > 0, paste(Lk[!is.na(Smax_Spn)], collapse = "/"), NA),
    Lake = ifelse(
      test = nCalc > 1 & (unique(Stock) %in% pooling) == FALSE,
      yes = paste(Lk[which(Smax_Spn == prSmax)]),
      no  = Lake
      ),
    Area_km2 = sum(Area_km2[!is.na(Smax_Spn)|Stock=="Asitka"]),
    .groups = "drop"
  ) %>% 
  #View(title = "smax")
  # Drop stocks missing Smax except Astika
  filter(!is.na(prSmax) | Stock == "Asitka") %>%
  # Remove Babine related stocks
  filter(!str_detect(Stock, "Babine Mid Wild"))

# View(smax.tab)

# Interpolate Asitka ------------------------------------------------------
# Determine average Smax per area for all Skeena lakes.  Note that follows
# McAllister Dec 14 analysis that applied a further adjustment to Babine Lake,
# before computing Smax per area. This adjustment may need to be revisited.

smax.area <- lake.meta  %>%
  # filter(!str_detect(Stks, "Fulton, Pinkut")) %>%
  filter(Basin == "Skeena") %>% 
  mutate(
    nStks = ifelse(Lake == "Babine",1,1),   # McAllister adjustment used on Babine only
    SmaxAdj = Smax_Spn/nStks,
    Smax.Area = SmaxAdj/Area_km2
    ) %>%
  select(Basin, Watershed, Lake, Area_km2, Smax_Spn, Smax.Area) %>% 
  # View(title="Smax")   %>%
  summarize(Smax.Area = mean(Smax.Area, na.rm=TRUE)) %>% unlist


# Predict Asitka Smax based on area
smax.tab <- smax.tab %>%
  mutate(
    prSmax = ifelse(Stock == "Asitka", Area_km2 * smax.area, prSmax),
    Lake = ifelse(Stock =="Asitka", "Asitka", Lake)
  )



# Combining lakes for Swan/Stephens ---------------------------------------
# Dec 14th analysis combined Swan and Stephens lakes for the Swan/Stephens
# Smax value.

# smax.tab <- smax.tab %>% filter(!str_detect(Stock, "Swan/Stephens")) %>%
#   add_row(
#     data.frame(
#       Basin = "Skeena",
#       Stock = c("Swan/Stephens"),
#       prSmax = filter(lake.meta, Lake %in% c("Swan", "Stephens"))$Smax_Spn  %>% sum
#     )
#   )


# Partition Babine --------------------------------------------------------
# Add Babine complex stocks based on an equal partitioning between stocks.

smax.tab <- smax.tab %>%
  add_row(
    data.frame(
      Basin = "Skeena",
      Stock = babine.stocks ,
      prSmax = filter(lake.meta, str_detect(Stks, "Fulton, Pinkut"))$Smax_Spn/5,
      Lake = "Babine"
    )
  ) %>%
  filter(!str_detect(Stock, "Fulton, Pinkut")) 


# Add CV Prior------------------------------------------------------------------
# used 0.3 for most stocks, 0.75 for Babine, and 0.5 for Alistka and Swan/Stephens
smax.tab <- smax.tab %>%
  mutate(
    prCV = ifelse(Stock %in% babine.stocks, 2, 0.3),
    prCV = ifelse(Stock %in% c("Asitka", "Morice"), 2, prCV)
  )


# Add Smax max ------------------------------------------------------------


smax.tab <- smax.tab %>% mutate(Smaxmax = prSmax * 5)


# Add Within basin ID -----------------------------------------------------
smax.tab <- smax.tab %>% 
  arrange(desc(Basin), Stock) %>%
  group_by(Basin) %>%
  mutate(
    ID = seq_along(Stock)
  ) %>%
  ungroup


# View(smax.tab)






library(tidyverse)
library(ggrepel)
library(scales)
# Settings ----------------------------------------------------------------





# Load --------------------------------------------------------------------
data.dir <- "~/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena"
xlsx.file <- file.path(data.dir, "McAllister_data.xlsx")
smax_MM <- read.xlsx(xlsx.file, "Meta")

data.dir <- "~/GitHub/Fisheries/Skeena-Nass-Sockeye-Model-Sandbox/DATA"
lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble


# Plot --------------------------------------------------------------------


babine.stocks <- c("Babine Early Wild", "Babine Late Wild", "Babine Mid Wild", "Fulton", "Pinkut")




smax.dat <-  lake.meta %>% 
  rename( Stock = Stks) %>%
  filter(Basin == "Skeena") %>%
  group_by(Basin, Stock) %>%
  summarize(
    nLake = n(),
    nCalc = sum(!is.na(Smax_Spn)),
    prSmax = ifelse(nCalc>0, max(Smax_Spn, na.rm=TRUE), NA),
    LakeID = ifelse(nCalc>0, which(Smax_Spn == prSmax ), ifelse(Stock == "Asitka", 1, NA)),
    Area_km2 = Area_km2[LakeID],
    .groups = "drop"
  ) %>%
  # Drop stocks missing Smax except Astika
  filter(!is.na(prSmax) | Stock == "Asitka") %>% 
  # Remove Babine related stocks
  filter(!str_detect(Stock, "Babine Mid Wild"))
  
  # filter(Stock != "Babine Mid Wild") %>% 
  # # Remove estimate of Babine Mid Wild, will be using a partition approach for Babine lake complex
  # # filter(!str_detect(Stock, "Fulton, Pinkut"))
  # filter(Stock == paste(babine.stocks, ))
  # 
  # 


# Interpolate Asitka ------------------------------------------------------
# Determine average Smax per area for all Skeena lakes.  Note that follows
# McAllister Dec 14 analysis that applied a further adjustment to Babine,
# this adjustment may need to be revisited

smax.area <- lake.meta  %>%
  # filter(!str_detect(Stks, "Fulton, Pinkut")) %>% 
  filter(Basin == "Skeena") %>%
  mutate(
    nStks = ifelse(Lake == "Babine",5,1),   # McAllister adjustment used on Babine only
    SmaxAdj = Smax_Spn/nStks,
    Smax.Area = SmaxAdj/Area_km2
    ) %>% 
  select(Basin, Watershed, Lake, Area_km2, Smax_Spn, Smax.Area) %>% 
  # View(title="Smax")   %>%
  summarize(Smax.Area = mean(Smax.Area, na.rm=TRUE)) %>% unlist


# Predict Asitka Smax based on area
smax.dat <- smax.dat %>%
  mutate(
    prSmax = ifelse(Stock == "Asitka", Area_km2 * smax.area, prSmax)
  )



# Combining lakes for Swan/Stephens ---------------------------------------


# Combining lakes for Swan/Stephens
smax.dat <- smax.dat %>% filter(!str_detect(Stock, "Swan/Stephens")) %>%
  add_row(
    data.frame(
      Basin = "Skeena",
      Stock = c("Swan/Stephens"),
      prSmax = filter(lake.meta, Lake %in% c("Swan", "Stephens"))$Smax_Spn  %>% sum
    )
  ) 


# Partition Babine --------------------------------------------------------


# Add Babine complex based on an equal partitioning
smax.dat <- smax.dat %>%
  add_row(
    data.frame(
      Basin = "Skeena",
      Stock = babine.stocks ,
      prSmax = filter(lake.meta, str_detect(Stks, "Fulton, Pinkut"))$Smax_Spn/5
    )
  ) %>%
  filter(!str_detect(Stock, "Fulton, Pinkut")) %>%
  arrange(Stock) %>%
  mutate(
    ID = seq_along(Stock)
  )





# Add CV ------------------------------------------------------------------

smax.dat <- smax.dat %>%
  mutate(
    prCV = ifelse(Stock %in% babine.stocks, 0.75, 0.3),
    prCV = ifelse(Stock %in% c("Asitka", "Swan/Stephens"), 0.5, prCV)
  )




# Validation check -------------------------------------------------------
# ensure we produced the same Smax and prior values as original analyis

if (exists("smax_MM")){
  check <-  left_join(
    smax.dat,
    smax_MM %>% select(Stock, prSmax, prCV) %>% rename(Smax_MM = prSmax, prCV_MM = prCV),
    by = "Stock"
  ) %>%
    mutate(
      PercDiff_Smax = (prSmax - Smax_MM)/Smax_MM,
      PercDiff_CV = (prCV - prCV_MM)/prCV_MM
      )
  
  check
}





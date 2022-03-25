# PREP: Smax Data ------------------------------------------------------------
# Late meta data

lake.meta <- read.csv(file.path(data.dir, "LakeInfo_Main_WithSR_Smax.csv")) %>% as_tibble

# smax.dat %>% select(ID, Basin, Stock, prSmax, pr)

# creates an smax.tab object containing all the Smax values for Skeena and Nass
# analyses
source(file.path(script.dir, smax.script))

# Adjust scaling
smax.tab <- smax.tab %>% mutate(
  prSmax = prSmax/sr.scale,
  Smaxmax = Smaxmax/sr.scale
)

# stop("DEV STOPPAGE L182", call. = F)


# VALIDATE: Smax  --------------------------------------------------------------


# message("WARNING: Using Mcalliser smax")
# smax.old <-  smax.dat
# smax.dat <- cbind(smax.alt, smax.old %>% select(ID, Basin,Stock))
# if (names(runs)[r] == "McAllister m29") smax.dat$prCV <- 2
# 

alt.datdir <- "/Users/wchallenger/GitHub/Fisheries/Skeena-Nass-HBM/McAllister-Skeena/2022-01-06"
# 
smax.alt <- read.table(file.path(alt.datdir, "data_v2h.txt"), skip = 4, nrows = 18) %>%
  rownames_to_column(var="ID") %>%
  mutate(ID = as.numeric(ID))
colnames(smax.alt) <- c("ID",  "prSmax", "prCV", "Smaxmax")


smax.compare <- full_join(
  x = smax.tab %>% filter(Basin == "Skeena") %>% select(ID,Stock, prSmax, prCV, Smaxmax),
  y = smax.alt %>% rename(prSmaxCheck = prSmax, prCVCheck=prCV, SmaxmaxCheck=Smaxmax),
  by = "ID"
) %>%
  mutate(
    prSmax_DIFF = prSmax - prSmaxCheck,
    prCV_DIFF = prCV - prCVCheck,
    Smaxmax_DIFF = Smaxmax - SmaxmaxCheck 
  ) 

check <- smax.compare %>% filter(abs(prSmax_DIFF) > 1 | abs(prCV_DIFF) > 0 | abs(Smaxmax_DIFF) > 5)


if (nrow(check)>0) message("Smax priors have changed for ", paste(check$Stock, collapse = ","))





# PREP: SR Data ----------------------------------------------------
# Stock Recruit Data

stock.info <- read.csv(file.path(data.dir, "StockInfo_Main.csv")) %>% as_tibble

sr.tab <-  left_join(
  x = read.csv(file.path(data.dir, "StockData_Main.csv")),
  y = stock.info %>% select(Stock, Basin, LifeHistory),
  by = "Stock"
) %>% as_tibble %>%
  # filter(Basin == "Skeena") %>%
  filter(!is.na(Rec) & !is.na(Spn)) %>%
  group_by(Basin) %>%
  mutate(year = Year - min(Year) + 1) %>%
  ungroup %>%
  mutate(
    # CU = factor(Stock) %>% as.numeric,
    lnRSobs = log(Rec/Spn)
  ) %>%
  left_join(
    .,
    y = smax.tab %>% select(ID, Stock) %>% rename(CU = ID),
    by = "Stock"
  ) %>%
  # rename(Spawn = Spn) %>%
  mutate(Spawn = Spn / sr.scale) %>%
  select(Basin, Stock, CU,Spawn, lnRSobs, year, Year) %>%
  arrange(desc(Basin), CU, year)


# Filter out extreme
warning("log Recruit/Spawner values over 4 excluded")
sr.tab <- sr.tab %>% filter(lnRSobs < 4)

# # Extract CU id to stock name
# stock.lookup <-  sr.dat %>% select(CU, Stock) %>% unique
# 


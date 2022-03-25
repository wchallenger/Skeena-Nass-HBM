message("Preparing Skeena analysis data")

# Data Prep ---------------------------------------------------------------
smax.dat <- smax.tab %>% filter(Basin == "Skeena")
sr.dat <- sr.tab %>% filter(Basin == "Skeena")


stocks <- intersect(
  sr.dat$Stock,
  smax.dat$Stock
)

smax.dat <- smax.dat %>% filter(Stock %in% stocks) %>% mutate(ID = seq_along(Stock))
sr.dat <- left_join(
  x = sr.dat %>% filter(Stock %in% stocks),
  y = smax.dat %>% select(Stock,ID),
  by = "Stock"
) %>%
  mutate(CU = ID)

# sr.dat %>% count(Stock, CU)


# Sensitivity run 6 uses base case with vague priors
if (str_detect(run.name,"m29")) {
  message("Using vague priors")
  smax.dat$prCV <- 2
}
# Check to ensure all the Smax database and SR database match.
if (any(is.na(sr.dat$CU)))  stop("Missing CU number in SR data")



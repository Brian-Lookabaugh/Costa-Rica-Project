############################################################################
################---Costa Rica Data Collection & Cleaning---#################
############################################################################

library(tidyverse)

# Load V-Dem Data
vdem <- readr::read_csv("Data/vdem.csv")

# Load COW Data
cow <- readr::read_csv("Data/NMC-60-abridged.csv")

# Merge the Data Sets
merged <- left_join(vdem, cow,
                    by = c("COWcode" = "ccode", "year"))

# Remove Variables Not Needed
merged <- merged %>%
  select(-c(stateabb, irst, pec, tpop, upop, cinc, version))

# Rename Variables
merged <- merged %>%
  rename("democracy" = "v2x_polyarchy",
         "libdem" = "v2x_libdem",
         "cs.part" = "v2x_cspart",
         "primary.school" = "v2peprisch",
         "second.school" = "v2pesecsch",
         "tertiary.school" = "v2petersch",
         "gdppc" = "e_gdppc",
         "inflation" = "e_miinflat",
         "pop" = "e_pop",
         "fuel.income" = "e_total_fuel_income_pc",
         "petrol.income" = "e_total_oil_income_pc",
         "all.resources" = "e_total_resources_income_pc",
         "pop2" = "e_mipopula",
         "urbanization" = "e_miurbani",
         "social.exc" = "v2xpe_exlsocgr",
         "male.slave" = "v2clslavem",
         "female.slave" = "v2clslavef",
         "slavery" = "v2xcl_slave",
         "state.econ" = "v2clstown",
         "property.male" = "v2clprptym",
         "property.female" = "v2clprptyw",
         "state.auth" = "v2svstterr",
         "fis.cap" = "v2stfisccap",
         "mil.adm" = "v2stcritapparm",
         "mil.ren" = "v2strenarm",
         "prop.rights" = "v2xcl_prpty",
         "civ.society" = "v2xcs_ccsi",
         "mil.spend" = "milex",
         "mil.per" = "milper",
         "region" = "e_regiongeo",
         "labor" = "v3cllabrig",
         "distribution" = "v2xeg_eqdr",
         "pol.v" = "v2caviol",
         "phys.v" = "v2x_clphy",
         "clean.elec" = "v2xel_frefair")

# Data Manipulation
merged <- merged %>%
  mutate(lgdppc = log(gdppc + 1), na.rm = T) %>%
  mutate(mil.per = na_if(mil.per, "-9")) %>%
  mutate(lmilper = log(mil.per + 1), na.rm = T) %>%
  mutate(mil.spend = na_if(mil.spend, "-9")) %>%
  mutate(lmilspend = log(mil.spend + 1), na.rm = T) %>%
  mutate(lpop = log(pop), na.rm = T) %>%
  mutate(lpop2 = log(pop2), na.rm = T)

# Create Region Samples
latin.am <- merged %>%
  # Central/South American and the Caribbean
  filter(region %in% c("17", "18", "19"))
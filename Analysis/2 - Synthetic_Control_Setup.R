############################################################################
############-------------Synthetic Control Set Up--------------#############
############################################################################

library(tidysynth)

# Remove Honduras Due to Missigness in the Outcome and Countries With No Data During 
# The Training Period
latin.am <- latin.am %>%
  filter(COWcode != 91 & COWcode != 115 & COWcode != 51 & COWcode != 52 & 
           COWcode != 53 & COWcode != 110) %>%
# Remove Irrelevant Years
  filter(year > 1934 & year < 1966) %>%
# Remove Countries With Civil Wars (Idiosyncratic Shocks)
  filter(COWcode != 100 & COWcode != 140 & COWcode != 90 & COWcode != 160 &
           COWcode != 150 & COWcode != 40 & COWcode != 42)

# Synthetic Control Set-Up (Mean Pre-Treatment Outcomes)
synth.mpto <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = T),
                     l.milspend = mean(lmilspend, na.rm = T),
                     pr = mean(prop.rights, na.rm = T),
                     dist = mean(distribution, na.rm = T),
                     ce = mean(clean.elec, na.rm = T),
                     pol.vi = mean(pol.v, na.rm = T),
                     phys.vi = mean(phys.v, na.rm = T)
                     ) %>%
  generate_predictor(time_window = 1935:1949,
                     ldem = mean(democracy)) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# First Half Pre-Treatment Outcomes
synth.fhpto <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = T),
                     l.milspend = mean(lmilspend, na.rm = T),
                     pr = mean(prop.rights, na.rm = T),
                     dist = mean(distribution, na.rm = T),
                     ce = mean(clean.elec, na.rm = T),
                     pol.vi = mean(pol.v, na.rm = T),
                     phys.vi = mean(phys.v, na.rm = T)
  ) %>%
  generate_predictor(time_window = 1935:1942,
                     ldem = mean(democracy)) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# First Three-Quarters Pre-Treatment Outcomes
synth.ftqpto <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = T),
                     l.milspend = mean(lmilspend, na.rm = T),
                     pr = mean(prop.rights, na.rm = T),
                     dist = mean(distribution, na.rm = T),
                     ce = mean(clean.elec, na.rm = T),
                     pol.vi = mean(pol.v, na.rm = T),
                     phys.vi = mean(phys.v, na.rm = T)
  ) %>%
  generate_predictor(time_window = 1935:1945,
                     ldem = mean(democracy)) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# Even Pre-Treatment Outcomes
synth.epto <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = T),
                     l.milspend = mean(lmilspend, na.rm = T),
                     pr = mean(prop.rights, na.rm = T),
                     dist = mean(distribution, na.rm = T),
                     ce = mean(clean.elec, na.rm = T),
                     pol.vi = mean(pol.v, na.rm = T),
                     phys.vi = mean(phys.v, na.rm = T)
  ) %>%
  generate_predictor(time_window = 1936,
                     dem.1936 = mean(democracy)) %>%
  generate_predictor(time_window = 1938,
                     dem.1938 = mean(democracy)) %>%
  generate_predictor(time_window = 1940,
                     dem.1940 = mean(democracy)) %>%
  generate_predictor(time_window = 1942,
                     dem.1942 = mean(democracy)) %>%
  generate_predictor(time_window = 1944,
                     dem.1944 = mean(democracy)) %>%
  generate_predictor(time_window = 1946,
                     dem.1946 = mean(democracy)) %>%
  generate_predictor(time_window = 1948,
                     dem.1948 = mean(democracy)) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# Odd Pre-Treatment Outcomes
synth.opto <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = T),
                     l.milspend = mean(lmilspend, na.rm = T),
                     pr = mean(prop.rights, na.rm = T),
                     dist = mean(distribution, na.rm = T),
                     ce = mean(clean.elec, na.rm = T),
                     pol.vi = mean(pol.v, na.rm = T),
                     phys.vi = mean(phys.v, na.rm = T)
  ) %>%
  generate_predictor(time_window = 1935,
                     dem.1935 = mean(democracy)) %>%
  generate_predictor(time_window = 1937,
                     dem.1937 = mean(democracy)) %>%
  generate_predictor(time_window = 1939,
                     dem.1939 = mean(democracy)) %>%
  generate_predictor(time_window = 1941,
                     dem.1941 = mean(democracy)) %>%
  generate_predictor(time_window = 1943,
                     dem.1943 = mean(democracy)) %>%
  generate_predictor(time_window = 1945,
                     dem.1945 = mean(democracy)) %>%
  generate_predictor(time_window = 1947,
                     dem.1947 = mean(democracy)) %>%
  generate_predictor(time_window = 1949,
                     dem.1949 = mean(democracy)) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()
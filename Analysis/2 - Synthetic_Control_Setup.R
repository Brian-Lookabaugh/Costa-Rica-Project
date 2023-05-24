############################################################################
############-------------Synthetic Control Set Up--------------#############
############################################################################

library(tidysynth)

# Remove Honduras Due to Missigness in the Outcome and Countries With No Data During 
# The Training Period
latin.am <- latin.am %>%
  filter(COWcode != 91 & COWcode != 115 & COWcode != 51 & COWcode != 52 & COWcode != 53 & COWcode != 110) %>%
  filter(year > 1934 & year < 1960)

# Synthetic Control Set-Up (Mean Pre-Treatment Outcomes)
synth1 <- latin.am %>%
  synthetic_control(outcome = democracy,
                    unit = country_name,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     l.gdppc = mean(lgdppc, na.rm = TRUE),
                     l.milper = mean(lmilper, na.rm = TRUE),
                     l.milspend = mean(lmilspend, na.rm = TRUE),
                     pr = mean(prop.rights, na.rm = TRUE),
                     dist = mean(distribution, na.rm = TRUE)
                     ) %>%
  generate_predictor(time_window = 1935,
                     d.1935 = democracy) %>%
  generate_predictor(time_window = 1940,
                     d.1940 = democracy) %>%
  generate_predictor(time_window = 1945,
                     d.1945 = democracy) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# First Half Pre-Treatment Outcomes

# First Three-Quarters Pre-Treatment Outcomes

# Even Pre-Treatment Outcomes

# Odd Pre-Treatment Outcomes
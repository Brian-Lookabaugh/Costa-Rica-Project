############################################################################
############-------------Synthetic Control Set Up--------------#############
############################################################################

library(tidysynth)

# Test Bivariate Correlation Between Outcome and Predictors
num <- merged %>%
  select(-c(country_name, year, COWcode, region))

cor(num$democracy, num$gdppc,
    use = "pairwise.complete.obs")
cor(num$democracy, num$cs.part,
    use = "pairwise.complete.obs")
cor(num$democracy, num$primary.school,
    use = "pairwise.complete.obs")
cor(num$democracy, num$second.school,
    use = "pairwise.complete.obs")
cor(num$democracy, num$tertiary.school,
    use = "pairwise.complete.obs")
cor(num$democracy, num$inflation, # Not Significant
    use = "pairwise.complete.obs")
cor(num$democracy, num$pop, # Not Significant
    use = "pairwise.complete.obs")
cor(num$democracy, num$fuel.income, # Not Significant
    use = "pairwise.complete.obs")
cor(num$democracy, num$petrol.income, # Not Significant
    use = "pairwise.complete.obs")
cor(num$democracy, num$all.resources, # Not Significant
    use = "pairwise.complete.obs")
cor(num$democracy, num$urbanization,
    use = "pairwise.complete.obs")
cor(num$democracy, num$social.exc,
    use = "pairwise.complete.obs")
cor(num$democracy, num$male.slave,
    use = "pairwise.complete.obs")
cor(num$democracy, num$female.slave,
    use = "pairwise.complete.obs")
cor(num$democracy, num$state.econ,
    use = "pairwise.complete.obs")
cor(num$democracy, num$property.male,
    use = "pairwise.complete.obs")
cor(num$democracy, num$property.female,
    use = "pairwise.complete.obs")
cor(num$democracy, num$state.auth,
    use = "pairwise.complete.obs")
cor(num$democracy, num$fis.cap,
    use = "pairwise.complete.obs")
cor(num$democracy, num$mil.adm,
    use = "pairwise.complete.obs")
cor(num$democracy, num$mil.ren,
    use = "pairwise.complete.obs")
cor(num$democracy, num$prop.rights,
    use = "pairwise.complete.obs")
cor(num$democracy, num$civ.society,
    use = "pairwise.complete.obs")
cor(num$democracy, num$mil.spend,
    use = "pairwise.complete.obs")
cor(num$democracy, num$mil.per, # Not Significant
    use = "pairwise.complete.obs")

# Synthetic Control Set-Up (Mean Pre-Treatment Outcomes)
synth1 <- latin.am %>%
  ## Create the Synthetic Control Object
  synthetic_control(outcome = democracy,
                    unit = COWcode,
                    time = year,
                    i_unit = "Costa Rica",
                    i_time = 1949,
                    generate_placebos = T) %>%
  generate_predictor(time_window = 1935:1949,
                     ) %>%
  generate_weights(optimization_window = 1935:1949,
                   margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# First Half Pre-Treatment Outcomes

# First Three-Quarters Pre-Treatment Outcomes

# Even Pre-Treatment Outcomes

# Odd Pre-Treatment Outcomes
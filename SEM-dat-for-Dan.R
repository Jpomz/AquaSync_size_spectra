# UK data for Perkins

library(tidyverse)
env <- read_csv("data/env_data_all.csv")
dat <- readRDS("derived_data/filtered_size_jan-11.RDS")
mle_lat <- readRDS("results/mle_lat_count2024-01-11.RDS")

env <- env %>%
  mutate(site = case_when(
    site == "Allt a\x92 Mharcaidh" ~ "Allt Aom",
    .default = site
  ))

uk_dat <- dat %>%
  filter(dat_id == "df_Perkins.xlsx")
uk_dat <- uk_dat %>%
  mutate(site = case_when(
    site == "Allt a‚ÄôMharcaidh" ~ "Allt Aom",
    .default = site
  ))
uk_mle <- mle_lat %>%
  filter(dat_id == "df_Perkins.xlsx") 
uk_mle <- uk_mle %>%
  mutate(site = case_when(
    site == "Allt a‚ÄôMharcaidh" ~ "Allt Aom",
    .default = site
  ))


rm(dat)
rm(mle_lat)

uk_biomass <- uk_dat %>%
  mutate(mass_m2 = body_mass*ind_n) %>%
  group_by(site) %>%
  summarize(sum_biomass = sum(mass_m2, na.rm = TRUE),
            sd_biomass = sd(mass_m2, na.rm = TRUE),
            n = n())

uk <- left_join(uk_biomass, uk_mle)
uk <- left_join(uk, env)
write_csv(uk, "results/UK-for-SEM.csv")
view(uk)

ggplot(uk, 
       aes(x = geographical_latitude, 
           y = mle_estimate,
           color = log10(sum_biomass))) +
  geom_point()

ggplot(uk, 
       aes(x = mle_estimate,
           y = log10(sum_biomass),
           ymin = log10(sum_biomass-sd_biomass),
           ymax = log10(sum_biomass+sd_biomass))) +
  geom_pointrange() 

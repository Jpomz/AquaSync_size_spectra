# multiplying inverts artificially increases sample size and CI's approach 0
# if Inverts or Inverts + fish, use number per meter squared
# If fish only, use no_km2

library(tidyverse)
library(sizeSpectra)
library(readxl)

neon <- read_excel("data/df_NEON.xlsx")

arik <- neon %>% filter(site == "ARIK_2017-03-22",
                organism_group == "Invertebrate") %>%
  select(site, sample, body_mass, count, multiplier)

arik %>%
  calcLike(
    negLL.fn = negLL.PLB.counts,
    x = .$body_mass,
    c = .$count,
    p = -1.5,
    vecDiff = 1)

arik %>% 
  mutate(ind_n = count * multiplier,
         n_sample = n_distinct(sample)) %>%
  calcLike(
    negLL.fn = negLL.PLB.counts,
    x = .$body_mass,
    c = .$ind_n,
    p = -1.5,
    vecDiff = 1)

arik %>% 
  mutate(ind_n = count * multiplier,
         n_sample = n_distinct(sample)) %>%
  ggplot(aes(x = body_mass,
             y = ind_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


arik %>% 
  mutate(ind_n = count * multiplier) %>%
  group_by(sample, body_mass) %>%
  summarise(sum_n = sum(ind_n)) %>%
  ungroup() %>%
  group_by(body_mass) %>%
  arrange(body_mass, sample) %>%
  summarise(mean_n = mean(sum_n)) %>%
  calcLike(
    negLL.fn = negLL.PLB.counts,
    x = .$body_mass,
    c = .$mean_n,
    p = -1.5,
    vecDiff = 1)
  
arik %>% 
  mutate(ind_n = count * multiplier) %>%
  group_by(sample, body_mass) %>%
  summarise(sum_n = sum(ind_n)) %>%
  ungroup() %>%
  group_by(body_mass) %>%
  arrange(body_mass, sample) %>%
  summarise(mean_n = mean(sum_n)) %>%
  ggplot(aes(x = body_mass,
             y = mean_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

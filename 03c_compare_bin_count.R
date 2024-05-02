#03c compare bin and count estimates

library(tidyverse)

count <- readRDS("derived_data/format_mle_count_results.RDS") %>%
  as_tibble() 
bin <- readRDS("derived_data/format_mle_bin_results.RDS") %>%
  as_tibble()


count2 <- count %>%
  rename(count_estimate = mle_estimate,
         count_lo = conf_lo,
         count_hi = conf_hi)
bin2 <- bin %>%
  rename(bin_estimate = mle_estimate,
         bin_lo = conf_lo,
         bin_hi = conf_hi)
# ggplot(bin,
#        aes(x = group_id,
#            y = bin_estimate)) +
#   geom_point()

dat2 <- full_join(count2, bin2) 

dat2 %>%
  select(group_id, organism_groups,
         count_estimate, bin_estimate) %>%
  mutate(count_minus_bin = count_estimate - bin_estimate) %>%
  ggplot(aes(x = group_id,
             y = count_minus_bin,
             color = organism_groups)) +
  geom_point()

dat2 %>%
  select(group_id, organism_groups,
         count_estimate, bin_estimate) %>%
  mutate(count_minus_bin = count_estimate - bin_estimate) %>%
  ggplot(aes(x = count_minus_bin,
             fill = organism_groups)) +
  geom_density(alpha = 0.5)

# compare estimates when count = 1
dat2 %>%
  select(dat_id, organism_groups,
         count_estimate, bin_estimate) %>%
  mutate(count_minus_bin = count_estimate - bin_estimate) %>%
  filter(dat_id %in% c("df.template - Lento Morin data_revised.xlsx", "df_NEON.xlsx")) %>%
  ggplot(aes(x = count_minus_bin)) +
  geom_density(alpha = 0.5) 

# long data

count$method = "count"
bin$method = "bin"
dat <- bind_rows(count, bin)

dat %>%
  ggplot(aes(x = mle_estimate,
             fill = method)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~organism_groups)

dat %>%
  select(group_id, method, geographical_latitude,
         organism_groups,
         mle_estimate,  
         conf_lo, conf_hi
         ) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate,
             ymin = conf_lo,
             ymax = conf_hi,
             color = method)) +
  geom_point(position = position_dodge(width = 0.5),
             alpha = 0.75) +
  facet_wrap(~organism_groups,
             scales = "free")



dat %>%
  select(group_id, method, geographical_latitude,
         organism_groups,
         mle_estimate,  
         conf_lo, conf_hi
  ) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate,
             ymin = conf_lo,
             ymax = conf_hi,
             color = method)) +
  geom_point(#position = position_dodge(),
             alpha = 0.75) +
  # facet_wrap(~organism_groups,
  #            scales = "free") +
  #scale_x_log10() +
  stat_smooth(method = "lm") +
  NULL


summary(lm(mle_estimate ~ abs(geographical_latitude) * method, 
           data = dat))



dat %>%
  filter(dat_id %in% c("df_NEON.xlsx"),
         group_id < 13525) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo,
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(~dat_id, 
             scales = "free") +
  labs(title = "NEON data including Fish + Macros",
       subtitle = "original data has counts not equal to 1")
ggsave("results/plots/neon_count_bin_estimates.jpg",
       width = 8, height = 8,
       dpi = 500)

dat %>%
  filter(dat_id %in% c("df_Pomeranz.xlsx",
                       "df_Perkins.xlsx",
                       "df_Arranz_01.xlsx")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo,
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(.~dat_id,
             scales = "free") +
  labs(title = "All data sets have counts = 1",
       subtitle = "Arranz = fish, Perkins = fish + macros, Pomeranz = macros")
ggsave("results/plots/count_1_count_bin_estimates.jpg",
       width = 8, height = 8,
       dpi = 500)

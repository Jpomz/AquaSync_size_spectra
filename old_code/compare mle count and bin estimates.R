# compare mle_bin and count


# Both analyses started with filtered data filtered_size_2024-02-07.RDS

library(tidyverse)

mle_count <- readRDS("results/mle_count_vecDiff-10_2024-03-14.rds")
mle_bin <- readRDS("results/mle_bin_filt_data_vecDiff-10_2024-03-18.rds")


mle_count$method <- "count"
mle_bin$method <- "bin"

names(mle_bin)

bind_rows(mle_count, mle_bin) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange()

bind_rows(mle_count, mle_bin) %>%
  filter(!str_detect(dat_id, "Irz|Arranz|AL|Carlos")) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  stat_smooth(method = "lm")

# facet by dat_id
bind_rows(mle_count, mle_bin) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(~dat_id, scales = "free_x")

bind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "Perkins|Pomeranz")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(~dat_id, scales = "free_x") +
  #stat_smooth(method = "lm") +
  theme_bw() +
  NULL

bind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "NEON")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(~dat_id, scales = "free_x") +
  #stat_smooth(method = "lm") +
  theme_bw() +
  NULL

bind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "NEON|Perkins|Pomeranz")) %>%
  ggplot(aes(x = geographical_latitude,
             #group = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  #facet_wrap(~dat_id, scales = "free_x") +
  labs(title = "NEON, Perkins, Pomeranz, estimates across latitude") +
  stat_smooth(method = "lm") +
  theme_bw() +
  NULL

bind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "Irz")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange() +
  facet_wrap(~dat_id, scales = "free_x") +
  #stat_smooth(method = "lm") +
  theme_bw() +
  NULL

bind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "Arranz")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange(
    linewidth = 0.75,
    position = position_dodge(width = 1)) +
  facet_wrap(~dat_id, scales = "free_x") +
  #stat_smooth(method = "lm") +
  theme_bw() +
  NULL

Zbind_rows(mle_count, mle_bin) %>%
  filter(str_detect(dat_id, "EKOLUR")) %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange(position = position_dodge(width = 1),
                  linewidth = 0.75) +
  facet_wrap(~dat_id, scales = "free_x") +
  #stat_smooth(method = "lm") +
  theme_bw() +
  ggthemes::scale_color_colorblind() +
  #scale_color_manual(values = c("coral", "dodgerblue")) +
  NULL

group_id_sample <- mle_count %>% 
  filter(abs(geographical_latitude) > 40,
         abs(geographical_latitude) < 50,
         !is.na(mle_estimate)) %>%
  select(group_id) %>%
  unique() %>%
  sample_n(50) %>%
  pull()

bind_rows(mle_count, mle_bin) %>%
  filter(group_id %in% group_id_sample) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange(#position = position_dodge(width = 0.1),
                  alpha = 0.75,
                  linewidth = 1.5) +
  theme_bw()



bind_rows(mle_count, mle_bin) %>%
  mutate(width = conf_hi - conf_lo) %>%
  filter(width>0.5) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange()


bind_rows(mle_count, mle_bin) %>%
  mutate(width = conf_hi - conf_lo) %>%
  filter(width>2) %>%
  group_by(method) %>%
  count()

bind_rows(mle_count, mle_bin) %>%
  filter(group_id == 676) %>%
  mutate(width = conf_hi - conf_lo)


bind_rows(mle_count, mle_bin) %>%
  mutate(width = conf_hi - conf_lo) %>%
  select(group_id, method, width) %>%
  filter(group_id != 13542) %>%
  filter(width > 1) %>%
  # dplyr::group_by(group_id, method) %>%
  # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # dplyr::filter(n > 1L) %>%
  pivot_wider(names_from = method, values_from = width) %>%
  mutate(bin_smaller = bin <= count) %>%
  filter(bin_smaller == TRUE)




bind_rows(mle_count, mle_bin) %>%
  mutate(width = conf_hi - conf_lo) %>%
  select(group_id, method, mle_estimate, conf_lo, conf_hi, width) %>%
  filter(group_id != 13542) %>%
  arrange(width, mle_estimate) %>%
  mutate(new_y = 1:n()) %>%
  ggplot(aes(y = new_y,
             x = mle_estimate, 
             xmin = conf_lo, 
             xmax = conf_hi,
             color = method)) +
  geom_pointrange()

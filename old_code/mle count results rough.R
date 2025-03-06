# load data
library(tidyverse)
#library(readxl)

mle_count <- readRDS("results/mle_count_vecDiff-10_2024-03-14.rds")

nrow(mle_count)

mle_count%>%
  summarize(mean = mean(mle_estimate, na.rm = TRUE),
            median = median(mle_estimate, na.rm = TRUE),
            min = min(mle_estimate, na.rm = TRUE),
            max = max(mle_estimate, na.rm = TRUE),
            q95 = quantile(mle_estimate, probs = 0.95, na.rm = TRUE),
            q05 = quantile(mle_estimate, probs = 0.05, na.rm = TRUE))

mle_count %>%
  filter(!is.na(mle_estimate)) %>%
  nrow() / nrow(mle_count)
# when vecDiff = 0.5, ~76% of the data cannot get a mle estimate
## only 615 estimates
# when vecdiff is 5 or 10, have a much higher coverage of estimates, but CI's are huge
# vecdiff = 10 --> 97.9% of data HAS estimates


names(mle_count)

# file_paths <- list.files("data/", 
#                          pattern = "*.xlsx")
# 
# 
# # Latitude ####
# #read in site data for lat long ####
# site_list <- list()
# for (i in 1:length(file_paths)){
#   df_site <- read_excel(
#     path = paste0("data/",
#                   path = file_paths[i]),
#     sheet = "site_data")
#   df_site <- df_site %>%
#     select(site, geographical_latitude) %>%
#     mutate(site = as.character(site))
#   site_list[[i]] <- df_site
# }
# # 
# # 
# names(site_list) <- file_paths
# lat_df <- bind_rows(site_list, .id = "id")
# lat_df <- lat_df %>%
#   rename(dat_id = id)
# lat_df
# # dat_lat <- left_join(dat_out,
# #                      lat_df)
# 
# setdiff(lat_df$dat_id, mle_count$dat_id)
# setdiff(mle_count$dat_id, lat_df$dat_id)
# 
# mle_lat <- left_join(mle_count, lat_df)

mle_lat <- mle_count

ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "MLE_count all estimates",
       subtitle = "2560 -204 rows with missing values = 2356") +
  theme_bw()

ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         color = organism_groups)) +
  geom_point(alpha = 0.5) +
  labs(title = "MLE_count all estimates, just estimate",
       subtitle = "2560 -204 rows with missing values = 2356")+
  theme_bw()

# # color based on nrow
# ggplot(mle_lat,
#        aes(
#          x = abs(geographical_latitude),
#          y = mle_estimate,
#          ymin = conf_lo,
#          ymax = conf_hi,
#          color = log10(nrow_dat_split))) +
#   geom_pointrange()

# ggplot(mle_lat,
#        aes(
#          y = mle_estimate,
#          ymin = conf_lo,
#          ymax = conf_hi,
#          x = log10(nrow_dat_split))) +
#   geom_pointrange()


mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width > 1) %>%
  nrow() / nrow(mle_lat) 
# vecdiff = 10 --> 73% of sites have ci_width > 1
mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width < 1) %>%
  nrow() / nrow(mle_lat) 
# vecdiff = 10 --> 24% of sites have ci_width < 1
mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width < 0.5) %>%
  nrow() / nrow(mle_lat) 
# vecdiff = 10 --> 22% of sites have ci_width < 0.5


# mle_lat %>%
#   mutate(ci_width = conf_hi - conf_lo) %>%
#   filter(ci_width > 1) %>%
#   select(dat_id, site) %>%
#   unique() %>%
#   View()

mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width > 4) %>%
  nrow() / nrow(mle_lat) 
# vecdiff = 10 --> 38% of sites have ci_width > 4


mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width < 1) %>%
  ggplot(
         aes(
           x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  #stat_smooth(method = "lm")+
  ylim(c(-3, 2)) +
  labs(title = "MLE_count ci's <1",
       subtitle = "626 sites -24 with missing data = 602")+
  theme_bw()

mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width < 1) %>%
  ggplot(
    aes(
      x = abs(geographical_latitude),
      y = mle_estimate,
      color = organism_groups)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm",
              inherit.aes = FALSE,
              aes(x = abs(geographical_latitude),
                  y = mle_estimate))+
  ylim(c(-3, 2)) +
  labs(title = "MLE_count ci's <1, just estimate",
       subtitle = "626 sites -24 with missing data = 602")+
  theme_bw()


# isd <- readRDS("results/fit3_4c_2000i_2024-02-07.rds")
# posts_varint = isd$data %>% 
#   distinct(site, xmin, xmax) %>% 
#   mutate(ind_n = 1) %>% 
#   tidybayes::add_epred_draws(isd, re_formula = NULL)
# 
# posts_lat <- left_join(posts_varint, lat_df)

posts <- readRDS("results/brm_posts_2024-03-05.rds")
names(posts)

# posts_lat <- posts
# posts_lat %>% 
#   ggplot(aes(x = abs(geographical_latitude),
#              y = .epred,
#              color = dat_id)) + 
#   #tidybayes::stat_halfeye(scale = 0.2) 
#   tidybayes::stat_pointinterval(position = position_dodge()) +
#   labs(title = "isdBayes posterior") +
#   theme(legend.position = "none")

# posts_lat %>% 
#   ggplot(aes(x = abs(geographical_latitude), y = .epred)) + 
#   #tidybayes::stat_halfeye(scale = 0.2) 
#   tidybayes::stat_pointinterval()
# 
# posts_lat %>% 
#   ggplot(aes(x = abs(geographical_latitude), y = .epred)) + 
#   #tidybayes::stat_halfeye(scale = 0.2) 
#   tidybayes::stat_pointinterval() +
#   stat_smooth(method = "lm") +
#   ylim(c(-3, 2))


ci_posts <- posts %>%
  group_by(group_id,
           dat_id,
           geographical_latitude,
           organism_groups,
           n_obs_dat,
           xmin, 
           xmax) %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.975)) %>%
  mutate(width = abs(l95 - u95),
         size_range = log10(xmax) - log10(xmin))

ci_posts %>%
  #filter(width <0.96) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "isdBayes posterior all estimates",
       subtitle = "2560 -130 rows with missing data = 2430 sites") +
  theme_bw() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free") +
  NULL

ci_posts %>%
  #filter(width <0.96) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             group = group_id,
             color = organism_groups)) +
  geom_point(alpha = 0.5) +
  labs(title = "isdBayes posterior, just median",
       subtitle = "2560 -130 rows with missing data = 2430 sites") +
  theme_bw() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free") +
  NULL


ci_posts %>%
  filter(width < 1) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "isdBayes posterior width < 1",
       subtitle = "620 sites") +
  theme_bw() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free") +
  NULL


ci_posts %>%
  filter(width < 1) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             group = group_id,
             color = organism_groups)) +
  geom_point(alpha = 0.5) +
  labs(title = "isdBayes posterior width < 1, just median",
       subtitle = "620 sites") +
  theme_bw() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free") +
  NULL

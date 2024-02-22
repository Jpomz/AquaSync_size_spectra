# load data
library(tidyverse)
library(readxl)

mle_count <- readRDS("results/mle_count_vecDiff-0.5_2024-02-07.rds")

mle_count%>%
  summarize(mean = mean(mle_estimate, na.rm = TRUE),
            median = median(mle_estimate, na.rm = TRUE),
            min = min(mle_estimate, na.rm = TRUE),
            max = max(mle_estimate, na.rm = TRUE),
            q95 = quantile(mle_estimate, probs = 0.95, na.rm = TRUE),
            q05 = quantile(mle_estimate, probs = 0.05, na.rm = TRUE))

mle_count %>%
  filter(is.na(mle_estimate)) %>%
  nrow() / nrow(mle_count)
# when vecDiff = 0.5, ~76% of the data cannot get a mle estimate
## only 633 estimates
# when vecdiff is 5 or 10, have a much higher coverage of estimates, but CI's are huge

file_paths <- list.files("data/", 
                         pattern = "*.xlsx")


# Latitude ####
#read in site data for lat long ####
site_list <- list()
for (i in 1:length(file_paths)){
  df_site <- read_excel(
    path = paste0("data/",
                  path = file_paths[i]),
    sheet = "site_data")
  df_site <- df_site %>%
    select(site, geographical_latitude) %>%
    mutate(site = as.character(site))
  site_list[[i]] <- df_site
}
# 
# 
names(site_list) <- file_paths
lat_df <- bind_rows(site_list, .id = "id")
lat_df <- lat_df %>%
  rename(dat_id = id)
lat_df
# dat_lat <- left_join(dat_out,
#                      lat_df)

setdiff(lat_df$dat_id, mle_count$dat_id)
setdiff(mle_count$dat_id, lat_df$dat_id)

mle_lat <- left_join(mle_count, lat_df)

ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi)) +
  geom_pointrange()

# color based on nrow
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = log10(nrow_dat_split))) +
  geom_pointrange()

ggplot(mle_lat,
       aes(
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         x = log10(nrow_dat_split))) +
  geom_pointrange()


mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width > 1) %>%
  nrow() / nrow(mle_lat) 
#0% of sites have ci_width > 1

mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width > 1) %>%
  select(dat_id, site) %>%
  unique() %>%
  View()

mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width > 4) %>%
  nrow() / nrow(mle_lat) 
# 33% > 4


mle_lat %>%
  mutate(ci_width = conf_hi - conf_lo) %>%
  filter(ci_width < 1) %>%
  ggplot(
         aes(
           x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi)) +
  geom_pointrange() +
  stat_smooth(method = "lm")+
  ylim(c(-3, 2))


isd <- readRDS("results/fit3_4c_2000i_2024-02-07.rds")
posts_varint = isd$data %>% 
  distinct(site, xmin, xmax) %>% 
  mutate(ind_n = 1) %>% 
  tidybayes::add_epred_draws(isd, re_formula = NULL)

posts_lat <- left_join(posts_varint, lat_df)

posts_lat %>% 
  ggplot(aes(x = abs(geographical_latitude),
             y = .epred,
             color = dat_id)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval(position = position_dodge())

posts_lat %>% 
  ggplot(aes(x = abs(geographical_latitude), y = .epred)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval()

posts_lat %>% 
  ggplot(aes(x = abs(geographical_latitude), y = .epred)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval() +
  stat_smooth(method = "lm") +
  ylim(c(-3, 2))

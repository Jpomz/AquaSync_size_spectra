# 3 datasets with HFI
# aquasync day 2
# JPZ

library(tidyverse)
library(sizeSpectra)
library(readxl)

env_data <- read_csv("data/env_data_subset.csv")

file_paths <- list.files("data/", 
                         pattern = "*.xlsx")

name_target <- c("site",
                 "sampling_method",
                 "sample",
                 "sampling_area",
                 "organism_group",
                 "taxon", 
                 "body_mass",
                 "body_length",
                 "body_weight_units",
                 "body_length_units",
                 "count",
                 "multiplier")

file_paths <- file_paths[c(13, 15, 16)]

data_list <- list()

for(i in 1:length(file_paths)){
  in_names <- names(
    read_excel(
      path = paste0("data/",
                    path = file_paths[i])))
  if(identical(name_target, in_names)){
    dat_in <- read_excel(
      path = paste0("data/",
                    file_paths[i]),
      col_types = c("text", "text", "numeric", 
                    "numeric", "text", "text", 
                    "numeric", "numeric", 
                    "text", "text", "numeric",
                    "numeric"))
  } else {
    {
      list_to_fix_names[[i]] <- file_paths[i]
      data_list[[i]] <- "Need to fix data"
      next
    }
  }
  data_list[[i]] <- dat_in
}
names(data_list) <- file_paths
dat_df <- bind_rows(data_list, .id = "id")
dat_df <- dat_df %>%
  separate(id, into = c("rm1", "dat_id", "rm2")) %>%
  select(-rm1, -rm2)

# what are the body weight units?
dat_df$body_weight_units %>% unique()

# convert everything to mg
dat_df <- dat_df %>%
  mutate(body_mass = case_when(
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "mg DW" ~ body_mass,
    body_weight_units == "mg" ~ body_mass
  ))

dat_df2 <- dat_df |> 
  #filter(site %in% filter_vector) %>%
  group_by(dat_id, site) %>%
  mutate(ind_n = count * multiplier) %>%
  filter(!is.na(ind_n)) %>%
  select(dat_id, site, body_mass, ind_n) %>%
  mutate(group_id = cur_group_id()) 

dat_split <- dat_df2 |>
  split(dat_df2$group_id)


mle_count <- dat_split |>
  map(\(df) calcLike(
    negLL.fn = negLL.PLB.counts,
    x = df$body_mass,
    c = df$ind_n,
    p = -1.5))

mle_count_rows <- list()
for (i in 1:length(mle_count)){
  out <- data.frame(mle_estimate = mle_count[[i]]$MLE,
                    conf_lo = mle_count[[i]]$conf[1],
                    conf_hi = mle_count[[i]]$conf[2])
  mle_count_rows[[i]] <- out
}

mle_count_results <- bind_rows(mle_count_rows, .id = "group_id") %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(dat_df2 %>%
              select(dat_id, site, group_id) %>%
              unique())

#read in site data for lat long ####
site_list <- list()
for (i in 1:length(file_paths)){
  df_site <- read_excel(
    path = paste0("data/",
                  path = file_paths[i]), 
    sheet = "site_data")
  df_site <- df_site %>%
    select(site, geographical_latitude)
  site_list[[i]] <- df_site
}

lat_df <- bind_rows(site_list) %>%
  unique()

# plot count ####
mle_lat <- left_join(mle_count_results,
                     lat_df)
names(mle_lat)

# combine mle and env data
m_site <- mle_lat$site %>% unique
e_site <- env_data$site %>% unique

setdiff(m_site, e_site)
setdiff(e_site, m_site)

env_data <- env_data %>%
  mutate(site = case_when(
    site == "Allt a\x92 Mharcaidh" ~ "Allt Aom",
    .default = site
  ))

mle_lat <- mle_lat %>%
  mutate(site = case_when(
    site == "Allt a‚ÄôMharcaidh" ~ "Allt Aom",
    .default = site
  ))

m_site <- mle_lat$site %>% unique
e_site <- env_data$site %>% unique

setdiff(m_site, e_site)
setdiff(e_site, m_site)

mle_lat <- left_join(env_data,
                     mle_lat)


# mle ~ lat, by data
ggplot(mle_lat,
       aes(x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  stat_smooth(aes(x = abs(geographical_latitude),
                  y = mle_estimate),
              method = "lm",
              inherit.aes = FALSE) +
  theme_bw() +
  labs(x = "Absolute Latitude",
       y = "lambda estimate")

# mle ~ 1993
ggplot(mle_lat,
       aes(x = buf_HFP_1993,
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  theme_bw() +
  stat_smooth(method = "lm")

# mle ~ 2009
ggplot(mle_lat,
       aes(x = buf_HFP_2009,
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  theme_bw()


# color by HFP
ggplot(mle_lat,
       aes(x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = buf_HFP_1993)) +
  geom_pointrange() +
  theme_bw() +
  labs(x = "Absolute Latitude",
       y = "lambda estimate")

ggplot(mle_lat,
       aes(x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = buf_HFP_2009)) +
  geom_pointrange() +
  theme_bw() + 
  labs(x = "Absolute Latitude",
       y = "lambda estimate")

ggplot(mle_lat,
       aes(x = buf_HFP_1993,
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = abs(geographical_latitude))) +
  geom_pointrange() +
  theme_bw() +
  labs(x = "HFP 1993",
       y = "lambda estimate")

ggplot(mle_lat,
       aes(x = buf_HFP_2009,
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = abs(geographical_latitude))) +
  geom_pointrange() +
  theme_bw()+
  labs(x = "HFP 2009",
       y = "lambda estimate")

ggplot(mle_lat,
       aes(x = abs(geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = buf_HFP_2009)) +
  geom_pointrange() +
  theme_bw()+
  labs(x = "Absolute Latitude",
       y = "lambda estimate")

ggplot(mle_lat,
       aes(x = buf_HFP_1993,
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi)) +
  geom_pointrange() +
  theme_bw() +
  geom_smooth(method = "lm")+
  labs(x = "HFP 1993",
       y = "lambda estimate")


# quick model fits for funsies
# just latitude
summary(lm(mle_estimate ~ abs(geographical_latitude), data = mle_lat))
# just hfp1993
summary(lm(mle_estimate ~ buf_HFP_1993, data = mle_lat))
# just hfp 2009
summary(lm(mle_estimate ~ buf_HFP_2009, data = mle_lat))

# additive 2009
summary(lm(mle_estimate ~ abs(geographical_latitude) + buf_HFP_2009, data = mle_lat))

# interaction 2009
summary(lm(mle_estimate ~ abs(geographical_latitude) * buf_HFP_2009, data = mle_lat))

# additive 1993
summary(lm(mle_estimate ~ abs(geographical_latitude) + buf_HFP_1993, data = mle_lat))

# interaction 1993
summary(lm(mle_estimate ~ abs(geographical_latitude) * buf_HFP_1993, data = mle_lat))

plot(density(env_data$buf_HFP_1993))
plot(density(env_data$buf_HFP_2009))

plot(density(scale(env_data$buf_HFP_1993)))
plot(density(scale(env_data$buf_HFP_2009)))

# brms multiple and future
# script for parallelizing 
library(tidyverse)
library(brms)
#library(cmdstanr)
library(future)
#library(furrr)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")
dat <- dat %>%
  group_by(group_id) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))

ggplot(dat %>% distinct(group_id),
       aes(x = group_id)) +
  geom_histogram(bins = 4)

neon <- dat %>% filter(dat_id == "df_NEON.xlsx")
neon %>% pull(group_id) %>% unique() %>% length()
# 272 groups

availableCores()
# 16

plan(tweak(multisession, workers = 14))

bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

neon_list <- neon %>%
  #filter(group_id < 2301) %>%
  group_split()

group_ids <- lapply(neon_list, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)

length(neon_list)  

tictoc::tic()
brm_single_mods = brm_multiple(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = neon_list,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4, 
  iter = 2000,
  future = TRUE,
  combine = FALSE)
tictoc::toc()

# all neon, 1 c 2000 iter = 815s = 13.5min
# all neon 4 c 2000 iter = 2400s = 40 min
pp_check(brm_single_mods[[4]], type = "boxplot") +
  scale_y_log10()

head(rhat(brm_single_mods[[1]]))

rhats <- brm_single_mods %>%
  lapply(FUN = rhat) %>%
  bind_rows()

rhats %>%
  filter(b_Intercept >= 1.1)

posts <- NULL
for (i in 1:length(brm_single_mods)){
  posts_out <- brm_single_mods[[i]]$data %>% 
    distinct(xmin, xmax) %>% 
    mutate(ind_n = 1) %>% 
    tidybayes::add_epred_draws(brm_single_mods[[i]], re_formula = NULL) 
  posts_out$group_id <- group_ids[i]
  posts[[i]] <- posts_out
}

posts <- bind_rows(posts)
ggplot(posts,
       aes(x = group_id,
           y = .epred)) +
  tidybayes::stat_pointinterval()

library(foreach)
tictoc::tic()
posts <- foreach(
  i = 1:length(brm_single_mods),
  .combine = rbind) %do% {
    draws <-  brm_single_mods[[i]]$data %>% 
      distinct(xmin, xmax) %>% 
      mutate(ind_n = 1) %>% 
      tidybayes::add_epred_draws(brm_single_mods[[i]], re_formula = NULL)
    draws$group_id <- group_ids[i] 
  draws
  }
tictoc::toc()
ggplot(posts,
       aes(x = group_id,
           y = .epred)) +
  tidybayes::stat_pointinterval()

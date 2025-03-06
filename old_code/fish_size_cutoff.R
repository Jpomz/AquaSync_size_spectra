# exploring fish size cutoffs

# tried cutting off at 1e03 and made the fits worse
# ci_widths for orig ~ 4.2; for filtered data ~ 18.8
# nearly all orig are smaller than filtered
library(tidyverse)
library(brms)
library(tidybayes)
library(isdbayes)

# dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")
# dat <- dat %>%
#   mutate(group_id = as.character(group_id))

irz1 <- readxl::read_excel("data/df_France_Irzetal_part1.xlsx")
irz2 <- readxl::read_excel("data/df_France_Irzetal_part2.xlsx")

irz <- bind_rows(irz1, irz2)

x <- irz$body_mass
minx <- min(x)
maxx <- max(x)
hist(log10(x), 
    breaks = 2^(floor(log2(minx)):
                  ceiling(log2(maxx))))


sizeSpectra::binData(x, binWidth = "2k")$binVals %>%
  ggplot(aes(x = binMid, y = binCountNorm)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


dat <- readRDS("derived_data/all_size_stitched_no_filtering.RDS")

# dat %>%
#   filter(dat_id == "df_CarlosUPM_18_01_2024_Aitor.xlsx",
#          organism_groups == "Fish",
#          organism_group == "Invertebrates") %>%
#   select(site, organism_group, organism_groups) %>%
#   distinct() %>% View()
  
dat %>%
  filter(dat_id == "df_CarlosUPM_18_01_2024_Aitor.xlsx",
         organism_groups == "Fish",
         organism_group == "Invertebrates") %>%
  pull(site) %>% unique()
  

# need to add code to update body_weight_units
dat <- dat %>%
  mutate(body_mass = case_when(
    # convert g to mg
    # convert wet weight to dry weight
    # Using an arbitrary value of 0.25 for now
    # assuming "dry_weight" is in mg
    body_weight_units == "dry_weight" ~ body_mass,
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "g WW" ~ body_mass *1000 *0.25,
    body_weight_units == "grams/wet" ~ body_mass *1000 *0.25,
    body_weight_units == "M.mg" ~ body_mass,
    body_weight_units == "mg" ~ body_mass,
    body_weight_units == "mg (dry_mass)" ~ body_mass,
    body_weight_units == "mg dry mass" ~ body_mass,
    body_weight_units == "mg DW" ~ body_mass,
    body_weight_units == "mg wet weight" ~ body_mass * 0.25,
    body_weight_units == "mg WW" ~ body_mass * 0.25,
    body_weight_units == "mg_DM" ~ body_mass,
    body_weight_units == "mg_dry_mass" ~ body_mass
  ))

dat <- dat %>%
  group_by(group_id) %>%
  mutate(ind_n = count * multiplier)
  

fish <- dat %>%
  filter(organism_groups == "Fish" |
           organism_groups == "fish" )

dim(fish)

head(fish)
fish %>% ungroup() %>% distinct(dat_id, sampling_method)

#View(fish)

fish_summary <- fish %>%
  group_by(dat_id,
           sampling_method,#group_id, 
           body_mass) %>%
  summarize(sum_n = sum(ind_n)) 

fish_summary %>%
  filter(dat_id == "df_CarlosUPM_18_01_2024_Aitor.xlsx") %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = sampling_method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  theme(legend.position = "none")+
  # labs(title = "Fish abundance by body size",
  #      subtitle = "-142 rows",
  #      y = "count*multiplier = ind_n",
  #      x = "body mass in mg dw") +
  NULL

fish_summary %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = sampling_method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  # labs(title = "Fish abundance by body size",
  #      subtitle = "-142 rows",
  #      y = "count*multiplier = ind_n",
  #      x = "body mass in mg dw") +
  NULL

fish_summary %>%
  filter(str_detect(dat_id, "Irzetal")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = sampling_method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  #facet_wrap(~dat_id, scales = "free") +
  theme_bw() +
  # theme(legend.position = "none")+
  labs(title = "Fish abundance by body size",
       subtitle = "Irz et al. part 1 & 2",
       y = "count*multiplier = ind_n",
       x = "body mass in log10 mg dw") +
  NULL

fish_summary %>%
  filter(str_detect(dat_id, "Irzetal")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = sampling_method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()  +
  facet_wrap(~sampling_method) +
  theme_bw()+
  theme(legend.position = "none")





fish_summary %>%
  filter(str_detect(dat_id, "uruguay|Myanmar")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  labs(title = "Fish abundance by body size",
       y = "count*multiplier = ind_n",
       x = "body mass in log10 mg dw") +
  theme_bw() +
  NULL

fish_summary %>%
  filter(str_detect(dat_id, "EKOLUR|AL_")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  labs(title = "Fish abundance by body size",
       y = "count*multiplier = ind_n",
       x = "body mass in log10 mg dw") +
  theme_bw() +
  NULL

fish_summary %>%
  filter(str_detect(dat_id, "Pelinson")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  labs(title = "Fish abundance by body size",
       y = "count*multiplier = ind_n",
       x = "body mass in log10 mg dw") +
  theme_bw() +
  NULL





# group summary -----------------------------------------------------------

group_summary <- fish %>%
  group_by(dat_id,
           sampling_method,
           group_id, 
           body_mass) %>%
  summarize(sum_n = sum(ind_n)) 

group_summary %>%
  filter(str_detect(dat_id, "RiMSEC")) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = group_id)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  labs(title = "Fish abundance by body size",
       y = "count*multiplier = ind_n",
       x = "body mass in log10 mg dw") +
  theme_bw() +
  NULL

group_summary %>%
  ungroup() %>%
  group_by(sampling_method, group_id, dat_id) %>%
  mutate(new_color = n()) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = new_color)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  # theme(legend.position = "none")+
  # labs(title = "Fish abundance by body size",
  #      subtitle = "-142 rows",
  #      y = "count*multiplier = ind_n",
  #      x = "body mass in mg dw") +
  NULL



# Code to update brms fits ------------------------------------------------



posts <- readRDS("results/brm_posts_2024-03-05.rds")

fish_posts <- posts %>%
  filter(organism_groups == "Fish" |
           organism_groups == "fish" ) %>%
  group_by(group_id,
           dat_id) %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.975)) %>%
  mutate(width = abs(l95 - u95))

fish_posts %>%
  ungroup() %>%
  select(group_id, dat_id, width) %>%
  filter(width >1) %>%
  distinct(dat_id)




# update fish fits with 1e03 cutoff ---------------------------------------
fish_filt <- fish %>%
  filter(body_mass >= 1e03) %>%
  group_by(group_id) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))


fish_filt %>%
  group_by(dat_id, body_mass) %>%
  summarize(sum_n = sum(ind_n)) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = dat_id)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~dat_id, scales = "free") +
  theme(legend.position = "none")

# I think this is all "fish" groups. 
# what I meant to do was just the "wide" fish fits based on posterior CrI. 
# but maybe this is ok actually, see if the overall CrI posts are smalle?
fish_ids <- pull(fish, group_id) %>% unique()

fish_dummy <- fish_filt %>%
  filter(group_id == fish_ids[1])

brm_dummy = brm(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = fish_dummy,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4,
  iter = 20)


# update full fish list ####
fish_update <- NULL
for(i in 1:length(fish_ids)){
  fish_out <- update(brm_dummy,
                     newdata = fish_filt %>%
                       filter(group_id == fish_ids[i]),
                     iter = 2000)
  fish_update[[i]] <- fish_out
}

# # %dopar% from original fit
# # can I modify this to improve speed in this update?
# plan(list(
#   tweak(multisession, workers = workers1),
#   tweak(multisession, workers = workers2) # drop this I think
# ))
# 
# run1_start <- Sys.time()
# 
# brm1 <- foreach(
#   i = 1:length(list1)) %dopar% {
#     update(object = brm0[[1]],
#            newdata = list1[[i]],
#            iter = update_iter)}
# 
# run1_end <- Sys.time()
# (run1 <- run1_end - run1_start)

tictoc::tic()
posts_fish_list <- NULL
for (i in 1:length(fish_update)){
  posts_out <- fish_update[[i]]$data %>%
    distinct(xmin, xmax) %>%
    mutate(ind_n = 1) %>%
    possibly(tidybayes::add_epred_draws(fish_update[[i]], re_formula = NULL), otherwise = NULL)
  posts_out$group_id <- fish_ids[i]
  posts_fish_list[[i]] <- posts_out
}
tictoc::toc()

names(posts_fish_list) <- fish_ids[1:length(posts_fish_list)]

# library(furrr)
# 
# fish_update %>%
#   map(\(dat) length(dat))
# 
# fish_dat_map <- 
#   fish_update |>
#   map(\(dat) dat$data%>%
#         distinct(xmin, xmax) %>%
#         mutate(ind_n = 1)) 
# 
# future_map2(
#   .x = fish_dat_map,
#   .y = fish_update,
#   seed = TRUE,
#   .f = possibly(
#   function(.x, .y){
#       add_epred_draws(newdata = .x,
#                       object = .y,
#                       re_formula = NULL)},
#   otherwise = NULL
#   ))
# 
# fish_post_map
# names(fish_post_map) <- fish_ids
# list_c(fish_post_map)
# 
# bind_rows(fish_post_map)
# # install.packages("doFuture")
# library(doFuture)
# plan(multisession, workers = 4)
# 
# getGlobalsAndPackages()
# tictoc::tic()
# doFuture_fish_list <- foreach(i = 1:length(fish_update)) %dofuture% {
#   posts_out <- fish_update[[i]]$data %>%
#     distinct(xmin, xmax) %>%
#     mutate(ind_n = 1) %>%
#     tidybayes::add_epred_draws(fish_update[[i]], re_formula = NULL)
#   posts_out$group_id <- fish_ids[i]
#   posts_out
# }
# tictoc::toc()



fish_post_filt <- bind_rows(posts_fish_list)

fish_post_filt %>% 
  left_join(dat %>%
              distinct(group_id,
                       dat_id,
                       geographical_latitude,
                       organism_groups)) %>%
  ggplot(aes(x = abs(geographical_latitude),
             group = group_id,
             y = .epred)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2)

names(fish_post_filt)

fish_post_filt %>%
  ungroup() %>%
  filter(.draw < 100) %>%
  select(group_id, .epred) %>%
  mutate(dat = "filt") %>%
  bind_rows(posts %>% 
              ungroup() %>%
              filter(.draw < 100,
                     group_id %in% fish_ids[1:length(posts_fish_list)]) %>%
              select(group_id, .epred) %>%
              mutate(dat = "orig")) %>%
  ggplot(aes(x = group_id,
             #group = group_id,
             y = .epred,
             color = dat)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2)
  


fish_post_filt %>%
  ungroup() %>%
  filter(.draw < 2000) %>%
  select(group_id, .epred) %>%
  mutate(dat = "filt") %>%
  bind_rows(posts %>% 
              ungroup() %>%
              filter(.draw < 2000,
                     group_id %in% fish_ids[1:length(posts_fish_list)]) %>%
              select(group_id, .epred) %>%
              mutate(dat = "orig")) %>%
  group_by(group_id, dat) %>%
  summarize(l95 = quantile(.epred, probs = 0.025),
            u95 = quantile(.epred, probs = 0.975)) %>%
  mutate(width = abs(l95 - u95)) %>%
  ungroup() %>%
  group_by(dat) %>%
  summarize( mean(width),
             sd(width))



fish_post_filt %>%
  ungroup() %>%
  filter(.draw < 2000) %>%
  select(group_id, .epred) %>%
  mutate(dat = "filt") %>%
  bind_rows(posts %>% 
              ungroup() %>%
              filter(.draw < 2000,
                     group_id %in% fish_ids[1:length(posts_fish_list)]) %>%
              select(group_id, .epred) %>%
              mutate(dat = "orig")) %>%
  group_by(group_id, dat) %>%
  summarize(l95 = quantile(.epred, probs = 0.025),
            u95 = quantile(.epred, probs = 0.975)) %>%
  mutate(width = abs(l95 - u95)) %>%
  pivot_wider(values_from = width, names_from = dat, id_cols = group_id) %>%
  mutate(filt_smaller = filt < orig) %>%
  filter(filt_smaller == TRUE)



fish_post_filt %>%
  ungroup() %>%
  filter(.draw < 2000) %>%
  select(group_id, .epred) %>%
  mutate(dat = "filt") %>%
  bind_rows(posts %>% 
              ungroup() %>%
              filter(.draw < 2000,
                     group_id %in% fish_ids[1:length(posts_fish_list)]) %>%
              select(group_id, .epred) %>%
              mutate(dat = "orig")) %>%
  group_by(group_id, dat) %>%
  summarize(l95 = quantile(.epred, probs = 0.025),
            u95 = quantile(.epred, probs = 0.975)) %>%
  mutate(width = abs(l95 - u95)) %>%
  pivot_wider(values_from = width, names_from = dat, id_cols = group_id) %>%
  mutate(diff = filt - orig) %>%
  ungroup() %>%
  summarize(mean(diff))


# fish count instead of ind_n ####

# 10006 has CI ~ 4.9 units wide
f1006 <- fish %>%
  filter(group_id == 10006) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))

brm_dummy = brm(
  body_mass | vreal(count, xmin, xmax) ~ 1,
  data = f1006,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4,
  iter = 20)

fish_out <- update(brm_dummy,
                   newdata = f1006,
                   cores = 4,
                   iter = 2000)


posts_out <- fish_out$data %>%
  distinct(xmin, xmax) %>%
  mutate(count = 1) %>%
  tidybayes::add_epred_draws(fish_out, re_formula = NULL)

fish_posts %>%
  filter(group_id == 10006)
posts_out %>% 
  summarize(median = median(.epred),
            l95 = quantile(.epred, probs = 0.025),
            u95 = quantile(.epred, probs = 0.975)) %>%
  mutate(width = abs(l95 - u95))

fish_out_ind_n <- update(brm_dummy,
                   newdata = f1006 %>%
                     select(-count) %>%
                     mutate(count = ind_n * 1000),
                   cores = 4,
                   iter = 2000)

fish_out_ind_n$data %>%
  distinct(xmin, xmax) %>%
  mutate(count = 1) %>%
  tidybayes::add_epred_draws(fish_out_ind_n,
                             re_formula = NULL) %>%
  summarize(median = median(.epred),
            l95 = quantile(.epred, probs = 0.025),
            u95 = quantile(.epred, probs = 0.975)) %>%
  mutate(width = abs(l95 - u95))

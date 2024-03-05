# parallel_update results
library(tidyverse)
library(brms)
library(tidybayes)
library(future)
library(foreach)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")
dat <- dat %>%
  mutate(group_id = as.character(group_id))

# names(dat)
dat %>% group_by(group_id,
                 dat_id,
                 geographical_latitude) %>%
  summarize(n = n(),
            sum_ind = sum(ind_n)) %>%
  mutate(n = case_when(
    sum_ind > n ~ sum_ind,
    n > sum_ind ~ n))

dat %>%
  group_by(dat_id, organism_groups) %>%
  filter(organism_groups == "Fish") %>%
  summarize(min = min(body_mass)) %>%
  pull(min) %>% sd
         
         
# dat %>% group_by(group_id,
#                  dat_id,
#                  geographical_latitude) %>%
#   summarize(n = n()) %>%
#   ggplot(aes(x = group_id,
#              y = n)) +
#   geom_point() +
#   scale_y_log10() +
#   geom_hline(yintercept = 300)

# dat <- dat %>%
#   select(group_id, organism_groups, geographical_latitude) %>%
#   unique()
brm1 <- readRDS("results/lenovo_run/brm1_2024-03-03.rds")
group_id1 <- readRDS("results/lenovo_run/group_ids_brm1_2024-03-03.rds")
names(brm1) <- group_id1

brm2 <- readRDS("results/lenovo_run/brm2_2024-03-03.rds")
group_id2 <- readRDS("results/lenovo_run/group_ids_brm2_2024-03-03.rds")
names(brm2) <- group_id2

brm3 <- readRDS("results/lenovo_run/brm3_2024-03-03.rds")
group_id3 <- readRDS("results/lenovo_run/group_ids_brm3_2024-03-03.rds")
names(brm3) <- group_id3

brm4 <- readRDS("results/lenovo_run/brm4_2024-03-03.rds")
group_id4 <- readRDS("results/lenovo_run/group_ids_brm4_2024-03-03.rds")
names(brm4) <- group_id4

brm <- c(brm1, brm2, brm3, brm4)
names(brm)
length(brm)
rm(brm1)
rm(brm2)
rm(brm3)
rm(brm4)
rm(group_id1)
rm(group_id2)
rm(group_id3)
rm(group_id4)

# plan(multisession, workers = 15)
# 
# tictoc::tic()
# posts <- foreach (
#   i = 1:length(brm)) %dopar% {
#   out <- brm[[i]]$data %>% 
#     distinct(xmin, xmax) %>% 
#     mutate(ind_n = 1) %>% 
#     tidybayes::add_epred_draws(brm[[i]], re_formula = NULL)
#   
#   out$group_id <- names(brm)[i]
#   return(out)
#   #posts[[i]] <- posts_out
#   }
# tictoc::toc() #520 seconds
# plan(sequential)

# add posterior draws #### 
tictoc::tic()
posts_seq <- NULL
for (i in 1:length(brm)){
  posts_out <- brm[[i]]$data %>% 
    distinct(xmin, xmax) %>% 
    mutate(ind_n = 1) %>% 
    tidybayes::add_epred_draws(brm[[i]], re_formula = NULL) 
  posts_out$group_id <- names(brm)[i]
  posts_out$n_obs_dat <- nrow(brm[[i]]$data)
  posts_seq[[i]] <- posts_out
}
tictoc::toc() # something like 360 seconds with workers = 15
# time with sequential = 365
# why is %dopar% above taking longer??

length(brm) == length(posts_seq)

posts <- bind_rows(posts_seq)

posts <- left_join(posts,
                   dat %>%
                     group_by(group_id,
                                    dat_id,
                                    geographical_latitude,
                                    organism_groups) %>%
                     summarize(n = n(),
                               sum_ind = sum(ind_n)) %>%
                     mutate(n_obs_dat = case_when(
                       sum_ind > n ~ sum_ind,
                       n > sum_ind ~ n)) %>%
                     ungroup() %>%
                     select(-n, -sum_ind),
                   by = "group_id")



dim(posts) # 10.2 million rows

# check NEON to see if it "looks right"
# make sure to use group = group_id ####
posts %>% 
  filter(dat_id == "df_NEON.xlsx") %>%
  ggplot(
         aes(x = abs(geographical_latitude),
             y = .epred,
             group = group_id)) +
  tidybayes::stat_pointinterval() 


# plot epred by n_obs ####
ggplot(posts,
       aes(x = n_obs_dat,
           y = .epred, 
           group = group_id)) +
  stat_pointinterval() +
  scale_x_log10() +
  NULL

posts %>%
  filter(n_obs_dat <= 100) %>%
  ggplot(aes(x = n_obs_dat,
             y = .epred, 
             group = group_id,
             color = organism_groups)) +
  stat_pointinterval() +
  #scale_x_log10() +
  NULL

posts %>%
  filter(n_obs_dat >= 1000) %>%
  ggplot(aes(x = n_obs_dat,
             y = .epred, 
             group = group_id,
             color = organism_groups)) +
  stat_pointinterval() +
  #scale_x_log10() +
  NULL

# what groups have small sample sizes?
posts %>%
  filter(n_obs_dat <= 50) %>%
  ungroup() %>%
  select(dat_id, group_id, n_obs_dat) %>%
  distinct()
# three groups from Pelinson_Schiesari_Upper
# not sure how these made it through the earlier filtering process in script 01?

posts %>%
  filter(n_obs_dat <= 300) %>%
  ungroup() %>%
  select(dat_id, group_id, n_obs_dat) %>%
  distinct()
# 1397 groups have less than 300 observations

posts %>%
  filter(n_obs_dat <= 500) %>%
  ungroup() %>%
  select(dat_id, group_id, n_obs_dat) %>%
  distinct()
# 1770 groups have less than 500 observations

posts %>%
  filter(n_obs_dat <= 1000) %>%
  ungroup() %>%
  select(dat_id, group_id, n_obs_dat) %>%
  distinct()
# 1966 groups have less than 1000 observations

# .epred ~ latitude ####
# color = organism_groups
ggplot(posts,
       aes(x = abs(geographical_latitude),
           y = .epred,
           group = group_id,
           color = organism_groups)) +
  tidybayes::stat_pointinterval() +
  facet_wrap(~organism_groups)

posts %>%
  filter(n_obs_dat >=1000) %>%
ggplot(
       aes(x = abs(geographical_latitude),
           y = .epred,
           group = group_id,
           color = organism_groups)) +
  tidybayes::stat_pointinterval() +
  #facet_wrap(~organism_groups) +
  NULL

# color = dat_id
tictoc::tic()
ggplot(posts,
       aes(x = abs(geographical_latitude),
           y = .epred,
           group = group_id,
           color = dat_id)) +
  tidybayes::stat_pointinterval() +
  theme(legend.position = "none")
tictoc::toc()

# fish only data sets ####
posts %>% 
  filter(organism_groups == "Fish" |
           organism_groups == "fish") %>%
  pull(group_id) %>%
  unique()

posts %>% 
  filter(organism_groups == "Fish" |
           organism_groups == "fish") %>%
  ggplot(
    aes(x = abs(geographical_latitude),
        y = .epred,
        group = group_id,
        color = dat_id)) +
  tidybayes::stat_pointinterval() +
  facet_wrap(~dat_id) +
  theme(legend.position = "none")
# F/fish = 

# spot pp_checks ####
# brm4 %>%
#   lapply(pp_check) 

brm$"4" %>% pp_check(type = "boxplot") + scale_y_log10()
brm$"13994" %>% 
  pp_check(type = "boxplot", 
           notch = FALSE) + 
  scale_y_log10()

brm$"406" %>% 
  pp_check(type = "boxplot") + 
  scale_y_log10()

# df_France_Irzetral_part2.xlsx
dat %>% filter(dat_id == "df_France_Irzetal_part2.xlsx") %>%
  select(group_id) %>% unique()

brm$"214"
plot(brm$"214")
pp_check(brm$"214", type = "boxplot") + scale_y_log10()

brm$"214"$data %>%
  group_by(body_mass) %>%
  summarize(n = sum(ind_n)) %>%
  ggplot(aes(x = body_mass, 
             y = n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

brm$"214"$data %>%
  group_by(body_mass) %>%
  summarize(n = sum(ind_n)) %>%
  ggplot(aes(x = body_mass)) +
  geom_histogram() +
  scale_x_log10()

# CI for posterior draws ####

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

ci_posts


# ci_posts widths ####
ci_posts %>%
  ungroup() %>%
  select(group_id, dat_id, width) %>%
  filter(width <=2)
# 774 groups

ci_posts %>%
  ungroup() %>%
  select(group_id, dat_id, width) %>%
  filter(width <=1)
# 610

ci_posts %>%
  ungroup() %>%
  select(group_id, dat_id, width) %>%
  filter(width <=0.5)
# 558

ci_posts %>%
  ungroup() %>%
  select(group_id, dat_id, width) %>%
  filter(width <0.96)
# 615 --> matches MLE count with vec diff 0.5

# ci_posts %>%
#   ggplot(aes(xmin = l95,
#              x = median,
#              xmax = u95,
#              y = median,
#              group = group_id,
#              color = organism_groups)) +
#   geom_pointrange() +
#   facet_wrap(~organism_groups, scales = "free")

# ci_posts plots ####
ci_posts %>%
  ggplot(aes(x = size_range,
             y = width,
             group = group_id,
             color = organism_groups)) +
  geom_point() +
  #scale_x_log10() +
  facet_wrap(~organism_groups, scales = "free")
  NULL

ci_posts %>%
  ggplot(aes(x = size_range,
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange() +
  #scale_x_log10() +
  facet_wrap(~organism_groups, scales = "free") +
  NULL

ci_posts %>%
  filter(width <0.96) %>%
  ggplot(aes(x = size_range,
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange() +
  #scale_x_log10() +
  facet_wrap(~organism_groups, scales = "free") +
  NULL

ci_posts %>%
  ggplot(aes(x = n_obs_dat,
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange() +
  scale_x_log10() +
  facet_wrap(~organism_groups, scales = "free_y") +
  NULL

ci_posts %>%
  filter(n_obs_dat > 1000) %>%
  ggplot(aes(x = n_obs_dat,
             y = width,
             group = group_id,
             color = organism_groups)) +
  geom_point() +
  scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free")
  NULL
  

ci_posts %>%
  filter(n_obs_dat > 1000) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free")
  NULL

ci_posts %>%
  #filter(n_obs_dat > 1000) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_pointrange() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free")
  NULL

ci_posts %>%
  #filter(n_obs_dat > 1000) %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = median,
             ymin = l95,
             ymax = u95,
             group = group_id,
             color = organism_groups)) +
  geom_point() +
  #scale_x_log10() +
  #facet_wrap(~organism_groups, scales = "free") +
  ylim(c(-5, 5)) +
  NULL

ci_posts %>%
  ggplot(aes(x = median,
             fill = organism_groups)) +
  geom_density(alpha = 0.25) +
  theme_bw() +
  xlim(c(-3, 1))

# why ci_posts not matchiong brm summary? ####
# used probs = 0.95 instead of 0.975
ci_posts %>%
  filter( group_id == "4117") %>%
  select(l95, u95, median)
brm$"4117"

posts %>%
  filter(group_id == "4117") %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.975))


# Summary stats -----------------------------------------------------------

# neon_posts <- posts %>%
#   filter(dat_id == "df_NEON.xlsx")
# 
# 
# fitdat_temp <- brm$"13634"$data %>%
#   sample_n(size = n_pred, 
#           replace = TRUE,
#           weight = ind_n) %>%
#   arrange(-body_mass) %>%
#   mutate(n_yx = 1:max(row_number()),
#          n = max(row_number()),
#          ismax = body_mass - xmax)

n_pred = 500
tictoc::tic()
sample_gm_list <- NULL
for (i in 1:length(brm)){
  out <- brm[[i]]$data %>% 
    sample_n(size = n_pred, 
             replace = TRUE,
             weight = ind_n) %>%
    arrange(-body_mass) %>%
    mutate(n_yx = 1:max(row_number()),
           n = max(row_number()),
           ismax = body_mass - xmax) %>% 
    # group_by(sample_id,sample_id_seq, site_id) %>% 
    reframe(gm = exp(mean(log(body_mass))),
            median_dw = median(body_mass))
  out$group_id <- names(brm)[i]
  sample_gm_list[[i]] <- out
}
tictoc::toc()
sample_gm <- bind_rows(sample_gm_list)

# sample yrep from posterior
post_gm <- posts %>%
  filter(.draw <= 100) %>%
  select(xmin, xmax, group_id, .epred, .draw) %>%
  expand_grid(individual = 1:n_pred) %>% 
  ungroup()  %>% 
  mutate(
    yrep = rparetocounts(nrow(.),
                         lambda = .epred,
                         xmin = xmin,
                         xmax = xmax))

reorder_ids = sample_gm %>% 
  ungroup() %>% 
  #group_by(group_id) %>% 
  #reframe(rank_gm = exp(mean(log(yrep)))) %>% 
  arrange(gm) %>% 
  mutate(sample_id_ordered = 1:nrow(.)) %>%
  select(group_id, sample_id_ordered)

post_gm_summary <- post_gm %>%
  group_by(group_id, xmin, xmax, .draw) %>%
  reframe(gm = exp(mean(log(yrep))),
          median_dw = median(yrep)) %>% 
  left_join(reorder_ids)
  
post_gm_summary %>% 
  ggplot(aes(x = sample_id_ordered, y = gm)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2,
                     aes(color = "Posterior Predictive",
                         shape = "Posterior Predictive")) +
  scale_y_log10() +
  geom_point(data = sample_gm %>% 
               left_join(reorder_ids), 
             aes(y = gm, color = "Raw Data", shape = "Raw Data"),
             size = 1) +
  guides(fill = "none") +
  guides(shape = "none") +
  scale_color_manual(values = c("#56b4e9", "black")) +
  labs(x = "",
       y = "Geometric mean body size (mgDM)") +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17)),
                              title = "")) +
  #theme(legend.position = c(0.2, 0.95)) +
  NULL


post_gm_summary %>% 
  left_join(dat %>%
              distinct(group_id,
                       dat_id,
                       geographical_latitude,
                       organism_groups)) %>%
  ggplot(aes(x = sample_id_ordered, y = gm)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2,
                     aes(color = "Posterior Predictive",
                         shape = "Posterior Predictive")) +
  scale_y_log10() +
  geom_point(data = sample_gm %>% 
               left_join(reorder_ids) %>%
               left_join(dat %>%
                           distinct(group_id,
                                    dat_id,
                                    geographical_latitude,
                                    organism_groups)), 
             aes(y = gm, color = organism_groups, shape = "Raw Data"),
             size = 1) +
  guides(fill = "none") +
  guides(shape = "none") +
  #scale_color_manual(values = c("#56b4e9", "black")) +
  labs(x = "Rank GM body size",
       y = "Geometric mean body size (mgDM)") +
  # guides(color = guide_legend(override.aes = list(shape = c(16, 17)),
  #                            title = "")) +
  facet_wrap(~organism_groups,
             scales = "free") +
  #theme(legend.position = c(0.2, 0.95)) +
  NULL

# re fit with manually set xmin
wide_fish_groups <- post_gm_summary %>% 
  left_join(dat %>%
              distinct(group_id,
                       dat_id,
                       organism_groups)) %>%
  filter(organism_groups == "Fish",
         sample_id_ordered > 1500) %>%
  sample_n(100) %>% 
  pull(group_id)

# # dat_id: 
# dat_id                                          
# <chr>                                           
# 1 df_France_Irzetal_part2.xlsx                    
# 2 df_Pelinson_Schiesari_Upper_Tiete_fish_data.xlsx
# 3 df_borneo_complete.xlsx                         
# 4 df_AL_IFdL_IdG-RiMSEC.xlsx                      
# 5 df_Arranz_01.xlsx                               
# 6 df_CarlosUPM_18_01_2024_Aitor.xlsx              
# 7 df_France_Irzetal_part1.xlsx       

post_gm_summary %>% 
  filter(group_id %in% wide_fish_groups) %>%
  ggplot(aes(x = sample_id_ordered, y = gm)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2,
                     aes(color = "Posterior Predictive",
                         shape = "Posterior Predictive")) +
  scale_y_log10() +
  geom_point(data = sample_gm %>% 
               filter(group_id %in% wide_fish_groups) %>%
               left_join(reorder_ids), 
             aes(y = gm, color = "Raw Data", shape = "Raw Data"),
             size = 1) +
  guides(fill = "none") +
  guides(shape = "none") +
  scale_color_manual(values = c("#56b4e9", "black"))

posts %>% 
  filter(group_id %in% wide_fish_groups) %>%
  ggplot(aes(x = abs(geographical_latitude),
             group = group_id,
             y = .epred)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2) 

brm[[wide_fish_groups[1]]]
brm[[wide_fish_groups[1]]] %>% plot()
brm[[wide_fish_groups[1]]] %>% 
  pp_check(type = "boxplot") + 
  scale_y_log10()

rhat(brm[[wide_fish_groups[5]]])


for(i in 1:10){
  print(pp_check(brm[[wide_fish_groups[i]]], type = "boxplot") + 
    scale_y_log10())
}

brm[[wide_fish_groups[5]]]
brm[[wide_fish_groups[5]]] %>% plot()
brm[[wide_fish_groups[5]]] %>% 
  pp_check(type = "boxplot") + 
  scale_y_log10()

dat_4117 <- brm$"4117"$data
dat_4117$xmin <- 0.0201

brm_4117 = brm(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = dat_4117,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4, # add cores argument?
  cores = 4,
  iter = 2000)

brm_4117 <- update(brm_4117,
                   newdata = dat_4117)


brm_4117
brm_4117 %>% plot()
brm_4117 %>% 
  pp_check(type = "boxplot") + 
  scale_y_log10()

fish_update <- NULL
for(i in 1:length(wide_fish_groups)){
  fish_out <- update(brm_4117,
                     newdata = brm[[wide_fish_groups[i]]]$data %>%
                       mutate(xmin = 0.0201),
                     cores = 4)
  fish_update[[i]] <- fish_out
}

tictoc::tic()
posts_fish_list <- NULL
for (i in 1:length(fish_update)){
  posts_out <- fish_update[[i]]$data %>% 
    distinct(xmin, xmax) %>% 
    mutate(ind_n = 1) %>% 
    tidybayes::add_epred_draws(fish_update[[i]], re_formula = NULL) 
  posts_out$group_id <- wide_fish_groups[i]
  posts_fish_list[[i]] <- posts_out
}
tictoc::toc()
posts_fish <- bind_rows(posts_fish_list)

posts_fish %>% 
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

posts_fish %>%
  group_by(group_id,
           xmin, 
           xmax) %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.95)) %>%
  mutate(width = l95 - u95,
         size_range = log10(xmax) - log10(xmin)) %>% 
  pull(width) %>% density() %>% plot()

posts_fish %>%
  group_by(group_id,
           xmin, 
           xmax) %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.95)) %>%
  mutate(width = l95 - u95,
         size_range = log10(xmax) - log10(xmin)) %>% 
  pull(median) %>% density() %>% plot()
  
posts %>%
  filter(group_id %in% wide_fish_groups) %>%
  group_by(group_id,
           xmin, 
           xmax) %>%
  summarize(
    median = median(.epred),
    l95 = quantile(.epred, probs = 0.025),
    u95 = quantile(.epred,  probs = 0.95)) %>%
  mutate(width = l95 - u95,
         size_range = log10(xmax) - log10(xmin)) %>% 
  pull(median) %>% density() %>% plot()



brm[[wide_fish_groups[5]]]$data %>%
  ggplot(aes(y = body_mass,
             x = NA)) +
  geom_violin() +
  scale_y_log10()

brm[[wide_fish_groups[5]]]$data %>%
  group_by(body_mass) %>%
  summarize(n = sum(ind_n)) %>%
  ggplot(aes(x = body_mass,
             y = n)) +
  geom_point()+
  scale_y_log10()+
  scale_x_log10()

# # median body mass
# post_gm_summary %>% 
#   ggplot(aes(x = sample_id_ordered, y = median_dw)) +
#   stat_pointinterval(alpha = 0.5,
#                      aes(color = "Posterior Predictive",
#                          shape = "Posterior Predictive")) +
#   scale_y_log10() +
#   geom_point(data = sample_gm %>% 
#                left_join(reorder_ids), 
#              aes(color = "Raw Data",
#                  shape = "Raw Data"),
#              size = 2.3) +
#   guides(fill = "none") +
#   scale_color_manual(values = c("#56b4e9", "black")) +
#   scale_shape_manual(values = c(16, 17)) +
#   labs(x = "",
#        y = "Median body size (mgDM)") 


# Bayes P -----------------------------------------------------------------


(bayes_p <- post_gm %>%
  group_by(group_id, xmin, xmax, .draw) %>% 
  reframe(gm = exp(mean(log(yrep)))) %>% 
  left_join(reorder_ids) %>% 
  left_join(sample_gm %>% 
              left_join(reorder_ids) %>% 
              rename(gm_raw = gm)) %>% 
  mutate(diff = gm - gm_raw,
         maxdraws = max(.draw)) %>% 
  group_by(group_id, maxdraws) %>% 
  reframe(sumdiff = sum(diff>0)) %>% 
  mutate(bayes_p = sumdiff/maxdraws))


range(bayes_p$bayes_p)
mean(bayes_p$bayes_p)
sd(bayes_p$bayes_p)
bad_p <- bayes_p %>% 
  filter(bayes_p > 0.9 | bayes_p <0.1) %>%
  pull(group_id)

post_gm_summary %>% 
  filter(group_id %in% bad_p) %>%
  ggplot(aes(x = sample_id_ordered, y = gm)) +
  stat_pointinterval(alpha = 0.5,
                     size = 0.2,
                     aes(color = "Posterior Predictive",
                         shape = "Posterior Predictive")) +
  scale_y_log10() +
  geom_point(data = sample_gm %>% 
               left_join(reorder_ids) %>%
               filter(group_id %in% bad_p), 
             aes(y = gm, color = "Raw Data", shape = "Raw Data"),
             size = 1)

# fish counts instead of ind_n
# check SI in Wesner et al. 2004 for discussion on scale, error bars, sample size, etc. 

library(tidyverse)
library(furrr)
library(sizeSpectra)

dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")

fish <- dat %>% filter(organism_groups == "Fish" |
                 organism_groups == "fish" )

fish %>%
  group_by(group_id, dat_id) %>%
  summarise(n_sample = n_distinct(sample)) %>%
  filter(n_sample >1) %>% View()

fish %>%
  filter(group_id == 2132)

fish %>% ungroup() %>%
  reframe(quantile(count))

fish %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_log10()+
  scale_y_log10()

fish %>% filter(count < 1)

fish %>% ungroup() %>%
  reframe(quantile(ind_n))

fish %>% 
  ggplot(aes(x = ind_n)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_log10()+
  scale_y_log10()

fish %>% filter(ind_n < 1)

# fish datset
dat_split <- fish %>%
  split(fish$group_id)

length(dat_split)
nrow_dat_split <- dat_split %>%
  map_dbl(\(dat) nrow(dat)) 

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_count <- dat_split |>
  future_map(possibly(function(.data){
    calcLike(
      negLL.fn = negLL.PLB.counts,
      x = .data$body_mass,
      c = .data$count,
      p = -1.5,
      vecDiff = vecDiff)}, 
    otherwise = "Error")
  )
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)

plan(cluster, workers = 1)

mle_count_rows <- list()
for (i in 1:length(mle_count)){
  if(mle_count[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_count[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_count[i]),
                      mle_estimate = mle_count[[i]]$MLE,
                      conf_lo = mle_count[[i]]$conf[1],
                      conf_hi = mle_count[[i]]$conf[2])
  }
  mle_count_rows[[i]] <- out
}


mle_count_results <- bind_rows(mle_count_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(dat %>%
              select(dat_id,
                     site, 
                     geographical_latitude,
                     organism_groups) %>%
              unique())

mle_count_results
ggplot(mle_count_results,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "MLE_count fish",
       subtitle = "using count instead of ind_n") +
  theme_bw()

mle_count_results$est_method <- "count"

mle_ind_n <- readRDS("results/mle_count_vecDiff-10_2024-03-14.rds")

fish_ind_n <- mle_ind_n %>%
  filter(organism_groups == "Fish" |
           organism_groups == "fish" )

fish_ind_n$est_method <- "ind_n"

mle <- bind_rows(mle_count_results,
                 fish_ind_n)

ggplot(mle,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = est_method)) +
  geom_pointrange(alpha = 0.5) +
  theme_bw()

mle %>%
  filter(mle_estimate > -10)%>%
  ggplot(aes(
         x = group_id,
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = est_method)) +
  geom_pointrange(alpha = 0.25,
                  position = position_dodge(width = 1)) +
  theme_bw()


mle_group_sub <- mle %>% 
  select(group_id) %>%
  unique() %>%
  sample_n(500) %>%
  pull

mle %>%
  filter(group_id %in% mle_group_sub)%>%
  ggplot(aes(
    x = group_id,
    y = mle_estimate,
    ymin = conf_lo,
    ymax = conf_hi,
    color = est_method)) +
  geom_pointrange(alpha = 0.25,
                  position = position_dodge(width = 1)) +
  theme_bw() +
  coord_cartesian(ylim = c(-5, 5)) +
  ggthemes::scale_color_colorblind()

mle %>%
  filter(group_id %in% mle_group_sub)%>%
  ggplot(aes(
    x = group_id,
    y = mle_estimate,
    ymin = conf_lo,
    ymax = conf_hi,
    color = est_method)) +
  geom_pointrange(alpha = 0.25,
                  position = position_dodge(width = 1)) +
  theme_bw() +
  coord_cartesian(ylim = c(-5, 5)) +
  ggthemes::scale_color_colorblind() +
  facet_wrap(~est_method)

# multiply fish and inverts by 1000 ---------------------------------------



# what happens when we multiply fish + inverts by ~1000?

dat_test1 <- dat %>%
  filter(group_id == 13637)

dat_test1$ind_n %>% unique()
dat_test1$count %>% unique()

no_m2_1 <-  dat_test1 %>%
  mutate(no_m2 = count * multiplier / n_distinct(sample)) %>%
  select(body_mass, count, multiplier, ind_n, no_m2)
  
no_m2_1$no_m2 %>% unique()
  
no_m2_1 %>%
  calcLike(
    negLL.fn = negLL.PLB.counts,
    x = .$body_mass,
    c = .$no_m2,
    p = -1.5,
    vecDiff = vecDiff,
    suppress.warnings = TRUE)

no_m2_1 %>%
  calcLike(
    negLL.fn = negLL.PLB.counts,
    x = .$body_mass,
    c = .$ind_n*100,
    p = -1.5,
    vecDiff = vecDiff,
    suppress.warnings = TRUE)



calcLike(
  negLL.fn = negLL.PLB.counts,
  x = dat_test1$body_mass,
  c = dat_test1$ind_n*10,
  p = -1.5,
  vecDiff = vecDiff,
  suppress.warnings = TRUE)

calcLike(
  negLL.fn = negLL.PLB.counts,
  x = dat_test1$body_mass,
  c = dat_test1$count,
  p = -1.5,
  vecDiff = vecDiff,
  suppress.warnings = TRUE)

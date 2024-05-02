library(tidyverse)
library(furrr)
library(sizeSpectra)

dat <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")

# mle_count ####
# full datset
dat_split <- dat %>%
  split(dat$group_id)

# nrow_dat_split <- dat_split %>%
#   map_dbl(\(dat) nrow(dat)) 


plan(cluster, workers = 10)

vecDiff = 1 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_count <- dat_split |>
  future_map(possibly(function(.data){
    calcLike(
      negLL.fn = negLL.PLB.counts,
      x = .data$body_mass,
      c = .data$ind_n,
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
                     organism_group,
                     organism_groups,
                     group_id) %>%
              unique())


# total rows
mle_count_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
mle_count_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()


# mle results ####
mle_count_results %>%
  summarize(mean = mean(mle_estimate, na.rm = TRUE),
            median = median(mle_estimate, na.rm = TRUE),
            min = min(mle_estimate, na.rm = TRUE),
            max = max(mle_estimate, na.rm = TRUE),
            q95 = quantile(mle_estimate, probs = 0.95, na.rm = TRUE),
            q05 = quantile(mle_estimate, probs = 0.05, na.rm = TRUE))

mle_count_results %>%
  na.omit() %>%
  summarize(min(conf_lo),
            max(conf_hi))


mle_lat <- mle_count_results

mle_lat <- mle_lat %>% 
  na.omit() %>%
  mutate(se = (conf_hi - conf_lo) / 2 * 1.96,
         var = se**2) %>%
  filter(var > 0)


saveRDS(mle_lat, "derived_data/format_mle_count_results.RDS")

mle_lat <- readRDS("derived_data/format_mle_count_results.RDS")
 # %>%
  # mutate(organism_groups = case_when(
  #   organism_groups == "fish" ~ "Fish",
  #   organism_groups == "Invertebrates, Fish" ~ "Invertebrates + Fish",
  #   .default = organism_groups),
  #   organism_group = case_when(
  #     organism_group == "fish" ~ "Fish",
  #     organism_group == "Invertebrate" ~ "Invertebrates",
  #     .default = organism_group
  #   ))

# plot all estimates
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.25) +
  labs(title = "MLE_count estimates",
       x = "Absolute Latitude",
       y = "Estimated \u03BB") +
  guides(color = guide_legend(title = "Taxa")) +
  theme_bw()

ggsave("results/plots/mle_count_lambda_latitude.jpg",
       width = 6.5, height = 8.5,
       dpi = 500)

# add ols regression
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "MLE_count all estimates",
       subtitle = "Simple OLS lambda ~ latitude") +
  theme_bw() +
  stat_smooth(inherit.aes = FALSE,
              aes(x = abs(geographical_latitude),
                  y = mle_estimate),
              method = "lm")

# weighted ols regression
mle_lat %>% 
  ggplot(aes(
    x = abs(geographical_latitude),
    y = mle_estimate,
    ymin = conf_lo,
    ymax = conf_hi,
    color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "MLE_count all estimates",
       subtitle = "weighted OLS 1/variance") +
  theme_bw() +
  stat_smooth(inherit.aes = FALSE,
              aes(x = abs(geographical_latitude),
                  y = mle_estimate,
                  weight = 1/var),
              method = "lm")

# plot by group
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = dat_id)) +
  geom_pointrange(alpha = 0.25) +
  labs(title = "MLE_count all estimates") +
  theme_bw() +
  facet_wrap(~organism_group,
             scales = "free_x") +
  theme(legend.position = "none")

# ols by group
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.25) +
  labs(title = "MLE_count all estimates",
       subtitle = "OLS regression") +
  theme_bw() +
  facet_wrap(~organism_groups) +
  stat_smooth(method = "lm")

# weighted ols by group
ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.25) +
  labs(title = "MLE_count all estimates",
       subtitle = "weighted OLS 1/variance") +
  theme_bw() +
  facet_wrap(~organism_groups) +
  stat_smooth(
    aes(weight = 1/var),
    method = "lm")



# plot variance
mle_lat %>% 
  ggplot(aes(
    x = abs(geographical_latitude),
    y = var,
    color = organism_groups)) +
  geom_point(alpha = 0.5) +
  labs(title = "Variance of lambda") +
  theme_bw() +
  scale_y_log10()

ggplot(mle_lat,
       aes(
         x = abs(geographical_latitude),
         y = mle_estimate,
         ymin = conf_lo,
         ymax = conf_hi,
         color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  labs(title = "MLE_count all estimates") +
  theme_bw() +
  facet_wrap(~organism_groups, scales = "free_x") +
  stat_smooth(method = "lm") +
  NULL

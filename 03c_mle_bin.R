# MLEbin
# by Pomeranz 
# began March 2024

library(weights)
library(sizeSpectra)
library(tidyverse)
library(furrr)



# start with filtered data and compare with MLEcount results
dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")

dat %>% ungroup() %>% reframe(range(body_mass))
dat %>% 
  ungroup() %>% 
  group_by(organism_groups) %>%
  reframe(range(body_mass))

dat %>% 
  ungroup() %>%
  distinct(dat_id, sampling_method)

dat %>%
  ungroup() %>%
  group_by(group_id, sampling_method) %>%
  distinct(group_id, sampling_method) %>%
  count() %>% View()
  arrange(-n)


# test df ####
# binData with weight?
x1 <- rPLB(n = 1000,b = -1.20, 1, 100)
round(x1, digits = 2)

(test_df_1 <- data.frame(x = round(x1, digits = 2)) %>%
    group_by(x) %>%
    summarise(count = n())%>%
    mutate(count_frac = count * 0.001) %>%
    as.data.frame())

# test_df_1[,1]
# test_df_1[,2]
# 
# rep(as.numeric(test_df_1[,1]),
#     as.numeric(test_df_1[,2]))
# # binData(counts = test_df_1,
# #         binWidth = "2k")
# 
# arrange(test_df_1)
hist(test_df_1$x,
     breaks = 2^(floor(log2(min(test_df_1$x))):ceiling(log2(max(test_df_1$x)))),
     plot = FALSE)

minx <- min(test_df_1$x)
maxx <- max(test_df_1$x)


wtd_result <- wtd.hist(x = test_df_1$x,
                       breaks = 2^(floor(log2(minx)):ceiling(log2(maxx))),
                       weight = test_df_1$count*0.1, 
                       plot = FALSE)

wtd_result$bin_width <- wtd_result$breaks[-length(wtd_result$breaks)]

# wtd_result$bin_width <- vector("numeric", length(2^(floor(log2(minx)):ceiling(log2(maxx))))-1)
# 
# for(i in 1:length(wtd_result$breaks) - 1){
#   wtd_result$bin_width[i] <- wtd_result$breaks[i+1]-wtd_result$breaks[i]
# }

wtd_result$count_norm <- wtd_result$counts / wtd_result$bin_width



wtd_result
calcLike(negLL.fn = negLL.PLB.binned,
         p = -1.5, # wtf is this?
         w = wtd_result$breaks,
         d = wtd_result$counts,
         J = length(wtd_result$counts),   # = num.bins
         vecDiff = 1)

## Normalized counts --> not used in Edwards' sizeSpectra vignettes. 

# calcLike(negLL.fn = negLL.PLB.binned,
#          p = -1.5, # wtf is this?
#          w = wtd_result$breaks,
#          d = wtd_result$count_norm,
#          J = length(wtd_result$count_norm),   # = num.bins
#          vecDiff = 1)

calcLike(negLL.fn = negLL.PLB,
         p = -1.5, # wtf is this?
         x = x1,
         n = length(x1),
         xmin = minx,
         xmax = maxx,
         sumlogx = sum(log(x1)),
         vecDiff = 1)


x2 <- rPLB(n = 100,b = -1.20, 1, 100)
test_df_2 <- data.frame(x = round(x2, digits = 2),
                        count = 1,
                        count_frac = 1 / 1000,
                        count_large = 1 * 1000,
                        count_corrected = (1/1000) * 1000)

calcLike(negLL.fn = negLL.PLB.counts,
         p = -1.5, # wtf is this?
         x = test_df_2$x,
         c = test_df_2$count,
         vecDiff = 50,
         suppress.warnings = TRUE)

calcLike(negLL.fn = negLL.PLB.counts,
         p = -1.5, # wtf is this?
         x = test_df_2$x,
         c = test_df_2$count_large,
         vecDiff = 50,
         suppress.warnings = TRUE)

calcLike(negLL.fn = negLL.PLB.counts,
         p = -1.5, # wtf is this?
         x = test_df_2$x,
         c = test_df_2$count_frac,
         vecDiff = 50,
         suppress.warnings = TRUE)

calcLike(negLL.fn = negLL.PLB.counts,
         p = -1.5, # wtf is this?
         x = test_df_2$x,
         c = test_df_2$count_corrected,
         vecDiff = 50,
         suppress.warnings = TRUE)

# lbn_bin plot ####
# for plot need to make my own version of sizeSpectra::binVals
# LBN_bin_plot(binValsTibble = x.binned$binVals,
#              b.MLE = MLEbin.res$MLE,
#              b.confMin = MLEbin.res$conf[1],
#              b.confMax = MLEbin.res$conf[2],
#              leg.text = "(c)",
#              log.xy = "xy",
#              plot.binned.fitted = TRUE)


# function ####
# function which takes body_mass and ind_n
# makes a weighted hist
# uses $breaks and $counts to estimate lambda

# ARIK_2017-03-22
dat_test1 <- dat %>%
  filter(group_id == 13637)

bin_fxn <- function(dat,
                    vecDiff = 1,
                    id_max = FALSE){
  # process data
  x = dat$body_mass
  ind_n = dat$ind_n
  if(id_max == TRUE){ # this doesn't work. Throws an error bc global min/max is outside of range of supplied vector of body masses. 
    minx <- unique(dat$dat_id_min_x)
    maxx <- unique(dat$dat_id_max_x)
  } else {
   minx <- min(x) 
   maxx <- max(x) 
  }
  
  # weighted histogram
  wtd_result <- wtd.hist(x = x,
                         breaks = 2^(floor(log2(minx)):ceiling(log2(maxx))),
                         weight = ind_n, 
                         plot = FALSE)
  
  # estimate lambda
  
  estimated_lambda <- calcLike(
    negLL.fn = negLL.PLB.binned,
    p = -1.5, # wtf is this?
    w = wtd_result$breaks,
    d = wtd_result$counts,
    J = length(wtd_result$counts),   # = num.bins
    vecDiff = vecDiff)
  
  return(estimated_lambda)
}

bin_fxn(dat_test1, vecDiff = 10)

dat_test1 %>%
  ungroup() %>%
  group_by(dat_id) %>%
  mutate(
    dat_id_min_x = min(body_mass, na.rm = TRUE),
    dat_id_max_x = max(body_mass, na.rm = TRUE)
  ) %>%
  bin_fxn(id_max = TRUE)

calcLike(
  negLL.fn = negLL.PLB.counts,
  x = dat_test1$body_mass,
  c = dat_test1$ind_n,
  p = -1.5,
  vecDiff = 1)


dat_test2 <- dat %>%
  filter(dat_id == "df_NEON.xlsx") %>%
  # filter(group_id >= 13637,
  #        group_id <= 13927) %>%
  mutate(
    local_min_x = min(body_mass, na.rm = TRUE),
    local_max_x = max(body_mass, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(dat_id) %>%
  mutate(
    dat_id_min_x = min(body_mass, na.rm = TRUE),
    dat_id_max_x = max(body_mass, na.rm = TRUE)
  )

dat_test2 %>%
  ungroup() %>%
  distinct(local_min_x, dat_id_min_x) %>%
  ggplot(aes(x = local_min_x,
             y = dat_id_min_x)) +
  geom_point()

dat_test2 %>%
  ungroup() %>%
  distinct(local_max_x, dat_id_max_x) %>%
  ggplot(aes(x = local_max_x,
             y = dat_id_max_x)) +
  geom_point()

dat_test2 %>%
  filter(group_id == 13637)%>%
  bin_fxn()

dat_test2 %>%
  filter(group_id == 13637)%>%
  bin_fxn(id_max = TRUE)

# # dat_test2 split ####
dat_split <- dat_test2 %>%
  split(dat_test2$group_id)

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- dat_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data, vecDiff = vecDiff)},
    otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = mle_bin[[i]]$MLE,
                      conf_lo = mle_bin[[i]]$conf[1],
                      conf_hi = mle_bin[[i]]$conf[2])
  }
  mle_bin_rows[[i]] <- out
}


mle_bin_results <- bind_rows(mle_bin_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(dat %>%
              select(dat_id,
                     site,
                     geographical_latitude,
                     organism_groups) %>%
              unique())

# add nrow to results data frame
# mle_count_results$nrow_dat_split <- nrow_dat_split
# two latitudes for groupd_id == 13542

# total rows
mle_bin_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
mle_bin_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()

# dat2 id_min ####
plan(cluster, workers = 10)

#vecDiff = 10 # bigger = more estimates, but huge CI's

vecDiff = 10

mle_start <- Sys.time()
bin_id_min <- dat_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data,
            vecDiff = vecDiff,
            id_max = TRUE)},
    otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

bin_id_min_rows <- list()
for (i in 1:length(bin_id_min)){
  if(bin_id_min[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(bin_id_min[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(bin_id_min[i]),
                      mle_estimate = bin_id_min[[i]]$MLE,
                      conf_lo = bin_id_min[[i]]$conf[1],
                      conf_hi = bin_id_min[[i]]$conf[2])
  }
  bin_id_min_rows[[i]] <- out
}


bin_id_min_results <- bind_rows(bin_id_min_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(dat %>%
              select(dat_id,
                     site,
                     geographical_latitude,
                     organism_groups) %>%
              unique())

# add nrow to results data frame
# mle_count_results$nrow_dat_split <- nrow_dat_split
# two latitudes for groupd_id == 13542

# total rows
bin_id_min_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
bin_id_min_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()

bind_rows(
  mle_bin_results %>%
    select(group_id, mle_estimate, conf_lo, conf_hi) %>%
    mutate(method = "local_min"),
  bin_id_min_results %>%
    select(group_id, mle_estimate, conf_lo, conf_hi) %>%
    mutate(method = "dat_id_min")) %>%
  ggplot(aes(x = mle_estimate,
             y = mle_estimate,
             ymin = conf_lo, 
             ymax = conf_hi,
             color = method)) +
  geom_pointrange(alpha = 0.25) +
  theme_bw()


# full dat split ####
dat_split <- dat %>%
  split(dat$group_id)

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- dat_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data, vecDiff = vecDiff)},
                      otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = mle_bin[[i]]$MLE,
                      conf_lo = mle_bin[[i]]$conf[1],
                      conf_hi = mle_bin[[i]]$conf[2])
  }
  mle_bin_rows[[i]] <- out
}


mle_bin_results <- bind_rows(mle_bin_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(dat %>%
              select(dat_id,
                     site, 
                     geographical_latitude,
                     organism_groups) %>%
              unique())

# add nrow to results data frame
# mle_count_results$nrow_dat_split <- nrow_dat_split
# two latitudes for groupd_id == 13542

# total rows
mle_bin_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
mle_bin_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()

saveRDS(mle_bin_results, paste0("results/mle_bin_filt_data_vecDiff-", vecDiff, "_", Sys.Date(),".rds"))

saveRDS(mle_count_results, paste0("results/mle_count_vecDiff-", vecDiff, "_", Sys.Date(),".rds"))



# full data not filtered ####
#dat <- readRDS("derived_data/all_size_stitched_no_filtering.RDS")

# # need to add code to update body_weight_units
# dat <- dat %>%
#   mutate(body_mass = case_when(
#     # convert g to mg
#     # convert wet weight to dry weight
#     # Using an arbitrary value of 0.25 for now
#     # assuming "dry_weight" is in mg
#     body_weight_units == "dry_weight" ~ body_mass,
#     body_weight_units == "g" ~ body_mass *1000,
#     body_weight_units == "g WW" ~ body_mass *1000 *0.25,
#     body_weight_units == "grams/wet" ~ body_mass *1000 *0.25,
#     body_weight_units == "M.mg" ~ body_mass,
#     body_weight_units == "mg" ~ body_mass,
#     body_weight_units == "mg (dry_mass)" ~ body_mass,
#     body_weight_units == "mg dry mass" ~ body_mass,
#     body_weight_units == "mg DW" ~ body_mass,
#     body_weight_units == "mg wet weight" ~ body_mass * 0.25,
#     body_weight_units == "mg WW" ~ body_mass * 0.25,
#     body_weight_units == "mg_DM" ~ body_mass,
#     body_weight_units == "mg_dry_mass" ~ body_mass
#   ))
# 
# dat <- dat %>%
#   group_by(group_id) %>%
#   mutate(ind_n = count * multiplier)


# just F/fish data ####
fish <- dat %>%
  filter(organism_groups == "Fish" |
           organism_groups == "fish" )

# binData(binwidth = "2k")
# binMids are always the same
# tested some random distributions of
# rPLB(
#   n = 1000,
#   b = # varied -1 to -2
#   xmin = # varied 0.001, 1
#   xmax = # varied 1, 10, 100
# )

fish_split <- fish %>%
  as.data.frame() %>%
  split(fish$group_id)

fish_small <- list(fish_split[[1]],
                   fish_split[[2]],
                   fish_split[[3]],
                   fish_split[[4]])

fish_split[[1]] %>%
  select(body_mass, ind_n) %>%
  binData(counts = .)

?hist()



# Irz et al. cut off------------------------------------------------------

# this doesn't seem to work....
# need to figure out how Ignasi made his histograms for the cutoff sizes. 


irz <- dat %>%
  filter(str_detect(dat_id, "Irz"))

irz %>%
  ggplot(aes(x = body_mass)) +
  geom_histogram(
    #binwidth = 1000
    # breaks = 2^(floor(log2(min(x))):ceiling(log2(max(x))))
    )+
   #scale_x_log10() +
  # scale_y_log10() +
  NULL



hist.data <- hist(irz$body_mass, 
     breaks = 2^(floor(log2(min(irz$body_mass))):ceiling(log2(max(irz$body_mass)))),
     plot = F)

hist.data$counts = log10(hist.data$counts)
hist.data$mids = log10(hist.data$mids)
hist.data$breaks = log10(hist.data$breaks)
plot(hist.data)

irz_summary <- irz %>%
  group_by(dat_id,
           sampling_method, group_id, 
           body_mass) %>%
  summarize(sum_n = sum(ind_n)) 


irz_summary %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = sampling_method)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  #facet_wrap(~dat_id, scales = "free") +
  theme_bw()


irz_split <- irz %>%
  split(irz$group_id)
length(irz_split)

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- irz_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data, vecDiff = vecDiff)},
    otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = mle_bin[[i]]$MLE,
                      conf_lo = mle_bin[[i]]$conf[1],
                      conf_hi = mle_bin[[i]]$conf[2])
  }
  mle_bin_rows[[i]] <- out
}


mle_bin_results <- bind_rows(mle_bin_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(irz %>%
              select(dat_id,
                     site,
                     sampling_method,
                     geographical_latitude,
                     organism_groups) %>%
              unique())

# add nrow to results data frame
# mle_count_results$nrow_dat_split <- nrow_dat_split
# two latitudes for groupd_id == 13542

# total rows
mle_bin_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
mle_bin_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()

mle_bin_results %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = sampling_method)) +
  geom_pointrange() +
  facet_wrap(~sampling_method)


# filter bod_mass > 1e03

irz_split <- irz %>%
  filter(body_mass > 1e03) %>%
  split(irz$group_id)
length(irz_split)

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- irz_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data, vecDiff = vecDiff)},
    otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = mle_bin[[i]]$MLE,
                      conf_lo = mle_bin[[i]]$conf[1],
                      conf_hi = mle_bin[[i]]$conf[2])
  }
  mle_bin_rows[[i]] <- out
}


irz_1e03 <- bind_rows(mle_bin_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(irz %>%
              select(dat_id,
                     site,
                     sampling_method,
                     geographical_latitude,
                     organism_groups) %>%
              unique())


# > 3000 ------------------------------------------------------------------

# filter bod_mass > 1e03

irz_split <- irz %>%
  #filter(body_mass > 3000) %>%
  split(irz$group_id, drop = TRUE)

length(irz_split)

irz_split <- irz_split %>%
  map(\(dat) filter(dat, body_mass > 3000))
names(irz_split)

plan(cluster, workers = 10)

vecDiff = 10 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- irz_split |>
  future_map(possibly(function(.data){
    bin_fxn(.data, vecDiff = vecDiff)},
    otherwise = "Error"))
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)


plan(cluster, workers = 1)

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = NA,
                      conf_lo = NA,
                      conf_hi = NA)
  } else{
    out <- data.frame(group_id = names(mle_bin[i]),
                      mle_estimate = mle_bin[[i]]$MLE,
                      conf_lo = mle_bin[[i]]$conf[1],
                      conf_hi = mle_bin[[i]]$conf[2])
  }
  mle_bin_rows[[i]] <- out
}


irz_3000 <- bind_rows(mle_bin_rows) %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(irz %>%
              select(dat_id,
                     site,
                     sampling_method,
                     geographical_latitude,
                     organism_groups) %>%
              unique())

irz_3000 %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = sampling_method)) +
  geom_pointrange() +
  facet_wrap(~sampling_method)


irz_1e03 %>%
  ggplot(aes(x = group_id,
             y = mle_estimate, 
             ymin = conf_lo, 
             ymax = conf_hi,
             color = sampling_method)) +
  geom_pointrange() +
  facet_wrap(~sampling_method)

irz_1e03 %>% filter(group_id == 4506 |
                         group_id == 4768)
irz_3000 %>% filter(group_id == 4506 |
                      group_id == 4768)
mle_bin_results %>% filter(group_id == 4506 |
                      group_id == 4768)


irz_summary %>% filter(group_id == 4506 |
                         group_id == 4768) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = as.factor(group_id))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  #facet_wrap(~dat_id, scales = "free") +
  theme_bw() +
  labs(title = "all size")

irz_summary %>% filter(group_id == 4506 |
                         group_id == 4768) %>%
  filter(body_mass > 1e03) %>%
  ggplot(aes(x = body_mass, 
             y = sum_n,
             color = as.factor(group_id))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  #facet_wrap(~dat_id, scales = "free") +
  theme_bw() +
  labs(title = "size >1e03")

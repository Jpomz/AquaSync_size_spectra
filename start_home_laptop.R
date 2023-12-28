# start at home 2023-12-07
# jpz

library(tidyverse)
library(sizeSpectra)
library(readxl)

# test <- read_excel("data/df_NEON.xlsx", 
#                    col_types = c("text", "text", "numeric", 
#                                  "numeric", "text", "text", 
#                                  "numeric", "numeric", 
#                                  "text", "text", "numeric",
#                                  "numeric"))
# 
# test$body_length %>% unique
# names(test)


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

# identical(name_target, names(test))

file_paths <- list.files("data/", 
                         pattern = "*.xlsx")

data_list <- list()
list_to_fix_names <- list()

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

list_to_fix_names

names(data_list) <- file_paths
dat_df <- bind_rows(data_list, .id = "id")

dat_df <- dat_df %>%
  separate(id, into = c("rm", "dat_id", "rm2")) %>%
  select(-rm, -rm2)

# Summarize data sets ####
dat_df %>%
  pull(count) %>%
  unique()
# counts are all over the place

# what are the body weight units?
dat_df$body_weight_units %>% unique()

# convert everything to mg
dat_df <- dat_df %>%
  mutate(body_mass = case_when(
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "mg DW" ~ body_mass,
    body_weight_units == "mg" ~ body_mass
  ))

dat_df %>%
  group_by(dat_id, site) %>%
  count()
# 386 sites or samples



dat_df %>%
  group_by(dat_id, site) %>%
  summarise(n = n(),
            max_size = log10(max(body_mass)),
            min_size = log10(min(body_mass))) %>%
  mutate(size_range = (max_size - min_size)) %>%
  filter(n > 100,
         size_range >= 3)
# 312 with n > 100 and size range > 3 orders of magnitude

# NEON sites with fish +macros ####
data_list$df_NEON.xlsx %>%
  select(site) %>%
  unique()
# 320 samples

neon_site_vector <- data_list$df_NEON.xlsx %>%
  filter(organism_group == "fish") %>%
  pull(site) %>%
  unique()
# 123 samples with fish

df_NEON_fish_macro <- data_list$df_NEON.xlsx %>%
  filter(site %in% neon_site_vector) %>%
  mutate(dat_id = "NEON")

# Tests: work out code ####
# test the count LL vs plb LL vs PLBbin
# Charming New Zealand ####
charm_test <- dat_df %>%
  filter(site == "Burke") %>%
  select(body_mass, count, multiplier) %>%
  mutate(ind_n = count * multiplier) 

charm_counts <- calcLike(negLL.fn = negLL.PLB.counts,
        x = charm_test$body_mass,
        c = charm_test$ind_n,
        p = -1.5)

charm_plb <- calcLike(negLL.fn = negLL.PLB,
         x = charm_test$body_mass,
         xmin = min(charm_test$body_mass),
         xmax = max(charm_test$body_mass),
         n = length(charm_test$body_mass),
         sumlogx = sum(log(charm_test$body_mass)),
         p = -1.5)

# bins
x.binned <- binData(x = charm_test$body_mass,
                    binWidth = "2k")
num.bins <- nrow(x.binned$binVals)
binBreaks <- c(dplyr::pull(x.binned$binVals, binMin),
               dplyr::pull(x.binned$binVals, binMax)[num.bins])

binCounts <- dplyr::pull(x.binned$binVals, binCount)

charm_bin <- calcLike(negLL.fn = negLL.PLB.binned,
         p = -1.5,
         w = binBreaks,
         d = binCounts,
         J = length(binCounts),   # = num.bins
         vecDiff = 1) 
data.frame(mle_counts = charm_counts$MLE,
           mle_bin = charm_bin$MLE,
           mle_plb = charm_plb$MLE)

# tadnoll UK ####
tad_test <- dat_df %>%
  filter(site == "Tadnoll Brook") %>%
  select(body_mass, count, multiplier) %>%
  mutate(ind_n = count * multiplier) 

ggplot(tad_test,
       aes(x = body_mass, y = ind_n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
ggplot(tad_test,
       aes(x = body_mass)) +
  geom_histogram() +
  scale_x_log10()

tad_test %>%
  summarize(min(body_mass))


tad_test %>% group_by(body_mass) %>%
  summarize(n())

tad_counts <- calcLike(negLL.fn = negLL.PLB.counts,
         x = tad_test$body_mass,
         c = tad_test$ind_n,
         p = -1.5)
tad_plb <- calcLike(negLL.fn = negLL.PLB,
         x = tad_test$body_mass,
         xmin = min(tad_test$body_mass),
         xmax = max(tad_test$body_mass),
         n = length(tad_test$body_mass),
         sumlogx = sum(log(tad_test$body_mass)),
         p = -1.5)
x.binned <- binData(x = tad_test$body_mass,
                    binWidth = "2k")
num.bins <- nrow(x.binned$binVals)
binBreaks <- c(dplyr::pull(x.binned$binVals, binMin),
               dplyr::pull(x.binned$binVals, binMax)[num.bins])

binCounts <- dplyr::pull(x.binned$binVals, binCount)

tad_bin <- calcLike(negLL.fn = negLL.PLB.binned,
                      p = -1.5,
                      w = binBreaks,
                      d = binCounts,
                      J = length(binCounts),   # = num.bins
                      vecDiff = 1)

data.frame(
  site = c("Charming_NZ", "Tadnoll_UK"),
  mle_counts = c(charm_counts$MLE, tad_counts$MLE),
           mle_bin = c(charm_bin$MLE, tad_bin$MLE),
           mle_plb = c(charm_plb$MLE, tad_plb$MLE))


# counts to full data ####

calcLike(negLL.fn = negLL.PLB.counts,
         x = charm_test$body_mass,
         c = charm_test$ind_n,
         p = -1.5)

# doesn't work... 
# dat_df %>%
#   filter(dat_id == "Pomeranz") %>%
#   group_by(dat_id, site) %>%
#   mutate(ind_n = count * multiplier) %>%
#   select(dat_id, site, body_mass, ind_n) %>%
#   nest(x = body_mass,
#        y = ind_n) %>%
#   mutate(mle_counts = map2(x, y, 
#     \(x, y) calcLike(
#       negLL.fn = negLL.PLB.counts,
#       x = x,
#       c = y,
#       p = -1.5)
#   ))

# mle counts full data ####
# Slow, but gets the job done

dat_df2 <- dat_df |> 
  #filter(dat_id == "Pomeranz") %>%
  group_by(dat_id, site) %>%
  mutate(ind_n = count * multiplier) %>%
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

 mle_count_results$method <- "count"
 #read in site data for lat long ####

 df_Pomeranz <- read_excel("data/df_Pomeranz.xlsx",
                           sheet = "site_data")


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
              inherit.aes = FALSE)
       

ggplot(mle_lat,
       aes(x = (geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  stat_smooth(aes(x = (geographical_latitude),
                  y = mle_estimate),
              method = "lm",
              inherit.aes = FALSE)

ggplot(mle_lat,
       aes(x = mle_estimate)) +
  geom_density()

mle_lat %>%
  filter(!is.na(mle_estimate)) %>%
  summarize(min = min(mle_estimate),
            median = median(mle_estimate),
            mean = mean(mle_estimate),
            max = max(mle_estimate),
            q05 = quantile(mle_estimate, probs = 0.05),
            q95 = quantile(mle_estimate, probs = 0.95))
            


# just data with fish + macros ####
# Perkins plus some NEON

fish_macro <- bind_rows(
  data_list$df_Perkins.xlsx %>%
    mutate(dat_id = "Perkins"),
  df_NEON_fish_macro
)


fish_macro <- fish_macro |> 
  #filter(dat_id == "Pomeranz") %>%
  group_by(dat_id, site) %>%
  mutate(ind_n = count * multiplier) %>%
  select(dat_id, site, body_mass, ind_n) %>%
  mutate(group_id = cur_group_id()) 

fish_split <- fish_macro |>
  split(fish_macro$group_id)

# estimate MLE lambda from count data
mle_count <- fish_split |>
  map(\(df) calcLike(
    negLL.fn = negLL.PLB.counts,
    x = df$body_mass,
    c = df$ind_n,
    p = -1.5))

# summarize to get xmin and xmax
fish_macro |>
  group_by(dat_id, site)|>
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))
  
  

mle_count_rows <- list()
for (i in 1:length(mle_count)){
  out <- data.frame(mle_estimate = mle_count[[i]]$MLE,
                    conf_lo = mle_count[[i]]$conf[1],
                    conf_hi = mle_count[[i]]$conf[2])
  mle_count_rows[[i]] <- out
}

fish_macro_results <- bind_rows(mle_count_rows, .id = "group_id") %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(fish_macro %>%
              select(dat_id, site, group_id) %>%
              unique()
            )

fish_macro_results <- fish_macro_results %>%
  ungroup() %>%
  left_join(lat_df)

fish_macro_results$method <- "count"

ggplot(fish_macro_results,
       aes(x = (geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  stat_smooth(aes(x = (geographical_latitude),
                  y = mle_estimate),
              method = "lm",
              inherit.aes = FALSE) +
  labs(title = "Fish + Macros",
       x = "latitude",
       y = "MLE count estimate") +
  theme_bw()

ggplot(fish_macro_results,
       aes(x = mle_estimate)) +
  tidybayes::stat_halfeye(fill = "dodgerblue") +
  labs(x = "MLE Count estimate",
       title = "Fish + macros",
       subtitle = "Density plot with mean (point), 66% (thick bar) and 95% (thin bar) quantiles") +
  theme_bw()

mle_lat %>%
  filter(!is.na(mle_estimate)) %>%
  summarize(min = min(mle_estimate),
            median = median(mle_estimate),
            mean = mean(mle_estimate),
            max = max(mle_estimate),
            q05 = quantile(mle_estimate, probs = 0.05),
            q95 = quantile(mle_estimate, probs = 0.95))

# fish_macro isd plots ####
names(fish_macro_results)
names(fish_macro)
dim(fish_macro_results)
dim(fish_macro)

isd_dat_to_plot <- left_join(fish_macro_results, fish_macro) %>%
  group_by(group_id, dat_id, site, mle_estimate, conf_lo, conf_hi) %>%
  summarise(xmin = min(body_mass),
            xmax = max(body_mass))


make_isd_dat <- function(isd_dat){
  xmin = min(isd_dat$xmin)
  xmax = max(isd_dat$xmax)
  
  x.PLB <- seq(xmin,
               xmax,
               length = nsamples)
  
  lambda = unique(isd_dat$mle_estimate)
  .lower = unique(isd_dat$conf_lo)
  .upper = unique(isd_dat$conf_hi)
  
  y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))*nsamples
  
  ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1))))*nsamples
  
  ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))*nsamples
  
  plot_dat <- tibble(x = x.PLB,
                     y = y.PLB,
                     y_low = ymin.PLB,
                     y_hi = ymax.PLB,
                     group_id = unique(isd_dat$group_id),
                     dat_id = unique(isd_dat$dat_id),
                     site = unique(isd_dat$site))
}

isd_dat <- list()
for(i in 1:nrow(isd_dat_to_plot)){
  in_dat <- isd_dat_to_plot[i,]
  out <- make_isd_dat(in_dat)
  isd_dat[[i]] <- out
}

bind_rows(isd_dat) %>%
  left_join(lat_df) %>%
  ggplot(
    aes(x = x, 
        y = y, # reason that wesner does y/1000???
        ymin = y_low,
        ymax = y_hi,
        group = group_id,
        color = geographical_latitude)) +
  geom_line(linewidth = 0.2) +
  geom_ribbon(alpha = 0.5) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  labs(y = "Proportion of body sizes \u2265 x",
       x = "Individual dry mass (mg)")
  


# workout function ####  
# isd_dat <- isd_dat_to_plot %>%
#   filter(group_id == 1)
# 
# nsamples = 1000
# xmin = min(isd_dat$xmin)
# xmax = max(isd_dat$xmax)
# 
# x.PLB <- seq(xmin,
#              xmax,
#              length = nsamples)
# 
# lambda = unique(isd_dat$mle_estimate)
# .lower = unique(isd_dat$conf_lo)
# .upper = unique(isd_dat$conf_hi)
# 
# y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))*nsamples
# 
# ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1))))*nsamples
# 
# ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))*nsamples
# 
# plot_dat <- tibble(x = x.PLB,
#                   y = y.PLB,
#                   y_low = ymin.PLB,
#                   y_hi = ymax.PLB,
#                   group_id = unique(isd_dat$group_id),
#                   dat_id = unique(isd_dat$dat_id),
#                   site = unique(isd_dat$site))
# 
# ggplot(plot_dat,
#        aes(x = x, y = y, ymin = y_low, ymax = y_hi)) +
#   geom_line() +
#   geom_ribbon() +
#   scale_y_log10() +
#   scale_x_log10()
# MLE binned --------------------------------------------------------------

bin_mle <- function(df){
  counts <- df %>%
    select(body_mass, count) %>%
    as.data.frame()
  x.binned <- binData(counts = counts,
                      binWidth = "2k")
  num.bins <- nrow(x.binned$binVals)
  # rm dplyr:: ?
  binBreaks <- c(dplyr::pull(x.binned$binVals, binMin),
                 dplyr::pull(x.binned$binVals, binMax)[num.bins])
  
  binCounts <- dplyr::pull(x.binned$binVals, binCount)
  
  mle_list <- calcLike(negLL.fn = negLL.PLB.binned,
                        p = -1.5,
                        w = binBreaks,
                        d = binCounts,
                        J = length(binCounts),   # = num.bins
                        vecDiff = 1)
  result <- data.frame(mle_estimate = mle_list$MLE,
                       conf_lo = mle_list$conf[1],
                       conf_hi = mle_list$conf[2])
  return(result)
  
}

# bin_mle(charm_test)

fish_macro2 <- bind_rows(
  data_list$df_Perkins.xlsx %>%
    mutate(dat_id = "Perkins"),
  df_NEON_fish_macro
) %>%
  select(dat_id, site, body_mass, count)



# making all counts integer
fish_macro2 <- fish_macro2 |> 
  mutate(count = round(count*10000)) %>%
  group_by(dat_id, site) %>%
  mutate(group_id = cur_group_id()) |>
  ungroup()

fish_split2 <- fish_macro2 |>
  split(fish_macro2$group_id)


# # takes FOREVER....
# mle_binned <- fish_split2 |>
#   map(\(df) bin_mle(df))
# 
# saveRDS(mle_binned, "binned_data_list.RDS")
mle_binned <- readRDS("binned_data_list.RDS")

bind_rows(mle_binned, .id = "group_id")



fish_macro_binned_results <- bind_rows(mle_binned, .id = "group_id") %>%
  mutate(group_id = as.numeric(group_id)) %>%
  left_join(fish_macro2 %>% 
              select(dat_id, site, group_id) %>%
              unique())

fish_macro_binned_results <- fish_macro_binned_results %>%
  left_join(lat_df)

## ggplots ####

ggplot(fish_macro_binned_results,
       aes(x = (geographical_latitude),
           y = mle_estimate,
           ymin = conf_lo,
           ymax = conf_hi,
           color = dat_id)) +
  geom_pointrange() +
  stat_smooth(aes(x = (geographical_latitude),
                  y = mle_estimate),
              method = "lm",
              inherit.aes = FALSE) +
  labs(title = "Fish + Macros",
       x = "latitude",
       y = "MLE binned estimate") +
  theme_bw()

ggplot(fish_macro_binned_results,
       aes(x = mle_estimate)) +
  tidybayes::stat_halfeye(fill = "dodgerblue") +
  labs(x = "MLE Binned estimate",
       title = "Fish + macros",
       subtitle = "Density plot with mean (point), 66% (thick bar) and 95% (thin bar) quantiles") +
  theme_bw()


fish_macro_binned_results %>%
  mutate(method = "binned") %>%
  bind_rows(fish_macro_results) %>%
  ggplot(aes(x = (geographical_latitude),
             y = mle_estimate,
             ymin = conf_lo,
             ymax = conf_hi,
             color = method)) +
  geom_pointrange(alpha = 0.75) +
  stat_smooth(method = "lm") +
  #scale_color_viridis_d(option = "magma") +
  labs(title = "Fish + Macros; count vs. binned",
       x = "latitude",
       y = "MLE  estimate") +
  theme_bw()


fish_macro_binned_results %>%
  mutate(method = "binned") %>%
  bind_rows(fish_macro_results) %>%
  filter(!is.na(mle_estimate)) %>%
  group_by(method) %>%
  summarize(min = min(mle_estimate),
            median = median(mle_estimate),
            mean = mean(mle_estimate),
            max = max(mle_estimate),
            q05 = quantile(mle_estimate, probs = 0.05),
            q95 = quantile(mle_estimate, probs = 0.95))

fish_macro_binned_results %>%
  mutate(method = "binned") %>%
  bind_rows(fish_macro_results) %>%
  ggplot(aes(x = mle_estimate,
             fill = method)) +
  geom_density(alpha = 0.75) +
  #scale_fill_viridis_d() +
  labs(title = "Fish + Macros; count vs. binned",
       x = "latitude",
       y = "MLE  estimate") +
  theme_bw()

fish_macro_binned_results %>%
  mutate(method = "binned") %>%
  bind_rows(fish_macro_results) %>%
ggplot(
       aes(x = mle_estimate,
           fill = method)) +
  tidybayes::stat_halfeye(alpha = 0.5) +
  labs(x = "MLE estimate",
       title = "Fish + macros; binned v count",
       subtitle = "Density plot with mean (point), 66% (thick bar) and 95% (thin bar) quantiles") +
  theme_bw()

summary(lm(mle_estimate ~ geographical_latitude, 
           data = fish_macro_binned_results))

summary(lm(mle_estimate ~ geographical_latitude, 
           data = fish_macro_results))

# LBN_bin_plot ####
# lbn bin plots from size spectra

LBN_bin_plot(binValsTibble = x.binned$binVals,
             b.MLE = MLEbin.res$MLE,
             b.confMin = MLEbin.res$conf[1],
             b.confMax = MLEbin.res$conf[2],
             #leg.text = "(c)",
             xLab = expression(paste("Body mass ", italic(x), "(mg)")),
             log.xy = "xy",
             plot.binned.fitted = TRUE)

title(uniq[iii])
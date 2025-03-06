# compare mle count and mle bins for neon data

library(tidyverse)
library(furrr)
library(sizeSpectra)
source("new_GoF_functions.R")   # load in new functions
source("bin_data.R")

raw_orig <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")

#raw_orig <- readr::read_csv("example_for_andrew.csv") %>% # this was the test file for Andrew
#dplyr::mutate_if(is.character, factor) %>%
#dplyr::mutate(group_id = as.factor(group_id))

raw_simp <- select(raw_orig,
                   dat_id,
                   site_date,
                   group_id,
                   body_mass,
                   ind_n)

neon <- raw_simp %>%
  filter(dat_id == "df_NEON.xlsx")


#group_id_vec <- unique(raw_simp$group_id) [c(500:3115)]   # group_id's in example

dat_split <- neon %>%
  split(neon$group_id)


plan(cluster, workers = 10)

vecDiff = 1 # bigger = more estimates, but huge CI's

mle_start <- Sys.time()
mle_bin <- dat_split |>
  future_map(possibly(function(.data){
    fit_one_list(.data)
    # calcLike(
    #   negLL.fn = negLL.PLB.counts,
    #   x = .data$body_mass,
    #   c = .data$ind_n,
    #   p = -1.5,
    #   vecDiff = vecDiff)
  }, 
  otherwise = "Error")
  )
mle_end <- Sys.time()
(mle_run <- mle_end - mle_start)

plan(cluster, workers = 1)

binVals <- dat_split |> # add the bin "peak" here
  future_map(possibly(function(.data){
    .data %>% 
      arrange(body_mass) %>%
      select(x = body_mass,
             counts = ind_n) %>%
      bin_data(binWidth = "2k") %>%
      .$binVals
  }, 
  otherwise = "Error")
  )

mle_bin_rows <- list()
for (i in 1:length(mle_bin)){
  if(mle_bin[[i]][[1]] == "Error"){
    out <- data.frame(
      group_id = as.numeric(names(mle_bin[i])),
      site_date=NA,
      min.x = NA,
      max.x = NA,
      n = NA,
      MLE.b =NA,
      MLE.b.l.ci = NA,
      MLE.b.u.ci =NA,
      p.val.k1 = NA,
      consistent.k1 = NA,
      p.val.k2 = NA,
      consistent.k2 = NA)
  } else{
    out <- mle_bin[[i]]
  }
  mle_bin_rows[[i]] <- out
}


mle_bin_res_df <- bind_rows(mle_bin_rows)
plot(density(na.omit(mle_bin_res_df$MLE.b)))

LBN_bin_plot(#binValsTibble = res_this_id$binned,
  binValsTibble = binVals[[1]][complete.cases(binVals[[1]]), ],
    #binValsTibble[complete.cases(binValsTibble), ], #  removes missing bins to avoid plotting error
  b.MLE = mle_bin[[1]]$MLE.b,
  b.confMin = mle_bin[[1]]$MLE.b.l.ci,
  b.confMax = mle_bin[[1]]$MLE.b.u.ci,
  leg.text = "(b)",
  log.xy = "xy",
  xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  plot.binned.fitted = TRUE)


# for(i in mle_df_complete$group_id){
#   print(i)
# }

mle_df_complete <- mle_bin_res_df %>%
  filter(!is.na(MLE.b))

for(i in mle_df_complete$group_id){
  
  i_char = as.character(i)
  
  binValsTibble = binVals[[i_char]]
  
  fit = mle_df_complete %>%
    filter(group_id == i)
  
  LBN_bin_plot(#binValsTibble = res_this_id$binned,
    binValsTibble = binValsTibble[complete.cases(binValsTibble), ],
    b.MLE = fit$MLE.b,
    b.confMin = fit$MLE.b.l.ci,
    b.confMax = fit$MLE.b.u.ci,
    leg.text = i_char,
    log.xy = "xy",
    xLab = expression(paste("Body mass ", italic(x), "(mg)")),
    plot.binned.fitted = TRUE)
}



# 13489

test <- dat_split[["13489"]]
test_binVals <- test %>%
  arrange(body_mass) %>%
  select(x = body_mass,
         counts = ind_n) %>%
  bin_data(binWidth = "2k") %>%
  .$binVals

test_fit_bin <- fit_one_list(test)

test_fit_count <- calcLike(
  negLL.fn = negLL.PLB.counts,
  x = test$body_mass,
  c = test$ind_n,
  p = -1.5,
  vecDiff = vecDiff)

LBN_bin_plot(#binValsTibble = res_this_id$binned,
  binValsTibble = test_binVals[complete.cases(test_binVals), ],
  b.MLE = test_fit_bin$MLE.b,
  b.confMin = test_fit_bin$MLE.b.l.ci,
  b.confMax = test_fit_bin$MLE.b.u.ci,
  leg.text = "all data",
  log.xy = "xy",
  xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  plot.binned.fitted = TRUE)




# 13557

test <- dat_split[["13557"]]
test_binVals <- test %>%
  arrange(body_mass) %>%
  select(x = body_mass,
         counts = ind_n) %>%
  bin_data(binWidth = "2k") %>%
  .$binVals

test_fit_bin <- fit_one_list(test %>%
                               filter(body_mass>0.03))

test_fit_count <- calcLike(
  negLL.fn = negLL.PLB.counts,
  x = test$body_mass,
  c = test$ind_n,
  p = -1.5,
  vecDiff = vecDiff)

LBN_bin_plot(#binValsTibble = res_this_id$binned,
  binValsTibble = test_binVals[complete.cases(test_binVals), ],
  b.MLE = test_fit_bin$MLE.b,
  b.confMin = test_fit_bin$MLE.b.l.ci,
  b.confMax = test_fit_bin$MLE.b.u.ci,
  leg.text = "all data",
  log.xy = "xy",
  xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  plot.binned.fitted = TRUE)


new_func <- function(dat){
  ### modify to return plot
  test_binVals <- dat %>%
    arrange(body_mass) %>%
    select(x = body_mass,
           counts = ind_n) %>%
    bin_data(binWidth = "2k") %>%
    .$binVals
  
  test_fit_bin <- fit_one_list(dat)
  test_fit_bin$method = "bin"
  test_fit_bin$group_id = unique(dat$group_id)
  bin_df <- test_fit_bin %>%
    select(method,
           group_id,
           b = MLE.b,
           low = MLE.b.l.ci,
           high = MLE.b.u.ci)
  
  test_fit_count <- calcLike(
    negLL.fn = negLL.PLB.counts,
    x = dat$body_mass,
    c = dat$ind_n,
    p = -1.5,
    vecDiff = vecDiff,
    suppress.warnings = TRUE)
  
  count_df <- data.frame(
    group_id = unique(dat$group_id),
    method = "count",
    b = test_fit_count$MLE,
    low = test_fit_count$conf[1],
    high = test_fit_count$conf[2])
  
  # save this as an object and return below
  # LBN_bin_plot(#binValsTibble = res_this_id$binned,
  #   binValsTibble = test_binVals[complete.cases(test_binVals), ],
  #   b.MLE = test_fit_bin$MLE.b,
  #   b.confMin = test_fit_bin$MLE.b.l.ci,
  #   b.confMax = test_fit_bin$MLE.b.u.ci,
  #   leg.text = paste("min size = ", 
  #                    as.character(round(min(dat$body_mass), 4))),
  #   log.xy = "xy",
  #   xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  #   plot.binned.fitted = TRUE)
  
  return( # add plot object
      bind_rows(bin_df,
                count_df))
}

new_func(test)

test %>%
  filter(body_mass > 0.3) %>%
  new_func()

plan(cluster, workers = 10)

vecDiff = 1
new_bin_all_data <- dat_split |>
  future_map(possibly(function(.data){
    new_func(.data)
  }, 
  otherwise = "Error")
  )
plan(cluster, workers = 1)


new_bin_df <- new_bin_all_data %>%
  map(\(vec) as.data.frame(vec)) %>%
  list_rbind()

ggplot(new_bin_df,
       aes(x = group_id,
           y = b, 
           ymin = low, 
           ymax = high, 
           color = method)) +
  geom_pointrange()

ggplot(new_bin_df,
       aes(x = b, 
           fill = method)) +
  geom_density(alpha = 0.5)


plan(cluster, workers = 10)
new_bin_gt_0.3 <- dat_split |>
  future_map(possibly(function(.data){
    .data %>%
      filter(body_mass > 0.0310) %>%
      new_func()
  },
  otherwise = "Error")
  )
plan(cluster, workers = 1)

filt_bin_df <- new_bin_gt_0.3 %>%
  map(\(vec) as.data.frame(vec)) %>%
  list_rbind()

filt_bin_df %>%
  group_by(method) %>%
  summarise(mean(b, na.rm = TRUE),
            median(b, na.rm = TRUE))

ggplot(filt_bin_df,
       aes(x = group_id,
           y = b, 
           ymin = low, 
           ymax = high, 
           color = method)) +
  geom_pointrange()

ggplot(filt_bin_df,
       aes(x = b, 
           fill = method)) +
  geom_density(alpha = 0.5)


# dat_split |>
#   future_map(possibly(function(.data){
#     .data %>%
#       filter(body_mass > 0.3) %>%
#       nrow(.)
#   },
#   otherwise = "Error")) 


min_bin <- function(dat){
  test_binVals <- dat %>%
    arrange(body_mass) %>%
    select(x = body_mass,
           counts = ind_n) %>%
    bin_data(binWidth = "2k") %>%
    .$binVals
  
  index_sum_norm <- which.max(test_binVals$binSumNorm)
  index_count_norm <- which.max(test_binVals$binCountNorm)
  
  peak_sum_norm <- test_binVals$binMin[index_sum_norm]
  peak_count_norm <- test_binVals$binMin[index_count_norm]
  
  return(data.frame(group_id = unique(dat$group_id),
                    site_date = unique(dat$site_date),
                    sum_norm = peak_sum_norm,
                    count_norm = peak_count_norm))
  
}

min_bin(test)

plan(cluster, workers = 10)

min_bin_df <- dat_split |>
  future_map(possibly(function(.data){
    min_bin(.data)
  }, 
  otherwise = data.frame(group_id = "Error",
                         site_date = "Error",
                         sum_norm = "Error",
                         count_norm = "Error"))
  ) %>%
  list_rbind()

plan(cluster, workers = 1)
min_bin_df

min_bin_df <- min_bin_df %>%
  separate_wider_delim(site_date, names = c("site", "date"), delim = "_",
                       cols_remove = TRUE)

min_bin_df %>%
  mutate(diff = sum_norm - count_norm) %>%
  View()

min_bin_df %>%
  summarize(median(sum_norm),
            median(count_norm))

min_bin_df %>%
  group_by(site) %>%
  summarize(median(sum_norm),
            median(count_norm)) %>%
  View()

ggplot(min_bin_df,
       aes(x = sum_norm,
           y = count_norm,
           color = site)) +
  geom_point()

min_bin_df %>%
  ggplot(alpha = 0.5) +
  geom_density(aes(x = sum_norm), color = "red") +
  geom_density(aes(x = count_norm), color = "black")

min_bin_df %>%
  pivot_longer(c(sum_norm, count_norm)) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(position = "identity")

min_bin_df %>%
  select(-group_id) %>%
  write_csv("NEON_cutoff.csv")

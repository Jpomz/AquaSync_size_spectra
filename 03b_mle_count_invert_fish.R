# script 03a
# using mleCount method for subset of data

devtools::install_github("andrew-edwards/sizeSpectra",
                         upgrade = FALSE)
library(tidyverse)
library(furrr)
library(sizeSpectra)

dat <- readRDS("derived_data/filtered_size_jan-11.RDS")%>%
  group_by(dat_id, site) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass),
         group_id = cur_group_id())

# filtered_vector <- c(
#   "df_NEON.xlsx",
#   "size_spectra_bio_BA.xlsx",
#   "size_spectra_bio_MG.xlsx",
#   "size_spectra_bio_PA.xlsx",
#   "size_spectra_bio_SP.xlsx",
#   "df_O_Gorman_1.xlsx",
#   "df_Perkins.xlsx" ,
#   "df_Pomeranz.xlsx"
# )

# test_dat <- dat %>%
#   filter(dat_id %in% filtered_vector) 
# 
# test_split <- test_dat %>%
#   split(test_dat$group_id)
# 
# plan(cluster, workers = 8)
# 
# mle_start <- Sys.time()
# mle_count <- test_split |>
#   future_map(function(.data){
#     calcLike(
#     negLL.fn = negLL.PLB.counts,
#     x = .data$body_mass,
#     c = .data$ind_n,
#     p = -1.5)}
#     )
# mle_end <- Sys.time()
# (mle_run <- mle_end - mle_start)
# 
# plan(cluster, workers = 1)
# 
# mle_count_rows <- list()
# for (i in 1:length(mle_count)){
#   out <- data.frame(group_id = names(mle_count[i]),
#                     mle_estimate = mle_count[[i]]$MLE,
#                     conf_lo = mle_count[[i]]$conf[1],
#                     conf_hi = mle_count[[i]]$conf[2])
#   mle_count_rows[[i]] <- out
# }
# 
# 
# 
# mle_count_results <- bind_rows(mle_count_rows) %>%
#   mutate(group_id = as.numeric(group_id)) %>%
#   left_join(dat %>%
#               select(dat_id, site, group_id) %>%
#               unique())




# full datset
dat_split <- dat %>%
  split(dat$group_id)

nrow_dat_split <- dat_split %>%
  map_dbl(\(dat) nrow(dat)) 

plan(cluster, workers = 10)

vecDiff = 0.5 # bigger = more estimates, but huge CI's

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
              select(dat_id, site, group_id) %>%
              unique())

# add nrow to results data frame
mle_count_results$nrow_dat_split <- nrow_dat_split

# total rows
mle_count_results %>%
  nrow()

# rows that failed to fit (most likely small vecDiff)
mle_count_results %>%
  filter(is.na(mle_estimate)) %>%
  nrow()
# 134/2659
#~5% data with no mle estimate


saveRDS(mle_run, paste0("results/mle_run_", Sys.Date(), ".rds"))
saveRDS(mle_count_results, paste0("results/mle_count_vecDiff-", vecDiff, "_", Sys.Date(),".rds"))

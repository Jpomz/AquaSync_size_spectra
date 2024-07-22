#rm(list=ls())

library(tidyverse)
library(furrr)
library(sizeSpectra)
source("new_GoF_functions.R")   # load in new functions
source("bin_data.R")

raw_orig <- readRDS("derived_data/formatted_files_stitched_filtered_July-2024.RDS")

site_info <- raw_orig %>%
  select(group_id, organism_group, organism_groups, geographical_latitude) %>%
  distinct()

#raw_orig <- readr::read_csv("example_for_andrew.csv") %>% # this was the test file for Andrew
  #dplyr::mutate_if(is.character, factor) %>%
  #dplyr::mutate(group_id = as.factor(group_id))

raw_simp <- select(raw_orig,
                   site_date,
                   group_id,
                   body_mass,
                   ind_n)


#group_id_vec <- unique(raw_simp$group_id) [c(500:3115)]   # group_id's in example

dat_split <- raw_simp %>%
  split(raw_simp$group_id)


plan(cluster, workers = 10)

vecDiff = 2 # bigger = more estimates, but huge CI's

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



#bind_rows(mle_bin)
# s.out <- data.frame(
#   site_date=unique(raw_simp_this_id$site_date),
#   min.x = min(raw_simp_this_id$body_mass),
#   max.x = max(raw_simp_this_id$body_mass),
#   n = length(raw_simp_this_id$body_mass),
#   MLE.b =result$MLEbin.res$MLE,
#   MLE.b.l.ci = result$MLEbin.res$conf[1],
#   MLE.b.u.ci =result$MLEbin.res$conf[2],
#   p.val.k1 = result$GoF_K1$Pvalue,
#   consistent.k1 = result$GoF_K1$consistent,
#   p.val.k2 = result$GoF_K2$Pvalue,
#   consistent.k2 = result$GoF_K2$consistent)


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


length(mle_bin_res_df$MLE.b) # 15866
length(mle_bin_res_df$MLE.b[is.na(mle_bin_res_df$MLE.b)]) # 2387 NAs

# how many estimates ?
length(mle_bin_res_df$MLE.b) - length(mle_bin_res_df$MLE.b[is.na(mle_bin_res_df$MLE.b)])
# 13,497 MLE_bin estimates


dim(mle_bin_res_df)
mle_bin_res_df <- mle_bin_res_df %>%
  left_join(site_info)
dim(mle_bin_res_df)

names(mle_bin_res_df)

ggplot(na.omit(mle_bin_res_df),
       aes(x = abs(geographical_latitude),
           y = MLE.b,
           ymin = MLE.b.l.ci,
           ymax = MLE.b.u.ci,
           color = organism_groups)) +
  geom_pointrange(alpha = 0.5) +
  theme_bw() +
  #geom_smooth(method = "lm") +
  facet_wrap(~organism_groups)

ggplot(mle_bin_res_df,
       aes(x = MLE.b,
           fill = organism_groups)) +
  geom_density(alpha = 0.5, bounds = c(-3, 0))


mle_res_id <- mle_bin_res_df %>%
  filter(!is.na(MLE.b)) %>%
  pull(group_id) %>%
  unique()

class(mle_res_id)

dat_split[[as.character(mle_res_id[1])]]

bin_tibble_list <- list()

for(i in 1:length(mle_res_id)){
  i_char <- as.character(mle_res_id[i])
  
  counts <- dat_split[[i_char]] %>%
    select(x = body_mass,
           counts = ind_n)
  binned_with_peak <- bin_data(counts,
                               binWidth = "2k")
  
  index_peak <- which.max(binned_with_peak$binVals$binSumNorm)
  
  binned <- binned_with_peak$binVals[index_peak:nrow(binned_with_peak$binVals), ]
  
  bin_tibble_list[[i]] <- binned
}

names(bin_tibble_list) <- as.character(mle_res_id)
length(mle_res_id)

saveRDS(mle_bin_res_df, "derived_data/mle_bin_gof_result.RDS")
saveRDS(bin_tibble_list, "derived_data/list_bin_tibbles.RDS")



# to do list july 2024 ----------------------------------------------------

# why are some collections not fitting?

mle_bin_res_df %>%
  filter(is.na(MLE.b)) %>%
  pull(group_id)

raw_simp_this_id <- dat_split[["254"]]
# 254 - error in combine_bin()
  # Error in combine_bins(binBreaks = binBreaks, binCounts = binCounts) :
  # sum(combined_counts) == sum(binCounts) is not TRUE
  # when I try to run through combine_bins() manually I don't get this error  

raw_simp_this_id <- dat_split[["256"]]
# same error
raw_simp_this_id <- dat_split[["351"]]
# same
raw_simp_this_id <- dat_split[["382"]]
# same
raw_simp_this_id <- dat_split[["396"]]
# same
raw_simp_this_id <- dat_split[["399"]]
# same
raw_simp_this_id <- dat_split[["418"]]
# same
raw_simp_this_id <- dat_split[["420"]]
# same
raw_simp_this_id <- dat_split[["1531"]]
# same
raw_simp_this_id <- dat_split[["2394"]]
# same

raw_simp_this_id <- dat_split[["6314"]]
# increase size of vecDiff 


raw_simp_this_id <- dat_split[["13591"]]
# combine_bins error
raw_simp_this_id <- dat_split[["13569"]]
# combine_bins error

raw_simp_this_id <- dat_split[["13649"]]
# `ind_n` column is all NAs
raw_simp_this_id <- dat_split[["13656"]]
# `ind_n` column is all NAs
raw_simp_this_id <- dat_split[["13656"]]
# `ind_n` column is all NAs



counts <- dplyr::select(raw_simp_this_id,
                        x = body_mass,
                        counts = ind_n)

# Prob has a peak. If first index is peak then still good.
binned_with_peak <- bin_data(counts,
                             binWidth = "2k")

index_peak <- which.max(binned_with_peak$binVals$binSumNorm)

binned <- binned_with_peak$binVals[index_peak:nrow(binned_with_peak$binVals), ]

# Note binned is just the tibble, shortened versino of
# binned_with_peak$binVals, as we don't need $indiv for calcs.

num.bins <- nrow(binned)

# bin breaks are the minima plus the max of the final bin:
binBreaks <- c(dplyr::pull(binned, binMin),
               dplyr::pull(binned, binMax)[num.bins])

binCounts <- dplyr::pull(binned,
                         binCount)

MLEbin.res <-  calcLike(negLL.PLB.binned,
                        p = -1.5,
                        w = binBreaks,
                        d = binCounts,
                        J = length(binCounts),   # = num.bins
                        vecDiff = vecDiff,
                        suppress.warnings = suppress.warnings)             # increase this if hit a bound

GoF_res_K1 <- GoF_PLB(bin_breaks = binBreaks,
                      bin_counts = binCounts,
                      b = MLEbin.res$MLE,
                      K = 1)

GoF_res_K2 <- GoF_PLB(bin_breaks = binBreaks,
                      bin_counts = binCounts,
                      b = MLEbin.res$MLE,
                      K = 2)


# old code from Perkins: --------------------------------------------------


# # set up dummy summary file for output to be saved
# s.out <- data.frame(
#   site_date=unique(raw_orig$site_date),
#   min.x = NA,
#   max.x = NA,
#   n = NA,
#   MLE.b =NA,
#   MLE.b.l.ci = NA,
#   MLE.b.u.ci =NA,
#   p.val.k1 = NA,
#   consistent.k1 = NA,
#   p.val.k2 = NA,
#   consistent.k2 = NA)
# 
# 
# 
# 
# # data set
# # res_all_group_id <- list()
# for(i in 1:length(group_id_vec)){
#   group_id_val <- group_id_vec[i]
#   
#   
#   res_this_id <- fit_one_group_id(raw_simp,
#                                   group_id_val)
#   
#   # ISD_bin_plot_nonoverlapping(binBreaks = res_this_id$binBreaks,
#   #                             binCounts = res_this_id$binCounts,
#   #                             b.MLE = res_this_id$MLEbin.res$MLE,
#   #                             b.confMin = res_this_id$MLEbin.res$conf[1],
#   #                             b.confMax = res_this_id$MLEbin.res$conf[2])
#   # 
#   # LBN_bin_plot(binValsTibble = res_this_id$binned,
#   #              b.MLE = res_this_id$MLEbin.res$MLE,
#   #              b.confMin = res_this_id$MLEbin.res$conf[1],
#   #              b.confMax = res_this_id$MLEbin.res$conf[2],
#   #              leg.text = "(a)",
#   #              log.xy = "x",
#   #              plot.binned.fitted = TRUE)
#   # 
#   # LBN_bin_plot(#binValsTibble = res_this_id$binned, 
#   #              binValsTibble <- binValsTibble[complete.cases(binValsTibble), ], #  removes missing bins to avoid plotting error
#   #              b.MLE = res_this_id$MLEbin.res$MLE,
#   #              b.confMin = res_this_id$MLEbin.res$conf[1],
#   #              b.confMax = res_this_id$MLEbin.res$conf[2],
#   #              leg.text = "(b)",
#   #              log.xy = "xy",
#   #              xLab = expression(paste("Body mass ", italic(x), "(mg)")),
#   #              plot.binned.fitted = TRUE)
#   # 
#   #print(res_this_id$MLEbin.res)
#   #print(res_this_id$GoF_K1)
#  #print(res_this_id$GoF_K2)
#   
#   # extract output to be saved
#   s.out$MLE.b[i] <- res_this_id$MLEbin.res$MLE
#   s.out$MLE.b.l.ci[i] <- res_this_id$MLEbin.res$conf[1]
#   s.out$MLE.b.u.ci[i] <- res_this_id$MLEbin.res$conf[2]
#   
#   site.df <- subset(raw_simp, group_id == group_id_vec[i])
# 
#   s.out$min.x[i] <- min(site.df$body_mass)
#   s.out$max.x[i] <- max(site.df$body_mass)
#   s.out$n[i] <- length(site.df$body_mass)
#   
#   s.out$p.val.k1[i] <- res_this_id$GoF_K1$Pvalue
#   s.out$consistent.k1[i] <- res_this_id$GoF_K1$consistent
#   s.out$p.val.k2[i] <- res_this_id$GoF_K2$Pvalue
#   s.out$consistent.k2[i] <- res_this_id$GoF_K2$consistent
# }
# 
# head(s.out)
# 
# 

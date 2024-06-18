#rm(list=ls())

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
                   site_date,
                   group_id,
                   body_mass,
                   ind_n)


#group_id_vec <- unique(raw_simp$group_id) [c(500:3115)]   # group_id's in example

dat_split <- raw_simp %>%
  split(raw_simp$group_id)


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

length(mle_bin_res_df$MLE.b) # 3115
length(mle_bin_res_df$MLE.b[is.na(mle_bin_res_df$MLE.b)]) # 124 NAs

# how many estimates ?
length(mle_bin_res_df$MLE.b) - length(mle_bin_res_df$MLE.b[is.na(mle_bin_res_df$MLE.b)])
# 2,991 MLE_bin estimates


saveRDS(mle_bin_res_df, "derived_data/mle_bin_gof_result.RDS")

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

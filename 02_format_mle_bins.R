rm(list=ls())

library(sizeSpectra)
library(dplyr)
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


group_id_vec <- unique(raw_simp$group_id) [c(500:3115)]   # group_id's in example


# set up dummy summary file for output to be saved
s.out <- data.frame(site_date=unique(raw_orig$site_date), min.x = NA, max.x = NA, n = NA, b = NA, MLE.b =NA, MLE.b.l.ci = NA, MLE.b.u.ci =NA,
                    p.val.k1 = NA, consistent.k1 = NA, p.val.k2 = NA, consistent.k2 = NA)

# data set
# res_all_group_id <- list()
for(i in 1:length(group_id_vec)){
  group_id_val <- group_id_vec[i]
  
  
  res_this_id <- fit_one_group_id(raw_simp,
                                  group_id_val)
  
  # ISD_bin_plot_nonoverlapping(binBreaks = res_this_id$binBreaks,
  #                             binCounts = res_this_id$binCounts,
  #                             b.MLE = res_this_id$MLEbin.res$MLE,
  #                             b.confMin = res_this_id$MLEbin.res$conf[1],
  #                             b.confMax = res_this_id$MLEbin.res$conf[2])
  # 
  # LBN_bin_plot(binValsTibble = res_this_id$binned,
  #              b.MLE = res_this_id$MLEbin.res$MLE,
  #              b.confMin = res_this_id$MLEbin.res$conf[1],
  #              b.confMax = res_this_id$MLEbin.res$conf[2],
  #              leg.text = "(a)",
  #              log.xy = "x",
  #              plot.binned.fitted = TRUE)
  # 
  # LBN_bin_plot(#binValsTibble = res_this_id$binned, 
  #              binValsTibble <- binValsTibble[complete.cases(binValsTibble), ], #  removes missing bins to avoid plotting error
  #              b.MLE = res_this_id$MLEbin.res$MLE,
  #              b.confMin = res_this_id$MLEbin.res$conf[1],
  #              b.confMax = res_this_id$MLEbin.res$conf[2],
  #              leg.text = "(b)",
  #              log.xy = "xy",
  #              xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  #              plot.binned.fitted = TRUE)
  # 
  #print(res_this_id$MLEbin.res)
  #print(res_this_id$GoF_K1)
 #print(res_this_id$GoF_K2)
  
  # extract output to be saved
  s.out$MLE.b[i] <- res_this_id$MLEbin.res$MLE
  s.out$MLE.b.l.ci[i] <- res_this_id$MLEbin.res$conf[1]
  s.out$MLE.b.u.ci[i] <- res_this_id$MLEbin.res$conf[2]
  
  site.df <- subset(raw_simp, group_id == group_id_vec[i])

  s.out$min.x[i] <- min(site.df$body_mass)
  s.out$max.x[i] <- max(site.df$body_mass)
  s.out$n[i] <- length(site.df$body_mass)
  
  s.out$p.val.k1[i] <- res_this_id$GoF_K1$Pvalue
  s.out$consistent.k1[i] <- res_this_id$GoF_K1$consistent
  s.out$p.val.k2[i] <- res_this_id$GoF_K2$Pvalue
  s.out$consistent.k2[i] <- res_this_id$GoF_K2$consistent
}

head(s.out)



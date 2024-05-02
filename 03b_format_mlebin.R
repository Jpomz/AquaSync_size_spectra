#03b_format_mleBin

library(tidyverse)
library(furrr)
library(sizeSpectra)
library(weights)

dat <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")

# dat_test2 <- dat_test1 %>% select(body_mass, ind_n) %>%
#   rename(x = body_mass,
#          counts = ind_n)
# 
# # binData does not work with non-integer counts
# binData(counts = dat_test2,
#         binWidth = "2K")


bin_fxn <- function(dat,
                    vecDiff = 1,
                    id_max = FALSE){
  # process data
  x = dat$body_mass
  ind_n =  dat$ind_n
  minx <- min(x) 
  maxx <- max(x) 
  
  # weighted histogram
  wtd_result <- wtd.hist(
    x = x,
    breaks = 2^(floor(log2(minx)):ceiling(log2(maxx))),
    weight = ind_n, 
    plot = FALSE)
  
  wtd_out <- bind_cols(wtd_result[c(2,5)])
  wtd_out <- wtd_out %>%
    mutate(log_counts = log(counts),
           log_mids = log(mids))
  
  # estimate lambda
  
  estimated_lambda <- calcLike(
    negLL.fn = negLL.PLB.binned,
    p = -1.5, # wtf is this?
    w = wtd_result$breaks,
    d = wtd_result$counts,
    J = length(wtd_result$counts),   # = num.bins
    vecDiff = vecDiff,
    suppress.warnings = TRUE)
  
  # binVals requires:
  # binMid, binMin, binMax, binWidth, binCount, binCountNorm, 
  # binSum, binSumNorm, log10binMid, log10binCount, log10BinSum,
  #log10binCountNorm, log10binSumNorm
  
  # LBN_bin_plot(binValsTibble = NULL,
  #              binBreaks = wtd_result$breaks,
  #              binCounts = wtd_result$counts,
  #              b.MLE = estimated_lambda$MLE,
  #              b.confMin = estimated_lambda$conf[1],
  #              b.confMax = estimated_lambda$conf[2],
  #              #leg.text = "(c)",
  #              xLab = expression(paste("Body mass ", italic(x), "(mg)")),
  #              log.xy = "xy",
  #              plot.binned.fitted = TRUE)
  
  
  return(estimated_lambda)
}

dat_test1 <- dat %>%
  filter(group_id == 13506)

bin_fxn(dat_test1, vecDiff = 1)


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
              select(group_id,
                     dat_id,
                     site, 
                     geographical_latitude,
                     organism_groups) %>%
              unique())

mle_bin_results %>%
  filter(is.na(mle_estimate))

mle_bin_results

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

saveRDS(mle_bin_results, "derived_data/format_mle_bin_results.RDS")

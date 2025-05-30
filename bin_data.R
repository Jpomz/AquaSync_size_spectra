# bin_data() and fit_one_group_id()

##' Construct bins either double in size or are of equal width, and encompass
##'  the data, and allows for non-integer counts.
##'
##' Adapted from sizeSpectra::binData() that did not allow for non-integer
##' counts. Had to write new function as non-integer counts implies we only have
##' the `counts_df` dataframe input, cannot have the `x` vector of individual
##' values.
##'
##' This sums the counts of each value into bins.
##'
##' TODO Construct bins that start from `floor(min(x))` or `min(x)` and either double
##'    in size or are of equal width, and encompass the data. More generalised
##'    version of `log2bins()`.
##'
##' @param counts_df dataframe (or array, can be a tibble) with first column `x` being the measured values
##'  (e.g. body masses or lengths), and second column `counts` being the counts of the
##'  number of individuals for that value. The `counts` column can have
##'   non-integer values, unlike for `binData()`.
##' @param binWidth type of bins to use:
##'   * `"2k"` will result in `binBreaks` that:
##'     + with `startInteger=TRUE` are powers of 2, i.e. ..., 0.25, 0.5, 1, 2, 4, 8, 16,....
##'     + with `startInteger=FALSE` are bins that double in size and  start with
##'       `min(x)`; not yet implemented, since have to think about what the width of
##'       the first bin should be.
##'   * numeric value (call it `a`) will result in binBreaks are separated by `a` and span the
##'       data, that:
##'     + with `startInteger=TRUE` start from `z = floor(min(x))` and are then
##'          `z, z+a, z+2a, z+3a, ....`   (if `z = 0` then power-law cannot be fit
##'        so then need to use `startInteger=FALSE`)
##'     + with `startInteger=FALSE` start from `z = min(x)` and are then
##'           `z, z+a, z+2a, z+3a, ....`
##'   * only `binWidth` or `binBreaks` can be specified.
##' @param binBreaks pre-defined bin breaks as a vector. Only `binWidth`
##'   or `binBreaks` can be specified.
##' @param startInteger TRUE or FALSE, whether to start the bin breaks at an integer
##'   power of 2 (for method `"2k"`) or an integer. See `binWidth` above.
##'   `startInteger` is ignored if `binBreaks` is specified.
##' @return list containing:
##'   * indiv: dataframe with a row for each `counts_df$x` value, with columns:
##'      + `x`: original `counts_df$x` values
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If indiv has `>=10^6` rows then it isn't
##'      saved. Keeping the name `indiv` as for `binData()`, but these are
##'      individual counts not individual organisms.
##'   * binVals: dataframe with a row for each new bin and columns:
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `binCount`: total count of numbers of individuals in that bin
##'      + `binCountNorm`: normalised bin count, `binCount / binWidth`
##'      + `binSum`: sum of numbers of individuals * x values in that bin (appropriate if `x`
##'         represents biomass, but not length)
##'      + `binSumNorm`: `binSum / binWidth`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @examples
##' \dontrun{
##' counts_ex <- tibble::tibble(x = as.numeric(1:50), counts = rep(c(0.19, 27.05, 9, 3.1, 0.001), 10))
##' bin_data(counts_ex, binWidth = 6)
##' }
##' @author Andrew Edwards
bin_data = function(counts_df,
                    binWidth = NULL,
                    binBreaks = NULL,
                    startInteger = TRUE)
    {
      if(dim(counts_df)[2] != 2)stop("counts_df needs two cols in binData")
      if(min(counts_df$x) < 0) {
        stop("x values in counts_df need to be >= 0 in binData") }
      if(min(counts_df$counts) < 0) {
        stop("numbers in counts_df need to be >= 0 in binData") }
        if(is.null(binWidth) & is.null(binBreaks)) {
          stop("need one of binWidth or binBreaks in binData") }
        if(!is.null(binWidth) & !is.null(binBreaks)) {
          stop("need only one of binWidth or binBreaks in binData") }
        if(startInteger != "TRUE" & startInteger != "FALSE"){
          stop("startInteger must be TRUE or FALSE in binData") }

      x = counts_df$x                   # need a lot, these are
                                        # measurements
      minx = min(x)  # min(dplyr::pull(counts_df ,1))
      maxx = max(x)  # max(dplyr::pull(counts_df ,1))

        if(!is.null(binBreaks))
           {
           if(minx < min(binBreaks) | maxx > max(binBreaks) )
             { stop("binBreaks do not span data in binData")
             }
           } else           # create binBreaks based on binWidth
           {
           if(binWidth == "2k")
             {
             if(startInteger)
               { binBreaks = 2^( floor(log2(minx)) : ceiling(log2(maxx)) )
               } else
               { stop("startInteger currently needs to be TRUE when
                   binWidth = 2k")
               }
             } else     # If not "2k"
             {
             if(!is.numeric(binWidth))
               { stop("binWidth must be 2k or a number (in quotes is okay
                         in quotes) in binData().")
               }
             # startInteger says whether to start from an integer value or
             #  start from min(x),
             z = floor(minx) * startInteger + minx * !startInteger
             binBreaks = seq( z, by=binWidth,
                        length = ceiling( (maxx - z)/binWidth) + 1)
             }
           }

      indiv = counts_df           # data.frame(x = x)       # dataframe with one
                                  # row for each x value in x
      # x
      indiv$binMid =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-length(binBreaks)] + 0.5*diff(binBreaks))
      indiv$binMin =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-length(binBreaks)])
      indiv$binMax =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-1])
      #
      indiv$binMid = as.numeric(as.character(indiv$binMid))
      indiv$binMin = as.numeric(as.character(indiv$binMin))
      indiv$binMax = as.numeric(as.character(indiv$binMax))
      # Now calculate biomass in each bin class:

        binVals = dplyr::summarise(dplyr::group_by(indiv, binMid),
                                   binMin = unique(binMin),
                                   binMax = unique(binMax),
                                   binWidth = binMax - binMin,
                                   binCount = sum(counts),         # was length(x) for binData
                                   binCountNorm = binCount / binWidth,
                                   binSum = sum(x * counts),       # only appropriate for body masses
                                   binSumNorm = binSum / binWidth )
      # binWidth uses new columns binMax and binMin
      # Indices for minima of bins that have zero counts and so do not
      #  appear in binVals yet:
        emptyBinMinInd = !(signif(binBreaks[-length(binBreaks)], digits = 8) %in%
                           signif(binVals$binMin, digits = 8))
                         # to avoid not-real differences due to rounding/storing
        emptyBinMin = binBreaks[emptyBinMinInd]
        empties = length(emptyBinMin)
        emptyBinMax = binBreaks[-1][emptyBinMinInd]
        emptyBinWidth = emptyBinMax - emptyBinMin
        emptyBinMid = emptyBinMin + emptyBinWidth/2

        emptyVals = as.data.frame(cbind(emptyBinMid,
                                        emptyBinMin,
                                        emptyBinMax,
                                        emptyBinWidth,
                                        matrix(0, nrow=empties, ncol=4)))
        names(emptyVals) = names(binVals)
        binVals = rbind(binVals, emptyVals)         # still a local df

        binVals = binVals[order(binVals$binMid), ]   # order by binMid

        binVals = dplyr::mutate(binVals,
                                log10binMid = log10(binMid),
                                log10binCount = log10(binCount),
                                log10binSum = log10(binSum),
                                log10binCountNorm = log10(binCountNorm),
                                # Had thought that last line is needed to avoid
                                # warnings (e.g. simulate-data2.R) and whole
                                # column being NA's. Maybe don't actually use it
                                # in results, but have put it in, may need to
                                # test it more.
                                log10binSumNorm = log10(binSumNorm))
        binVals[is.infinite(binVals$log10binCount),
                "log10binCount"] = NA
                  # lm can't cope with -Inf, which appear if 0 counts in a bin
        binVals[is.infinite(binVals$log10binCountNorm),
                "log10binCountNorm"] = NA
        binVals[is.infinite(binVals$log10binSum),
                "log10binSum"] = NA
        binVals[is.infinite(binVals$log10binSumNorm),
                "log10binSumNorm"] = NA
        if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
          y = list(indiv = indiv, binVals = binVals)
          } else
          {
          y = list(binVals = binVals)
          }
        return(y)
    }



fit_one_group_id <- function(raw_simp,
                             group_id_here){

  raw_simp_this_id <- dplyr::filter(raw_simp,
                                    group_id == group_id_here) %>%
    dplyr::arrange(body_mass)

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
                          vecDiff = 1)             # increase this if hit a bound

  GoF_res_K1 <- GoF_PLB(bin_breaks = binBreaks,
                        bin_counts = binCounts,
                        b = MLEbin.res$MLE,
                        K = 1)

  GoF_res_K2 <- GoF_PLB(bin_breaks = binBreaks,
                        bin_counts = binCounts,
                        b = MLEbin.res$MLE,
                        K = 2)

  return(list(binned = binned,
              binBreaks = binBreaks,
              binCounts = binCounts,
              MLEbin.res = MLEbin.res,
              GoF_K1 = GoF_res_K1,
              GoF_K2 = GoF_res_K2))
}

# add a true/false for peak or no peak? ####
fit_one_list <- function(raw_simp,
                         vecDiff = 1,
                         cut_off = TRUE,
                         suppress.warnings = TRUE){
  
  # for testing purposes
  # raw_simp <- dat_split$'15737'
  raw_simp_this_id <- raw_simp %>%
    dplyr::arrange(body_mass)
  
  counts <- dplyr::select(raw_simp_this_id,
                          x = body_mass,
                          counts = ind_n)
  
  # Prob has a peak. If first index is peak then still good.
  binned_with_peak <- bin_data(counts,
                               binWidth = "2k")
  if(cut_off == TRUE){
  
  index_peak <- which.max(binned_with_peak$binVals$binSumNorm)
  
  binned <- binned_with_peak$binVals[index_peak:nrow(binned_with_peak$binVals), ]
  min_body <- binned$binMin[1]
  
  indiv <- binned_with_peak$indiv |>
    filter(x >= min_body)
  }
  
  if(cut_off == FALSE){
    binned <- binned_with_peak$binVals
    indiv <- binned_with_peak$indiv
  }
  
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
  
  result <- list(binned = binned,
              binBreaks = binBreaks,
              binCounts = binCounts,
              MLEbin.res = MLEbin.res,
              GoF_K1 = GoF_res_K1,
              GoF_K2 = GoF_res_K2)
  
  # s.out$min.x[i] <- min(raw_simp_this_id$body_mass)
  # s.out$max.x[i] <- max(raw_simp_this_id$body_mass)
  # s.out$n[i] <- length(raw_simp_this_id$body_mass)
  
  # s.out$p.val.k1[i] <- res_this_id$GoF_K1$Pvalue
  # s.out$consistent.k1[i] <- res_this_id$GoF_K1$consistent
  # s.out$p.val.k2[i] <- res_this_id$GoF_K2$Pvalue
  # s.out$consistent.k2[i] <- res_this_id$GoF_K2$consistent
  
  s.out <- data.frame(
    analysis_id = unique(raw_simp_this_id$analysis_id),
    site_date=unique(raw_simp_this_id$site_date),
    min.x = min(indiv$x), # change to binned$indiv
    max.x = max(indiv$x),
    n = length(indiv$x),
    sum_bin_counts = sum(binCounts),
    MLE.b =result$MLEbin.res$MLE,
    MLE.b.l.ci = result$MLEbin.res$conf[1],
    MLE.b.u.ci =result$MLEbin.res$conf[2],
    p.val.k1 = result$GoF_K1$Pvalue,
    consistent.k1 = result$GoF_K1$consistent,
    p.val.k2 = result$GoF_K2$Pvalue,
    consistent.k2 = result$GoF_K2$consistent)
  return(s.out)
}


# bin_tibbles ####
 bin_tibbles <- function(raw_simp){
  
  # for testing purposes
  # raw_simp <- dat_split$'7'
  raw_simp_this_id <- raw_simp %>%
    dplyr::arrange(body_mass)
  
  counts <- dplyr::select(raw_simp_this_id,
                          x = body_mass,
                          counts = ind_n)
  
  # Prob has a peak. If first index is peak then still good.
  binned_with_peak <- bin_data(counts,
                               binWidth = "2k")
  
  # number of bins which have counts in them
  num_bins_peak <- binned_with_peak$binVals |>
    filter(binCount >= 1) |>
    count() |>
    pull()
  
  binned_with_peak$binVals$num_bins <- num_bins_peak
  
  # biomass with peak
  sum_biomass_peak <- sum(binned_with_peak$indiv$x * binned_with_peak$indiv$counts)
  
  # add analysis id to tibbles
  binned_with_peak$indiv$analysis_id = unique(raw_simp_this_id$analysis_id)
  binned_with_peak$binVals$analysis_id = unique(raw_simp_this_id$analysis_id)
  # make biomass a df with analysis_id
  sum_biomass_peak <- data.frame(
    analysis_id = unique(raw_simp_this_id$analysis_id),
    biomass = sum_biomass_peak
  )
  
  peak_out <- list(indiv = binned_with_peak$indiv,
                   binVals = binned_with_peak$binVals,
                   biomass = sum_biomass_peak)
  # calculate the "peak" in the binned data
  index_peak <- which.max(binned_with_peak$binVals$binSumNorm)
  
  # binned with peak removed
  binned_no_peak <- binned_with_peak$binVals[index_peak:nrow(binned_with_peak$binVals), ]
  min_body <- binned_no_peak$binMin[1]
  
  num_bins_no_peak <- binned_no_peak |>
    filter(binCount >= 1) |>
    count() |>
    pull()
  
  binned_no_peak$num_bins <- num_bins_no_peak
  
  
  indiv_no_peak <- binned_with_peak$indiv |>
    filter(x >= min_body)

  sum_biomass_no_peak <- sum(indiv_no_peak$x * indiv_no_peak$counts)
  
  sum_biomass_no_peak <- data.frame(
    analysis_id = unique(raw_simp_this_id$analysis_id),
    biomass = sum_biomass_no_peak
  )
  
  # add analysis id to tibbles
  indiv_no_peak$analysis_id = unique(raw_simp_this_id$analysis_id)
  binned_no_peak$analysis_id = unique(raw_simp_this_id$analysis_id)
  
  no_peak_out <- list(indiv = indiv_no_peak,
                      binVals = binned_no_peak,
                      biomasss = sum_biomass_no_peak)
  
  out <- list(cut = no_peak_out,
              no_cut = peak_out)
  
  return(out)
  }

 
# mle from tibbles ####
 mle_from_cut_tibbles <- function(input_dat, # list with no/cut bins
                              vecDiff = 1,
                              suppress.warnings = TRUE){
   
   bin_dat <- input_dat$cut
   # for testing purposes
   # bin_dat <- binned_tibbles[[7]]
   # bin_dat <- binned_tibbles[[65]] # doesn't fit
   
   # get the number of bins
   num_bins_no_peak <- nrow(bin_dat$binVals)
   
   
   # Results for no peak ####
   # bin breaks are the minima plus the max of the final bin:
   binBreaks_no_peak <- c(dplyr::pull(bin_dat$binVals, binMin),
                          dplyr::pull(bin_dat$binVals, binMax)[num_bins_no_peak])
   
   binCounts_no_peak <- dplyr::pull(bin_dat$binVals,
                                    binCount)
   
   MLEbin.res_no_peak <-  calcLike(negLL.PLB.binned,
                                   p = -1.5,
                                   w = binBreaks_no_peak,
                                   d = binCounts_no_peak,
                                   J = length(binCounts_no_peak),
                                   xmin = min(binBreaks_no_peak),
                                   xmax = max(binBreaks_no_peak),
                                   vecDiff = vecDiff,
                                   suppress.warnings = suppress.warnings)             # increase this if hit a bound
   
   result_ok <- !is.character(MLEbin.res_no_peak)
   
   if(result_ok == TRUE){
   GoF_res_K1_no_peak <- GoF_PLB(bin_breaks = binBreaks_no_peak,
                                 bin_counts = binCounts_no_peak,
                                 b = MLEbin.res_no_peak$MLE,
                                 K = 1)
   
   GoF_res_K2_no_peak <- GoF_PLB(bin_breaks = binBreaks_no_peak,
                                 bin_counts = binCounts_no_peak,
                                 b = MLEbin.res_no_peak$MLE,
                                 K = 2)
   
  
   s.out_no_peak <- data.frame(
     analysis_id = unique(bin_dat$biomass$analysis_id),
     #site_date = unique(raw_simp_this_id$site_date),
     min.x = min(bin_dat$indiv$x), # change to binned$indiv
     max.x = max(bin_dat$indiv$x),
     n = length(bin_dat$indiv$x),
     sum_bin_counts = sum(bin_dat$binVals$binCount),
     MLE.b = MLEbin.res_no_peak$MLE,
     MLE.b.l.ci =  MLEbin.res_no_peak$conf[1],
     MLE.b.u.ci = MLEbin.res_no_peak$conf[2],
     p.val.k1 =  GoF_res_K1_no_peak$Pvalue,
     consistent.k1 =  GoF_res_K1_no_peak$consistent,
     p.val.k2 =  GoF_res_K2_no_peak$Pvalue,
     consistent.k2 =  GoF_res_K2_no_peak$consistent,
     num_bins = num_bins_no_peak,
     biomass = bin_dat$biomass$biomass)
   } else{
     s.out_no_peak <- data.frame(
       analysis_id = unique(bin_dat$biomass$analysis_id),
       #site_date = unique(raw_simp_this_id$site_date),
       min.x = min(bin_dat$indiv$x), # change to binned$indiv
       max.x = max(bin_dat$indiv$x),
       n = length(bin_dat$indiv$x),
       sum_bin_counts = sum(bin_dat$binVals$binCount),
       MLE.b = NA,
       MLE.b.l.ci =  NA,
       MLE.b.u.ci = NA,
       p.val.k1 =  NA,
       consistent.k1 =  NA,
       p.val.k2 =  NA,
       consistent.k2 =  NA,
       num_bins = num_bins_no_peak,
       biomass = bin_dat$biomass$biomass)
     
   }
   
   return(s.out_no_peak)
 }



# mle from no cut tibbles
mle_from_no_cut_tibbles <- function(input_dat, # list with no/cut bins
                                 vecDiff = 1,
                                 suppress.warnings = TRUE){
  
  bin_dat <- input_dat$no_cut
  # for testing purposes
  # bin_dat <- binned_tibbles[[7]]
  # bin_dat <- binned_tibbles[[65]] # doesn't fit
  
  # get the number of bins
  num_bins_peak <- nrow(bin_dat$binVals)
  
  # Peak Results ####
  # MLE fit and results for the peaked data
  
  # bin breaks are the minima plus the max of the final bin:
  binBreaks_peak <- c(dplyr::pull(bin_dat$binVals, binMin),
                      dplyr::pull(bin_dat$binVals, binMax)[num_bins_peak])
  
  binCounts_peak <- dplyr::pull(bin_dat$binVals,
                                binCount)
  
  MLEbin.res_peak <-  calcLike(negLL.PLB.binned,
                               p = -1.5,
                               w = binBreaks_peak,
                               d = binCounts_peak,
                               J = length(binCounts_peak),   # = num.bins
                               xmin = min(binBreaks_peak),
                               xmax = max(binBreaks_peak),
                               vecDiff = vecDiff, # increase this if hit a bound
                               suppress.warnings = suppress.warnings)   
  
  result_ok <- !is.character(MLEbin.res_peak)
  
  if(result_ok == TRUE){
    
  GoF_res_K1_peak <- GoF_PLB(bin_breaks = binBreaks_peak,
                             bin_counts = binCounts_peak,
                             b = MLEbin.res_peak$MLE,
                             K = 1)
  
  GoF_res_K2_peak <- GoF_PLB(bin_breaks = binBreaks_peak,
                             bin_counts = binCounts_peak,
                             b = MLEbin.res_peak$MLE,
                             K = 2)
  
    
    s.out_peak <- data.frame(
      analysis_id = unique(bin_dat$biomass$analysis_id),
      #site_date = unique(raw_simp_this_id$site_date),
      min.x = min(bin_dat$indiv$x), # change to binned$indiv
      max.x = max(bin_dat$indiv$x),
      n = length(bin_dat$indiv$x),
      sum_bin_counts = sum(bin_dat$binVals$binCount),
      MLE.b = MLEbin.res_peak$MLE,
      MLE.b.l.ci =  MLEbin.res_peak$conf[1],
      MLE.b.u.ci = MLEbin.res_peak$conf[2],
      p.val.k1 =  GoF_res_K1_peak$Pvalue,
      consistent.k1 =  GoF_res_K1_peak$consistent,
      p.val.k2 =  GoF_res_K2_peak$Pvalue,
      consistent.k2 =  GoF_res_K2_peak$consistent,
      num_bins = num_bins_peak,
      biomass = bin_dat$biomass$biomass,
      vecDiff = vecDiff)
  } else{
    s.out_peak <- data.frame(
      analysis_id = unique(bin_dat$biomass$analysis_id),
      #site_date = unique(raw_simp_this_id$site_date),
      min.x = min(bin_dat$indiv$x), # change to binned$indiv
      max.x = max(bin_dat$indiv$x),
      n = length(bin_dat$indiv$x),
      sum_bin_counts = sum(bin_dat$binVals$binCount),
      MLE.b = NA,
      MLE.b.l.ci =  NA,
      MLE.b.u.ci = NA,
      p.val.k1 =  NA,
      consistent.k1 =  NA,
      p.val.k2 =  NA,
      consistent.k2 =  NA,
      num_bins = num_bins_no_peak,
      biomass = bin_dat$biomass$biomass,
      vecDiff = vecDiff)
    
  }
  return(s.out_peak)
}
 
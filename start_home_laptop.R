# start at home 2023-12-07
# jpz

library(tidyverse)
library(sizeSpectra)
library(readxl)

file_paths <- list.files("data/", 
                         pattern = "*.xlsx")

data_list <- list()
for(i in 1:length(file_paths)){
  dat_in <- read_excel(paste0(
    "data/", file_paths[i]
  ))
  data_list[[i]] <- dat_in
}
names(data_list) <- file_paths
dat_df <- bind_rows(data_list, .id = "id")

dat_df <- dat_df %>%
  separate(id, into = c("rm", "dat_id", "rm2")) %>%
  select(-rm, -rm2)

dat_df %>%
  pull(count) %>%
  unique()
# all only one count 

# what are the body weight units?
dat_df$body_weight_units %>% unique()

dat_df <- dat_df %>%
  mutate(body_mass = case_when(
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "mg DW" ~ body_mass
  ))


dat_df %>%
  group_by(dat_id, site) %>%
  summarise(n = n(),
            max_size = max(body_mass),
            min_size = min(body_mass)) %>%
  filter(n > 100,
         size_range >= 3)

# test the count LL vs plb LL vs PLBbin
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

# tadnoll
tad_test <- dat_df %>%
  filter(site == "Tadnoll Brook") %>%
  select(body_mass, count, multiplier) %>%
  mutate(ind_n = count * multiplier) 

tad_test %>% group_by(body_mass) %>%
  summarize(n())

calcLike(negLL.fn = negLL.PLB.counts,
         x = tad_test$body_mass,
         c = tad_test$ind_n,
         p = -1.5)
calcLike(negLL.fn = negLL.PLB,
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

calcLike(negLL.fn = negLL.PLB.binned,
                      p = -1.5,
                      w = binBreaks,
                      d = binCounts,
                      J = length(binCounts),   # = num.bins
                      vecDiff = 1)

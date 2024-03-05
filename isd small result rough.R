library(tidybayes)
library(tidyverse)
library(brms)

# which data is this?
isd <- readRDS("results/fit_full_8c500i_2024-02-14.rds")

# check model
plot(isd)

# pp_check as a "GOF" ?
# geometric mean? 
# bayesian p value, rank plot, farter away means worse
# predictions are all based on power law
# measure deviation from that prediction/model 

# neon_slim
# code 3 3.1 pvalues

# fitting speed, resampling whole data set
# 
isd_new = isd$data %>% # list of sites
  sample_n(size = 100000, replace = T, weight = ind_n)

pp_check(isd, newdata = isd_new) +
  scale_x_log10()


pp_check(isd, type = "boxplot",  newdata = isd_new) +
  scale_y_log10()
# faster?, all 1.7 million points
# # from isdbayes vignette
# pp_check(fit2, type = "dens_overlay_grouped", group = "group") +
#   scale_x_log10()

# isd$data repeats
# "bin" count the data? tally up the data

posts_varint = isd$data %>% 
  distinct(site, xmin, xmax) %>% 
  mutate(ind_n = 1) %>% 
  tidybayes::add_epred_draws(isd, re_formula = NULL) 

posts_varint %>% 
  ggplot(aes(x = site, y = .epred)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval()

posts_varint %>% 
  ggplot(aes(x = .epred)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval()



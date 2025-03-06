library(tidybayes)
library(tidyverse)

isd <- readRDS("results/fit3_4c_2000i_2024-02-07.rds")

# check model
plot(isd)
pp_check(isd)
# # from isdbayes vignette
# pp_check(fit2, type = "dens_overlay_grouped", group = "group") +
#   scale_x_log10()

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



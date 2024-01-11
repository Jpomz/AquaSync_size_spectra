# isd bayes fitting

library(tidyverse)
library(brms)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_jan-11.RDS")

# dat %>%
#   select(dat_id, site) %>%
#   unique() %>% View

# filtered_vector <- c(
#   "size_spectra_bio_BA.xlsx",
#   "size_spectra_bio_MG.xlsx",
#   "size_spectra_bio_PA.xlsx",
#   "size_spectra_bio_SP.xlsx",
#   "df_O_Gorman_1.xlsx",
#   "df_Perkins.xlsx" ,
#   "df_Pomeranz.xlsx"
# )
# 
# test_dat <- dat %>%
#   filter(dat_id %in% filtered_vector)
# 
# test_dat %>% pull(site) %>% unique() %>% sample(5)
# 
# test_dat <- test_dat %>%
#   filter(site %in% c("Upper Llugwy",
#                      "P21",
#                      "River Lyde",
#                      "Hengill IS7 B",
#                      "Narrator Brook")) %>%
#   group_by(dat_id, site) %>% 
#   mutate(xmin = min(body_mass),
#          xmax = max(body_mass)) 

# isdbayes ###

# priors ####
# get priors
get_prior(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
          data = test_dat,
          stanvars = stanvars,
          family = paretocounts())
# set priors
bprior <- c(prior(normal(-1.3,0.4), class = Intercept),
            prior(exponential(2), class = sd))

# plot(density(rnorm(1000, -1.3, 0.4)))
# plot(density(rexp(1000, 2)))
# move mouse app

# fit model ####
test_dat <- dat
fit1 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
           data = test_dat,
           stanvars = stanvars,
           prior = bprior,
           family = paretocounts(),
           chains = 1, iter = 10) # real values 4, 2000
# update()

fit1 = update(fit1, iter = 1000)

posts_varint = fit1$data %>% 
  distinct(site, xmin, xmax) %>% 
  mutate(ind_n = 1) %>% 
  tidybayes::add_epred_draws(fit1, re_formula = NULL) 

posts_varint %>% 
  ggplot(aes(x = site, y = .epred)) + 
  tidybayes::stat_halfeye(scale = 0.2) 

# times 17,000 rows ~ 9 hours on the cluster
# with predictors
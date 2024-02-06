# isd bayes fitting

devtools::install_github("jswesner/isdbayes", upgrade = FALSE)
library(tidyverse)
library(brms)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_jan-11.RDS")

# for full data
# test_dat <- dat %>%
#   group_by(dat_id, site) %>%
#   mutate(xmin = min(body_mass),
#          xmax = max(body_mass))

# # dat %>%
# #   select(dat_id, site) %>%
# #   unique() %>% View
# 

# 
filtered_vector <- c(
  "size_spectra_bio_BA.xlsx",
  "size_spectra_bio_MG.xlsx",
  "size_spectra_bio_PA.xlsx",
  "size_spectra_bio_SP.xlsx",
  "df_O_Gorman_1.xlsx",
  "df_Perkins.xlsx" ,
  "df_Pomeranz.xlsx",
  "Croatia_length and mass_data_permanent_IdG.xlsx",
  "Czech_length and mass_data_permanent_IdG.xlsx",
  "df.template - Lento Culp data_revised.xlsx",
  "df.template - Lento Morin data_revised.xlsx",
  "df.template_anbiotek_VG_BasqueCountry.xlsx",
  "df.template_EKOLUR_AL_VG_Gipuzkoa.xlsx",
  "df.template_RO_final.xlsx",
  "df_AL-Martinezetal2016.xlsx",
  "df_AL_IFdL_IdG-RiMSEC.xlsx",
  "df_Arranz_01.xlsx"
)
 
test_dat <- dat %>%
  filter(dat_id %in% filtered_vector) %>%
  group_by(dat_id, site) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))

#test_dat %>% pull(site) %>% unique() %>% sample(5)

# test_dat <- test_dat %>%
#   filter(site %in% c("Upper Llugwy",
#                      "P21",
#                      "River Lyde",
#                      "Hengill IS7 B",
#                      "Narrator Brook")) %>%
#   group_by(dat_id, site) %>%
#   mutate(xmin = min(body_mass),
#          xmax = max(body_mass))


dim(test_dat) / dim(dat)
# 26% of data

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
# test_dat <- dat%>%
#      mutate(xmin = min(body_mass),
#             xmax = max(body_mass))
  
run_4_10_start <- Sys.time()
fit_4_10 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
           data = test_dat,
           stanvars = stanvars,
           prior = bprior,
           family = paretocounts(),
           chains = 4,
           cores = 4,
           iter = 10) # real values 4, 2000
# more iterations
run_4_10_end <- Sys.time()
run_4_10_time <- run_4_10_end - run_4_10_start
saveRDS(run_4_10_time, file = paste0("results/run_4_10_time_", Sys.Date(), ".rds"))

run_4_2000_start <- Sys.time()

fit_4_2000 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
                 data = test_dat,
                 stanvars = stanvars,
                 prior = bprior,
                 family = paretocounts(),
                 chains = 4,
                 cores = 4,
                 iter = 2000)

run_4_2000_end <- Sys.time()

saveRDS(fit_4_2000, paste0("results/fit_4_2000_", Sys.Date(), ".rds"))

run_4_2000_time <- run_4_2000_end - run_4_2000_start
saveRDS(run_4_2000_time, file = paste0("results/run_4_2000_time_", Sys.Date(), ".rds"))

paste0("results/fit_", iter, "_", Sys.Date(), ".RDS")

posts_varint = fit_4_10$data %>% 
  distinct(site, xmin, xmax) %>% 
  mutate(ind_n = 1) %>% 
  tidybayes::add_epred_draws(fit_4_10, re_formula = NULL) 

posts_varint %>% 
  ggplot(aes(x = site, y = .epred)) + 
  #tidybayes::stat_halfeye(scale = 0.2) 
  tidybayes::stat_pointinterval()

# times 17,000 rows ~ 9 hours on the cluster
# with predictors
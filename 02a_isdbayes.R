# isd bayes fitting
# script 02 a
# just the Neon, Perkins, O'Gorman and Brazil data sets

# start with 4 chains, 10 iterations and save the results
# fit again with 500 iterations and save, then 2000

devtools::install_github("jswesner/isdbayes", upgrade = FALSE)

library(tidyverse)
library(brms)
library(isdbayes)

# what is the newest filtered data?
dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")

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
#   "df_Pomeranz.xlsx",
#   "df_NEON.xlsx"
# )

test_dat <- dat %>%
  #filter(dat_id %in% filtered_vector) %>%
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
  # group_by(dat_id, site) %>%
  # mutate(xmin = min(body_mass),
  #        xmax = max(body_mass))

# isdbayes ###

# priors ####
# get priors
# get_prior(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site),
#           data = test_dat,
#           stanvars = stanvars,
#           family = paretocounts())
# set priors
exp_prior = 12
bprior <- c(prior(normal(-1.3,0.4), class = Intercept),
            prior(exponential(12), class = sd)) #exponential(2, 8, 10)?

# plot(density(rnorm(1000, -1.3, 0.4)))
# plot(density(rexp(1000, 8)))
# move mouse app

# fit model ####
# test_dat <- dat

# full data 8 chains, 100 iter
iter = 1#100
chain_core = 8
fit1_start <- Sys.time()

# wtf is body mass?
# sum up the grouped body size, not tally/count

# group_split()
# brm_multiple
# isd_data_list = isd_data %>% group_by(sample_id) %>% group_split()
# 
# dummy_mod = brm_multiple(x|asdasdf ~ 1,
#                    data = isd_data_list)
# for{update()}

# #
# sim_data_list = sim_data %>% group_by(group) %>% group_split()
# 
# dummy_mod = brm(x | vreal(counts, xmin, xmax) ~ 1,
#                 data = sim_data_list[[1]],
#                 stanvars = stanvars,
#                 family = paretocounts(),
#                 chains = 1, iter = 20,
#                 cores = 4)
# 
# 
# update_fits = NULL
# 
# for(i in length(sim_data_list)){
#   update_fits[[i]] = update(dummy_mod, newdata = sim_data_list, iter = 2000, chains = 4, cores = 4)
# }
# 


# sim_data_list = sim_data %>% group_by(group) %>% group_split()
# 
# # fit and save separate intercept only models to each lambda
# brm_single_mods = brm_multiple(x | vreal(counts, xmin, xmax) ~ 1,
#                                data = sim_data_list,
#                                stanvars = stanvars,
#                                family = paretocounts(),
#                                chains = 4, iter = 2000,
#                                cores = 4,
#                                combine = F)



fit_full_8c_100i <- brm(body_mass | vreal(ind_n, xmin, xmax) ~ group_id + (1|dat_id), # just group_id as fixed or include as random?
                          #(1|site), 
                        data = test_dat,
                        stanvars = stanvars,
                        prior = bprior,
                        family = paretocounts(),
                        chains = chain_core,
                        cores = chain_core,
                        iter = iter)

fit1_end <- Sys.time()
fit1_run <- fit1_end - fit1_start

saveRDS(fit1_run, 
        paste0("results/run_fit_full_", chain_core, "c",
               iter, "i_", exp_prior, "exp_", Sys.Date(), ".rds"))
saveRDS(fit_full_8c_100i,
        paste0("results/fit_full_", chain_core, "c",
               iter, "i_", Sys.Date(), ".rds"))



# #fit1 = 4 chains, 10 iter
# 
# fit1_start <- Sys.time()
# iter = 10
# fit1 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
#            data = test_dat,
#            stanvars = stanvars,
#            prior = bprior,
#            family = paretocounts(),
#            chains = 4,
#            cores = 4,
#            iter = iter)
# 
# fit1_end <- Sys.time()
# fit1_run <- fit1_end - fit1_start
# 
# saveRDS(fit1_run, paste0("results/fit1_run_", Sys.Date(), ".rds"))
# saveRDS(fit1, paste0("results/fit1_4c_", iter, "i_", Sys.Date(), ".rds"))

# # 500 iter
# iter = 500
# fit2_start <- Sys.time()
# fit2 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
#            data = test_dat,
#            stanvars = stanvars,
#            prior = bprior,
#            family = paretocounts(),
#            chains = 4,
#            cores = 4,
#            iter = iter)
# 
# fit2_end <- Sys.time()
# fit2_run <- fit2_end - fit2_start
# saveRDS(fit2_run, paste0("results/fit2_run_", Sys.Date(), ".rds"))
# saveRDS(fit2, paste0("results/fit2_4c_", iter, "i_", Sys.Date(),".rds"))
# 
# 
# # 500 iter
# iter = 2000
# fit3_start <- Sys.time()
# fit3 = brm(body_mass | vreal(ind_n, xmin, xmax) ~ (1|site), 
#            data = test_dat,
#            stanvars = stanvars,
#            prior = bprior,
#            family = paretocounts(),
#            chains = 4,
#            cores = 4,
#            iter = iter)
# 
# fit3_end <- Sys.time()
# fit3_run <- fit3_end - fit3_start
# saveRDS(fit3_run, paste0("results/fit3_run_", Sys.Date(), ".rds"))
# saveRDS(fit3, paste0("results/fit3_4c_", iter, "i_", Sys.Date(),".rds"))
# 


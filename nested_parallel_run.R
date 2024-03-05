# script for cmdstan r
# we recommend running this is a fresh R session or restarting your current session
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# 
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# cmdstanr::install_cmdstan()

library(tidyverse)
library(brms)
library(cmdstanr)
library(future)
library(furrr)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")
dat <- dat %>%
  group_by(group_id) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))

plan(tweak(multisession, workers = 3))

bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

model <- make_stancode(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,  
  data = dat %>%
    filter(group_id == 2290) %>%
    select(body_mass, ind_n, xmin, xmax),
  stanvars = stanvars,
  prior = bprior,
  family = paretocounts(),
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
) %>% 
  write_stan_file() %>%
  cmdstan_model()

tictoc::tic()
isd_models <- dat %>%
  #filter(group_id < 2295) %>%
  select(body_mass, ind_n, xmin, xmax) %>%
  nest(mod_dat = c(body_mass, ind_n, xmin, xmax)) %>%
  ungroup() %>%
  mutate(
    stan_data = map(
      mod_dat,
      ~ make_standata(
        body_mass | vreal(ind_n, xmin, xmax) ~ 1,  
        data = .x,
        family = paretocounts(),
        prior = bprior)
    ),
    isd_mod = 
      future_map(
        .x = stan_data,
        .options = furrr_options(
          scheduling = 1,
          seed = TRUE,
          prefix = "prefix"
        ),
        ~ model$sample(
          data = .x,
          iter_sampling = 500,
          iter_warmup = 500,
          chains = 4, 
          parallel_chains = 4
        )
      ))
run_time <- tictoc::toc()$callback_msg

saveRDS(run_time, "results/time_parallel_run_2024-Feb.rds")
saveRDS(isd_models,
        "results/parallel_run_2024-Feb.rds")
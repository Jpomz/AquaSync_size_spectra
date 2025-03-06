# script for parallelizing 
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

neon <- dat %>% filter(dat_id == "df_NEON.xlsx")
neon %>% pull(group_id) %>% unique() %>% length()
# 272 groups

availableCores()
# 16

plan(tweak(multisession, workers = 4))

get_prior(body_mass | vreal(ind_n, xmin, xmax) ~ 1,
          data = dat,
          stanvars = stanvars,
          family = paretocounts())

bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

model <- make_stancode(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,  
  data = neon %>%
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
neon %>%
  filter(group_id < 2295) %>%
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
          iter_sampling = 1000,
          iter_warmup = 500,
          chains = 4, 
          parallel_chains = 4
        )
      ))
tictoc::toc()

# 5 groups 500warm 1000iter 3 workers, parallel = 4 = 13 seconds
# workers = 10, chains = 4, no parallel chains = 22.64
# workers = 4, parallel = 4 = 14 seconds

# 4 workers, 4 parallel chains

# 5 groups 500warm 500iter 3 workers, parallel = 4 = 10.58 seconds


# 272 groups 500warm 1000iter # workers = 3, parallel_chains = 4 = 737 sec = 10 minutes 


# does isd_mod have the "data" in it? add ???
# tidybayes::add_epred_draws(isd, re_formula = NULL) ?

isd_small <- neon %>%
  filter(group_id < 2292) %>%
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
          iter_sampling = 1000,
          iter_warmup = 500,
          chains = 4,
          parallel_chains = 4
        )
      ))


mod1 <- isd_small$isd_mod[[1]]

stanfit <- rstan::read_stan_csv(mod1$output_files())
class(stanfit)
class(mod1)
pp_check(stanfit, fun = )

bayesplot::mcmc_hist(mod1$draws("b_Intercept"))
posterior::as_draws_df(mod1)

isd_small %>%
  mutate(
    draws =
      map(
        .x = isd_mod,
        ~ posterior::as_draws_df(.x))) %>%
  unnest(c(stan_data,draws)) %>%
  select(group_id, b_Intercept) %>%
  ggplot(aes(x = group_id,
             y = b_Intercept)) +
  tidybayes::stat_pointinterval() +
  facet_wrap(~group_id,
             scales = "free")


isd_small %>%
  mutate(
    estimates =
      map(
        .x = isd_mod,
        ~ posterior::summarise_draws(
          .x,
          mean,
          sd,
          q2.5 = ~ quantile(.x, probs = c(0.025)),
          q97.5 = ~ quantile(.x, probs = c(0.975))
        ) |>
          filter(variable == "Intercept")
      ),
    est = map_dbl(
      .x = estimates,
      ~ .x |>
        select(mean) |>
        pull()
    ),
    std_error = map_dbl(
      .x = estimates,
      ~ .x |>
        select(sd) |>
        pull()
    ),
    lower = map_dbl(
      .x = estimates,
      ~ .x |>
        select(`2.5%`) |>
        pull()
    ),
    upper = map_dbl(
      .x = estimates,
      ~ .x |>
        select(`97.5%`) |>
        pull()
    )
  ) %>%
  select(group_id, est:upper) %>%
  ggplot(aes(x = group_id,
             y = est, 
             ymin = lower, 
             ymax = upper)) +
  geom_pointrange()

isd_small
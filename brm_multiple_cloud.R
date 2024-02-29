# brms multiple and future
# script for parallelizing 

devtools::install_github("jswesner/isdbayes", upgrade = FALSE)

library(tidyverse)
library(brms)
#library(cmdstanr)
library(future)
#library(furrr)
library(isdbayes)

dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")

dat <- dat %>%
  group_by(group_id) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))

workers = availableCores() - 1


plan(tweak(multisession, workers = workers))

bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

dat_list <- dat %>%
  group_split()

group_ids <- lapply(dat_list, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)

tictoc::tic()
brm_single_mods = brm_multiple(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = neon_list,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4, 
  iter = 2000,
  future = TRUE,
  combine = FALSE)
run_time <- tictoc::toc()$callback_msg

saveRDS(run_time, "results/time_brm_multiple_2024-Feb.rds")
saveRDS(brm_single_mods,
        "results/brm_multiple_2024-Feb.rds")
saveRDS(group_ids,
        "results/brm_mult_group_ids.rds")
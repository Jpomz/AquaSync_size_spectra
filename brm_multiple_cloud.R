# brms multiple and future
# script for parallelizing 

#devtools::install_github("jswesner/isdbayes", upgrade = FALSE)

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

#workers = availableCores() - 1


plan(multisession)

bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

dat_list <- dat %>%
  group_split()

group_ids <- lapply(
  dat_list,
  function(x) x[1,"group_id"]) %>%
  map_chr(as.character)

tictoc::tic()
brm_single_mods = brm_multiple(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = dat_list,
  stanvars = stanvars,
  family = paretocounts(),
  chains = 4, 
  iter = 2000,
  future = TRUE,
  combine = FALSE)
run_time <- tictoc::toc()$callback_msg

saveRDS(run_time, paste0("results/time_brm_multiple_", Sys.Date(), ".rds"))
saveRDS(brm_single_mods,
        paste0("results/brm_multiple_", Sys.Date(), ".rds"))
saveRDS(group_ids,
        paste0("results/group_ids_brm_multiple_", Sys.Date(), ".rds"))
# list of lists for brm_multiple
# libraries ####

devtools::install_github("jswesner/isdbayes", upgrade = FALSE)

library(tidyverse)
library(brms)
library(future)
library(isdbayes)
library(foreach)

update_iter = 2000
workers1 = (parallel::detectCores() - 1)
workers2 = 4

# full data ####
dat <- readRDS("derived_data/filtered_size_2024-02-07.RDS")
dat <- dat %>%
  group_by(group_id) %>%
  mutate(xmin = min(body_mass),
         xmax = max(body_mass))
#dat$group_id %>% range()
#14177 / 4 # ~3545
# < 3545
# > 3545 < 7090
# > 7090 < 10635
# > 10635 < 14177

# dat splits and lists ####
dat0 <- dat %>% filter(group_id <= 5)
# dat0.1 <- dat %>% filter(group_id > 5, group_id <=10 )

dat1 <- dat %>% filter(group_id <= 3545)
dat2 <- dat %>% 
  filter(group_id > 3545, group_id <= 7090)
dat3 <- dat %>%
  filter(group_id > 7090, group_id <= 10635)
dat4 <- dat %>%
  filter(group_id > 10635, group_id <= 14177)

# nrow(dat) - (nrow(dat0) + nrow(dat1) +
#                nrow(dat2) +
#                nrow(dat3) +
#                nrow(dat4) )

list0 <- dat0 %>% group_split()
# list0.1 <- dat0.1 %>% group_split()
list1 <- dat1 %>%
  group_split()
list2 <- dat2 %>%
  group_split()
list3 <- dat3 %>%
  group_split()
list4 <- dat4 %>%
  group_split()

# rm(dats)
rm(dat)
rm(dat0) 
rm(dat1)
rm(dat2)
rm(dat3)
rm(dat4)

# group ids ####
group0_ids <- lapply(list0, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)

group1_ids <- lapply(list1, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)
group2_ids <- lapply(list2, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)
group3_ids <- lapply(list3, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)
group4_ids <- lapply(list4, function(x) x[1,"group_id"]) %>%
  map_chr(as.character)

# save group ids and rm()
saveRDS(group1_ids,
        paste0("results/group_ids_brm1_", Sys.Date(), ".rds"))
saveRDS(group2_ids,
        paste0("results/group_ids_brm2_", Sys.Date(), ".rds"))
saveRDS(group3_ids,
        paste0("results/group_ids_brm3_", Sys.Date(), ".rds"))
saveRDS(group4_ids,
        paste0("results/group_ids_brm4_", Sys.Date(), ".rds"))

rm(group1_ids)
rm(group2_ids)
rm(group3_ids)
rm(group4_ids)


# plan ####

plan(multisession, workers = 5) # full data, 1c 10 i = 9.318 minutes

# plan(list(
#   tweak(multisession, workers = 3),
#   tweak(multisession, workers = 4)
# )) ## full data, 1c 10 i =
# # keep getting errors 'attempting to set up 4 local hosts with only 1 core available

# prior ####
bprior <- c(prior(normal(-1.3,0.4), class = Intercept))

# dummy model, plan(multisession, workers = 4), just one data set
# don't need to make data0 or 0.1?


# dummy model; group 0 ####
chains = 4
iter = 10 
run_full_start <- run0_start <- Sys.time()

brm0 = brm_multiple(
  body_mass | vreal(ind_n, xmin, xmax) ~ 1,
  data = list0,
  stanvars = stanvars,
  family = paretocounts(),
  chains = chains, # add cores argument?
  iter = iter,
  future = TRUE,
  combine = FALSE)
run0_end <- Sys.time()
run0 <- run0_end - run0_start

rm(list0)
rm(run0)
rm(run0_end)
rm(run0_start)


# Group 1 ####
plan(list(
  tweak(multisession, workers = workers1),
  tweak(multisession, workers = workers2)
))

run1_start <- Sys.time()

brm1 <- foreach(
  i = 1:length(list1)) %dopar% {
  update(object = brm0[[1]],
         newdata = list1[[i]],
         iter = update_iter)}

run1_end <- Sys.time()
(run1 <- run1_end - run1_start)

# save run1 ####
saveRDS(run1, paste0("results/time_brm1_", Sys.Date(), ".rds"))
saveRDS(brm1,
        paste0("results/brm1_", Sys.Date(), ".rds"))

rm(run1)
rm(brm1)
rm(run1_end)
rm(run1_start)
rm(list1)

# group 2 #####
# list 2 = 778 elements ~1.5 times as big, timing?
# set nested plan to run 3 datasets at a time, parallel process 4 chains simultaneoulsy
plan(list(
  tweak(multisession, workers = workers1),
  tweak(multisession, workers = workers2)
))
# with nested plan above = 
# is it ~1.5 times as long as run 1?

run2_start <- Sys.time()

brm2 <- foreach(i = 1:length(list2)) %dopar% {
  update(object = brm0[[1]],
         newdata = list2[[i]],
         iter = update_iter)}

run2_end <- Sys.time()
(run2 <- run2_end - run2_start)

saveRDS(run2, paste0("results/time_brm2_", Sys.Date(), ".rds"))
saveRDS(brm2,
        paste0("results/brm2_", Sys.Date(), ".rds"))

rm(run2)
rm(brm2)
rm(run2_end)
rm(run2_start)
rm(list2)

# group 3 #####
plan(list(
  tweak(multisession, workers = workers1),
  tweak(multisession, workers = workers2)
))

run3_start <- Sys.time()

brm3 <- foreach(i = 1:length(list3)) %dopar% {
  update(object = brm0[[1]],
         newdata = list3[[i]],
         iter = update_iter)}

run3_end <- Sys.time()
(run3 <- run3_end - run3_start)

saveRDS(run3, paste0("results/time_brm3_", Sys.Date(), ".rds"))
saveRDS(brm3,
        paste0("results/brm3_", Sys.Date(), ".rds"))

rm(run3)
rm(brm3)
rm(run3_end)
rm(run3_start)
rm(list3)

# group 4 #####
plan(list(
  tweak(multisession, workers = workers1),
  tweak(multisession, workers = workers2)
))

run4_start <- Sys.time()

brm4 <- foreach(i = 1:length(list4)) %dopar% {
  update(object = brm0[[1]],
         newdata = list4[[i]],
         iter = update_iter)}

run4_end <- run_full_end <- Sys.time()

(run4  <- run4_end - run4_start)

full_run <- run_full_end - run_full_start

saveRDS(run4, paste0("results/time_brm4_", Sys.Date(), ".rds"))

saveRDS(brm4,
        paste0("results/brm4_", Sys.Date(), ".rds"))


saveRDS(full_run, paste0("results/full_time_", Sys.Date(), ".rds"))
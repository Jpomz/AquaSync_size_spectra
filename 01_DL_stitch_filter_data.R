# load data
library(tidyverse)
library(sizeSpectra)
library(readxl)

name_target <- c("site",
                 "sampling_method",
                 "sample",
                 "sampling_area",
                 "organism_group",
                 "taxon", 
                 "body_mass",
                 "body_length",
                 "body_weight_units",
                 "body_length_units",
                 "count",
                 "multiplier")

file_paths <- list.files("data/", 
                         pattern = "*.xlsx")

data_list <- list()
list_to_fix_names <- list()

for(i in 1:length(file_paths)){
  in_names <- names(
    read_excel(
      path = paste0("data/",
                    path = file_paths[i])))
  if(identical(name_target, in_names)){
    dat_in <- read_excel(
      path = paste0("data/",
                    file_paths[i]),
      col_types = c("text", "text", "numeric", 
                    "numeric", "text", "text", 
                    "numeric", "numeric", 
                    "text", "text", "numeric",
                    "numeric"))
  } else {
    {
      list_to_fix_names[[i]] <- file_paths[i]
      data_list[[i]] <- "Need to fix data"
      next
    }
  }
  data_list[[i]] <- dat_in
}

list_to_fix_names

names(data_list) <- file_paths
dat_df <- bind_rows(data_list, .id = "id")
dat_df <- dat_df %>%
  rename(dat_id = id)

# remove NA body_weight_units 
dat_df <- dat_df %>%
  filter(!is.na(body_weight_units),
         !is.na(body_mass)) %>%
  filter(body_mass>0)

# body_weight_units ####
# convert everything to mg
# what are the body weight units?
dat_df$body_weight_units %>% unique()

# need to add code to update body_weight_units
dat_df <- dat_df %>%
  mutate(body_mass = case_when(
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "g WW" ~ body_mass *1000 *0.25,
    body_weight_units == "mg DW" ~ body_mass,
    body_weight_units == "mg dry mass" ~ body_mass,
    #body_weight_units == "mg wet weight" ~ body_mass,
    # convert wet weight to dry weight
    # Using an arbitrary value of 0.25 for now
    body_weight_units == "mg WW" ~ body_mass * 0.25,
    body_weight_units == "M.mg" ~ body_mass,
    body_weight_units == "mg" ~ body_mass
  ))

# change weight units

# small body mass ####
# Filter out small body sizes??

# N and size range ####
# make a vector of sites to KEEP
filter_vector <- dat_df %>%
  group_by(dat_id, site) %>%
  summarise(n = n(), 
            max_size = log10(max(body_mass)),
            min_size = log10(min(body_mass))) %>%
  mutate(size_range = (max_size - min_size)) %>%
  filter(n > 100,
         size_range >= 3) %>%
  pull(site) %>%
  unique()

filter_vector

dat_out <- dat_df |> 
  filter(site %in% filter_vector) %>%
  group_by(dat_id, site) %>%
  mutate(ind_n = count * multiplier) %>%
  filter(!is.na(ind_n)) %>%
  mutate(group_id = cur_group_id())

# Latitude ####
#read in site data for lat long ####
site_list <- list()
for (i in 1:length(file_paths)){
  df_site <- read_excel(
    path = paste0("data/",
                  path = file_paths[i]), 
    sheet = "site_data")
  df_site <- df_site %>%
    select(site, geographical_latitude)
  site_list[[i]] <- df_site
}

lat_df <- bind_rows(site_list) %>%
  unique()

dat_lat <- left_join(dat_out,
                     lat_df)

# save data ####
saveRDS(dat_out, "derived_data/filtered_size_jan-11.RDS")

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
    dat_in$dat_id <- file_paths[i]
  } else {
    {
      list_to_fix_names[[i]] <- file_paths[i]
      data_list[[i]] <- "Need to fix data"
      next
    }
  }
  data_list[[i]] <- dat_in
}

# check files
list_to_fix_names

length(data_list)
length(file_paths)
#head(data_list[[1]])

#names(data_list) <- file_paths
dat_df <- bind_rows(data_list)
dat_df %>% select(dat_id, site) %>%
  unique() %>% dim()


# Latitude ####
# # read in site data for lat long ####
# # Arranz identical site names ??? ####
site_list <- list()
for (i in 1:length(file_paths)){
  df_site <- read_excel(
    path = paste0("data/",
                  path = file_paths[i]),
    sheet = "site_data")
  df_site <- df_site %>%
    # add organism_groups
    select(site, organism_groups, geographical_latitude) %>%
    mutate(site = as.character(site))
  df_site$dat_id <- file_paths[i]
  site_list[[i]] <- df_site
}

#site_list
site_df <- bind_rows(site_list)
# 12975 rows
site_df <- site_df %>%
  unique() # NEON data has one row per collection method. 
            # pulling out unique to cancel duplicates
# 12 736

dim(dat_df)
dat_df %>% select(dat_id, site) %>% unique() %>% dim()
dim(site_df)
setdiff(site_df$site, dat_df$site)
setdiff(dat_df$site, site_df$site)
# R00001-2005-09-27
# R00001 site_df --> add collection date?

site_df %>%
  filter(str_detect(site,
                    regex("^R0", ignore_case = TRUE))) %>%
  select(dat_id, site)
# df.template_anbiotek_VG_BasqueCountry.xlsx
site_df %>%
  filter(str_detect(site,
                    regex("^BUK"))) %>%
  select(dat_id, site)
# Hungary_length and mass_data_permanent_IdG.xlsx

site_df %>%
  filter(str_detect(site,
                    regex("^Gen"))) %>%
  select(dat_id, site)
# Spain_length and mass_data_permanent_IdG.xlsx

# other site issues that I dealt with manually ####
# site_df %>%
#   filter(site == "Allt a’ Mharcaidh") %>%
#   select(dat_id, site)
# # df_Perkins # fixed manually
# 
# site_df %>%
#   filter(site == "Cananeia_Saito S1") %>%
#   select(dat_id, site)
# # df_Victor_Saito_Cananeia # added _ manually
# 
# site_df %>%
#   filter(site == "Due11_40793" ) %>%
#   select(dat_id, site)
# # df_carlosUPM # capitalized manually 


# join dat and site info ####

dat_df <- left_join(dat_df, site_df,
          by = join_by(dat_id, site))#,
#          multiple = "any") # many-to-many relationship
                            # repeated site names, 
                            #i.e., Miranda de Ebro in 
                                  # df_Arranz_01.xlsx
                            # I thought I talked to Ignasi and this was fixed?? Do I have the wrong data files?

# row Row 2242905, ARIK_2017-0...

# which sites are doubled?
# dat_df %>%
#   select(dat_id, site, geographical_latitude) %>%
#   unique() %>%
#   group_by(dat_id, site) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   slice_head() %>%
#   View()


dat_df <- dat_df %>%
  group_by(dat_id, site) %>%
  mutate(group_id = cur_group_id()) 


# remove NA body_weight_units 
dat_df <- dat_df %>%
  filter(!is.na(body_weight_units),
         !is.na(body_mass)) %>%
  filter(body_mass>0,
         count > 0)

# body_weight_units ####
# convert everything to mg
# what are the body weight units?
dat_df$body_weight_units %>% unique() %>% sort()

# need to add code to update body_weight_units
dat_df <- dat_df %>%
  mutate(body_mass = case_when(
    # convert g to mg
    # convert wet weight to dry weight
    # Using an arbitrary value of 0.25 for now
    # assuming "dry_weight" is in mg
    body_weight_units == "dry_weight" ~ body_mass,
    body_weight_units == "g" ~ body_mass *1000,
    body_weight_units == "g WW" ~ body_mass *1000 *0.25,
    body_weight_units == "grams/wet" ~ body_mass *1000 *0.25,
    body_weight_units == "M.mg" ~ body_mass,
    body_weight_units == "mg" ~ body_mass,
    body_weight_units == "mg (dry_mass)" ~ body_mass,
    body_weight_units == "mg dry mass" ~ body_mass,
    body_weight_units == "mg DW" ~ body_mass,
    body_weight_units == "mg wet weight" ~ body_mass * 0.25,
    body_weight_units == "mg WW" ~ body_mass * 0.25,
    body_weight_units == "mg_DM" ~ body_mass,
    body_weight_units == "mg_dry_mass" ~ body_mass
  )) %>% 
  # Filter out small body sizes
  filter(body_mass > 0.0026)

# change weight units
dat_df <- dat_df %>%
  mutate(corrected_mass_units = "mg_dw")

# organism groups
dat_df %>%
  ungroup() %>%
  select(dat_id, site, organism_groups) %>%
  unique() %>%
  filter(organism_groups == "Invertebrates + Fish") %>%
  count()

dat_df %>%
  ungroup() %>%
  select(dat_id, site, organism_groups) %>%
  unique() %>%
  filter(organism_groups == "Invertebrates + Fish") %>%
  View()

# dat_df %>%
#   ungroup() %>%
#   filter(organism_groups == "Invertebrates + Fish") %>%
#   write_csv("derived_data/invert_fish_2024-02-20.csv")



# counts
dat_df %>% pull(count) %>% unique
dat_df %>%
  ungroup() %>%
  select(dat_id, count) %>%
  unique() %>%
  View()
# databases with non-integer counts
# df.template - Lento Morin data_revised.xlsx
# 

# multiple passes for fish??
# how to deal with that?

dat_df %>%
  ungroup() %>%
  mutate(ind_n = count * multiplier,
         n_size = case_when(
           ind_n < 1 ~1,
           ind_n >=1 ~ceiling(ind_n))) %>%
  select(group_id, count, multiplier, ind_n, n_size) %>%
  group_by(group_id) %>%
  summarize(sum_n_size = sum(n_size)) %>%
  filter(sum_n_size > 100)
# sites with sum_n_size > 100
# 6815 sites

dat_df %>%
  group_by(group_id) %>%
  summarise(n = n()) %>%
  filter(n > 100)
# sites with n > 100
# 6599 sites



dat_df %>%
  ungroup() %>%
  mutate(ind_n = count * multiplier,
         n_size = case_when(
           ind_n < 1 ~1,
           ind_n >=1 ~ceiling(ind_n))) %>%
  select(group_id, count, multiplier, ind_n, n_size, body_mass) %>%
  group_by(group_id) %>%
  summarize(sum_n_size = sum(n_size), 
            max_size = log10(max(body_mass)),
            min_size = log10(min(body_mass))) %>%
  mutate(size_range = (max_size - min_size)) %>%
  filter(sum_n_size > 100,
         size_range > 3)
# sum > 100 and range > 3
# 2602

dat_df %>%
  group_by(group_id) %>%
  summarise(n = n(), 
          max_size = log10(max(body_mass)),
          min_size = log10(min(body_mass))) %>%
  mutate(size_range = (max_size - min_size)) %>%
  filter(n > 100,
         size_range >= 3)
# 2563 
# small body mass ####

# distributions 
plot(density(na.omit(log10(dat_df$count))))
hist(na.omit((dat_df$count)))
plot(density(na.omit(log10(dat_df$body_mass))))

dat_df %>%
  filter(str_detect(organism_group,
                    regex("^Inv", ignore_case = TRUE))) %>%
  select(dat_id, site, body_mass, organism_group) %>%
  group_by(dat_id, site, organism_group) %>%
  na.omit() %>%
  summarise(min = min(body_mass),
            q05 = quantile(body_mass, probs = 0.05))%>%
  ggplot(aes(x = site, y = min, color = organism_group)) +
  geom_point() +
  #scale_y_log10() +
  NULL

dat_df %>% dim
dat_df  %>% dim()



### dealing with counts??
dat_df %>%
  mutate(n_size = case_when(
    count < 1 ~1,
    count >=1 ~ceiling(count)),
    ind_n = n_size * multiplier)

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

filter_vector2 <- dat_df %>%
  mutate(n_size = case_when(
    count < 1 ~1,
    count >=1 ~ceiling(count))) %>%
  group_by(group_id, site) %>%
  summarise(n = sum(count), 
            max_size = log10(max(body_mass)),
            min_size = log10(min(body_mass))) %>%
  mutate(size_range = (max_size - min_size)) %>%
  filter(n > 100,
         size_range >= 3) %>%
  pull(site) %>%
  unique()

length(filter_vector)
length(filter_vector2)

filter_vector

dat_out <- dat_df |> 
  filter(site %in% filter_vector) %>%
  group_by(dat_id, site) %>%
  mutate(ind_n = count * multiplier) %>%
  filter(!is.na(ind_n)) %>%
  mutate(group_id = cur_group_id())



# save data ####
saveRDS(dat_out, paste0("derived_data/filtered_size_", Sys.Date(), ".RDS"))

paste0("derived_data/filtered_size_", Sys.Date(), ".RDS")


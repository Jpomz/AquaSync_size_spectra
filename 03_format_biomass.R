# calculate total biomass per site_date

library(tidyverse)

dat <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")


head(dat)

dat %>% filter(site_date == "ARIK_2016-03-31")

sm_dat <-  dat %>% 
  select(site_date, group_id, 
         sample,
         organism_group, organism_groups,
         geographical_latitude,
         body_mass, ind_n)

m_coll <- sm_dat %>%
  mutate(mass_area = body_mass * ind_n) %>%
  group_by(group_id, sample,
           geographical_latitude,
           organism_groups,
           organism_group) %>%
  summarize(mass_sample = sum(mass_area)) %>%
  group_by(group_id, geographical_latitude,
           organism_groups,
           organism_group) %>%
  summarize(mean_mass_collection = sum(mass_sample) / 
              n_distinct(sample)) %>%
  ungroup()

dim(m_coll)
distinct(m_coll, organism_groups)
distinct(m_coll, organism_group)

# un correct fish to get back to m^2
m_coll <- m_coll %>%
  mutate(mean_mass_collection = 
           case_when(organism_groups == "Fish" ~ mean_mass_collection / 1000,
                     .default = mean_mass_collection)) 
dim(m_coll)

m_coll %>%
  ggplot(aes(x = abs(geographical_latitude),
             y = mean_mass_collection,
             color = organism_groups)) +
  geom_point() +
  #facet_wrap(~organism_groups) +
  scale_y_log10() +
  theme_bw() +
  labs(x = "Absolute Latitude",
       y = "Mean biomass")

ggsave("results/plots/biomass01.jpg",
       width = 6.5, height = 8.5,
       dpi = 500) 

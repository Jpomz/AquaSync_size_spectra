# waterfall plots

library(tidyverse)

dat <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")

dat %>%
  ggplot(aes(x = count,
         fill = organism_group)) +
  geom_density() +
  scale_x_log10()

dat %>% filter(count !=1) %>%
  distinct(dat_id)

mle <- readRDS("derived_data/format_mle_count_results.RDS")

# dat %>% 
#   filter(group_id == 13647) %>% pull(ind_n)
# # group_ids == 2182, 13456, 13645, 13646, 13647... More groups, skipping for now
# # all ind_n = NA?

# invertebrates + fish
dat %>% 
  filter(!is.na(ind_n),
         organism_groups == "Invertebrates + Fish") %>%
  group_by(group_id) %>%
  sample_n(size = 1000,
           weight = ind_n,
           replace = TRUE) %>%
  select(group_id, body_mass, geographical_latitude,
         organism_group, ind_n) %>%
  arrange(group_id, -body_mass) %>%
  mutate(order = row_number()) %>%
  ggplot(aes(x = body_mass,
             y = order,
             #size = body_mass,
             color = abs(geographical_latitude))) +
  scale_colour_distiller(palette = "RdBu",
                         direction = 1) +
  geom_point(shape = 1, alpha = 0.5) +
  #theme_dark() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Number of values \u2265 x",
       x = "Individual body mass",
       title = "Invertebrates + Fish",
       color = "Absolute Latitude") +
  guides(size = "none") +
  NULL

# 01 = default colors
# 02 = RdYlBu
# 03 = RdBu
# 04 = RdBu with theme_dark()
# 05 = RdBu with default theme (gray background)
ggsave("results/plots/waterfall_invert_fish.jpg",
       width = 6.5, height = 8.5,
       dpi = 500)

# invertebrates
dat %>% 
  filter(!is.na(ind_n),
         organism_groups == "Invertebrates") %>%
  group_by(group_id) %>%
  sample_n(size = 1000,
           weight = ind_n,
           replace = TRUE) %>%
  select(group_id, body_mass, geographical_latitude,
         organism_group, ind_n) %>%
  arrange(group_id, -body_mass) %>%
  mutate(order = row_number()) %>%
  ggplot(aes(x = body_mass,
             y = order,
             #size = body_mass,
             color = abs(geographical_latitude))) +
  scale_colour_distiller(palette = "RdBu",
                         direction = 1) +
  geom_point(shape = 1, alpha = 0.5) +
  #theme_dark() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Number of values \u2265 x",
       x = "Individual body mass",
       title = "Invertebrates",
       color = "Absolute Latitude") +
  guides(size = "none") +
  NULL

ggsave("results/plots/waterfall_invert.jpg",
       width = 6.5, height = 8.5,
       dpi = 500)



# fish
dat %>% 
  filter(!is.na(ind_n),
         organism_groups == "Fish") %>%
  group_by(group_id) %>%
  sample_n(size = 1000,
           weight = ind_n,
           replace = TRUE) %>%
  select(group_id, body_mass, geographical_latitude,
         organism_group, ind_n) %>%
  arrange(group_id, -body_mass) %>%
  mutate(order = row_number()) %>%
  ggplot(aes(x = body_mass,
             y = order,
             #size = body_mass,
             color = abs(geographical_latitude))) +
  scale_colour_distiller(palette = "RdBu",
                         direction = 1) +
  geom_point(shape = 1, alpha = 0.5) +
  #theme_dark() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Number of values \u2265 x",
       x = "Individual body mass",
       title = "Fish",
       color = "Absolute Latitude") +
  guides(size = "none") +
  NULL

ggsave("results/plots/waterfall_fish.jpg",
       width = 6.5, height = 8.5,
       dpi = 500)  

# # invertebrates
# dat %>% 
#   filter(!is.na(ind_n),
#          organism_groups == "Invertebrates") %>%
#   # filter(!group_id %in% c(2182,
#   #                         13456,
#   #                         13645,
#   #                         13646,
#   #                         13647) ) %>%
#   group_by(group_id) %>%
#   sample_n(size = 1000,
#            weight = ind_n,
#            replace = TRUE) %>%
#   select(group_id, body_mass, organism_group, ind_n) %>%
#   arrange(group_id, -body_mass) %>%
#   mutate(order = row_number()) %>%
#   ggplot(aes(x = body_mass,
#              y = order,
#              size = body_mass,
#              color = organism_group)) +
#   geom_point(shape = 1) + 
#   scale_x_log10() +
#   scale_y_log10() +
#   ggthemes::scale_color_colorblind() +
#   labs(y = "Number of values \u2265 x",
#        x = "Individual body mass",
#        color = "") +
#   guides(size = "none",
#          alpha = "none") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0),
#         legend.position = "top") +
#   facet_wrap(~group_id)


# #fish
# dat %>% 
#   filter(!is.na(ind_n),
#          organism_groups == "Fish") %>%
#   # filter(!group_id %in% c(2182,
#   #                         13456,
#   #                         13645,
#   #                         13646,
#   #                         13647) ) %>%
#   group_by(group_id) %>%
#   sample_n(size = 1000,
#            weight = ind_n,
#            replace = TRUE) %>%
#   select(group_id, body_mass, organism_group, ind_n) %>%
#   arrange(group_id, -body_mass) %>%
#   mutate(order = row_number()) %>%
#   ggplot(aes(x = body_mass,
#              y = order,
#              size = body_mass)) +
#   geom_point(shape = 1) + 
#   scale_x_log10() +
#   scale_y_log10() +
#   #ggthemes::scale_color_colorblind() +
#   labs(y = "Number of values \u2265 x",
#        x = "Individual body mass",
#        color = "") +
#   guides(size = "none",
#          alpha = "none") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0),
#         legend.position = "top") +
#   facet_wrap(~group_id)
# data subset for edwards
library(tidyverse)

dat <- readRDS("derived_data/formatted_files_stitched_filtered_April-2024.RDS")

neon_sm <- dat %>%
  filter(dat_id %in% c("df_NEON.xlsx"),
                  group_id < 13525)

other <- dat %>%
  filter(dat_id %in% c("df_Pomeranz.xlsx",
                     "df_Perkins.xlsx",
                     "df_Arranz_01.xlsx"))

other_full <- bind_rows(neon_sm, other)

other_vector <- other_full %>%
  group_by(dat_id) %>%
  distinct(group_id) %>%
  sample_n(2) %>%
  pull(group_id)


other_sm <- other %>%
  filter(group_id %in% other_vector)

write_csv(other_sm, "derived_data/example_for_andrew.csv")
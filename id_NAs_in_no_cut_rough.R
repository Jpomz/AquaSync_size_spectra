# looking into bin_tibble_no_cutoff_list issues

library(tidyverse)

bin_tibble_no_cutoff_list <- readRDS(
  "derived_data/list_bin_no_cutoff_tibbles.RDS")

bin_df <- bind_rows(bin_tibble_no_cutoff_list)

out <- list()
for(i in 1:length(bin_tibble_no_cutoff_list)){
  indiv_id <- bin_tibble_no_cutoff_list[[i]]$indiv |>
    distinct(analysis_id, group_id) |>
    rename(indiv_analysis = analysis_id,
           indiv_group = group_id)
  bin_id <- bin_tibble_no_cutoff_list[[i]]$binned |>
    distinct(analysis_id, group_id)|>
    rename(bin_analysis = analysis_id,
           bin_group = group_id)
  bin_id$list_num <- i
  out[[i]] <- bind_cols(indiv_id, bin_id)
}

out_df <- bind_rows(out)

out_df |>
  filter(is.na(indiv_analysis)|
           is.na(indiv_group) |
           is.na(bin_analysis) |
           is.na(bin_group) )

out_df |>
  select(indiv_analysis, bin_analysis) |>
  rowwise() |>
  mutate(check = n_distinct(c_across(indiv_analysis:bin_analysis))==1) |>
  filter(check == FALSE)

         
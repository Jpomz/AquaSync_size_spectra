# download all data files and put them in a folder
# in this example, they are in a "data/" folder in your working directory

library(readxl)
library(tidyverse)

# get all the file names
file_paths <- list.files("data/", 
                         pattern = "*.xlsx")

# make an empty list
site_list <- list()

# for loop to read in all info
# !!! make sure that each excel workbook has a sheet named "site_data" !!!
# Will NOT work if sheets are named something different

## For example
## "df_Martel_et_al_2007.xlsx" has a different sheet name, so the for loop stops there
## Need to fix this file on the shared drive, then re download and start over...

for (i in 1:length(file_paths)){
  df_site <- read_excel(
    path = paste0("data/",
                  path = file_paths[i]), 
    sheet = "site_data")
  df_site <- df_site %>%
    select(site, geographical_latitude)
  site_list[[i]] <- df_site
}

# name each item in list according to filename
names(site_list) <- file_paths

# combine all list elements into one data frame
# using the .id = "id" argument
dat_df <- bind_rows(site_list, .id = "id")

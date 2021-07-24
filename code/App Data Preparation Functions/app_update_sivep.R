
# Library -----------------------------------------------------------------

library(tidyverse)



# functions ---------------------------------------------------------------
source("code/Auxiliary Functions/download_sivep.R")


# release_date <- as.Date(Sys.time())

# data update procedure ---------------------------------------------------
release_date <- "2021-07-19"


## Download SIVEP CSVs
df_sivep <- download_sivep(date = release_date,
                           save_file = FALSE, return_df = TRUE)


## Run SIVEP data preparation
source(here::here("code/App Data Preparation Functions/run_app_data_preparation_sivep_covid.R"))

df_sivep_covid_adults_prep <- run_app_data_preparation_sivep_covid(df_sivep)


## Obtain volume data
source(here::here("code/App Data Preparation Functions/run_app_prop_volume_data.R"))
run_app_prop_volume_data(df_sivep_covid_adults_prep)


## Obtain IHM data


## Obtain descriptive table


## Obtain Google mobility report data



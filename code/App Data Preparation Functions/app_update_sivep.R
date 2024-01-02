
# Library -----------------------------------------------------------------




run_sivep_app_update <- function(release_date) {

    # functions ---------------------------------------------------------------
    source("code/Auxiliary Functions/download_sivep.R")
    
    
    # release_date <- as.Date(Sys.time())
    
    # data update procedure ---------------------------------------------------
    
    
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
    source(here::here("code/App Data Preparation Functions/run_app_figure_IHM.R"))
    run_app_figure_IHM(df_sivep_covid_adults_prep)
    
    
    ## Obtain descriptive table
    source(here::here("code/App Data Preparation Functions/run_app_table_descriptive.R"))
    run_app_table_descriptive(df_sivep_covid_adults_prep)
    
    
    ## Obtain Google mobility report data
    source(here::here("code/App Data Preparation Functions/run_app_figure_google_mob_report.R"))
    run_app_figure_google_mob_report()
    
    
    
    ## Updating last release date
    df_last_updates <- suppressMessages(readr::read_csv(here::here("input/app_data/last_date_sivep.csv")))
    
    
    if (release_date %in% df_last_updates$release_date) {
        return(NULL)
    } else {
        df_last_updates <- df_last_updates %>% 
            bind_rows(
                tibble(
                    release_date = as.Date(release_date),
                    nrows = nrow(df_sivep_covid_adults_prep)
                )
            )
        
        suppressMessages(readr::write_csv(df_last_updates, here::here("input/app_data/last_date_sivep.csv")))
        }

    }




# Main - Run functions ----------------------------------------------------

## Check if date is OK or last update
check_last_update <- function(release_date) {
    library(tidyverse)
    
    sivep_path <- "https://d26692udehoye.cloudfront.net/SRAG/"
    
    sivep_url <- paste0(sivep_path, "2023/INFLUD23-", stringr::str_sub(release_date, 9, 10),"-", 
                        stringr::str_sub(release_date, 6, 7), "-", 
                        stringr::str_sub(release_date, 1, 4), ".csv")
    
    
    if (!httr::http_error(httr::GET(sivep_url))) {
        release_date <- release_date
        
    } else {
        
        df_last_updates <- suppressMessages(readr::read_csv(here::here("input/app_data/last_date_sivep.csv")))
        
        last_update_date <- df_last_updates %>% arrange(release_date) %>% slice(n()) %>% pull(release_date)
        
        release_date <- last_update_date
    }
    
    print(release_date)
    
    return(release_date)

}



## Updating release date dataframe




## obtaining data
library(tidyverse)

## gets current date
current_date <- as.Date(Sys.time()) - 2 # data is from 2 days before the release date
# current_date <- as.Date("2022-01-26") # data is from 2 days before the release date
# current_date <- "2022-03-09"

## checks date
# release_date <- check_last_update(current_date)
release_date <- "2023-10-16"

## run update on new date of last update date
run_sivep_app_update(release_date)



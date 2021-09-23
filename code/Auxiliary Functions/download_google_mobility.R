### Simple function to download Google Mobility Data
## Currently, data covers years 2020 and 2021


## Requires packages: 'vroom', 'readr', 'stringr', 'here', 'purrr', 'data.table'

download_google_mobility <- function(country = "Brazil", 
                                     output_folder = here::here(),
                                     save_file = TRUE,
                                     return_df = FALSE) {

    
    output_file <- paste0(output_folder, "/google_mobility_raw_", gsub(" ", "_", country), ".csv.gz")
    
    if (file.exists(output_file)) {
        print("File already exists! Overwriting")
    } else{
        print("Creating a new file")
    }
    
    # URL for Downloading Google Mobility statistics CSV (All countries)
    g_mob_path <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"



    
    if (country == "All") {
        g_mob_raw <- 
            data.table::fread(g_mob_path, na.strings = c("", "NA"), encoding = "UTF-8") %>% 
            as_tibble()
        
        
    } else {
        ## Filter data to a specific country
        g_mob_raw <- 
            dplyr::filter(
                data.table::fread(g_mob_path, na.strings = c("", "NA"), encoding = "UTF-8") %>%
                    as_tibble()
                    , country_region == country
            )
        
    }

    
    if (save_file) {
        # Exports SIVEP in a CSV file
        data.table::fwrite(g_mob_raw, paste0(output_file))
        print(paste0("Finished! File created at: workdir/", output_folder))
    }
    
    
    if (return_df) {
        # Exports SIVEP in a CSV file
        return(g_mob_raw)
        
    } 

}


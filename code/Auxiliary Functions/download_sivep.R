### Simple function to download SIVEP-Gripe files from OpenDATASUS
## and outputs a compressed csv with 2020 + 2021

## Default date: April 05/04/2021 (last update)

## Requires packages: 'vroom', 'readr', 'stringr', 'here', 'purrr', 'data.table'

download_sivep <- function(date, output_folder = here::here(),
                           return_df = FALSE, save_file = TRUE) {
    library(tidyverse)
    
    
    # if (file.exists(output_file)) {
    #     print("File already exists! Overwriting")
    # } else{
    #     print("Creating a new file")
    # }

    # SIVEP URLs for 2020 and 2021 files
    # sivep_path <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/"
    sivep_path <- "https://d26692udehoye.cloudfront.net/SRAG/"

    ls_sivep_urls <- list(
        "SIVEP_2020" = paste0(sivep_path, "2020/INFLUD20-", stringr::str_sub(date, 9, 10),"-", stringr::str_sub(date, 6, 7), "-", stringr::str_sub(date, 1, 4), ".csv"),
        "SIVEP_2021" = paste0(sivep_path, "2021/INFLUD21-", stringr::str_sub(date, 9, 10),"-", stringr::str_sub(date, 6, 7), "-", stringr::str_sub(date, 1, 4), ".csv"),
        "SIVEP_2022" = paste0(sivep_path, "2022/INFLUD22-", stringr::str_sub(date, 9, 10),"-", stringr::str_sub(date, 6, 7), "-", stringr::str_sub(date, 1, 4), ".csv")
        )

    output_file <- paste0(output_folder, "/sivep_raw_", date, ".csv.gz")
    
    # Downloads SIVEP 2020-2021    
    sivep_raw <- purrr::map_df(
        ls_sivep_urls,
        function(df) {
            
            data.table::fread(df, na.strings = c("", "NA", NULL), ) %>% 
                mutate(
                    FATOR_RISC = as.character(FATOR_RISC)
                )
            }
        ) 
    
    
    
    if (save_file) {
        # Exports SIVEP in a CSV file
        data.table::fwrite(sivep_raw, paste0(output_file))
        print(paste0("Finished! File created at: workdir/", output_folder))
        
    }
    
    if (return_df) {
        # Returns sivep data as a DF
        return(tibble(sivep_raw))
        
    } 
    


}


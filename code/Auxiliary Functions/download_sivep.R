### Simple function to download SIVEP-Gripe files from OpenDATASUS
## and outputs a compressed csv with 2020 + 2021

## Default date: April 05/04/2021 (last update)

## Requires packages: 'vroom', 'readr', 'stringr', 'here', 'purrr', 'data.table'

download_sivep <- function(date = "2021-04-05", output_folder = here::here(),
                           return_df = FALSE) {

    output_file <- paste0(output_folder, "/sivep_raw_", date, ".csv.gz")
    
    if (file.exists(output_file)) {
        print("File already exists! Overwriting")
    } else{
        print("Creating a new file")
    }

    # SIVEP URLs for 2020 and 2021 files
    sivep_path <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/"

    
    
    ls_sivep_urls <- list(
        "SIVEP_2020" = paste0(sivep_path, "2020/INFLUD-", stringr::str_sub(date, 9, 10),"-", stringr::str_sub(date, 6, 7), "-", stringr::str_sub(date, 1, 4), ".csv"),
        "SIVEP_2021" = paste0(sivep_path, "2021/INFLUD21-", stringr::str_sub(date, 9, 10),"-", stringr::str_sub(date, 6, 7), "-", stringr::str_sub(date, 1, 4), ".csv")
        )
    


    # Downloads SIVEP 2020-2021    
    sivep_raw <- purrr::map_df(
        ls_sivep_urls,
        ~vroom::vroom(.,
                      col_types = vroom::cols(
                          .default  = vroom::col_character(),
                          SEM_NOT    = vroom::col_double(),
                          SEM_PRI    = vroom::col_double(),
                          NU_IDADE_N = vroom::col_double(),
                          TP_IDADE   = vroom::col_double()
                          )
                      )
        )


    
    if (return_df) {
        # Exports SIVEP in a CSV file
        return(sivep_raw)
        
    } else{
        # Exports SIVEP in a CSV file
        data.table::fwrite(sivep_raw, paste0(output_file))
        print(paste0("Finished! File created at: workdir/", output_folder))
    }

}


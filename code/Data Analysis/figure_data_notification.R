
Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(patchwork)


#### importing previous cleanned database
srag_adults_covid <-
    data.table::fread("data/sivep_raw_2021-05-10.csv.gz",
                      na.strings = c("", "NA")) %>%
    as_tibble() 
# %>%
# filter(SG_UF_INTE == "PA")


srag_adults_covid %>%
    filter(
        CLASSI_FIN == 5
        ) %>% 
    mutate(
        HOSP_NOT = case_when(
            is.na(HOSPITAL) | HOSPITAL == 9 ~ "Missing/Not reported",
            HOSPITAL == 1 ~ "Admitted",
            HOSPITAL == 2 ~ "Not admitted"
        )
    ) %>% 
    count(
        SG_UF_INTE, HOSP_NOT
    ) %>% 
    ggplot() +
    geom_col(aes(x = SG_UF_INTE, y = n, fill = HOSP_NOT), position = "fill") +
    theme_classic()

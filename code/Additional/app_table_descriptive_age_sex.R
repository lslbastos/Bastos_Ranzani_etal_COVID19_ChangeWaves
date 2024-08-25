#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Descriptive table - Notifications, Deaths and IHM                                                    ##
### Coding Leonardo Bastos (@lslbastos), Otavio Ranzani (@oranzani)                                      ##
### March 2021                                                                                           ##
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gtsummary)


release_date <- "2021-07-19"
release_file <- paste0("data/srag_adults_covid_", release_date,".csv.gz")

delay <- 4

#### importing previous cleanned database
srag_adults_covid_deaths <-
    data.table::fread(release_file,
                      na.strings = c("", "NA")) %>%
    as_tibble() %>% 
    # filter(
    #     SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay)
    # ) %>% 
    filter(EVOLUCAO == "Death", !is.na(CS_SEXO))
    # mutate(
    #     CS_SEXO = case_when(
    #         is.na(CS_SEXO) ~ "Not reported",
    #         TRUE ~ CS_SEXO
    #     )
    # )



# Brazil - Descriptive - Notifications ------------------------------------
# Variable labels
ls_labels_all <- 
    list(
        total ~ "In-hospital deaths, n(%)",
        NU_IDADE_N ~ "Age, median (IQR)",
        FAIXA_IDADE ~ "",
        CS_SEXO ~ "Sex, n(%)"
    )


df_covid_desc_age_sex <- 
    srag_adults_covid_deaths %>%
    select(REGIAO, CS_SEXO, NU_IDADE_N, FAIXA_IDADE) %>% 
    mutate(
        total = 1
    )


### Table Function
run_desc_table <- function(df) {
    
    df_covid_all_desc_filter <- df %>% 
        filter(!is.na(REGIAO))
    
    
## Table: Comparison between 1st and 2nd wave
    tb_covid_all <- 
        df_covid_all_desc_filter %>% 
        select(
            total,
            NU_IDADE_N,
            FAIXA_IDADE,
            CS_SEXO
        ) %>% 
        tbl_summary(
            by = CS_SEXO,
            # label = ls_labels_all,
            # statistic = list(
            #     all_categorical() ~ "{n}"
            # ),
            digits = list(
                all_categorical() ~ c(0, 1)
            )
        ) %>% 
        add_overall() %>% 
        add_n() 
        # add_overall()




tb_overall_desc <- 
    tb_covid_all$table_body %>% 
    filter(!(row_type == "missing")) %>% 
    select(-c(var_type)) %>% 
    mutate(
        label = ifelse(row_type == "label", paste0(label, " [n = ", n, "]"), label)
        # stat_0 = ifelse(variable == "total", str_remove_all(stat_0, "(100.0%)"), stat_0)
        ) %>% 
    rename("Characteristics" = "label",
           "Overall" = "stat_0",
           "Female" = "stat_1",
           "Male" = "stat_2"
           # "Not Reported" = "stat_3"
    ) %>% 
    select(-c(n, row_type, var_label, variable))

    return(tb_overall_desc)

}






## Run table for Brazil
tb_desc_br <- 
    df_covid_desc_age_sex %>% 
    mutate(
        REGIAO = "Brazil"
    ) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>% 
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()

    

## Run table for Region
tb_desc_region <- 
    df_covid_desc_age_sex %>% 
    filter(!is.na(REGIAO)) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>%
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()



## Run table for State
tb_desc_UF <- 
    df_covid_desc_age_sex %>% 
    filter(!is.na(REGIAO)) %>% 
    mutate(
        REGIAO = SG_UF_INTE
    ) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>%
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()



## Binding tables
tb_desc_deaths <- bind_rows(
    tb_desc_br, 
    tb_desc_region,
    tb_desc_UF
    )

# writexl::write_xlsx(tb_desc_br, "output/tb_desc_2021-05-10.xlsx")



# 
# dates_periods <- 
#     df_covid_all_desc %>% 
#     group_by(period) %>% 
#     summarise(
#         date_label = paste0(format(min(date_sint), format = "%d/%b/%Y"), 
#                             " - ", 
#                             format(max(date_sint), format = "%d/%b/%Y")
#                             )
#         )
# 


write_csv(tb_desc_deaths,  "shiny_app_sivep/app_data/tb_desc_deaths.csv.gz")
# 
# write_csv(tb_desc_admissions %>% 
#             select(
#                 REGIAO, Characteristics, Overall
#                 # Period2, Period2.1, Period2.2
#             ) %>% 
#             # set_names(
#             #     c("REGIAO", "Characteristics", "Overall", 
#             #       as.character(dates_periods[1, 2]), as.character(dates_periods[2, 2])
#             #     )
#             # ) %>% 
#             mutate(
#                 Characteristics = case_when(
#                     Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
#                     Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
#                     TRUE ~ Characteristics
#                 )
#             )
#         , "shiny_app_sivep/app_data/tb_descriptive_waves.csv.gz")
# 
# 
# 
# 
# write_csv(tb_desc_admissions %>% 
#               select(
#                   REGIAO, Characteristics, Overall
#                   # Period2, Period2.1, Period2.2
#               ) %>% 
#               # set_names(
#               #     c("REGIAO", "Characteristics", "Overall", 
#               #       as.character(dates_periods[1, 2]), as.character(dates_periods[2, 2])
#               #     )
#               # ) %>% 
#               mutate(
#                   Characteristics = case_when(
#                       Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
#                       Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
#                       TRUE ~ Characteristics
#                   )
#               )
#           , "input/app_data/tb_descriptive_waves.csv.gz")

# 
# saveRDS(tb_desc_admissions %>% 
#               select(
#                   REGIAO, Characteristics, Overall, Period1, Period2
#                   # Period2, Period2.1, Period2.2
#               ) %>% 
#             set_names(
#                 c("REGIAO", "Characteristics", "Overall", 
#                   as.character(dates_periods[1, 2]), as.character(dates_periods[2, 2])
#                   )
#             ) %>% 
#             mutate(
#                 
#                 Characteristics = case_when(
#                     Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
#                     Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
#                     TRUE ~ Characteristics
#                 )
#             )
#         , "shiny_app_sivep/app_data/tb_descriptive_waves.rds")
# 

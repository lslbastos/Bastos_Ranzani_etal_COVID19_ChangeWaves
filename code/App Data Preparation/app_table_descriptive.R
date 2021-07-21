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
library(tidylog)
library(gtsummary)
library(patchwork)



## User functions 
paste_iqr <- function(x) {
    med <- median(x)
    q1 <-  quantile(x, probs = 0.25)
    q3 <-  quantile(x, probs = 0.75)
    
    med_iqr <- paste0(round(med), " (", round(q1), "-", round(q3), ")")
    }


release_date <- "2021-07-19"
release_file <- paste0("data/srag_adults_covid_hosp_", release_date,".csv.gz")

delay <- 4

#### importing previous cleanned database
srag_adults_covid <-
    data.table::fread(release_file,
                      na.strings = c("", "NA")) %>%
    as_tibble() %>% 
    filter(
        SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay)
    ) %>% 
    mutate(
        FAIXA_IDADE_SIMP = case_when(
            FAIXA_IDADE %in% c("20-39") ~ "20-39",
            FAIXA_IDADE %in% c("40-49", "50-59") ~ "40-59",
            TRUE ~ ">=60"
        )
    )


### Reference dates from E484 mutation (outbreak.info)
# E484 description: the date/week with the first sharp increase of prevalence 
#                   (around October 09 or 10, 2020 - Epi. Week 43/2020)
# E484 dominance: the date/week with prevalence over 50% of samples
#                 (around December 28 or 29, 2020 - Epi. Week 53/2020)

# df_date_ref <- 
#     tibble(
#         week_bp    = c(43, 53),
#         week_label = c("E484 Description", "E484 Domain")
#     )




# Brazil - Descriptive - Notifications ------------------------------------
# Variable labels
ls_labels_all <- 
    list(
        total ~ "Admissions, n(%)",
        total_outcome ~ "Admissions with an outcome, n(%)",
        CS_SEXO ~ "Sex, n (%)",
        
        NU_IDADE_N ~ "Age, median (IQR)",
        age_20_39 ~ "20-39 years, n(%)",
        age_40_59 ~ "40-59 years",
        age_high60 ~ ">=60 years",
        
        CS_RACA ~ "Self-reported race, n(%)",
        
        CS_ESCOL_N ~ "Self-reported level of education, n (%)",
        IS_CAPITAL ~ "Residing in state capital, n (%)",
        CS_ZONA ~ "Area of residence, n(%)",
        
        SATURACAO_m ~ "Hypoxaemia, n (%)",

        UTI ~ "ICU admission, n (%)", 

        prop_niv_imv ~ "Respiratory Support, n(%)",
        prop_niv ~ "NIV, n (%)",
        prop_imv ~ "IMV, n (%)",
        prop_imv_in ~ "IMV inside ICU, n(%)",
        prop_imv_out ~ "IMV outside ICU, n(%)",
        
        ihm_outcome ~ "In-hospital mortality (IHM) - admissions with an outcome, n (%)",
        age_20_39_outcome  ~ "IHM: 20-39 years",
        age_40_59_outcome  ~ "IHM: 40-59 years",
        age_high60_outcome ~ "IHM:>= 60 years",
        icu_outcome ~ "IHM: ICU admission, n (%)",
        niv_outcome ~ "IHM: NIV, n (%)",
        imv_outcome ~ "IHM: IMV, n (%)"
    )


df_covid_all_desc <- 
    srag_adults_covid %>%
    # mutate(
    #     period = case_when(
    #         SEM_PRI_CONT <= 43 ~ 1,
    #         TRUE ~ 2
    #         ),
    #     ) %>%
    mutate(
        total = HOSPITAL == "Yes",
        total_outcome = HOSPITAL == "Yes" & EVOLUCAO %in% c("Death", "Discharge"),
        
        age_20_39 = if_else(FAIXA_IDADE_SIMP == "20-39", 1, 0),
        age_40_59 = if_else(FAIXA_IDADE_SIMP == "40-59", 1, 0),
        age_high60 = if_else(FAIXA_IDADE_SIMP == ">=60", 1, 0),
        
        age_less60_sat = case_when(
            SATURACAO_m == "Yes" & FAIXA_IDADE_SIMP == "<60" ~ 1,
            SATURACAO_m == "No" & FAIXA_IDADE_SIMP == "<60" ~ 0
        ),
        age_high60_sat = case_when(
            SATURACAO_m == "Yes" & FAIXA_IDADE_SIMP == ">=60" ~ 1,
            SATURACAO_m == "No" & FAIXA_IDADE_SIMP == ">=60" ~ 0
        ),
        prop_icu_sat = case_when(
            UTI == "Yes" ~ 1,
            UTI == "No"  ~ 0
            ),
        prop_icu_less60 = case_when(
            UTI == "Yes" & FAIXA_IDADE_SIMP == "<60" ~ 1,
            UTI == "No"  & FAIXA_IDADE_SIMP == "<60" ~ 0
            ),

        prop_icu_high60 = case_when(
            UTI == "Yes" & FAIXA_IDADE_SIMP == ">=60" ~ 1,
            UTI == "No"  & FAIXA_IDADE_SIMP == ">=60" ~ 0
            ),
        prop_niv_imv = case_when(
            SUPORT_VEN %in% c("Non-invasive", "Invasive") ~ 1,
            !is.na(SUPORT_VEN) ~ 0
            ),
        prop_niv = case_when(
            SUPORT_VEN  == "Non-invasive" ~ 1,
            !is.na(SUPORT_VEN) ~ 0
            ),
        prop_imv = case_when(
            SUPORT_VEN  == "Invasive" ~ 1,
            !is.na(SUPORT_VEN) ~ 0
            ),
        prop_imv_in = case_when(
            UTI == "Yes" & SUPORT_VEN == "Invasive" ~ 1,
            UTI == "No"  & SUPORT_VEN == "Invasive" ~ 0
            ),
        prop_imv_out = case_when(
            UTI == "Yes" & SUPORT_VEN == "Invasive" ~ 0,
            UTI == "No"  & SUPORT_VEN == "Invasive" ~ 1
            ),    
        ihm_outcome = case_when(
            EVOLUCAO == "Death" ~ 1,
            EVOLUCAO == "Discharge" ~ 0,
            ),
        age_20_39_outcome = case_when(
            FAIXA_IDADE_SIMP == "20-39" & EVOLUCAO == "Death" ~ 1,
            FAIXA_IDADE_SIMP == "20-39" & EVOLUCAO == "Discharge" ~ 0,
        ),
        age_40_59_outcome = case_when(
            FAIXA_IDADE_SIMP == "40-59" & EVOLUCAO == "Death" ~ 1,
            FAIXA_IDADE_SIMP == "40-59" & EVOLUCAO == "Discharge" ~ 0,
        ),
        age_high60_outcome = case_when(
            FAIXA_IDADE_SIMP == ">=60" & EVOLUCAO == "Death" ~ 1,
            FAIXA_IDADE_SIMP == ">=60" & EVOLUCAO == "Discharge" ~ 0,
        ),
        
        sat_outcome = case_when(
            EVOLUCAO == "Death" ~ 1,
            EVOLUCAO == "Discharge" ~ 0,
            ),
        icu_outcome = case_when(
            UTI == "Yes" & EVOLUCAO == "Death" ~ 1,
            UTI == "Yes" & EVOLUCAO == "Discharge" ~ 0,
            ),
        niv_outcome = case_when(
            SUPORT_VEN == "Non-invasive" & EVOLUCAO == "Death" ~ 1,
            SUPORT_VEN == "Non-invasive" & EVOLUCAO == "Discharge" ~ 0
            ),
        imv_outcome = case_when(
            SUPORT_VEN == "Invasive" & EVOLUCAO == "Death" ~ 1,
            SUPORT_VEN == "Invasive" & EVOLUCAO == "Discharge" ~ 0
            )
        )







### Table Function
run_desc_table <- function(df) {
    
    df_covid_all_desc_filter <- df %>% 
        filter(!is.na(REGIAO))
    
    
## Table: Comparison between 1st and 2nd wave
tb_covid_all <- 
    df_covid_all_desc_filter %>% 
    select(
        # period, 
        total,
        CS_SEXO,
        NU_IDADE_N,
        age_20_39,
        age_40_59,
        age_high60,
        CS_RACA,
        CS_ESCOL_N,
        IS_CAPITAL,
        CS_ZONA,
        SATURACAO_m,
        UTI,
        prop_niv_imv,
        prop_niv,
        prop_imv,
        prop_imv_in,
        prop_imv_out,
        total_outcome,
        ihm_outcome,
        age_20_39_outcome,
        age_40_59_outcome,
        age_high60_outcome,
        icu_outcome,
        niv_outcome,
        imv_outcome
    ) %>% 
    tbl_summary(
        # by = "period",
        missing = "no",
        label = ls_labels_all,
        digits = list(
            all_categorical() ~ c(0, 1)
        )
    ) %>% 
    add_n() 
    # %>% 
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
           "Overall" = "stat_0"
           # "Period1" = "stat_1",
           # "Period2" = "stat_2"
    ) %>% 
    select(-c(n, row_type, var_label))






# Admissions per Week and Maximum Admissions ------------------------------



## Admissions per week - 1st and 2nd wave
df_admissions_week_wave <-
    df_covid_all_desc_filter %>%
    mutate(period = 0) %>% 
    # bind_rows(
    #     df_covid_all_desc %>%
    #         mutate(period = 0)
    # ) %>%
    group_by(period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>%
    summarise(
        total = n()
    ) %>%
    ungroup() %>%
    group_by(period) %>%
    summarise(
        median_iqr = paste_iqr(total),
        max_admissions = as.character(max(total))
    ) %>%
    pivot_longer(-c(period), names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>%
    rename(
        Overall = `0`
        # Period1 = `1`,
        # Period2 = `2`
    ) %>%
    # mutate(
    #     diff_wave = case_when(
    #         metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
    #         metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
    #         TRUE ~ NA_real_
    #     )
    # ) %>%
    select(metrics, Overall)






## Descriptive Tables with rate ratios

tb_desc_admissions <- 
    bind_rows(
        tb_overall_desc %>% 
            slice(1),
        df_admissions_week_wave %>%
            select(
                Characteristics = metrics,
                Overall
                # Period1,
                # Period2
            ), 
        tb_overall_desc %>% 
            slice(-1)
    )
    # df_admissions_week_wave %>%
    # select(
    #     Characteristics = metrics,
    #     Overall
    #     # Period1,
    #     # Period2
    # ) 
    # %>% 
    # bind_rows(
    #     tb_overall_desc %>% 
    #         select(-variable)
    # )

    return(tb_desc_admissions)

}






## Run table for Brazil
tb_desc_br <- 
    df_covid_all_desc %>% 
    mutate(
        REGIAO = "Brazil"
    ) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>% 
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()

    

## Run table for Region
tb_desc_region <- 
    df_covid_all_desc %>% 
    filter(!is.na(REGIAO)) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>%
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()



## Run table for State
tb_desc_UF <- 
    df_covid_all_desc %>% 
    filter(!is.na(REGIAO)) %>% 
    mutate(
        REGIAO = SG_UF_INTE
    ) %>% 
    split(.$REGIAO) %>% 
    map(run_desc_table) %>%
    imap(~tibble(REGIAO = .y, .x)) %>% 
    bind_rows()



## Binding tables
tb_desc_admissions <- bind_rows(
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


write_csv(tb_desc_admissions %>% 
            select(
                REGIAO, Characteristics, Overall
                # Period2, Period2.1, Period2.2
            ) %>% 
            # set_names(
            #     c("REGIAO", "Characteristics", "Overall", 
            #       as.character(dates_periods[1, 2]), as.character(dates_periods[2, 2])
            #     )
            # ) %>% 
            mutate(
                Characteristics = case_when(
                    Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
                    Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
                    TRUE ~ Characteristics
                )
            )
        , "shiny_app_sivep/app_data/tb_descriptive_waves.csv.gz")




write_csv(tb_desc_admissions %>% 
              select(
                  REGIAO, Characteristics, Overall
                  # Period2, Period2.1, Period2.2
              ) %>% 
              # set_names(
              #     c("REGIAO", "Characteristics", "Overall", 
              #       as.character(dates_periods[1, 2]), as.character(dates_periods[2, 2])
              #     )
              # ) %>% 
              mutate(
                  Characteristics = case_when(
                      Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
                      Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
                      TRUE ~ Characteristics
                  )
              )
          , "input/app_data/tb_descriptive_waves.csv.gz")

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

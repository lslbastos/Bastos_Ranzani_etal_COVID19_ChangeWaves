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


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(gtsummary)
library(patchwork)

release_date <- "2021-05-24"
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
        # age_less60 ~ "<60 years, n (%)",
        age_high60 ~ ">=60 years",
        
        CS_RACA ~ "Self-reported race, n(%)",
        
        CS_ESCOL_N ~ "Self-reported level of education, n (%)",
        CS_ZONA ~ "Area of residence, n(%)",
        
        SATURACAO_m ~ "Hypoxaemia, n (%)",
        # age_20_39_sat ~ "Age 20-39 with hipoxaemia",
        # age_40_59_sat ~ "Age 40-59 60 with hipoxaemia",
        # age_less60_sat ~ "Age < 60 with hipoxaemia",
        # age_high60_sat ~ "Age >= 60 with hipoxaemia",
        
        UTI ~ "ICU admission, n (%)", 
        # prop_icu_20_39 ~ "Age 20-39 admitted to the ICU",
        # prop_icu_40_59 ~ "Age 40-59 admitted to the ICU",
        # prop_icu_less60 ~ "Age < 60 admitted to the ICU",
        # prop_icu_high60 ~ "Age >= 60 admitted to the ICU",
        # SUPORT_VEN ~ "Respiratory Support, n (%)",
        
        prop_niv_imv ~ "Total Respiratory Support, n(%)",
        prop_niv ~ "NIV, n (%)",
        prop_imv ~ "IMV, n (%)",
        prop_imv_in ~ "IMV inside ICU, n(%)",
        prop_imv_out ~ "IMV outside ICU, n(%)",
        
        ihm_outcome ~ "Overall, n (%)",
        age_20_39_outcome ~ "20-39 years",
        age_40_59_outcome ~ "40-59 years",
        age_high60_outcome ~ ">= 60 years",
        icu_outcome ~ "ICU admission, n (%)",
        niv_outcome ~ "NIV, n (%)",
        imv_outcome ~ "IMV, n (%)"
    )


df_covid_all_desc <- 
    srag_adults_covid %>%
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            TRUE ~ 2
            ),
        # period_dominance = case_when(
        #     SEM_PRI_CONT <= 43 ~ 1,
        #     SEM_PRI_CONT <= 53 ~ 2,
        #     TRUE ~ 3
        #     ),
        ) %>%
    mutate(
        total = HOSPITAL == "Yes",
        total_outcome = HOSPITAL == "Yes" & EVOLUCAO %in% c("Death", "Discharge"),
        
        age_20_39 = if_else(FAIXA_IDADE_SIMP == "20-39", 1, 0),
        age_40_59 = if_else(FAIXA_IDADE_SIMP == "40-59", 1, 0),
        # age_less60 = if_else(FAIXA_IDADE_SIMP == "<60", 1, 0),
        age_high60 = if_else(FAIXA_IDADE_SIMP == ">=60", 1, 0),
        
        # age_20_39_sat = case_when(
        #     SATURACAO_m == "Yes" & FAIXA_IDADE_SIMP == "20-39" ~ 1,
        #     SATURACAO_m == "No" & FAIXA_IDADE_SIMP == "20-39" ~ 0
        # ),
        # age_40_59_sat = case_when(
        #     SATURACAO_m == "Yes" & FAIXA_IDADE_SIMP == "40-59" ~ 1,
        #     SATURACAO_m == "No" & FAIXA_IDADE_SIMP == "40-59" ~ 0
        # ),
        
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
        # prop_icu_20_39 = case_when(
        #     UTI == "Yes" & FAIXA_IDADE_SIMP == "20-39" ~ 1,
        #     UTI == "No"  & FAIXA_IDADE_SIMP == "20-39" ~ 0
        # ),
        # prop_icu_40_59 = case_when(
        #     UTI == "Yes" & FAIXA_IDADE_SIMP == "40-59" ~ 1,
        #     UTI == "No"  & FAIXA_IDADE_SIMP == "40-59" ~ 0
        # ),
        
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
        # age_less_60_outcome = case_when(
        #     FAIXA_IDADE_SIMP == "<60" & EVOLUCAO == "Death" ~ 1,
        #     FAIXA_IDADE_SIMP == "<60" & EVOLUCAO == "Discharge" ~ 0,
        # ),
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




## Table: Comparison between 1st and 2nd wave
tb_covid_all <- 
    df_covid_all_desc %>% 
    select(
        period, 
        total,
        CS_SEXO,
        NU_IDADE_N,
        age_20_39,
        age_40_59,
        # age_less60,
        age_high60,
        CS_RACA,
        CS_ESCOL_N,
        IS_CAPITAL,
        CS_ZONA,
        SATURACAO_m,
        # age_20_39_sat,
        # age_40_59_sat,
        # age_less60_sat,
        # age_high60_sat,
        UTI,
        # prop_icu_20_39,
        # prop_icu_40_59,
        # prop_icu_less60,
        # prop_icu_high60,
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
        # age_less_60_outcome,
        icu_outcome,
        niv_outcome,
        imv_outcome
        
    ) %>% 
    tbl_summary(
        by = "period",
        missing = "no",
        label = ls_labels_all,
        digits = list(
            all_categorical() ~ c(0, 1)
        )
    ) %>% 
    add_n()




tb_overall_desc <- 
    tb_covid_all$table_body %>% 
    filter(!(row_type == "missing")) %>% 
    select(-c(var_type)) %>% 
    mutate(label = ifelse(row_type == "label", paste0(label, " [n = ", n, "]"), label)) %>% 
    rename("Characteristics" = "label",
           # "Overall" = "stat_0",
           "Period1" = "stat_1",
           "Period2" = "stat_2"
           # "Period2.1" = "stat_2",
           # "Period2.2" = "stat_3"
    ) %>% 
    select(-c(n, row_type, var_label))
    



## Table: 2nd wave - Comparison between before and after E484 dominance 
tb_covid_per2 <- 
    df_covid_all_desc %>% 
    filter(period != 1) %>% 
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            SEM_PRI_CONT <= 53 ~ 2,
            TRUE ~ 3
            )
        ) %>% 
    select(
        period, 
        total,
        CS_SEXO,
        NU_IDADE_N,
        age_20_39,
        age_40_59,
        # age_less60,
        age_high60,
        CS_RACA,
        CS_ESCOL_N,
        IS_CAPITAL,
        CS_ZONA,
        SATURACAO_m,
        # age_20_39_sat,
        # age_40_59_sat,
        # age_less60_sat,
        # age_high60_sat,
        UTI,
        # prop_icu_20_39,
        # prop_icu_40_59,
        # prop_icu_less60,
        # prop_icu_high60,
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
        # age_less_60_outcome,
        icu_outcome,
        niv_outcome,
        imv_outcome
    ) %>% 
    tbl_summary(
        by = "period",
        missing = "no",
        label = ls_labels_all,
        digits = list(
            all_categorical() ~ c(0, 1)
            )
    ) %>% 
    add_n()




tb_overall_desc_per2 <- 
    tb_covid_per2$table_body %>% 
    filter(!(row_type == "missing")) %>% 
    select(-c(var_type)) %>% 
    mutate(label = ifelse(row_type == "label", paste0(label, " [n = ", n, "]"), label)) %>% 
    rename("Characteristics" = "label",
           # "Period1" = "stat_1",
           "Period2.1" = "stat_1",
           "Period2.2" = "stat_2"
    ) %>% 
    select(-c(n, row_type, var_label))




## Combining both tables into a single one
tb_desc <- 
    tb_overall_desc %>% 
    bind_cols(
        tb_overall_desc_per2 %>% 
            select(Period2.1, Period2.2)
    )










# Admissions per Week and Maximum Admissions ------------------------------

paste_iqr <- function(x) {
    med <- median(x)
    q1 <-  quantile(x, probs = 0.25)
    q3 <-  quantile(x, probs = 0.75)
    
    med_iqr <- paste0(round(med), " (", round(q1), "-", round(q3), ")")
}

## Admissions per week - 1st and 2nd wave
df_admissions_week_wave <-
    df_covid_all_desc %>%
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
        Period1 = `1`,
        Period2 = `2`
    ) 
    # %>%
    # mutate(
    #     diff_wave = case_when(
    #         metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
    #         metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
    #         TRUE ~ NA_real_
    #     )
    # )



## Admissions per week - 2nd wave - before and after E484 dominance 
df_admissions_week_dominance <-
    df_covid_all_desc %>%
    filter(
        period != 1
    ) %>%
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            SEM_PRI_CONT <= 53 ~ 2,
            TRUE ~ 3
        )
    ) %>%
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
    pivot_longer(-period, names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>%
    rename(
        Period2.1 = `2`,
        Period2.2 = `3`
    ) 
    # %>%
    # mutate(
    #     diff_dominance = case_when(
    #         metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2.2) / as.numeric(Period2.1)) - 1)), 1),
    #         metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2.2, ".*\\s")) / as.numeric(str_extract(Period2.1, ".*\\s"))) - 1)), 1),
    #         TRUE ~ NA_real_
    #     )
    # )






# Risk ratios -------------------------------------------------------------

# v_variables <- c("total_outcome", "age_less60", "age_high60", "SATURACAO_m", "age_less60_sat", "age_high60_sat",
#                  "UTI", "prop_icu_less60", "prop_icu_high60", "prop_niv_imv", "prop_niv",
#                  "prop_imv", "prop_imv_in", "prop_imv_out", "ihm_outcome", "icu_outcome",
#                  "niv_outcome", "imv_outcome")

# v_variables <- c("total_outcome", "age_20_39","age_40_59", "age_high60",
#                  "SATURACAO_m", "UTI", "prop_niv_imv", "prop_niv", "prop_imv", 
#                  "prop_imv_in", "prop_imv_out", "ihm_outcome", "icu_outcome", 
#                  "age_20_39_outcome", "age_40_59_outcome",
#                  "niv_outcome","imv_outcome" )

## Calculate and paste RR with Confidence inTervals (bootstrap, 5000 resamples - default)
# get_rr_ci <- 
#     function(data, x) {
#         df <- data[, c("period", x)]
#         tb <- table(df$period, df %>% pull(x))
#         compar <- round(epitools::riskratio(tb, method = "boot")$measure[2, ], 3)
#         rr <- compar[1]
#         ci <- paste0(compar[c(2, 3)], collapse = "-")
#         rr_ci <- paste0(rr, " (",ci, ")")
#         print(x)
#         return(rr_ci)
#     }
# 
# 
# df_rr_waves <- 
#     v_variables %>% 
#     map_df(
#         ~tibble(
#             variable = .,
#             rr_ci_wave = get_rr_ci(df_covid_all_desc, .)
#             )
#         )
# 
# 
# df_rr_dominance <- 
#     v_variables %>% 
#     map_df(
#         ~tibble(
#             variable = .,
#             rr_ci_dominance = get_rr_ci(
#                 df_covid_all_desc %>%
#                     filter(period == 2) %>% 
#                     mutate(
#                         period = case_when(
#                             # SEM_PRI_CONT <= 43 ~ 1,
#                             SEM_PRI_CONT <= 53 ~ 2,
#                             TRUE ~ 3
#                             )
#                         )
#                 , .)
#             )
#         )
# 


## Descriptive Tables with rate ratios
tb_desc_rr <- 
    tb_desc 
    # %>% 
    # left_join(
    #     df_rr_waves
    # ) %>%
    # left_join(
    #     df_rr_dominance
    # ) %>%
    # mutate(
    #     diff_wave      = 100 * (as.numeric(str_sub(rr_ci_wave, 1, 5)) - 1),
    #     diff_dominance = 100 * (as.numeric(str_sub(rr_ci_dominance, 1, 5)) - 1)
    # 
    # ) %>%
    # select(
    #     Characteristics, Period1, Period2, diff_wave, rr_ci_wave,
    #     Period2.1, Period2.2, diff_dominance, rr_ci_dominance
    # )


tb_desc_rr_admissions <- 
    # tb_desc_rr %>% 
    bind_rows(
        df_admissions_week_wave %>%
            bind_cols(
                df_admissions_week_dominance %>%
                    select(-metrics)
            ) %>%
            rename(
                Characteristics = metrics
            ), 
            # %>%
            # mutate(
            #     rr_ci_wave = NA,
            #     rr_ci_dominance = NA
            # )
        tb_desc_rr
    ) %>% 
    mutate(
        
        Characteristics = case_when(
            Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
            Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
            TRUE ~ Characteristics
        )
    )

writexl::write_xlsx(tb_desc_rr_admissions %>% 
                        select(
                            Characteristics, Period1, Period2, Period2.1, Period2.2
                        ) %>% 
                        rename(
                            "First wave" = Period1,
                            "Second wave" = Period2,
                            "Before E484K mutation dominance" = Period2.1,
                            "After E484K mutation dominance" = Period2.2,
                        ), paste0("output/Correspondence/tb_desc_rr_", release_date, ".xlsx"))



# write_csv(tb_desc_rr_admissions %>% 
#               select(
#                   Characteristics, Period1, Period2, Period2.1, Period2.2
#               ) %>% 
#               rename(
#                   "First wave" = Period1,
#                   "Second wave" = Period2,
#                   "Before E484K mutation dominance" = Period2.1,
#                   "After E484K mutation dominance" = Period2.2,
#               ), "shiny_app_sivep/app_data/tb_descriptive_waves.csv.gz")
# 

# 
# flextable::flextable(tb_desc_rr_admissions)
#     flextable::save_as_docx(., path = "output/tb_desc_rr.docx")



# Admissions per week stratified by age and respiratory support -----------






## Hypoxemia - Admissions per week - 1st and 2nd wave
df_covid_all_desc %>%
    filter(!is.na(SATURACAO_m)) %>% 
    group_by(SATURACAO_m, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>%
    summarise(
        total = n()
    ) %>%
    ungroup() %>%
    group_by(SATURACAO_m, period) %>%
    summarise(
        median_iqr = paste_iqr(total),
        max_admissions = as.character(max(total))
    ) %>%
    pivot_longer(-c(SATURACAO_m, period), names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>%
    rename(
        Period1 = `1`,
        Period2 = `2`
    ) %>%
    mutate(
        diff_wave = case_when(
            metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
            metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
            TRUE ~ NA_real_
        )
    )


## Respiratory support - Admissions per week - 1st and 2nd wave
df_covid_all_desc %>% 
    filter(!is.na(SUPORT_VEN)) %>% 
    group_by(SUPORT_VEN = SUPORT_VEN %in% c("Non-invasive", "Invasive"), period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    ungroup() %>% 
    group_by(SUPORT_VEN, period) %>% 
    summarise(
        median_iqr = paste_iqr(total),
        max_admissions = as.character(max(total))
    ) %>% 
    pivot_longer(-c(SUPORT_VEN, period), names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>% 
    rename(
        Period1 = `1`,
        Period2 = `2` 
    ) %>% 
    mutate(
        diff_wave = case_when(
            metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
            metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
            TRUE ~ NA_real_
        )
    )



## Respiratory support (stratified) - Admissions per week - 1st and 2nd wave
df_covid_all_desc %>% 
    filter(!is.na(SUPORT_VEN)) %>% 
    group_by(SUPORT_VEN, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    ungroup() %>% 
    group_by(SUPORT_VEN, period) %>% 
    summarise(
        median_iqr = paste_iqr(total),
        max_admissions = as.character(max(total))
    ) %>% 
    pivot_longer(-c(SUPORT_VEN, period), names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>% 
    rename(
        Period1 = `1`,
        Period2 = `2` 
    ) %>% 
    mutate(
        diff_wave = case_when(
            metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
            metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
            TRUE ~ NA_real_
        )
    )

## ICU - Admissions per week - 1st and 2nd wave
df_covid_all_desc %>% 
    filter(!is.na(SUPORT_VEN)) %>% 
    group_by(UTI, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    ungroup() %>% 
    group_by(UTI, period) %>% 
    summarise(
        median_iqr = paste_iqr(total),
        max_admissions = as.character(max(total))
    ) %>% 
    pivot_longer(-c(UTI, period), names_to = "metrics", values_to = "val") %>%
    pivot_wider(names_from = "period", values_from = "val") %>% 
    rename(
        Period1 = `1`,
        Period2 = `2` 
    ) %>% 
    mutate(
        diff_wave = case_when(
            metrics == "max_admissions" ~ round((100 * ((as.numeric(Period2) / as.numeric(Period1)) - 1)), 1),
            metrics == "median_iqr" ~ round((100 * ((as.numeric(str_extract(Period2, ".*\\s")) / as.numeric(str_extract(Period1, ".*\\s"))) - 1)), 1),
            TRUE ~ NA_real_
        )
    )

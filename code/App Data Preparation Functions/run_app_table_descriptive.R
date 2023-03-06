
# Auxiliary functions -----------------------------------------------------

## Manual IQR calculation
paste_iqr <- function(x) {
    med <- median(x)
    q1 <-  quantile(x, probs = 0.25)
    q3 <-  quantile(x, probs = 0.75)
    
    med_iqr <- paste0(round(med), " (", round(q1), "-", round(q3), ")")
    }


### Table Function
run_desc_table <- function(df) {
    library(tidyverse)
    library(gtsummary)
    
    df_covid_all_desc_filter <- df %>% 
        filter(!is.na(REGIAO))
    
    
    
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
    
    
    
    
    
    ## Table: Comparison between 1st and 2nd wave
    tb_covid_all <- 
        df_covid_all_desc_filter %>% 
        select(
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
        mutate(
            label = ifelse(row_type == "label", paste0(label, " [n = ", n, "]"), label)
        ) %>% 
        rename("Characteristics" = "label",
               "Overall" = "stat_0"
        ) %>% 
        select(-c(n, row_type, var_label))
    
    
    
    
    
    
    # Admissions per Week and Maximum Admissions ------------------------------
    
    
    
    ## Admissions per week - 1st and 2nd wave
    df_admissions_week_wave <-
        df_covid_all_desc_filter %>%
        mutate(period = 0) %>% 
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
        ) %>%
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
                ), 
            tb_overall_desc %>% 
                slice(-1)
        )
    return(tb_desc_admissions)
    
}




### Run descriptive table building

run_app_table_descriptive <- function(df) {
    library(tidyverse)
    
    delay <- 4
    #### importing previous cleanned database
    srag_adults_covid <-
        df %>% 
        filter(
            SEM_PRI_ADJ <= (max(SEM_PRI_ADJ, na.rm = TRUE) - delay)
        ) %>% 
        mutate(
            FAIXA_IDADE_SIMP = case_when(
                FAIXA_IDADE %in% c("20-39") ~ "20-39",
                FAIXA_IDADE %in% c("40-49", "50-59") ~ "40-59",
                TRUE ~ ">=60"
            )
        )
    
    
       # Brazil - Descriptive - Notifications ------------------------------------
 
    
    
    df_covid_all_desc <- 
        srag_adults_covid %>%
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

    write_csv(tb_desc_admissions %>% 
                  select(
                      REGIAO, Characteristics, Overall
                  ) %>% 
                  mutate(
                      Characteristics = case_when(
                          Characteristics == "median_iqr" ~ "Admissions per week, median (IQR)",
                          Characteristics == "max_admissions" ~ "Highest number of admissions in a week",
                          TRUE ~ Characteristics
                      )
                  )
              , here::here("input", "app_data", "tb_descriptive_waves.csv.gz"))
    

    
    
}















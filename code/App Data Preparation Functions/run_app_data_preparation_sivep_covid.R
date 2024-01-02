
run_app_data_preparation_sivep_covid <- function(df) {
    
    library(tidyverse)
    options(dplyr.summarise.inform = FALSE)

    srag <- df
    
    ## Filter: Admissions after Feb 16 (Epidemiological Week 8 - COVID in Brazil)
    srag_date <- 
        srag %>%
        mutate(
            dt_not = as.Date(DT_NOTIFIC, format = "%d/%m/%Y")
        ) %>% 
        filter(dt_not >= as.Date("2020-02-16")) %>% 
        # filter(dt_sint >= as.Date("2020-02-16")) %>% 
        mutate(index = 1:n())
    
    
    rm(srag)
    
    
    # Adjusting CLASSIFIN==5 (COVID) for those cases with PCR+ for SARS-COV2 but not CLASSIFIN==5,as by Ministry of Health official recommendation
    # MoH has been correcting it over time, so less frequent than beggining of the pandemic
    srag_classifin <- 
        srag_date %>% 
        mutate(
            CLASSI_FIN = case_when(
                PCR_SARS2 == 1 ~ 5L, # column Sars-CoV-2
                str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA|CIVID") & !str_detect(DS_PCR_OUT,"63|43|229|HK|RINO|SINCI|PARE") == TRUE ~ 5L, #other PCRs, Sars-CoV2
                TRUE ~ CLASSI_FIN
            )
        )
    
    rm(srag_date)
    
    # Filter patients with COVID-19 and confirmed Hospitalization
    srag_covid <- srag_classifin %>%
        filter(CLASSI_FIN == 5)
    
    rm(srag_classifin) # Deletes raw SIVEP file
    
    
    
    
    
    # Creates "PCR" variable with extra conditions besides PCR_SARS2 == 1
    # Filtros PCR patients
    srag_covid <- 
        srag_covid %>% 
        mutate(PCR = (PCR_SARS2 == 1) |
                   (str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA|CIVID") & !str_detect(DS_PCR_OUT,"63|43|229|HK|RINO|SINCI|PARE")) |
                   (PCR_RESUL == 1 & CRITERIO == 1 & is.na(DS_PCR_OUT) &  ## Adicional filter: among those cases already defined as COVID-19 by laboratory criterion,
                        (PCR_RINO   != 1 | is.na(PCR_RINO)) &                  ## those with a positive PCR result and not being positive for other PCRs
                        (POS_PCRFLU != 1 | is.na(POS_PCRFLU)) & 
                        (PCR_OUTRO  != 1 | is.na(PCR_OUTRO)) & 
                        (POS_PCROUT != 1 | is.na(POS_PCROUT)) & 
                        (                  is.na(PCR_VSR)) & 
                        (                  is.na(PCR_METAP)) & 
                        (                  is.na(PCR_PARA1)))) 
    
    
    
    
    
    # Filter: Age > 20 years (adult hospitalizations)
    ## Correcting for extreme age values with wrong date of birth
    srag_adults_covid <- srag_covid %>%
        mutate(NU_IDADE_N = ifelse(TP_IDADE == 3 & NU_IDADE_N > 120, (100 - NU_IDADE_N), NU_IDADE_N)) %>% #2 typos (probably wrong DT_NASC)
        filter(NU_IDADE_N >= 20 & TP_IDADE == 3)
    
    
    rm(srag_covid) # Removes SRAG COVID with all ages
    
    
    
    
    
    
    
    # Data Cleaning and preparation -------------------------------------------
    srag_adults_covid <- 
        srag_adults_covid %>%
        mutate(
            date_not      = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
            date_int      = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
            date_sint     = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
            date_desf     = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
            date_enc      = as.Date(DT_ENCERRA, format = "%d/%m/%Y"),
            # date_ent_uti  = as.Date(DT_ENTUTI,  format = "%d/%m/%Y"),
            # date_said_uti = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"),
            # date_pcr      = as.Date(DT_PCR,     format = "%d/%m/%Y")
        ) %>% 
        mutate(
            date_int = as.Date(case_when(
                as.numeric(str_sub(DT_INTERNA, 7, 10)) > 2022 ~ paste0(str_sub(DT_INTERNA, 1, 6), str_sub(DT_NOTIFIC, 7, 10)),
                # DT_INTERNA == "31/10/7202" ~ "31/07/2020", # typo
                # date_int > end_date & lubridate::year(date_int)  > lubridate::year(end_date)  ~ paste0(str_sub(DT_INTERNA,1, 6), "2020"),
                TRUE ~ as.character(DT_INTERNA)), format = "%d/%m/%Y")
        ) 
    # %>% 
    # mutate(
    #     date_not = as.Date(case_when(
    #         date_not > end_date & lubridate::year(date_not) > lubridate::year(end_date) ~ paste0(str_sub(DT_NOTIFIC,1, 6), "2020"),
    #         TRUE ~ as.character(DT_NOTIFIC)), format = "%d/%m/%Y")
    #     ) %>% 
    # mutate(
    #     date_sint = as.Date(case_when(
    #         date_sint > end_date & lubridate::year(date_sint) > lubridate::year(end_date) ~ paste0(str_sub(DT_SIN_PRI,1, 6), "2020"),
    #         TRUE ~ as.character(DT_SIN_PRI)), format = "%d/%m/%Y")
    #     ) 
    # mutate(date_enc = as.Date(case_when(
    #     date_enc > end_date & lubridate::year(date_enc) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENCERRA,1, 6), "2020"),
    #     TRUE ~ as.character(DT_ENCERRA)), format = "%d/%m/%Y")) %>% 
    # mutate(date_uti = as.Date(case_when(
    #     date_uti > end_date & lubridate::year(date_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENTUTI,1, 6), "2020"),
    #     TRUE ~ as.character(DT_ENTUTI)), format = "%d/%m/%Y")) %>% 
    # mutate(date_said_uti = as.Date(case_when(
    #     date_said_uti > end_date & lubridate::year(date_said_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_SAIDUTI,1, 6), "2020"),
    #     TRUE ~ as.character(DT_SAIDUTI)), format = "%d/%m/%Y")) %>% 
    # mutate(date_pcr = as.Date(case_when(
    #     date_pcr > end_date & lubridate::year(date_pcr) > lubridate::year(end_date) ~ paste0(str_sub(DT_PCR,1, 6), "2020"),
    #     TRUE ~ as.character(DT_PCR)), format = "%d/%m/%Y"))
    
    ## Columns with same options, Yes, No, Ignored, NA
    
    columns <- c("FEBRE", "TOSSE", "GARGANTA", "DISPNEIA", "DESC_RESP",
                 "SATURACAO", "DIARREIA", "VOMITO", "OUTRO_SIN", "PUERPERA",
                 "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", "HEPATICA", "ASMA",
                 "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE",
                 "RENAL", "OBESIDADE", "OUT_MORBI", "VACINA_COV")
    
    ## other symptoms appeared latter on the database, such as abdomnal cramps, anosmia, etc.
    ## We did not use them, because they were missing for the majority of our analysis period
    
    ## Creating, recoding, age, regions, times
    ## Adapting for descriptive tables
    
    srag_adults_covid <- 
        srag_adults_covid %>%
        mutate(CS_SEXO = case_when(CS_SEXO == "M" ~ "Male",
                                   CS_SEXO == "F" ~ "Female")
        ) %>% 
        mutate(FAIXA_IDADE = case_when(NU_IDADE_N <= 39 ~ "20-39",
                                       NU_IDADE_N <= 49 ~ "40-49",
                                       NU_IDADE_N <= 59 ~ "50-59",
                                       NU_IDADE_N <= 69 ~ "60-69",
                                       NU_IDADE_N <= 79 ~ "70-79",
                                       TRUE ~ "80+")
        ) %>%
        mutate(FAIXA_IDADE_SIMP = case_when(NU_IDADE_N <= 39 ~ "20-39",
                                            NU_IDADE_N <= 59 ~ "40-59",
                                            TRUE ~ "60+")
        ) %>%
        mutate(CS_RACA = case_when(CS_RACA == "1" ~ "White",
                                   CS_RACA == "2" ~ "Black/Brown",
                                   CS_RACA == "3" ~ "Asian",
                                   CS_RACA == "4" ~ "Black/Brown",
                                   CS_RACA == "5" ~ "Indigenous")
        ) %>% 
        mutate(CS_ESCOL_N = case_when(CS_ESCOL_N == "0" ~ "Illiterate",   # Not applicable not present in this filtered adults dataset
                                      CS_ESCOL_N == "1" ~ "Up to high school",
                                      CS_ESCOL_N == "2" ~ "Up to high school",
                                      CS_ESCOL_N == "3" ~ "High school",
                                      CS_ESCOL_N == "4" ~ "College/University")
        ) %>% 
        mutate(REGIAO = case_when(SG_UF_INTE %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                                  SG_UF_INTE %in% c("SC", "RS", "PR") ~ "South",
                                  SG_UF_INTE %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                                  SG_UF_INTE %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                                  SG_UF_INTE %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")
        ) %>% 
        mutate(REGIAO_RES = case_when(SG_UF %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                                  SG_UF %in% c("SC", "RS", "PR") ~ "South",
                                  SG_UF %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                                  SG_UF %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                                  SG_UF %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")
        ) %>% 
        mutate(EVOLUCAO = case_when(EVOLUCAO == 1 ~ "Discharge",
                                    EVOLUCAO == 2 ~ "Death",
                                    EVOLUCAO == 3 ~ "Death",
                                    TRUE ~ "Ongoing")
        ) %>%
        mutate(SUPORT_VEN = case_when(SUPORT_VEN == 1 ~ "Invasive",
                                      SUPORT_VEN == 2 ~ "Non-invasive",
                                      SUPORT_VEN == 3 ~ "None")
        ) %>% 
        mutate(UTI = case_when(UTI == 1 ~ "Yes",
                               UTI == 2 ~ "No")
        ) %>%
        mutate(
            HOSPITAL = case_when(
                HOSPITAL == 1 ~ "Yes",
                HOSPITAL == 2 ~ "No"
            )
        ) %>% 
        mutate_at(all_of(columns), function(x) {case_when(x == 1 ~ "Yes",
                                                          x == 2 ~ "No")}) %>% 
        mutate(
            SRAG_original = case_when(
                FEBRE == "Yes" &
                    (TOSSE == "Yes" | GARGANTA == "Yes") &
                    (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
            SRAG_original_total = case_when(
                !is.na(FEBRE) &
                    (!is.na(TOSSE) | !is.na(GARGANTA)) &
                    (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing"),
            SRAG_sfebre = case_when(
                (TOSSE == "Yes" | GARGANTA == "Yes") &
                    (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
            SRAG_sfebre_total = case_when(
                (!is.na(TOSSE) | !is.na(GARGANTA)) &
                    (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing")
        ) %>% 
        mutate(
            SRAG_original = ifelse((is.na(SRAG_original) & SRAG_original_total == "Not missing"),  "No", SRAG_original),
            SRAG_sfebre   = ifelse((is.na(SRAG_sfebre)  & SRAG_sfebre_total == "Not missing"),  "No", SRAG_sfebre)
        ) %>% 
        select(-c(SRAG_original_total, SRAG_sfebre_total))
    
    
    
    ## comorbidities / symptoms
    
    # keeping the original with missing values, generating new with real missing sufix _m
    db_temp <- srag_adults_covid %>% 
        select(index, all_of(columns))
    
    db_temp <- db_temp %>% 
        rename_at(all_of(columns), function(x) paste0(x, "_m"))
    
    srag_adults_covid <- srag_adults_covid %>% 
        left_join(db_temp, by = "index")
    
    # now considering missing as No for commorbidities only
    
    comorbidades <-  c("PUERPERA", "CARDIOPATI", "HEMATOLOGI",
                       "SIND_DOWN", "HEPATICA", "ASMA",
                       "DIABETES", "NEUROLOGIC", "PNEUMOPATI",
                       "IMUNODEPRE", "RENAL", "OBESIDADE", "OUT_MORBI")
    
    srag_adults_covid <-  
        srag_adults_covid %>% 
        mutate(PUERPERA = case_when(CS_SEXO == "Male" ~ NA_character_, TRUE ~ PUERPERA), # few missing considered as entered for puerpera
               CS_SEXO  = case_when(PUERPERA == "Yes" ~ "Female", TRUE ~ CS_SEXO)) %>%  # 1 case puerpera with missing sex
        mutate_at(all_of(comorbidades), function(x){case_when(x == "Yes" ~ 1,
                                                              TRUE ~ 0)}) %>%
        mutate(
            CONT_COMORB = CARDIOPATI + HEMATOLOGI +  HEPATICA + DIABETES +
                NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL +
                OBESIDADE,
            n_comorb = case_when(
                CONT_COMORB == 0 ~ 0,
                CONT_COMORB == 1 ~ 1,
                CONT_COMORB == 2 ~ 1,
                CONT_COMORB >  2 ~ 2)
        ) %>% 
        mutate(
            CONT_COMORB_m = case_when(
                is.na(CARDIOPATI_m) & is.na(HEMATOLOGI_m) & is.na(HEPATICA_m) &
                    is.na(DIABETES_m) & is.na(NEUROLOGIC_m) & is.na(PNEUMOPATI_m) &
                    is.na(IMUNODEPRE_m) & is.na(RENAL_m) & is.na(OBESIDADE_m) ~ NA_real_,
                TRUE ~ CONT_COMORB
            ),
            n_comorb_m = case_when(
                CONT_COMORB_m == 0 ~ 0,
                CONT_COMORB_m == 1 ~ 1,
                CONT_COMORB_m == 2 ~ 1,
                CONT_COMORB_m >  2 ~ 2),
            
        ) %>% 
        mutate(CONT_COMORB_mreal = case_when(!is.na(CARDIOPATI_m) & !is.na(HEMATOLOGI_m) & !is.na(HEPATICA_m) &
                                                 !is.na(DIABETES_m) & !is.na(NEUROLOGIC_m) & !is.na(PNEUMOPATI_m) &
                                                 !is.na(IMUNODEPRE_m) & !is.na(RENAL_m) & !is.na(OBESIDADE_m) ~ CONT_COMORB,
                                             TRUE ~ NA_real_
        ),
        n_comorb_mreal = case_when(
            CONT_COMORB_mreal == 0 ~ 0,
            CONT_COMORB_mreal == 1 ~ 1,
            CONT_COMORB_mreal == 2 ~ 1,
            CONT_COMORB_mreal >  2 ~ 2)
        ) %>% 
        mutate(
            CS_ZONA = case_when(
                CS_ZONA == 1 ~ "Urban",
                CS_ZONA == 2 ~ "Rural",
                CS_ZONA == 3 ~ "Peri-urban"
            )
        )
    
    
    
    rm(db_temp) ## Removing temporary database for comorbidity prep
    
    
    ### Creating week/year reference table (auxiliary table)
    df_date_weeks <- 
        tibble(
            dates = seq.Date(as.Date("2019-12-29"),
                             as.Date(release_date), by = "1 day")
        ) %>% 
        mutate(
            year_epi = lubridate::epiyear(dates),
            week_epi = lubridate::epiweek(dates)
        )
    
    df_date_weeks_cont <- 
        df_date_weeks %>% 
        left_join(
            df_date_weeks %>% 
                group_by(
                    year_epi, week_epi
                ) %>% 
                summarise(
                    week_start = min(dates)
                    # week_end   = max(dates),
                ) %>% 
                ungroup() %>% 
                mutate(
                    ano_week_epi = paste0(week_epi, "/", year_epi),
                    week_epi_cont = 1:n()
                )
        ) %>% 
        ungroup()

    
    ## Filtering columns of interest
    srag_adults_covid_final <- 
        bind_cols(
            ID = rownames(srag_adults_covid),
            srag_adults_covid
        ) %>%
        select(ID, date_not, SEM_NOT, SEM_PRI, date_sint, SG_UF_INTE, REGIAO,
               CS_SEXO, NU_IDADE_N, FAIXA_IDADE, FAIXA_IDADE_SIMP, CS_ESCOL_N, CS_RACA,
               NOSOCOMIAL, FEBRE_m, TOSSE_m, GARGANTA_m, DISPNEIA_m, DESC_RESP_m, SATURACAO_m, DIARREIA_m, VOMITO_m, OUTRO_SIN_m,
               OUTRO_DES, DOR_ABD, FADIGA, PERD_OLFT, PERD_PALA, SRAG_original, SRAG_sfebre, PUERPERA_m, FATOR_RISC,
               CARDIOPATI_m, HEMATOLOGI_m, SIND_DOWN_m, HEPATICA_m,  ASMA_m, DIABETES_m, NEUROLOGIC_m, PNEUMOPATI_m, IMUNODEPRE_m, RENAL_m,
               OBESIDADE_m, OUT_MORBI_m, MORB_DESC, CO_MUN_RES, SG_UF, REGIAO_RES, 
               HOSPITAL, date_int, CO_MU_INTE, REGIAO,
               UTI, SUPORT_VEN, RES_AN, RES_IGG, RES_IGM, RES_IGA, AN_SARS2,
               PCR_RESUL, PCR_SARS2, DS_PCR_OUT, CRITERIO, PCR, CLASSI_FIN, EVOLUCAO, date_desf, date_enc,
               n_comorb_m, n_comorb_mreal, CONT_COMORB_m, CONT_COMORB_mreal, CO_UNI_NOT, CS_ZONA,
               VACINA_COV, DOSE_1_COV, DOSE_2_COV, LAB_PR_COV, LOTE_1_COV, LOTE_2_COV, FNT_IN_COV
        ) %>%
        mutate(
            ano_pri = lubridate::epiyear(date_sint),
            ano_obi = lubridate::epiyear(date_desf),
            SEM_OBI = lubridate::epiweek(date_desf),
            date_sint = as.Date(date_sint),
            date_desf = as.Date(date_desf)
        ) %>%

        # Onset of symptoms (adjusting weeks for plotting)
        left_join(
            df_date_weeks_cont %>%
                select(dates,
                       year_epi,
                       week_epi,
                       week_start,
                       SEM_PRI_CONT = week_epi_cont,
                       ano_pri_week = ano_week_epi)
            , by = c("date_sint" = "dates",
                     "ano_pri" = "year_epi",
                     "SEM_PRI" = "week_epi")
        ) %>%
        # Date of outcome (Death) -weeks
        left_join(
            df_date_weeks_cont %>%
                select(dates,
                       year_epi,
                       week_epi,
                       week_start_obi = week_start,
                       SEM_OBI_CONT = week_epi_cont,
                       ano_obi_week = ano_week_epi)
            , by = c("date_desf" = "dates",
                     "ano_obi" = "year_epi",
                     "SEM_OBI" = "week_epi")
        ) %>%
        mutate(
            SEM_PRI = case_when(
                ano_pri == 2020 & SEM_PRI <= 8 ~ 8,
                TRUE ~ SEM_PRI
            )
        ) %>% 
        # Adjusting initial weeks for better IHM estimate in plots
        mutate(
            SEM_PRI_ADJ = case_when(
                ano_pri == 2020 & SEM_PRI_CONT <= 12 ~ 12L,
                TRUE ~ SEM_PRI_CONT
            ),
            ano_pri_week_IHM = case_when(
                ano_pri == 2020 ~ paste0(SEM_PRI_ADJ, "/", ano_pri),
                TRUE ~ paste0(SEM_PRI, "/", ano_pri)
            ) 
        ) %>% 
        mutate(
            # week_start = min(date_sint),
            # week_end   = max(date_sint),
            CO_MU_INTE = as.character(CO_MU_INTE),
        ) %>% 
        # group_by(SEM_OBI_CONT) %>% 
        # mutate(
        #     week_start_obi = min(date_desf, na.rm = TRUE)
        # ) %>% 
        ungroup() %>% 
        left_join(
            read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv") %>% 
                mutate(codigo_ibge_6dig = str_sub(codigo_ibge, 1, 6)) %>%
                select(capital, codigo_ibge_6dig), 
            by = c("CO_MU_INTE" = "codigo_ibge_6dig")
        ) %>% 
        mutate(
            IS_CAPITAL = ifelse(capital == 1, "Yes", "No")
        )
    
    rm(srag_adults_covid) # Removes SIVEP with all columns
    
    srag_adults_covid_final_hosp <- srag_adults_covid_final %>% 
        filter(HOSPITAL == "Yes") %>% 
        filter(date_sint <= as.Date(release_date))
    

    name_file_output_hosp <- paste0("srag_adults_covid_hosp_", release_date)
    # arrow::write_parquet(srag_adults_covid_final_hosp, here::here("input", paste0(name_file_output_hosp,".parquet")))
    # data.table::fwrite(srag_adults_covid_final_hosp, here::here("input", paste0(name_file_output_hosp,".csv.gz")))
    
    print("Data preparation complete!")

    return(srag_adults_covid_final_hosp)

}




# finished


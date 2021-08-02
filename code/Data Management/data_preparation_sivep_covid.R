#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Trends in severity, resource use and outcomes of severe COVID-19 in Brazil:                  ##
###                             a nationwide data analysis                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### March 2021                                                                                           ##
###                                                                                                      ##
### Importing data, filters, and data preparation                                                        ##    
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


### libraries
library(tidyverse)
# library(tidylog)

## Import SIVEP-Gripe files from 2020 and 2021
release_date <- "2021-07-26"


# Downloading from URL
source("code/Auxiliary Functions/download_sivep.R")
srag <- download_sivep(date = release_date, 
                       return_df = TRUE, 
                       save_file = FALSE, 
                       output_folder = "data"
) 
# %>% 
# tibble()

# Reading from disk file
# release_file <- paste0("data/sivep_raw_", release_date,".csv.gz")
# srag <- data.table::fread(release_file, na.strings = c("", "NA")) %>%
#     tibble()

# srag <- vroom::vroom("data/sivep_raw_2021-05-17.csv.gz",
#                      col_types = cols(
#                          .default = col_character(),
#                          SEM_NOT = col_double(),
#                          SEM_PRI = col_double(),
#                          NU_IDADE_N = col_double(),
#                          TP_IDADE = col_double()
#                          ),
#                      )

name_file_output <- paste0("srag_adults_covid_hosp_", release_date)

## Filter: Admissions after Feb 16 (Epidemiological Week 8 - COVID in Brazil)
srag <- 
    srag %>%
    mutate(
        dt_not = as.Date(DT_NOTIFIC, format = "%d/%m/%Y")
    ) %>% 
    filter(dt_not >= as.Date("2020-02-16")) %>% 
    mutate(index = 1:n())





# Adjusting CLASSIFIN==5 (COVID) for those cases with PCR+ for SARS-COV2 but not CLASSIFIN==5,as by Ministry of Health official recommendation
# MoH has been correcting it over time, so less frequent than beggining of the pandemic
srag <- 
    srag %>% 
    mutate(
        CLASSI_FIN = case_when(
            PCR_SARS2 == 1 ~ 5L, # column Sars-CoV-2
            str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA|CIVID") & !str_detect(DS_PCR_OUT,"63|43|229|HK|RINO|SINCI|PARE") == TRUE ~ 5L, #other PCRs, Sars-CoV2
            TRUE ~ CLASSI_FIN
        )
    )


# Filter patients with COVID-19 and confirmed Hospitalization
srag_covid <- srag %>%
    filter(HOSPITAL == 1, CLASSI_FIN == 5)

rm(srag) # Deletes raw SIVEP file





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
            as.numeric(str_sub(DT_INTERNA, 7, 10)) > 2021 ~ paste0(str_sub(DT_INTERNA, 1, 6), str_sub(DT_NOTIFIC, 7, 10)),
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
    select(index, columns)

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
           OBESIDADE_m, OUT_MORBI_m, MORB_DESC,
           HOSPITAL, date_int, CO_MU_INTE, UTI, SUPORT_VEN, RES_AN, RES_IGG, RES_IGM, RES_IGA, AN_SARS2,
           PCR_RESUL, PCR_SARS2, DS_PCR_OUT, CRITERIO, PCR, CLASSI_FIN, EVOLUCAO, date_desf, date_enc, 
           n_comorb_m, n_comorb_mreal, CONT_COMORB_m, CONT_COMORB_mreal, CO_UNI_NOT, CS_ZONA,
           VACINA_COV, DOSE_1_COV, DOSE_2_COV, LAB_PR_COV, LOTE_1_COV, LOTE_2_COV, FNT_IN_COV
    ) %>% 
    mutate(
        ano_pri = lubridate::year(date_sint),
        ano_obi = lubridate::year(date_desf),
        SEM_OBI = lubridate::epiweek(date_desf)
    ) %>% 
    # Onset of symptoms (adjusting weeks for plotting)
    mutate(
        ano_pri = case_when(
            SEM_PRI == 53 ~ 2020,
            TRUE ~ as.numeric(ano_pri)
        ),
        SEM_PRI_CONT = case_when(
            ano_pri == 2021 ~ SEM_PRI + 53,
            TRUE ~ as.numeric(SEM_PRI)
        ),
        SEM_PRI_ADJ = case_when(
            ano_pri == 2020 & SEM_PRI_CONT <= 12 ~ 12,
            TRUE ~ SEM_PRI_CONT
        ),
        # SEM_PRI_GROUP = ceiling(SEM_PRI_ADJ / 4),
        ano_pri_week = case_when(
            ano_pri == 2020 ~ paste0(SEM_PRI_CONT, "/", ano_pri),
            ano_pri == 2021 ~ paste0(SEM_PRI, "/", ano_pri)
        ),
        ano_pri_week_IHM = case_when(
            ano_pri == 2020 ~ paste0(SEM_PRI_ADJ, "/", ano_pri),
            ano_pri == 2021 ~ paste0(SEM_PRI, "/", ano_pri)
        )
        
    ) %>% 
    mutate(
        FAIXA_IDADE_SIMP = if_else(FAIXA_IDADE_SIMP == "60+", ">=60", "<60")
    )  %>% 
    # Date of outcome
    mutate(
        ano_obi = case_when(
            SEM_OBI == 53 ~ 2020,
            TRUE ~ ano_obi
        ),
        SEM_OBI_CONT = case_when(
            ano_obi == 2021 ~ SEM_OBI + 53,
            TRUE ~ SEM_OBI
        ),
        # SEM_OBI_ADJ = case_when(
        #     ano_obi == 2020 & SEM_OBI_CONT <=12 ~ 12,
        #     TRUE ~ SEM_OBI_CONT
        # ),
        # SEM_OBI_GROUP = ceiling(SEM_OBI_ADJ / 4),
        ano_obi_week = case_when(
            ano_obi == 2020 ~ paste0(SEM_OBI_CONT, "/", ano_obi),
            ano_obi == 2021 ~ paste0(SEM_OBI, "/", ano_obi)
        )
    ) %>% 
    group_by(SEM_PRI_CONT) %>% 
    mutate(
        week_start = min(date_sint),
        week_end   = max(date_sint),
        CO_MU_INTE = as.character(CO_MU_INTE),
    ) %>% 
    ungroup() %>% 
    group_by(SEM_OBI_CONT) %>% 
    mutate(
        week_start_obi = min(date_desf, na.rm = TRUE)
    ) %>% 
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


# write_csv(srag_adults_covid_final, paste0("data/", name_file_output,".csv.gz"))

data.table::fwrite(srag_adults_covid_final, paste0("data/", name_file_output,".csv.gz"))

# saveRDS(srag_adults_covid_final, paste0("data/", name_file_output,".rds"))


# finished

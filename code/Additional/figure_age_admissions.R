#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Analysis of Notifications, Deaths and IHM                                                            ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### March 2021  
### Code for data preparation of covid_brazil app
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Sys.setlocale(category = "LC_ALL", locale = "english")
# Sys.setlocale("LC_ALL", "en_US.UTF-8")


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(patchwork)

release_date <- "2021-06-21"
release_file <- paste0("data/srag_adults_covid_hosp_", release_date,".csv.gz")

#### importing previous cleaned database
srag_adults_covid <-
    data.table::fread(release_file,
                      na.strings = c("", "NA")) %>%
    as_tibble() %>% 
    mutate(
        FAIXA_IDADE_SIMP = case_when(
            NU_IDADE_N < 70 ~ "<70",
            TRUE ~ ">=70" 
        )
    )


# Volume per Epidemiological Weeks ----------------------------------------

## Volume: All Admissions, Hipoxaemia, Age, NIV and IMV among 



# Number of weeks for delay
delay <- 4

max_vol_labels <- max(df_covid_data_week_all$notif) - 500

df_date_ref <-
    tibble(
        week_bp    = c(43, 53),
        week_label = c("E484 Description", "E484 Domain")
    )

df_plot_label_ref <-
    tibble(
        x = c(13, 47, 54, max(df_covid_data_week_all$week) - delay + 1),
        y = c(max_vol_labels, max_vol_labels, max_vol_labels - 10000, max_vol_labels - 10000),
        label = c("1st wave", "2nd wave", "Dominance of\nE484K mutation", "Notification delay"),
        fontface = c("bold", "bold", "plain", "plain"),
        angle = c(0, 0, 90, 90)
        # size = c(3, 3, 2)
    )





## Plot: Age group among patients with hipoxemia

# Volume per Age
df_covid_data_week_age <-  
    srag_adults_covid %>%  
    group_by(FAIXA_IDADE_SIMP, UTI, week = SEM_PRI_ADJ, ano_pri_week, week_start) %>%
    summarise(
        notif = n(),
    ) %>% 
    replace_na(list(notif = 0, sat_yes = 0)) %>%
    ungroup() %>% 
    filter(week > 12) %>% 
    pivot_wider(names_from = FAIXA_IDADE_SIMP, values_from = notif) %>% 
    mutate(
        prop_60high = `>=60` / `<60`
    ) 


df_epi_weeks_label <-
    df_covid_data_week_age %>%
    distinct(week, ano_pri_week, week_start) %>%
    filter(week %in% c(seq(13, 13*6, 13))) %>%
    arrange(week)



plot_age_prop_adm <-
    df_covid_data_week_age %>%
    ggplot() +
    geom_line(aes(x = week, y = prop_60high)) +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
                  ymin = 0, ymax = Inf),
              fill = "gray80", alpha = 0.02) +
    scale_y_continuous(labels = scales::comma_format(),
                       # limits = c(0, 45000),
                       # breaks = seq(0, 45000, 5000)
    ) +
    scale_x_continuous(breaks = df_epi_weeks_label$week,
                       labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    # scale_fill_manual(name = "Age",
    #                   values = c("#42B540FF", "#42B5407F")
    # ) +
    facet_wrap(. ~ UTI) +
    labs(
        x = "Semana epidemiológica (primeiros sintomas)",
        y = "Proporção >= 70 anos / < 70 anos",
        title = "Proporção de admissões hospitalares"
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")
        
    )



##########


df_covid_data_week_age_death <-  
    srag_adults_covid %>%  
    filter(EVOLUCAO == "Death") %>% 
    group_by(FAIXA_IDADE_SIMP, UTI, week = SEM_PRI_ADJ, ano_pri_week, week_start) %>%
    summarise(
        notif = n(),
    ) %>% 
    replace_na(list(notif = 0, sat_yes = 0)) %>%
    ungroup() %>% 
    filter(week > 12) %>% 
    pivot_wider(names_from = FAIXA_IDADE_SIMP, values_from = notif) %>% 
    mutate(
        prop_60high = `>=60` / `<60`
    ) 



plot_age_prop_death <-
    df_covid_data_week_age_death %>%
    ggplot() +
    geom_line(aes(x = week, y = prop_60high)) +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
                  ymin = 0, ymax = Inf),
              fill = "gray80", alpha = 0.02) +
    scale_y_continuous(labels = scales::comma_format(),
                       # limits = c(0, 45000),
                       # breaks = seq(0, 45000, 5000)
    ) +
    scale_x_continuous(breaks = df_epi_weeks_label$week,
                       labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    ) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    # scale_fill_manual(name = "Age",
    #                   values = c("#42B540FF", "#42B5407F")
    # ) +
    facet_wrap(. ~ UTI) +
    labs(
        x = "Semana epidemiológica (óbito)",
        y = "Proporção >= 70 anos / < 70 anos",
        title = "Proporção de óbitos hospitalares"
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")
        
    )


plot_comb_week_all <-
    (plot_age_prop_adm / plot_age_prop_death) +
    plot_annotation(title = "SRAG COVID-19",
                    caption = "Dados: SIVEP-Gripe (SRAG) 21/06/2021 \n OpenDataSUS (https://opendatasus.saude.gov.br/) \n @lslbastos")


ggsave(paste0("output/Update/fig_prop_age", release_date,".png"),
       plot = plot_comb_week_all, width = 8, height = 9,
       unit = "in", dpi = 800)

# ggsave(paste0("output/fig1_hosp_week_", release_date,".tiff"),compression = "lzw",
#        plot = plot_comb_week_all, width = 13, height = 9,
#        unit = "in", dpi = 800)




# Finished








# Volume of deaths per ICU
df_covid_data_week_age <-  
    srag_adults_covid %>%  
    filter(EVOLUCAO == "Death", SG_UF_INTE == "SP") %>% 
    group_by(FAIXA_IDADE_SIMP, UTI, week = SEM_PRI_ADJ, ano_pri_week, week_start) %>%
    summarise(
        notif = n()
        ) %>% 
    replace_na(list(notif = 0, sat_yes = 0)) %>%
    ungroup() %>% 
    filter(week > 12) %>% 
    pivot_wider(names_from = UTI, values_from = notif) %>% 
    mutate(
        prop_out = No / (Yes + No)
    ) 

plot_vol_death <-
    df_covid_data_week_age %>%
    ggplot() +
    geom_line(aes(x = week, y = prop_out)) +
    geom_smooth(aes(x = week, y = prop_out)) +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
                  ymin = 0, ymax = Inf),
              fill = "gray80", alpha = 0.02) +
    scale_y_continuous(labels = scales::comma_format(),
                       # limits = c(0, 45000),
                       # breaks = seq(0, 45000, 5000)
    ) +
    scale_x_continuous(breaks = df_epi_weeks_label$week,
                       labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    ) +
    # geom_hline(aes(yintercept = 1), linetype = "dashed") +
    # scale_fill_manual(name = "Age",
    #                   values = c("#42B540FF", "#42B5407F")
    # ) +
    facet_wrap(. ~ FAIXA_IDADE_SIMP) +
    labs(
        x = "Semana epidemiológica (óbito)",
        y = "Proporção de óbitos fora da UTI",
        title = "Óbitos hospitalares de COVID-19"
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")
        
    )









# Exporting data for shiny app --------------------------------------------
# 
# write_csv(df_covid_data_week_all, "shiny_app_sivep/app_data/df_covid_data_week_all.csv.gz")
# 
# write_csv(df_covid_data_week_age, "shiny_app_sivep/app_data/df_covid_data_week_age.csv.gz")
# 
# # saveRDS(df_covid_data_week_all, "shiny_app_sivep/app_data/df_covid_data_week_all.rds")
# # 
# # saveRDS(df_covid_data_week_age, "shiny_app_sivep/app_data/df_covid_data_week_age.rds")
# 
# write_csv(df_covid_data_week_all, "input/app_data/df_covid_data_week_all.csv.gz")
# 
# write_csv(df_covid_data_week_age, "input/app_data/df_covid_data_week_age.csv.gz")
# 
# # saveRDS(df_covid_data_week_all, "input/app_data/df_covid_data_week_all.rds")
# # 
# # saveRDS(df_covid_data_week_age, "input/app_data/df_covid_data_week_age.rds")
# 
# delay <- 4
# 
# # Metadata
# df_metadat_volume <-
#     bind_rows(
#         srag_adults_covid, 
#         srag_adults_covid %>% 
#             mutate(REGIAO = "Brazil"),
#         srag_adults_covid %>% 
#             mutate(REGIAO = SG_UF_INTE)
#     ) %>% 
#     group_by(REGIAO) %>% 
#     summarise(
#         total = n(),
#         total_outcome = sum(EVOLUCAO != "Ongoing"),
#         deaths = sum(EVOLUCAO == "Death"),
#         total_delay = sum(SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay)),
#         death_delay = sum(SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay) & EVOLUCAO == "Death"),
#     ) %>% 
#     mutate(
#         last_notif_update = max(srag_adults_covid$date_not, na.rm = T)
#     )
# 
# 
# write_csv(df_metadat_volume, "shiny_app_sivep/app_data/df_metadat_volume.csv.gz")
# 
# write_csv(df_metadat_volume, "input/app_data/df_metadat_volume.csv.gz")

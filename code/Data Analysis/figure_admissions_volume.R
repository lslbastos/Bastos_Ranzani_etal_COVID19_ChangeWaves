#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Analysis of Notifications, Deaths and IHM                                                            ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### March 2021                                                                                           ##
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(patchwork)


#### importing previous cleanned database
srag_adults_covid <-
    data.table::fread("input/srag_adults_covid_2021-04-12_hosp.csv.gz", 
                      na.strings = c("", "NA")) %>%
    as_tibble()
    


### Reference dates from E484 mutation (outbreak.info)
# S:E484K mutation description: the date/week with the first sharp increase of prevalence 
#                   (around October 09 or 10, 2020 - Epi. Week 43/2020)
# S:E484K mutation dominance: the date/week with prevalence over 50% of samples
#                 (around December 28 or 29, 2020 - Epi. Week 53/2020)

df_date_ref <- 
    tibble(
        week_bp    = c(43, 53),
        week_label = c("E484 Description", "E484 Domain")
    )

df_plot_label_ref <- 
    tibble(
        x = c(13, 47, 57),
        y = c(40000, 40000, 38000),
        label = c("1st wave", "2nd wave", "Dominance\nE484K mutation"),
        fontface = c("bold", "bold", "plain")
        # size = c(3, 3, 2)
    )



delay <- 4

# Volume per Epidemiological Weeks ----------------------------------------

## Volume: All Admissions, Hipoxemia, Age, NIV and IMV among 
df_covid_data_week_all <-
    srag_adults_covid %>% 
    bind_rows(
        srag_adults_covid %>% 
            mutate(REGIAO = "Brazil")
    ) %>% 
    group_by(REGIAO, week = SEM_PRI_CONT, ano_pri_week) %>%
    summarise(
        notif  = n(),
        sat_missing_no = sum(is.na(SATURACAO_m) | SATURACAO_m == "No", na.rm = TRUE),
        sat_yes = sum(SATURACAO_m == "Yes", na.rm = TRUE),
        niv = sum(SUPORT_VEN == "Non-invasive", na.rm = TRUE),
        imv = sum(SUPORT_VEN == "Invasive", na.rm = TRUE)
    ) %>% 
    replace_na(list(notif = 0, sat = 0)) %>%
    ungroup() %>% 
    left_join(
        srag_adults_covid %>% 
            group_by(week = SEM_OBI_CONT, ano_obi_week) %>%
            summarise(
                deaths  = sum(EVOLUCAO == "Death"),
            ),
        by = c("week" = "week",
               "ano_pri_week" = "ano_obi_week")
    ) %>% 
    replace_na(list(notif = 0, sat = 0, deaths = 0))




## Plot settings for 'x' axis - Epidemiological weeks
week_range <- c(seq(13, 46, 13), 53, max(df_covid_data_week_all$week))
epi_weeks_label <- unique(df_covid_data_week_all$ano_pri_week[df_covid_data_week_all$week %in% week_range])




## Plot: All Admissions and Hipoxemia
plot_covid_week_notif_sat <-
    df_covid_data_week_all %>% 
    filter(REGIAO == "Brazil") %>% 
    select(week, sat_yes, sat_missing_no) %>% 
    pivot_longer(-week, names_to = "sat", values_to = "val") %>% 
    mutate(
        sat = factor(sat, 
                     levels = c("sat_missing_no", "sat_yes"),
                     labels = c("All admissions", "Hypoxaemia (SaO2 < 95%)")
                     )
    ) %>%
    ggplot() +
    geom_rect(
        aes(xmin = max(week) - delay, xmax = 53, 
            ymin = 0, ymax = Inf), fill = "lightyellow"
    ) + 
    geom_area(aes(x = week, y = val, fill = sat), position = "stack") +
    scale_y_continuous(labels = scales::comma_format(),
                       limits = c(0, 45000),
                       breaks = seq(0, 45000, 5000)) +
    scale_x_continuous(breaks = week_range,
                       labels = epi_weeks_label
    ) + 
    scale_fill_manual(name = "Hospital Admissions",
                      values = rev(c("#0099B47F", "#0099B4FF"))
                      ) +
    geom_vline(data = df_date_ref, aes(xintercept = 43), linetype = "dashed") +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week), 
                  ymin = min(val), ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    geom_text(data = df_plot_label_ref, 
              aes(x = x, y = y, label = label, fontface = fontface), size = 3.5) +
    labs(
        x = "Epidemiological Weeks",
        y = "Number of admissions",
        title = ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")
    )






## Plot: Respiratory Support (NIV and IMV) among patients with hipoxemia
plot_covid_week_resp_supp <-
    df_covid_data_week_all %>% 
    filter(REGIAO == "Brazil") %>% 
    select(week, niv, imv) %>%
    pivot_longer(-week, names_to = "resp_support", values_to = "val") %>% 
    mutate(
        resp_support = factor(resp_support, 
                              levels = c("imv", "niv"),
                              labels = c("Invasive", "Non-invasive")
                              )
    ) %>% 
    ggplot() +
    geom_area(aes(x = week, y = val, fill = resp_support), position = "stack") +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week), 
                  ymin = 0, ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    scale_fill_manual(name = "Respiratory Support",
                      values = rev(c("#AD002A99", "#AD002AFF"))
                      ) +
    scale_y_continuous(labels = scales::comma_format(), 
                       limits = c(0, 35000),
                       breaks = seq(0, 35000, 5000)) +
    scale_x_continuous(breaks = week_range,
                       labels = epi_weeks_label
                       ) + 
    labs(
        x = "Epidemiological Weeks",
        y = "Number of admissions",
        title = ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")
    )




## Plot: All Admissions and Hipoxemia
plot_covid_week_death <-
    df_covid_data_week_all %>% 
    filter(REGIAO == "Brazil") %>% 
    ggplot() +
    geom_area(aes(x = week, y = deaths, fill = "Deaths")) +
    scale_y_continuous(labels = scales::comma_format(),
                       limits = c(0, 16000),
                       breaks = seq(0, 16000, 2000),
                       ) +
    scale_x_continuous(breaks = week_range,
                       labels = epi_weeks_label
                       ) + 
    scale_fill_manual(name = "In-hospital deaths",
                      labels = c(""),
                      values = c("#925E9FFF")
                      ) +
    geom_rect(aes(xmin = max(week) - 2, xmax = max(week),
                  ymin = 0, ymax = Inf),
              fill = "gray80", alpha = 0.02) +
    labs(
        x = "Epidemiological Weeks",
        y = "Number of deaths",
        title = ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold"),
        legend.key = element_blank(),
        legend.background = element_blank()
    )




















 ## Plot: Age group among patients with hipoxemia

# Volume per Age
df_covid_data_week_age <-  
    srag_adults_covid %>% 
    bind_rows(
        srag_adults_covid %>% 
            mutate(REGIAO = "Brazil")
    ) %>% 
    group_by(REGIAO, FAIXA_IDADE_SIMP, week = SEM_PRI_CONT, ano_pri_week) %>%
    summarise(
        notif = n(),
    ) %>% 
    replace_na(list(notif = 0, sat_yes = 0)) %>%
    ungroup() 


# Plot with volume per age
plot_covid_week_age <-
    df_covid_data_week_age %>% 
    filter(REGIAO == "Brazil") %>% 
    mutate(
        FAIXA_IDADE_SIMP = factor(FAIXA_IDADE_SIMP, 
                                  levels = c(">=60",
                                             "<60"),
                                  labels = c(">=60 years",
                                             "<60 years")
                                  )
    ) %>%
    ggplot() +
    geom_area(aes(x = week, y = notif, fill = FAIXA_IDADE_SIMP), position = "stack") +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week), 
                  ymin = 0, ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    scale_y_continuous(labels = scales::comma_format(), 
                       limits = c(0, 45000),
                       breaks = seq(0, 45000, 5000)
                       ) +
    scale_x_continuous(breaks = week_range,
                       labels = epi_weeks_label
                       ) + 
    scale_fill_manual(name = "Age",
                      values = c("#42B540FF", "#42B5407F")
                      ) +
    labs(
        x = "Epidemiological Weeks",
        y = "Number of admissions",
        title = ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.title = element_text(size = 10, face = "bold")

    )
        



plot_comb_week_all <-
    (plot_covid_week_notif_sat /
         (plot_covid_week_age +
              plot_covid_week_resp_supp + 
              plot_covid_week_death) ) 


ggsave("output/fig1_hosp_week_2021-04-12.pdf",
       plot = plot_comb_week_all, width = 12, height = 8,
       unit = "in", dpi = 800)




# Finished




# Exporting data for shiny app --------------------------------------------

write_csv(df_covid_data_week_all, "shiny_app_sivep/app_data/df_covid_data_week_all.csv.gz")

write_csv(df_covid_data_week_age, "shiny_app_sivep/app_data/df_covid_data_week_age.csv.gz")

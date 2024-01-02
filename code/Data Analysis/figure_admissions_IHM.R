#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Analysis of Notifications and In-hospital mortality                                                  ##
### Coding Leonardo S.L. Bastos (@lslbastos), Otavio T. Ranzani (@oranzani)                              ##
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


release_date <- "2021-05-24"
release_file <- paste0("data/srag_adults_covid_hosp_", release_date,".csv.gz")


#### importing previous cleanned database
srag_adults_covid <-
    data.table::fread(release_file,
                      na.strings = c("", "NA")) %>%
    as_tibble()



### Reference dates from E484 mutation (outbreak.info)
# E484 description: the date/week with the first sharp increase of prevalence 
#                   (around October 09 or 10, 2020 - Epi. Week 43/2020)
# E484 dominance: the date/week with prevalence over 50% of samples
#                 (around December 28 or 29, 2020 - Epi. Week 53/2020)

df_date_ref <- 
    tibble(
        week_bp    = c(43, 53),
        week_label = c("E484 Description", "E484 Domain")
    )

df_plot_label_ref <- 
    tibble(
        x = c(13, 47, 60),
        y = c(0.75, 0.75, 0.70),
        label = c("1st wave", "2nd wave", "Dominance of\nE484K mutation"),
        fontface = c("bold", "bold", "plain")
        # size = c(3, 3, 2)
    )


delay <- 4


# In-hospital Mortality ---------------------------------------------------


## Overall in-hospital mortality and stratified by Hipoxemia and Resp. Support
df_covid_ihm_week <-  
    srag_adults_covid %>% 
    bind_rows(
        srag_adults_covid, 
        srag_adults_covid %>% 
            mutate(REGIAO = "Brazil"),
        srag_adults_covid %>% 
            mutate(REGIAO = SG_UF_INTE)
    ) %>% 
    mutate(
        week_start = case_when(
            SEM_PRI_ADJ == 12 ~ as.Date("2020-03-15"), ## Adjusting week_start for combined weeks <=12/2020
            TRUE ~ as.Date(week_start)

        )
    ) %>%
    group_by(REGIAO, week = SEM_PRI_ADJ, ano_pri_week_IHM, week_start) %>% 
    summarise(
        total_outcome = sum(EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        deaths = sum(EVOLUCAO == "Death", na.rm = TRUE),
        # ihm    = sum(EVOLUCAO == "Death", na.rm = TRUE) / sum(EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        
        total_sat_yes  = sum(SATURACAO_m == "Yes" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        deaths_sat_yes = sum(SATURACAO_m == "Yes" & EVOLUCAO == "Death", na.rm = TRUE),
        # ihm_sat_yes    = sum(SATURACAO_m == "Yes" & EVOLUCAO == "Death", na.rm = TRUE) / sum(SATURACAO_m == "Yes" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        
        total_niv  = sum(SUPORT_VEN == "Non-invasive" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        deaths_niv = sum(SUPORT_VEN == "Non-invasive" & EVOLUCAO == "Death", na.rm = TRUE),
        # ihm_niv    = sum(SUPORT_VEN == "Non-invasive" & EVOLUCAO == "Death", na.rm = TRUE) / sum(SUPORT_VEN == "Non-invasive" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        
        total_imv  = sum(SUPORT_VEN == "Invasive" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        deaths_imv = sum(SUPORT_VEN == "Invasive" & EVOLUCAO == "Death", na.rm = TRUE),
        # ihm_imv    = sum(SUPORT_VEN == "Invasive" & EVOLUCAO == "Death", na.rm = TRUE) / sum(SUPORT_VEN == "Invasive" & EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE)
    ) %>% 
    ungroup() %>%
    mutate(
        ihm = deaths / total_outcome,
        ihm_sat_yes = deaths_sat_yes / total_sat_yes,
        ihm_niv = deaths_niv / total_niv,
        ihm_imv = deaths_imv / total_imv
    ) %>% 
    replace_na(list(ihm = 0, ihm_sat_yes = 0, ihm_niv = 0, ihm_imv = 0)) %>% 
    group_by(REGIAO, week) %>% 
    # slice(2:n()) %>% 
    ungroup()



## Plot settings for 'x' axis - Epidemiological weeks
# week_range <- c(seq(13, 46, 13), 53, max(df_covid_ihm_week$week))
# epi_weeks_label <- unique(df_covid_ihm_week$ano_pri_week_IHM[df_covid_ihm_week$week %in% week_range])

df_epi_weeks_label_IHM <-
    df_covid_ihm_week %>% 
    distinct(week, ano_pri_week_IHM, week_start) %>% 
    filter(week %in% c(seq(13, 13*5, 13))) %>% 
    arrange(week)



# Plot: In-hospital mortality - all admissions and hipoxemia
plot_covid_week_ihm_sat <-
    df_covid_ihm_week %>% 
    filter(REGIAO == "Brazil") %>% 
    select(week, ihm, ihm_sat_yes) %>% 
    pivot_longer(-week, names_to = "sat", values_to = "val") %>% 
    mutate(
        sat = factor(sat, 
                     levels = c("ihm", "ihm_sat_yes"),
                     labels = c("All admissions", "Hypoxaemia (SaO2 < 95%)")
                     )
    ) %>% 
    filter(!is.nan(val)) %>% 
    ggplot() +
    geom_rect(
        aes(xmin = max(week) - delay, xmax = 53,
            ymin = 0, ymax = Inf), fill = "lightyellow"
    ) +
    geom_line(aes(x = week, y = val, color = sat), size = 1) +
    geom_smooth(aes(x = week, y = val, group = sat), 
                size = 0.65, linetype = "dashed", se = FALSE) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.75),
                       breaks = c(0, 0.25, 0.5, 0.75)
                       ) +
    scale_x_continuous(breaks = df_epi_weeks_label_IHM$week,
                       labels = format(df_epi_weeks_label_IHM$ano_pri_week_IHM, format = "%d/%b/%y")
                       ) + 
    scale_color_manual(name = "Hospital Admissions",
                       values = c("#0099B47F", "#0099B4FF")
                       ) +
    geom_vline(data = df_date_ref, aes(xintercept = 43), linetype = "dashed") +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
                  ymin = 0, ymax = Inf), fill = "gray80", 
              alpha = 0.02) +
    geom_text(data = df_plot_label_ref, 
              aes(x = x, y = y, label = label, fontface = fontface), size = 3) +
    labs(
        x = "Epidemiological Weeks (onset of symptoms)",
        y = "In-hospital Mortality"
    ) +
    theme_classic() +
    theme(
        title = element_text(size = 9),
        legend.position = "top"
    )





# Plot: In-hospital mortality - Respiratory Support (NIV and IMV)
plot_covid_week_ihm_resp_supp <-
    df_covid_ihm_week %>% 
    filter(REGIAO == "Brazil") %>% 
    select(week, ihm_niv, ihm_imv) %>%
    pivot_longer(-week, names_to = "resp_support", values_to = "val") %>% 
    mutate(
        resp_support = factor(resp_support, 
                              levels = c("ihm_niv", "ihm_imv"),
                              labels = c("Non-invasive", "Invasive"))
    ) %>%
    ggplot() +
    geom_line(aes(x = week, y = val, color = resp_support), size = 1) +
    geom_smooth(aes(x = week, y = val, group = resp_support), 
                size = 0.65, linetype = "dashed", se = FALSE) +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week), 
                  ymin = 0, ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    scale_color_manual(name = "Respiratory Support",
                       values = c("#AD002A7F", "#AD002AFF")
                       ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks = df_epi_weeks_label_IHM$week,
                       labels = format(df_epi_weeks_label_IHM$ano_pri_week_IHM, format = "%d/%b/%y")
    ) + 
    geom_vline(data = df_date_ref, aes(xintercept = 43), linetype = "dashed") +
    labs(
        x = "Epidemiological Weeks (onset of symptoms)",
        y = "In-hospital mortality"
    ) +
    theme_classic() +
    theme(
        title = element_text(size = 9),
        legend.position = "top"
    )





## In-hospital mortality per age group
df_covid_ihm_week_age <-  
    bind_rows(
        srag_adults_covid, 
        srag_adults_covid %>% 
            mutate(REGIAO = "Brazil"),
        srag_adults_covid %>% 
            mutate(REGIAO = SG_UF_INTE)
    ) %>% 
    mutate(
        week_start = case_when(
            SEM_PRI_ADJ == 12 ~ as.Date("2020-03-15"), ## Adjusting week_start for combined weeks <=12/2020
            TRUE ~ as.Date(week_start)
            
        )
    ) %>%
    group_by(REGIAO, FAIXA_IDADE_SIMP, week = SEM_PRI_ADJ, ano_pri_week_IHM, week_start) %>% 
    summarise(
        total_outcome = sum(EVOLUCAO %in% c("Death", "Discharge"), na.rm = TRUE),
        deaths = sum(EVOLUCAO == "Death", na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    mutate(
        ihm = deaths / total_outcome
    ) %>% 
    replace_na(list(ihm = 0))


# Plot: In-hospital mortality per age group
plot_covid_week_ihm_age <-
    df_covid_ihm_week_age %>% 
    filter(REGIAO == "Brazil") %>% 
    mutate(
        FAIXA_IDADE_SIMP = factor(FAIXA_IDADE_SIMP, 
                                  levels = c("<60", ">=60"),
                                  labels = c("<60 years", "\u2265 60 years"),
                                  )
    ) %>% 
    ggplot() +
    geom_line(aes(x = week, y = ihm, color = FAIXA_IDADE_SIMP), size = 1) +
    geom_smooth(aes(x = week, y = ihm, group = FAIXA_IDADE_SIMP), 
                size = 0.65,linetype = "dashed", se = FALSE) +
    geom_rect(aes(xmin = max(week) - delay, xmax = max(week), 
                  ymin = 0, ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    scale_color_manual(name = "Age",
                       values = c("#42B5407F", "#42B540FF"),
                       ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.8)) +
    scale_x_continuous(breaks = df_epi_weeks_label_IHM$week,
                       labels = format(df_epi_weeks_label_IHM$ano_pri_week_IHM, format = "%d/%b/%y")
    ) + 
    geom_vline(data = df_date_ref, aes(xintercept = 43), linetype = "dashed") +
    labs(
        x = "Epidemiological Weeks (onset of symptoms)",
        y = "In-hospital Mortality"
    ) +
    theme_classic() +
    theme(
        title = element_text(size = 9),
        legend.position = "top"
    )




plot_comb_week_all <-
    (plot_covid_week_ihm_sat / plot_covid_week_ihm_age / plot_covid_week_ihm_resp_supp)


ggsave(paste0("output/Correspondence/fig_plot_comb_IHM_", release_date,".png"),
       plot = plot_comb_week_all, width = 6, height = 11,
       unit = "in", dpi = 800)




# Finished





# Exporting data for shiny app --------------------------------------------

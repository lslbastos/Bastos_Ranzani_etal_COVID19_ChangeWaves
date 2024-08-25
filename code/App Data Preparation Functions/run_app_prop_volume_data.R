run_app_prop_volume_data <- function(df) {
    library(tidyverse)

    #### importing previous cleaned database
    srag_adults_covid <- df %>% 
        mutate(
            FAIXA_IDADE_SIMP = case_when(NU_IDADE_N <= 59 ~ "<60",
                                         TRUE ~ ">=60")
        )
    
    
    # Volume per Epidemiological Weeks ----------------------------------------
    
    ## Volume: All Admissions, Hipoxaemia, Age, NIV and IMV among 
    df_covid_data_week_all <-
        bind_rows(
            srag_adults_covid, 
            srag_adults_covid %>% 
                mutate(REGIAO = "Brazil"),
            srag_adults_covid %>% 
                mutate(REGIAO = SG_UF_INTE)
        ) %>% 
        group_by(REGIAO, week = SEM_PRI_CONT, ano_pri_week, week_start) %>%
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
            bind_rows(
                srag_adults_covid %>% 
                    group_by(REGIAO, week = SEM_OBI_CONT, ano_obi_week) %>%
                    summarise(
                        deaths  = sum(EVOLUCAO == "Death")
                    ),
                srag_adults_covid %>% 
                    mutate(REGIAO = "Brazil") %>% 
                    group_by(REGIAO, week = SEM_OBI_CONT, ano_obi_week) %>%
                    summarise(
                        deaths  = sum(EVOLUCAO == "Death")
                    ),
                srag_adults_covid %>% 
                    mutate(REGIAO = SG_UF_INTE) %>% 
                    group_by(REGIAO, week = SEM_OBI_CONT, ano_obi_week) %>%
                    summarise(
                        deaths  = sum(EVOLUCAO == "Death")
                    )
            ),
            by = c("REGIAO" = "REGIAO",
                   "week" = "week",
                   "ano_pri_week" = "ano_obi_week")
        ) %>% 
        replace_na(list(notif = 0, sat = 0, deaths = 0))
    
    
    
    ### Reference dates from E484 mutation (outbreak.info)
    # S:E484K mutation description: the date/week with the first sharp increase of prevalence 
    #                   (around October 09 or 10, 2020 - Epi. Week 43/2020)
    # S:E484K mutation dominance: the date/week with prevalence over 50% of samples
    #                 (around December 28 or 29, 2020 - Epi. Week 53/2020)
    
    
    # Number of weeks for delay
    # delay <- 4
    # 
    # max_vol_labels <- max(df_covid_data_week_all$notif) - 500
    # 
    # df_date_ref <-
    #     tibble(
    #         week_bp    = c(43, 53),
    #         week_label = c("E484 Description", "E484 Domain")
    #     )
    # 
    # df_plot_label_ref <-
    #     tibble(
    #         x = c(13, 47, 54, max(df_covid_data_week_all$week) - delay + 1),
    #         y = c(max_vol_labels, max_vol_labels, max_vol_labels - 10000, max_vol_labels - 10000),
    #         label = c("1st wave", "2nd wave", "Dominance of\nE484K mutation", "Notification delay"),
    #         fontface = c("bold", "bold", "plain", "plain"),
    #         angle = c(0, 0, 90, 90)
    #         # size = c(3, 3, 2)
    #     )
    # 
    
    
    
    
    
    ## Plot settings for 'x' axis - Epidemiological weeks
    # week_range <- c(seq(13, 46, 13), 53, max(df_covid_data_week_all$week))
    # epi_weeks_label <- unique(df_covid_data_week_all$ano_pri_week[df_covid_data_week_all$week %in% week_range])
    
    
    # df_epi_weeks_label <-
    #     df_covid_data_week_all %>%
    #     distinct(week, ano_pri_week, week_start) %>%
    #     filter(week %in% c(seq(13, 13*6, 13))) %>%
    #     arrange(week)
    # 
    
    
    ## Plot: All Admissions and Hipoxemia
    # plot_covid_week_notif_sat <-
    #     df_covid_data_week_all %>%
    #     filter(REGIAO == "Brazil") %>%
    #     select(week, sat_yes, sat_missing_no) %>%
    #     pivot_longer(-week, names_to = "sat", values_to = "val") %>%
    #     mutate(
    #         sat = factor(sat,
    #                      levels = c("sat_missing_no", "sat_yes"),
    #                      labels = c("All admissions", "Hypoxaemia (SaO2 < 95%)")
    #         )
    #     ) %>%
    #     ggplot() +
    #     geom_rect(
    #         aes(xmin = max(week) - delay, xmax = 53,
    #             ymin = 0, ymax = Inf), fill = "lightyellow"
    #     ) +
    #     geom_area(aes(x = week, y = val, fill = sat), position = "stack") +
    #     scale_y_continuous(labels = scales::comma_format()
    #                        # limits = c(0, 45000),
    #                        # breaks = seq(0, 45000, 5000)
    #     ) +
    #     scale_x_continuous(breaks = df_epi_weeks_label$week,
    #                        labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    #     ) +
    #     scale_fill_manual(name = "Hospital Admissions",
    #                       values = rev(c("#0099B47F", "#0099B4FF"))
    #     ) +
    #     geom_vline(data = df_date_ref, aes(xintercept = 43), linetype = "dashed") +
    #     geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
    #                   ymin = min(val), ymax = Inf),
    #               fill = "gray80", alpha = 0.02) +
    #     geom_text(data = df_plot_label_ref,
    #               aes(x = x, y = y, label = label, fontface = fontface,
    #                   angle = angle), size = 3.5) +
    #     labs(
    #         x = "Epidemiological Weeks",
    #         y = "Number of admissions",
    #         title = ""
    #     ) +
    #     theme_classic() +
    #     theme(
    #         legend.position = "top",
    #         legend.title = element_text(size = 10, face = "bold")
    #     )
    # 
    
    
    
    
    
    ## Plot: Respiratory Support (NIV and IMV) among patients with hipoxemia
    # plot_covid_week_resp_supp <-
    #     df_covid_data_week_all %>%
    #     filter(REGIAO == "Brazil") %>%
    #     select(week, niv, imv) %>%
    #     pivot_longer(-week, names_to = "resp_support", values_to = "val") %>%
    #     mutate(
    #         resp_support = factor(resp_support,
    #                               levels = c("imv", "niv"),
    #                               labels = c("Invasive", "Non-invasive")
    #         )
    #     ) %>%
    #     ggplot() +
    #     geom_area(aes(x = week, y = val, fill = resp_support), position = "stack") +
    #     geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
    #                   ymin = 0, ymax = Inf),
    #               fill = "gray80", alpha = 0.02) +
    #     scale_fill_manual(name = "Respiratory Support",
    #                       values = rev(c("#AD002A99", "#AD002AFF"))
    #     ) +
    #     scale_y_continuous(labels = scales::comma_format()
    #                        # limits = c(0, 35000),
    #                        # breaks = seq(0, 35000, 5000)
    #     ) +
    #     scale_x_continuous(breaks = df_epi_weeks_label$week,
    #                        labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    #     ) +
    #     labs(
    #         x = "Epidemiological Weeks",
    #         y = "Number of admissions",
    #         title = ""
    #     ) +
    #     theme_classic() +
    #     theme(
    #         legend.position = "top",
    #         legend.title = element_text(size = 10, face = "bold")
    #     )
    # 
    
    
    
    ## Plot: All Admissions and Hipoxemia
    # plot_covid_week_death <-
    #     df_covid_data_week_all %>%
    #     filter(REGIAO == "Brazil") %>%
    #     ggplot() +
    #     geom_area(aes(x = week, y = deaths, fill = "Deaths")) +
    #     scale_y_continuous(labels = scales::comma_format()
    #                        # limits = c(0, 16000),
    #                        # breaks = seq(0, 16000, 2000),
    #     ) +
    #     scale_x_continuous(breaks = df_epi_weeks_label$week,
    #                        labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    #     ) +
    #     scale_fill_manual(name = "In-hospital deaths",
    #                       labels = c(""),
    #                       values = c("#925E9FFF")
    #     ) +
    #     geom_rect(aes(xmin = max(week) - 2, xmax = max(week),
    #                   ymin = 0, ymax = Inf),
    #               fill = "gray80", alpha = 0.02) +
    #     labs(
    #         x = "Epidemiological Weeks",
    #         y = "Number of deaths",
    #         title = ""
    #     ) +
    #     theme_classic() +
    #     theme(
    #         legend.position = "top",
    #         legend.title = element_text(size = 10, face = "bold"),
    #         legend.key = element_blank(),
    #         legend.background = element_blank()
    #     )
    # 
    
    
    
    
    
    
    
    ## Plot: Age group among patients with hipoxemia
    
    # Volume per Age
    df_covid_data_week_age <-  
        bind_rows(
            srag_adults_covid, 
            srag_adults_covid %>% 
                mutate(REGIAO = "Brazil"),
            srag_adults_covid %>% 
                mutate(REGIAO = SG_UF_INTE)
        ) %>% 
        group_by(REGIAO, FAIXA_IDADE_SIMP, 
                 week = SEM_PRI_CONT, ano_pri_week, week_start) %>%
        summarise(
            notif = n(),
        ) %>% 
        replace_na(list(notif = 0, sat_yes = 0)) %>%
        ungroup() 
    
    
    df_covid_data_week_age_death <-  
        bind_rows(
            srag_adults_covid, 
            srag_adults_covid %>% 
                mutate(REGIAO = "Brazil"),
            srag_adults_covid %>% 
                mutate(REGIAO = SG_UF_INTE)
        ) %>% 
        filter(EVOLUCAO == "Death") %>% 
        group_by(REGIAO, FAIXA_IDADE_SIMP, week = SEM_OBI_CONT, 
                 ano_obi_week, week_start_obi) %>%
        summarise(
            deaths = n(),
        ) %>% 
        replace_na(list(notif = 0, sat_yes = 0)) %>%
        ungroup() 
    
    
    # Plot with volume per age
    # plot_covid_week_age <-
    #     df_covid_data_week_age %>%
    #     filter(REGIAO == "Brazil") %>%
    #     mutate(
    #         FAIXA_IDADE_SIMP = factor(FAIXA_IDADE_SIMP,
    #                                   levels = c(">=60",
    #                                              "<60"),
    #                                   labels = c("\u2265 60 years",
    #                                              "< 60 years")
    #         )
    #     ) %>%
    #     ggplot() +
    #     geom_area(aes(x = week, y = notif, fill = FAIXA_IDADE_SIMP), position = "stack") +
    #     geom_rect(aes(xmin = max(week) - delay, xmax = max(week),
    #                   ymin = 0, ymax = Inf),
    #               fill = "gray80", alpha = 0.02) +
    #     scale_y_continuous(labels = scales::comma_format(),
    #                        # limits = c(0, 45000),
    #                        # breaks = seq(0, 45000, 5000)
    #     ) +
    #     scale_x_continuous(breaks = df_epi_weeks_label$week,
    #                        labels = format(df_epi_weeks_label$ano_pri_week, format = "%d/%b/%y")
    #     ) +
    #     scale_fill_manual(name = "Age",
    #                       values = c("#42B540FF", "#42B5407F")
    #     ) +
    #     labs(
    #         x = "Epidemiological Weeks",
    #         y = "Number of admissions",
    #         title = ""
    #     ) +
    #     theme_classic() +
    #     theme(
    #         legend.position = "top",
    #         legend.title = element_text(size = 10, face = "bold")
    #         
    #     )
    # 
    
    
    
    # plot_comb_week_all <-
    #     (plot_covid_week_notif_sat /
    #          (plot_covid_week_age +
    #               plot_covid_week_resp_supp +
    #               plot_covid_week_death) )
    # 
    # 
    # ggsave(paste0("output/Update/fig1_hosp_week_", release_date,".png"),
    #        plot = plot_comb_week_all, width = 13, height = 9,
    #        unit = "in", dpi = 800)
    
    # ggsave(paste0("output/fig1_hosp_week_", release_date,".tiff"),compression = "lzw",
    #        plot = plot_comb_week_all, width = 13, height = 9,
    #        unit = "in", dpi = 800)
    
    
    
    
    # Finished
    
    
    
    
    # Exporting data for shiny app --------------------------------------------
    
    write_csv(df_covid_data_week_all, here::here("input", "app_data", "df_covid_data_week_all.csv.gz"))
    
    write_csv(df_covid_data_week_age, here::here("input", "app_data", "df_covid_data_week_age.csv.gz"))
    
    write_csv(df_covid_data_week_age_death, here::here("input", "app_data", "df_covid_data_week_age_death.csv.gz"))
    
    
    delay <- 4
    
    # Metadata
    df_metadat_volume <-
        bind_rows(
            srag_adults_covid, 
            srag_adults_covid %>% 
                mutate(REGIAO = "Brazil"),
            srag_adults_covid %>% 
                mutate(REGIAO = SG_UF_INTE)
        ) %>% 
        group_by(REGIAO) %>% 
        summarise(
            total = n(),
            total_outcome = sum(EVOLUCAO != "Ongoing"),
            deaths = sum(EVOLUCAO == "Death"),
            total_delay = sum(SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay)),
            death_delay = sum(SEM_PRI_ADJ <= (max(SEM_PRI_ADJ) - delay) & EVOLUCAO == "Death"),
        ) %>% 
        mutate(
            last_notif_update = max(srag_adults_covid$date_not, na.rm = T)
        )
    
    
    write_csv(df_metadat_volume, here::here("input", "app_data", "df_metadat_volume.csv.gz"))
    
}
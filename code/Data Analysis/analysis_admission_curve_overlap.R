#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Overlapping of first and second waves in Brazil                                                      ##
### Coding Otavio Ranzani (@oranzani), Leonardo Bastos (@lslbastos)                                      ##
### March 2021                                                                                           ##
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(gtsummary)
library(patchwork)


release_date <- "2021-05-24"
release_file <- paste0("input/srag_adults_covid_hosp_", release_date,".csv.gz")


#### importing previous cleanned database
srag_adults_covid <-
    data.table::fread(release_file,
                      na.strings = c("", "NA")) %>%
    as_tibble()


delay <- 4

# Overlapping of first and second waves -----------------------------------
df_period_overlap_all_sat <-
    srag_adults_covid %>% 
    filter(
        !(ano_pri == 2020 & SEM_PRI < 8)
    ) %>% 
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            TRUE ~ 2
        )
    ) %>% 
    group_by(SATURACAO_m, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    bind_rows(
        srag_adults_covid %>% 
            mutate(
                SATURACAO_m = "All"
            ) %>% 
            filter(
                !(ano_pri == 2020 & SEM_PRI < 8)
            ) %>% 
            mutate(
                period = case_when(
                    SEM_PRI_CONT <= 43 ~ 1,
                    TRUE ~ 2
                )
            ) %>% 
            group_by(SATURACAO_m, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
            summarise(
                total = n()
            )
    ) %>% 
    group_by(SATURACAO_m, period) %>% 
    mutate(
        week = 1:n()
    ) %>%
    ungroup() %>% 
    mutate(
        is_delay = (SEM_PRI_CONT >= max(SEM_PRI_CONT) - delay)
    ) %>% 
    filter(SATURACAO_m %in% c("All", "Yes")) %>% 
    mutate(
        SATURACAO_m = case_when(
            SATURACAO_m == "Yes" ~ "Hipoxaemia",
            SATURACAO_m == "All" ~ "All admissions"
        )
    )



# Overall
plot_overlap_all <-
    df_period_overlap_all_sat %>%
    mutate(
        period = factor(period, levels = c(2, 1))
    ) %>% 
    ggplot() +
    geom_ribbon(aes(x = week,
                    ymin = 0,
                    ymax = total,
                    fill = factor(period)
    ),
    size = 1, alpha = 0.6, stat = "identity") +
    geom_line(aes(x = week, 
                  y = total,
                  color = factor(period),
                  linetype = is_delay),
              size = 1) +
    scale_linetype_discrete(guide = FALSE) +
    scale_color_manual(
        values = c("black", "red"),
        guide = FALSE
    ) +
    scale_fill_manual(
        name = "", 
        labels = c("Second wave", "First wave"), 
        values = c("blue", "red")
        
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_wrap(. ~ SATURACAO_m, scales = "free", nrow = 1) +
    labs(
        x = "Weeks",
        y =  ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )




## Age
df_period_overlap_age <-
    srag_adults_covid %>% 
    filter(
        !(ano_pri == 2020 & SEM_PRI < 8)
    ) %>% 
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            TRUE ~ 2
        )
    ) %>% 
    group_by(FAIXA_IDADE_SIMP, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    group_by(FAIXA_IDADE_SIMP, period) %>% 
    mutate(
        week = 1:n()
    ) %>%
    ungroup() %>% 
    mutate(
        is_delay = (SEM_PRI_CONT >= max(SEM_PRI_CONT) - delay)
    )


plot_overlap_age <- 
    df_period_overlap_age %>%
    mutate(
        period = factor(period, levels = c(2, 1)),
        FAIXA_IDADE_SIMP = factor(FAIXA_IDADE_SIMP, 
                                  levels = c("<60", ">=60"),
                                  labels = c("<60 years", "\u2265 60 years"),
        )
    ) %>% 
    ggplot() +
    geom_ribbon(aes(x = week,
                    ymin = 0,
                    ymax = total,
                    fill = factor(period)
    ),
    size = 1, alpha = 0.6, stat = "identity") +
    geom_line(aes(x = week, 
                  y = total,
                  color = factor(period),
                  linetype = is_delay),
              size = 1) +
    scale_linetype_discrete(guide = FALSE) +
    scale_color_manual(
        values = c("black", "red"),
        guide = FALSE
    ) +
    scale_fill_manual(
        name = "", 
        labels = c("Second wave", "First wave"), 
        values = c("blue", "red")
        
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_wrap(. ~ FAIXA_IDADE_SIMP, scales = "free_y") +
    labs(
        x = "",
        y =  ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )









## Respiratory support
df_period_overlap_resp_supp <-
    srag_adults_covid %>% 
    filter(
        !(ano_pri == 2020 & SEM_PRI < 8),
        SUPORT_VEN %in% c("Non-invasive", "Invasive")
    ) %>% 
    mutate(
        period = case_when(
            SEM_PRI_CONT <= 43 ~ 1,
            TRUE ~ 2
        )
    ) %>% 
    group_by(SUPORT_VEN, period, ano_pri, SEM_PRI, SEM_PRI_CONT) %>% 
    summarise(
        total = n()
    ) %>% 
    group_by(SUPORT_VEN, period) %>% 
    mutate(
        week = 1:n()
    ) %>%
    ungroup() %>% 
    mutate(
        is_delay = (SEM_PRI_CONT >= max(SEM_PRI_CONT) - delay)

    ) 


plot_overlap_resp_support <-
    df_period_overlap_resp_supp %>%
    mutate(
        period = factor(period, levels = c(2, 1))
    ) %>% 
    ggplot() +
    geom_ribbon(aes(x = week,
                    ymin = 0,
                    ymax = total,
                    fill = factor(period)
                    ),
              size = 1, alpha = 0.6, stat = "identity") +
    geom_line(aes(x = week, 
                  y = total,
                  color = factor(period),
                  linetype = is_delay),
              size = 1) +
    scale_linetype_discrete(guide = FALSE) +
    scale_color_manual(
        values = c("black", "red"),
        guide = FALSE
    ) +
    scale_fill_manual(
        name = "", 
        labels = c("Second wave", "First wave"), 
        values = c("blue", "red")
        
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    facet_wrap(. ~ SUPORT_VEN, scales = "free_y") +
    labs(
        x = "Weeks",
        y =  ""
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )



plot_overlap_comb <-
    (plot_overlap_all /
    plot_overlap_age /
    plot_overlap_resp_support) +
    plot_layout(guides = "collect") & 
    theme(legend.position = "top")



ggsave(paste0("output/Correspondence/plot_overlap_comb_ribbon_", release_date,".png"),
       plot = plot_overlap_comb, width = 6, height = 9,
       unit = "in", dpi = 800)



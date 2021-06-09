#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Prevalence of Variants of Concern (VOC)                                                            ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### March 2021                                                                                           ##
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
Sys.setlocale(category = "LC_ALL", locale = "english")

library(tidyverse)
library(tidylog)

# Input Data --------------------------------------------------------------


## Average daily prevalence of lineage/mutation
## Data extracted from https://outbreak.info/situation-reports
##   in March 31, 2021

# Input Data
df_SE484K <- read_tsv("input/SE484K_outbreakinfo_mutation_report_data_2021-05-26.tsv")

df_P1 <- read_tsv("input/P1_outbreakinfo_mutation_report_data_2021-05-26.tsv")

df_B117 <- read_tsv("input/B117_outbreakinfo_mutation_report_data_2021-05-26.tsv")



# Combining data into a single DF
df_variants <- 
    bind_rows(
        df_SE484K %>% 
            mutate(variant = "E484K"),
        df_P1 %>% 
            mutate(variant = "P.1"),
        df_B117 %>% 
            mutate(variant = "B.1.1.7")
    )




# Plots: Average daily prevalence of variants/mutation in Time -----------------------------------
df_plot_label_ref <- 
    tibble(
        x = c("2021-01-28"),
        y = c( 1),
        label = c("Dominance of\nE484K mutation"),
        fontface = c("plain")
        # size = c(3, 3, 2)
    )

date_last_update <- format(as.Date(max(df_variants$date)), "%B %d, %Y")


plot_daily_prev <- 
    df_variants %>% 
    ggplot() +
    geom_rect(
        aes(xmin = as.Date("2020-12-29"), xmax = max(date),
            ymin = 0, ymax = Inf), fill = "lightyellow"
    ) +
    geom_line(aes(x = date, y = proportion, color = variant)) +
    scale_x_date() +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_hline(aes(yintercept = 0.5), linetype = "dashed") +
    geom_text(data = df_plot_label_ref, 
              aes(x = as.Date(x), y = y, label = label, fontface = fontface), size = 2) +
    scale_color_discrete(name = "Lineage/Mutation") +
    geom_rect(aes(xmin = max(date) - 7, xmax = max(date), 
                  ymin = min(0), ymax = Inf), 
              fill = "gray80", alpha = 0.02) +
    labs(
        x = "",
        y = "Prevalence",
        title = "Average daily prevalence",
        subtitle = paste0("Source: GISAID - SARS-CoV-2 (hCoV-19) Mutation Reports (outbreak.info/situation-reports)\n",
                          "Last update: ", date_last_update)
    ) +
    theme_classic()


ggsave(paste0("output/Correspondence/plot_daily_prev_variants_2021-05-26.png"),
       plot = plot_daily_prev, width = 7, height = 4,
       unit = "in", dpi = 800)




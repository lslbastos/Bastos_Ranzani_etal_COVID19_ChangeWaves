#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Google mobility report                                                                               ##
### Coding Leonardo S. L. Bastos (@lslbastos), Otavio T. Ranzani (@oranzani)                             ##
### March 2021                                                                                           ##
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
Sys.setlocale(category = "LC_ALL", locale = "english")

library(tidyverse)
library(tidylog)

# Input Data --------------------------------------------------------------



# Analysis of mobility - Google Mobility reports --------------------------
# Data extracted in 31/03/2021 - CSV data Brazil
# https://www.google.com/covid19/mobility/
#

# Input Data

## Importing data from URL
# source("code/Auxiliary Functions/download_google_mobility.R")
# df_mob_report <- 
#     download_google_mobility(return_df = TRUE) %>% 
#     filter(is.na(sub_region_1)) %>% 
#     select(date, ends_with("_percent_change_from_baseline")) %>% 
#     rename(
#         "Retail and Recreation" = retail_and_recreation_percent_change_from_baseline,
#         "Grocery and Pharmacy"  = grocery_and_pharmacy_percent_change_from_baseline,
#         "Parks"              = parks_percent_change_from_baseline,
#         "Transit stations"   = transit_stations_percent_change_from_baseline,
#         "Workplaces"         = workplaces_percent_change_from_baseline,
#         "Residential"        = residential_percent_change_from_baseline  
#     )


# Combining data into a single DF
df_mob_report <- 
    vroom::vroom("input/google_mobility_raw_Brazil.csv.gz") %>% 
    filter(is.na(sub_region_1)) %>%
    select(date, ends_with("_percent_change_from_baseline")) %>% 
    rename(
        "Retail and Recreation" = retail_and_recreation_percent_change_from_baseline,
        "Grocery and Pharmacy"  = grocery_and_pharmacy_percent_change_from_baseline,
        "Parks"              = parks_percent_change_from_baseline,
        "Transit stations"   = transit_stations_percent_change_from_baseline,
        "Workplaces"         = workplaces_percent_change_from_baseline,
        "Residential"        = residential_percent_change_from_baseline  
    )




# Plots: Average daily prevalence of variants/mutation in Time -----------------------------------
df_plot_label_ref <- 
    tibble(
        x = c("2020-03-02","2020-11-24", "2021-02-14"),
        y = c(1, 1, 0.9),
        label = c("First wave", "Second wave", "Dominance\nE484K mutation"),
        fontface = c("bold", "bold", "plain")
        # size = c(3, 3, 2)
    )

date_modified <- curl::curl_fetch_memory("https://www.google.com/covid19/mobility/")$modified
date_export <- format(as.Date(date_modified), "%B %d, %Y")
date_last_update <- format(as.Date(max(df_mob_report$date)), "%B %d, %Y")


plot_mobility <- 
    df_mob_report %>% 
    pivot_longer(-date, names_to = "type", values_to = "change") %>% 
    mutate(
        change = change / 100
    ) %>% 
    ggplot() +
    geom_rect(
        aes(xmin = as.Date("2020-12-29"), xmax = max(date),
            ymin = -Inf, ymax = Inf), fill = "lightyellow"
    ) +
    geom_line(aes(x = date, y = change, color = type)) +
    scale_x_date() +
    scale_y_continuous(labels = scales::percent_format()) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = as.Date("2020-10-10")), linetype = "dashed") +
    geom_text(data = df_plot_label_ref, 
              aes(x = as.Date(x), y = y, label = label, fontface = fontface), size = 2) +
    scale_color_discrete(name = "", guide = FALSE) +
    facet_wrap(. ~ type, ncol = 2) +
    labs(
        x = "",
        y = "Change from daily weekday baseline (Jan-Feb 2020)",
        title = "Daily average change in mobility",
        subtitle = paste0("Source: Google COVID-19 Community Mobility Reports (google.com/covid19/mobility/)\n",
                          "Last update: " , date_last_update, " | ",
                          "Data export: ", date_export)
        ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave(paste0("output/", as.Date(date_modified),"_plot_google_mobility.png"),
       plot = plot_mobility, width = 6.5, height = 8,
       unit = "in", dpi = 800)



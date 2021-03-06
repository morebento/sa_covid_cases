#' sa_lga_cases.R
#'
#' ben moretti 6 jan 2022
#'
#' scrapes data from covid live and covid near me then does a few calcs before plotting
#'



# libraries -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(ggthemes)
library(ggrepel)
library(scales)

# configuration -----------------------------------------------------------

# urls for data sources 

lga_cases_url <- "https://covidlive.com.au/report/cases-by-lga/sa"

air_lga_url <- "https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv"


# gather data -----------------------------------------------------------

# get the lga data 
lga_cases_data_tbl <- lga_cases_url %>%
    read_html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() 

# get the aust immunisation register data 
air_lga_tbl <- read_csv(air_lga_url) %>% 
    clean_names()

# get the descriptor from from lga data set
description_text <- lga_cases_url %>%
    read_html %>%
    html_nodes("h3") %>%
    html_text() 

# condition -----------------------------------------------------------

# tidy the lga case data from covidlive
tidy_lga_cases_data_tbl <- lga_cases_data_tbl %>%
    clean_names() %>%
    select(-var) %>%
    arrange(lga) %>%
    mutate(
        active = str_remove_all(active, ","),
        cases = str_remove_all(cases, ","),
        net = str_remove_all(net, ","),
        active = as.integer(active),
        cases = as.integer(cases),
        net = as.integer(net)
    )

# tidy the air data from covidnearme
tidy_air_lga_tbl <- air_lga_tbl %>%
    filter(state == "SA") %>%
    select(date_as_at, state, abs_name, air_first_dose_pct:abs_erp_2019_population) %>%
    drop_na(air_second_dose_pct) %>%
    slice_max(date_as_at) 


# define a lookup table for translating lga names be
lga_lookup_tbl <- tribble(
    ~covidlive, ~covidnearme, ~type,
    "Adelaide", "Adelaide (C)", "C",
    "Adelaide Hills", "Adelaide Hills (DC)", "DC",
    "Adelaide Plains", "Adelaide Plains (DC)", "DC",
    "Alexandrina", "Alexandrina (DC)", "DC",
    "Anangu Pitjantjatjara", "APY Lands", "DC",
    "Barossa", "Barossa (DC)", "DC",
    "Barunga West", "Barunga West (DC)", "DC",
    "Berri and Barmera", "Berri and Barmera (DC)", "DC",
    "Burnside", "Burnside (C)", "C",
    "Campbelltown", "Campbelltown (C) (SA)", "C",
    "Ceduna", "", "",
    "Charles Sturt", "Charles Sturt", "C",
    "Clare and Gilbert Valleys", "Clare and Gilbert Valleys (DC)", "DC",
    "Cleve","", "",
    "Coober Pedy","", "",
    "Copper Coast", "Copper Coast (DC)", "DC",
    "Elliston","", "",
    "Flinders Ranges", "Flinders Ranges (DC)", "DC",
    "Franklin Harbour","", "",
    "Gawler", "Gawler (T)", "T",
    "Goyder", "Goyder (DC)", "DC",
    "Grant","", "",
    "Holdfast Bay", "Holdfast Bay (C)", "C",
    "Interstate / Overseas","", "",
    "Kangaroo Island","", "",
    "Karoonda East Murray", "Karoonda East Murray (DC)", "DC",
    "Kimba","", "",
    "Kingston", "Kingston (DC) (SA)", "DC",
    "Light", "Light (RegC)", "RegC",
    "Lower Eyre Peninsula","", "",
    "Loxton Waikerie", "Loxton Waikerie (DC)", "DC",
    "Maralinga Tjarutja","", "",
    "Marion", "Marion (C)", "C",
    "Mid Murray", "Mid Murray (DC)", "DC",
    "Mitcham", "Mitcham (C)", "C",
    "Mount Barker", "Mount Barker (DC)", "DC",
    "Mount Gambier", "Mount Gambier (C) & Grant (DC)", "C",
    "Mount Remarkable", "Mount Remarkable (DC)", "DC",
    "Murray Bridge", "Murray Bridge (RC)", "RC",
    "Naracoorte and Lucindale", "Naracoorte and Lucindale (DC)", "DC",
    "Northern Areas", "Northern Areas (DC)", "DC",
    "Norwood Payneham St Peters", "Norwood Payneham St Peters (C)", "C",
    "Onkaparinga", "Onkaparinga (C)", "C",
    "Orroroo/Carrieton", "Orroroo/Carrieton (DC)", "DC",
    "Peterborough", "Peterborough (DC)", "DC",
    "Playford", "Playford (C)", "C",
    "Port Adelaide Enfield", "Port Adelaide Enfield (C)", "C",
    "Port Augusta", "Port Augusta (C)", "C",
    "Port Lincoln","", "",
    "Port Pirie City and Dists", "Port Pirie City and Dists (M)", "M",
    "Prospect", "Prospect (C)", "C",
    "Renmark Paringa", "Renmark Paringa (DC)", "DC",
    "Robe", "Robe (DC)", "DC",
    "Roxby Downs","", "",
    "Salisbury", "Salisbury (C)", "C",
    "Southern Mallee","", "",
    "Streaky Bay","", "",
    "Tatiara", "Tatiara (DC)", "DC",
    "Tea Tree Gully", "Tea Tree Gully (C)", "C",
    "The Coorong", "The Coorong (DC)", "DC",
    "Tumby Bay","", "",
    "Unincorporated SA","", "",
    "Unley", "Unley (C)", "C",
    "Victor Harbor", "Victor Harbor (C)", "C",
    "Wakefield", "Wakefield (DC)", "DC",
    "Walkerville", "Walkerville (M)", "M",
    "Wattle Range", "Wattle Range (DC)", "DC",
    "West Torrens", "West Torrens (C)", "C",
    "Whyalla", "Whyalla (C)", "C",
    "Wudinna","", "",
    "Yankalilla", "Yankalilla (DC)", "DC",
    "Yorke Peninsula", "Yorke Peninsula (DC)", "DC"
    )

# join together using the lookup table
joined_tbl <- lga_lookup_tbl %>%
    inner_join(tidy_lga_cases_data_tbl, by = c("covidlive"="lga")) %>%
    inner_join(tidy_air_lga_tbl, by = c("covidnearme"="abs_name")) %>%
    rename(
        abs_name = covidnearme
    ) %>%
    select(-covidlive)

# analysis ----------------------------------------------------------------------

# calculations
joined_tbl <- joined_tbl %>%
    
    # normalise cases per population 
    mutate(
        cases_popn_pct = cases / abs_erp_2019_population * 100
    )  %>%
    
    # reclassify lga types
    mutate(
        type_reclass = case_when(
            type == "C" ~ "Council",
            type == "DC" ~ "District Council",
            type == "M" ~ "Town",
            type == "RC" ~ "Regional Council",
            type == "RegC" ~ "Regional Council",
            type == "T" ~ "Town"
        )
    )

# visualise -----------------------------------------------------------------


# define text for subtitle and caption
subtitle_text <- str_glue("Latest data: {description_text}
                           Data sources: 
                          
                           LGA Cases {lga_cases_url}
                           AIR LGA Data {air_lga_url}")

caption_text <- str_glue("Author: @morebento. Code: https://github.com/morebento/sa_covid_cases")




# bar chart of the total cases by LGA 
sa_active_cases_by_lga_plot <- tidy_lga_cases_data_tbl %>%
    ggplot(aes(x=reorder(lga, active), active)) +
    geom_col(aes(fill="Active Cases")) +
    coord_flip() +
    theme_clean() + 
    scale_fill_tableau() +
    theme(
        legend.position = ""
    ) +
    labs(
        title = "South Australian COVID-19 Active Cases by LGA",
        subtitle = subtitle_text,
        caption = caption_text,
        x = "",
        y = "Active Cases"
    ) 

# bar chart of cases as % of pop'n
sa_active_cases_pct_by_lga_plot <- joined_tbl %>%
    ggplot(aes(x=reorder(abs_name, cases_popn_pct), cases_popn_pct)) +
    geom_col(aes(fill="Cases as Population %")) +
    coord_flip() +
    theme_clean() + 
    scale_fill_tableau() +
    theme(
        legend.position = ""
    ) +
    labs(
        title = "South Australian COVID-19 Cases as Population % by LGA",
        subtitle = subtitle_text,
        caption = caption_text,
        x = "",
        y = "Cases as Population %"
    ) 


# bubble chart of casses as % of pop'n vs vaccination rate 
sa_total_cases_pct_pop_vs_vax_rate_plot <- joined_tbl %>%
    #filter(cases_popn_pct < 4) %>%
    ggplot(aes(x=air_second_dose_pct, y=cases_popn_pct, label = abs_name, shape=type_reclass)) +
    geom_point(aes(size=cases, colour=abs_erp_2019_population)) +
    geom_label_repel(size=3) +
    theme_clean() + 
    scale_colour_gradient_tableau(labels=comma) + # from ggthemes
    labs(
        title = "South Australian COVID-19 Infection Rate (%) vs Vaccination Second Dose (%) by Local Government Areas",
        subtitle = subtitle_text,
        caption = caption_text,
        x = "Second Dose %",
        y = "COVID Infection %",
        size="Cases",
        colour = "Population (2016 ABS)",
        shape = "Type"
    )



# export ---------------------------------------------------------------------


# save to disc
ggsave(
    sa_total_cases_pct_pop_vs_vax_rate_plot, 
    filename = "plots/sa_total_cases_pct_pop_vs_vax_rate_plot.png", 
    width=297, 
    height=210, 
    units = "mm"
)


# save to disc
ggsave(
    sa_active_cases_pct_by_lga_plot, 
    filename = "plots/sa_active_cases_pct_by_lga_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)


# save to disc
ggsave(
    sa_active_cases_by_lga_plot, 
    filename = "plots/sa_active_cases_by_lga_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)


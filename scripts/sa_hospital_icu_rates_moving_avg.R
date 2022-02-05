#' sa_hospital_icu_rates_moving_avg.R
#'
#' ben moretti, 6 jan 2022
#'
#' scrapes data for daily hospitalisation and calculates rates of change based upon 5 day moving average 
#'
#'


# libraries ------------------------------------------------------------------- 

library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)
library(ggthemes)
library(ggrepel)
library(scales)
library(tibbletime)



# configuration  ------------------------------------------------------------------- 

sa_hospital_url <- "https://covidlive.com.au/report/daily-hospitalised/sa"

# gather data  ------------------------------------------------------------------- 

# scrape and get data which is represented as a table 
sa_hospital_data_tbl <- sa_hospital_url %>%
    read_html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() 


# data tidying  ------------------------------------------------------------------- 

# tidy data
tidy_sa_hospital_data_tbl <- sa_hospital_data_tbl %>% 
    clean_names() %>%
    mutate(
        date = dmy(date)
    ) %>%
    # remove commas
    mutate(
        hosp = str_remove_all(hosp, ","),
        icu = str_remove_all(icu, ","),
        vent = str_remove_all(vent, ",")
    ) %>%
    # cast to integers
    mutate(
        hosp = as.integer(hosp),
        icu = as.integer(icu),
        vent = as.integer(vent)
    ) %>%
    arrange(date)

# get date of the latest data
latest_date <- tidy_sa_hospital_data_tbl %>%
    drop_na() %>%
    summarise(
        latest_date = max(date)
    ) %>%
    pull(latest_date)


# analysis  ------------------------------------------------------------------- 

# define a function for five day moving average
rolling_avg_fun = rollify(mean, window = 5)

# calculate 5 day moving average and rate of change
rate_sa_hospital_data_tbl <- tidy_sa_hospital_data_tbl %>%
    # calculate 5 day moving average 
    mutate(
        hosp_5_day_moving_avg = rolling_avg_fun(hosp),
        icu_5_day_moving_avg = rolling_avg_fun(icu),
        vent_5_day_moving_avg = rolling_avg_fun(vent)
    ) %>%
    # calculate rate as a product of the moving avg
    mutate(
        hosp_rate_5_day_moving_avg = hosp / hosp_5_day_moving_avg,
        icu_rate_5_day_moving_avg = icu / icu_5_day_moving_avg,
        vent_rate_5_day_moving_avg = vent / vent_5_day_moving_avg
    ) %>%
    select(
        date, hosp_rate_5_day_moving_avg, icu_rate_5_day_moving_avg, vent_rate_5_day_moving_avg
    )


# visualisation  ------------------------------------------------------------------- 


# define subtitle and caption text
subtitle_text <- str_glue("Latest data: {latest_date}
                           Data source: {sa_hospital_url}")

caption_text <- str_glue("Author: @morebento. Code: https://github.com/morebento/sa_covid_cases")

# plot raw data
tidy_sa_hospital_data_plot <- tidy_sa_hospital_data_tbl %>%
    pivot_longer(-date) %>%
    mutate(
        name = str_to_title(name)
    ) %>%
    filter(date > "2021-11-20")  %>%
    ggplot(aes(x=date, y=value)) +
    geom_line(aes(colour=name)) +
    facet_wrap(vars(name), scales = "free_y", ncol = 1) +
    geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype=4) +
    theme_clean() +
    scale_colour_tableau() +
    theme(
        legend.position = "" 
    ) +
    labs(
        title = "Hospitalisation Cases for SA Covid",
        subtitle = subtitle_text,
        colour = "Category",
        x = "Date",
        y = "Cases",
        caption = caption_text
    )

# plot rates
rate_sa_hospital_data_plot <- rate_sa_hospital_data_tbl %>%
    filter(date > "2021-11-20")  %>%
    pivot_longer(-date) %>%
    mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
    ) %>%
    ggplot(aes(x=date, y=value)) +
    geom_line(aes(colour=name)) +
    facet_wrap(vars(name), scales = "free_y", ncol = 1) +
    theme_clean() +
    scale_colour_tableau() +
    theme(
        legend.position = ""
    ) +
    labs(
        title = "Hospitalisation Rates for SA Covid",
        subtitle = subtitle_text,
        colour = "Category",
        x = "Date",
        y = "Rate",
        caption = caption_text
        
    )


# export ---------------------------------------------------------------------


# save to disc
ggsave(
    tidy_sa_hospital_data_plot, 
    filename = "plots/tidy_sa_hospital_data_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)



# save to disc
ggsave(
    rate_sa_hospital_data_plot, 
    filename = "plots/rate_sa_hospital_data_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)


#' sa_deaths.R
#'
#' ben moretti 24 jan 2022
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
library(tibbletime)


# configuration -----------------------------------------------------------

# urls for data sources 

sa_deaths_url <- "https://covidlive.com.au/report/daily-deaths/sa"


# gather data -----------------------------------------------------------

# get the lga data 
sa_deaths_tbl <- sa_deaths_url %>%
    read_html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() 


# get the descriptor from from lga data set
description_text <- sa_deaths_url %>%
    read_html %>%
    html_nodes("h3") %>%
    html_text() 



# data tidying  ------------------------------------------------------------------- 

tidy_deaths_tbl <- sa_deaths_tbl %>%
    clean_names() %>%
    select(-var) %>%
    mutate(
        date = dmy(date)
    )


# analyse ---------------------------------------------------------------------


# define a function for five day moving average
rolling_avg_fun = rollify(mean, window = 5)

# apply the moving average function
tidy_deaths_tbl <- tidy_deaths_tbl %>%
    mutate(
        daily_deaths_5_day_moving_avg = rolling_avg_fun(net)
    )


# visualise -----------------------------------------------------------------


# define text for subtitle and caption
subtitle_text <- str_glue("Latest data: {description_text}
                           Data source: {sa_deaths_url}")

caption_text <- str_glue("Author: @morebento. Code: https://github.com/morebento/sa_covid_cases")


deaths_plot <- tidy_deaths_tbl %>%
    filter(date > "2021-11-20")  %>%
    pivot_longer(-date) %>%
    mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
    ) %>%
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
        title = "SA Deaths from Covid",
        subtitle = subtitle_text,
        colour = "Category",
        x = "Date",
        y = "Cases",
        caption = caption_text
    )



# export ---------------------------------------------------------------------


# save to disc
ggsave(
    deaths_plot, 
    filename = "plots/sa_deaths_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)


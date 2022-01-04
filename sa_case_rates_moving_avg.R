#'
#' sa_case_rates_moving_avg.R
#'
#' gathers data from covidlive and calculates moving averages 
#'
#'
#' 3 jan 2022
#'
#' ben moretti
#' 
#' 
#'
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

sa_cases_url <- "https://covidlive.com.au/report/daily-cases/sa"


# gather data  ------------------------------------------------------------------- 

# scrape and get data
sa_cases_data_tbl <- sa_cases_url %>%
    read_html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() 


# data tidying  ------------------------------------------------------------------- 

# tidy data
tidy_sa_cases_data_tbl <- sa_cases_data_tbl %>% 
    clean_names() %>%
    select(-var, -net) %>%
    mutate(
        date = dmy(date)
    ) %>%
    mutate(
        new = str_remove_all(new, ","),
        cases = str_remove_all(cases, ","),
        new = str_remove_all(new, "-"),
        cases = str_remove_all(cases, "-"),
    ) %>%
    mutate(
        new = as.integer(new),
        cases = as.integer(cases)
    ) %>%
    arrange(date)

# get latest data
latest_date <- tidy_sa_cases_data_tbl %>%
    summarise(
        latest_date = max(date)
    ) %>%
    pull(latest_date)



# analysis  ------------------------------------------------------------------- 

# define a function for five day moving average
rolling_avg_fun = rollify(mean, window = 5)


# calculations
tidy_sa_cases_data_tbl <- tidy_sa_cases_data_tbl %>%
    
    # calcualte 5 day moving average new cases
    mutate(
        new_five_day_moving_avg = rolling_avg_fun(new)
    ) %>%
    
    # calculate the naive rate of change
    mutate(
        previous_cases = lag(cases, order_by = date),
        rate_naive = cases / previous_cases
    ) %>%
    
    # calculate 5 day moving average rate of change
    mutate(
        five_day_moving_avg = rolling_avg_fun(cases),
        rate_five_day_moving_avg = cases / five_day_moving_avg
    ) 


# visualisation  ------------------------------------------------------------------- 


subtitle_text <- str_glue("Latest data: {latest_date}
                           Data source: {sa_cases_url}")


caption_text <- str_glue("Author: @morebento. Code: https://github.com/morebento/sa_covid_cases")


# rate of change plot
tidy_sa_cases_data_tbl %>% 
    drop_na(new) %>%
    filter(date > "2021-08-01") %>%
    select(-previous_cases) %>%
    
    ggplot(aes(x=date)) +
    geom_line(aes(y=new, colour="New Cases")) +
    geom_line(aes(y=new_five_day_moving_avg, colour="New Cases 5 Day Moving Avg")) +
    geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype=4) +
    theme_clean() +
    scale_colour_tableau() +
    labs(
        title = "SA Covid New Cases",
        subtitle = subtitle_text,
        colour = "Cases",
        x = "Date",
        y = "Rate of Change",
        caption = caption_text
        
    )



# rate of change plot
tidy_sa_cases_data_tbl %>% 
    drop_na(new) %>%
    filter(date > "2021-08-01") %>%
    select(-previous_cases) %>%
    
    ggplot(aes(x=date)) +
    geom_line(aes(y=rate_five_day_moving_avg, colour="5 Day Moving Avg.")) +
    geom_line(aes(y=rate_naive, colour= "Previous Day")) +
    geom_vline(xintercept = as.numeric(as.Date("2021-11-20")), linetype=4) +
    theme_clean() +
    scale_colour_tableau() +
    labs(
        title = "Rate of Change for SA Covid Total Cases",
        subtitle = subtitle_text,
        colour = "Rate of Change",
        x = "Date",
        y = "Rate of Change",
        caption = caption_text
        
    )




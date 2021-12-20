#' sa_covid_cases.R
#' 
#' Ben Moretti
#' 
#' 19 Dec 2021
#'
#' grabs data from covid19nearme and plots for south australia
#'

# libraries ------------------------------------

library(tidyverse)
library(janitor)
library(scales)
library(ggthemes)

# get the data ------------------------------------

data_url <- "https://govtstats.covid19nearme.com.au/data/all.csv"

data_tbl <- read_csv(data_url)


# conditioning ------------------------------------

# only select the sa metrics and date
sa_data_tbl <- data_tbl %>%
    clean_names() %>%
    select(date, starts_with("sa_")) 

# create a standard caption footer
plot_footer <- stringr::str_glue("Author: @morebento. Data from: https://github.com/jxeeno/aust-govt-covid19-stats Code: https://github.com/morebento/sa_covid_cases")

# get the latest date
latest_date <- sa_data_tbl %>%
    select(date) %>%
    summarise(max(date)) %>%
    pull()

plot_subtitle =  str_glue("Latest Data: {latest_date}. 
                           Sourced from https://covid19nearme.com.au/state/sa")


# plots --------------------------------------------

sa_cases_plot <- sa_data_tbl %>%
    
    # only get the case related data 
    select(date, starts_with("sa_cases_")) %>% 
    
    # normalise
    pivot_longer(!date, names_to="metric", values_to = "value") %>% 
    
    # tidy up the metric names
    mutate(
        metric = str_replace_all(metric, "_", " "),
        metric = str_to_title(metric),
        metric = str_replace_all(metric, "Sa", "SA"),
        metric = str_replace_all(metric, "Icu", "ICU")
    ) %>%
    
    # do a line plot 
    ggplot(aes(date, value)) +
    geom_line(aes(colour=metric)) +
    facet_wrap(vars(metric), scales = "free_y", ncol = 2) +
    theme_clean() + 
    scale_colour_tableau() + 
    theme(legend.position = "none") +
    labs(
        title = "SA Case Data",
        subtitle = plot_subtitle,
        caption =  plot_footer,
        y = "Metric Value",
        x = "Date"
    )


# output ----------------------------------------------

# save to disc
ggsave(
    sa_cases_plot, 
    filename = "sa_cases_plot.png", 
    height=297, 
    width=210, 
    units = "mm"
)

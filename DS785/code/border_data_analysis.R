library(xgboost) # note that xgboost needs to be loaded before tidyverse bc of xgboost::slice()
library(tidyverse)
library(fs)
library(janitor)
library(skimr)
library(slider)
library(tidymodels)
library(fable)
library(tsibble)
library(fabletools)
library(feasts)
library(doParallel)
library(vip)
library(glmnet)
library(rules)
library(stacks)


options(scipen = 999)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS785")


# https://catalog.data.gov/dataset/border-crossing-entry-data-683ae
# https://www.bts.gov/explore-topics-and-geography/geography/border-crossingentry-data



#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


dir_ls("data")

# get border_data_1 ####
border_data_1 <- read_csv(file = "data/Border_Crossing_Entry_Data.csv") %>%
        clean_names() %>%
        rename(date_original = date) %>%
        mutate(date = my(date_original),
               measure = case_when(measure == "Trucks" ~ "Commercial trucks",
                                   measure == "Personal Vehicles" ~ "Personal vehicles",
                                   TRUE ~ measure))


#//////////////////////


# inspect
border_data_1
border_data_1 %>% glimpse()

border_data_1 %>% count(date, date_original) %>% print(n = nrow(.))
border_data_1 %>% count(port_name, state, border) %>% arrange(desc(n))
border_data_1 %>% count(measure) %>% arrange(desc(n))

# check date
border_data_1 %>% 
        filter(measure %in% c("Personal vehicles", "Commercial trucks", "Pedestrians")) %>%
        distinct(measure, date) %>%
        count(date) %>%
        count(n)

border_data_1 %>% 
        filter(measure %in% c("Personal vehicles", "Commercial trucks", "Pedestrians")) %>%
        group_by(measure) %>%
        count(date) %>%
        arrange(date) %>%
        slice(1)

border_data_1 %>% 
        filter(measure %in% c("Personal vehicles", "Commercial trucks", "Pedestrians")) %>%
        group_by(measure) %>%
        count(date) %>%
        arrange(desc(date)) %>%
        slice(1)


#////////////////////////////////////////////////////////////////////////////////////////////


# get border_data_2 ####
border_data_2 <- border_data_1 %>% 
        filter(measure %in% c("Personal vehicles", "Commercial trucks", "Pedestrians")) %>%
        group_by(measure, date) %>%
        summarize(monthly_count = sum(value)) %>%
        ungroup() %>%
        mutate(monthly_count_lag_1 = lag(monthly_count, n = 1),
               monthly_count_lag_2 = lag(monthly_count, n = 2),
               monthly_count_lag_3 = lag(monthly_count, n = 3),
               monthly_count_lag_4 = lag(monthly_count, n = 4),
               monthly_count_lag_5 = lag(monthly_count, n = 5),
               monthly_count_lag_6 = lag(monthly_count, n = 6),
               monthly_count_lag_7 = lag(monthly_count, n = 7),
               monthly_count_lag_8 = lag(monthly_count, n = 8),
               monthly_count_lag_9 = lag(monthly_count, n = 9),
               monthly_count_lag_10 = lag(monthly_count, n = 10),
               monthly_count_lag_11 = lag(monthly_count, n = 11),
               monthly_count_lag_12 = lag(monthly_count, n = 12),
               monthly_count_diff_2 = lag(monthly_count, n = 1) - lag(monthly_count, n = 2),
               monthly_count_diff_3 = lag(monthly_count, n = 1) - lag(monthly_count, n = 3),
               monthly_count_diff_4 = lag(monthly_count, n = 1) - lag(monthly_count, n = 4),
               monthly_count_diff_5 = lag(monthly_count, n = 1) - lag(monthly_count, n = 5),
               monthly_count_diff_6 = lag(monthly_count, n = 1) - lag(monthly_count, n = 6),
               monthly_count_diff_7 = lag(monthly_count, n = 1) - lag(monthly_count, n = 7),
               monthly_count_diff_8 = lag(monthly_count, n = 1) - lag(monthly_count, n = 8),
               monthly_count_diff_9 = lag(monthly_count, n = 1) - lag(monthly_count, n = 9),
               monthly_count_diff_10 = lag(monthly_count, n = 1) - lag(monthly_count, n = 10),
               monthly_count_diff_11 = lag(monthly_count, n = 1) - lag(monthly_count, n = 11),
               monthly_count_diff_12 = lag(monthly_count, n = 1) - lag(monthly_count, n = 12),
               monthly_count_diff_13 = lag(monthly_count, n = 1) - lag(monthly_count, n = 13),
                year = year(date),
               month = month(date),
               #  pandemic_flag = case_when(measure %in% c("Personal vehicles") & year %in% c(2020, 2021, 2022) ~ 1, 
               #                            measure %in% c("Pedestrians") & year %in% c(2020, 2021) ~ 1,
               #                            measure == "Commercial trucks" & year == 2020 & month %in% (1:6) ~ 1,
               #                            TRUE ~ 0),
               # post_pandemic_flag = case_when(year >= 2020 ~ 1, TRUE ~ 0),
               financial_crisis_flag = case_when(year == 2008 ~ 1, TRUE ~ 0),
               post_financial_crisis_flag = case_when(year >= 2008 ~ 1, TRUE ~ 0),
               post_911_flag = case_when(date >= "2001-09-01" ~ 1, TRUE ~ 0))


#///////////////////////


border_data_2
border_data_2 %>% glimpse()
border_data_2 %>% skim()

border_data_2 %>% count(measure) %>% arrange(desc(n))
border_data_2 %>%
        filter(pandemic_flag == 1) %>%
        count(measure, date) %>%
        arrange(measure, date) %>%
        print(n = nrow(.))

# plot
border_data_2 %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line()

border_data_2 %>%
        filter(measure == "Personal vehicles") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_2 %>%
        filter(measure == "Commercial trucks") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_2 %>%
        filter(measure == "Pedestrians") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#///////////////////////////////////


# plot moving_avg
border_data_2 %>%
        filter(measure == "Personal vehicles") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count_ma_3, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_2 %>%
        filter(measure == "Commercial trucks") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count_ma_3, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_2 %>%
        filter(measure == "Pedestrians") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count_ma_3, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get unemployment_rate ####


us_unemployment <- read_csv(file = "data/us_unemployment_rate.csv") %>%
                  rename(date = DATE,
                         us_unemployment_rate = UNRATENSA)

mexico_unemployment <- read_csv(file = "data/mexico_unemployment_rate.csv") %>%
                          rename(date = DATE,
                                 mexico_unemployment_rate = LRHUTTTTMXM156N)

canada_unemployment <- read_csv(file = "data/canada_unemployment_rate.csv") %>%
                          rename(date = DATE,
                                 canada_unemployment_rate = LRUNTTTTCAM156N)


#/////////////////////////


us_unemployment
mexico_unemployment
canada_unemployment


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get gdp_per_capita ####
        
us_gdp_per_capita <- read_csv(file = "data/us_gdp_per_capita.csv") %>%
        rename(date = DATE,
               annual_value = NYGDPPCAPKDUSA) %>%
        mutate(annual_diff = lead(annual_value, n = 1) - annual_value,
               monthly_diff = annual_diff / 12,
               month_1 = annual_value,
               month_2 = annual_value + (monthly_diff * 1),
               month_3 = annual_value + (monthly_diff * 2),
               month_4 = annual_value + (monthly_diff * 3),
               month_5 = annual_value + (monthly_diff * 4),
               month_6 = annual_value + (monthly_diff * 5),
               month_7 = annual_value + (monthly_diff * 6),
               month_8 = annual_value + (monthly_diff * 7),
               month_9 = annual_value + (monthly_diff * 8),
               month_10 = annual_value + (monthly_diff * 9),
               month_11 = annual_value + (monthly_diff * 10),
               month_12 = annual_value + (monthly_diff * 11)) %>%
        pivot_longer(cols = matches(match = "^month_"), names_to = "month_number", values_to = "monthly_value") %>%
        rename(date_original = date) %>%
        mutate(month = str_replace_all(string = month_number, pattern = "[A-z]*", replacement = ""),
               year = year(date_original),
               date = ymd(str_glue("{year}-{month}-01"))) %>%
        rename(us_gdp_per_capita_monthly = monthly_value)

mexico_gdp_per_capita <- read_csv(file = "data/mexico_gdp_per_capita.csv") %>%
        rename(date = DATE,
               annual_value = NYGDPPCAPKDMEX) %>%
        mutate(annual_diff = lead(annual_value, n = 1) - annual_value,
               monthly_diff = annual_diff / 12,
               month_1 = annual_value,
               month_2 = annual_value + (monthly_diff * 1),
               month_3 = annual_value + (monthly_diff * 2),
               month_4 = annual_value + (monthly_diff * 3),
               month_5 = annual_value + (monthly_diff * 4),
               month_6 = annual_value + (monthly_diff * 5),
               month_7 = annual_value + (monthly_diff * 6),
               month_8 = annual_value + (monthly_diff * 7),
               month_9 = annual_value + (monthly_diff * 8),
               month_10 = annual_value + (monthly_diff * 9),
               month_11 = annual_value + (monthly_diff * 10),
               month_12 = annual_value + (monthly_diff * 11)) %>%
        pivot_longer(cols = matches(match = "^month_"), names_to = "month_number", values_to = "monthly_value") %>%
        rename(date_original = date) %>%
        mutate(month = str_replace_all(string = month_number, pattern = "[A-z]*", replacement = ""),
               year = year(date_original),
               date = ymd(str_glue("{year}-{month}-01"))) %>%
        rename(mexico_gdp_per_capita_monthly = monthly_value)

canada_gdp_per_capita <- read_csv(file = "data/canada_gdp_per_capita.csv") %>%
        rename(date = DATE,
               annual_value = NYGDPPCAPKDCAN) %>%
        mutate(annual_diff = lead(annual_value, n = 1) - annual_value,
               monthly_diff = annual_diff / 12,
               month_1 = annual_value,
               month_2 = annual_value + (monthly_diff * 1),
               month_3 = annual_value + (monthly_diff * 2),
               month_4 = annual_value + (monthly_diff * 3),
               month_5 = annual_value + (monthly_diff * 4),
               month_6 = annual_value + (monthly_diff * 5),
               month_7 = annual_value + (monthly_diff * 6),
               month_8 = annual_value + (monthly_diff * 7),
               month_9 = annual_value + (monthly_diff * 8),
               month_10 = annual_value + (monthly_diff * 9),
               month_11 = annual_value + (monthly_diff * 10),
               month_12 = annual_value + (monthly_diff * 11)) %>%
        pivot_longer(cols = matches(match = "^month_"), names_to = "month_number", values_to = "monthly_value") %>%
        rename(date_original = date) %>%
        mutate(month = str_replace_all(string = month_number, pattern = "[A-z]*", replacement = ""),
               year = year(date_original),
               date = ymd(str_glue("{year}-{month}-01"))) %>%
        rename(canada_gdp_per_capita_monthly = monthly_value)


#////////////////////////


us_gdp_per_capita
us_gdp_per_capita %>% glimpse()
us_gdp_per_capita %>% skim

mexico_gdp_per_capita
canada_gdp_per_capita

us_gdp_per_capita %>% count(date)
us_gdp_per_capita %>% 
        filter(year(date) > 2000, year(date) < 2005) %>%
        print(n = nrow(.))

us_gdp_per_capita %>% 
        filter(year(date) > 2000, year(date) < 2005) %>%
        ggplot(data = ., mapping = aes(x = date, y = us_gdp_per_capita_monthly)) + 
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get border_data_3 ####

border_data_3 <- border_data_2 %>%
        left_join(., us_unemployment, by = c("date" = "date")) %>%
        left_join(., mexico_unemployment, by = c("date" = "date")) %>%
        left_join(., canada_unemployment, by = c("date" = "date")) %>%
        left_join(., us_gdp_per_capita %>% select(date, us_gdp_per_capita_monthly), by = c("date" = "date")) %>%
        left_join(., mexico_gdp_per_capita %>% select(date, mexico_gdp_per_capita_monthly), by = c("date" = "date")) %>%
        left_join(., canada_gdp_per_capita %>% select(date, canada_gdp_per_capita_monthly), by = c("date" = "date")) %>% 
        mutate(us_unemployment_rate_ma_6 = slide_dbl(us_unemployment_rate, .f = mean, .before = 5),
               mexico_unemployment_rate_ma_6 = slide_dbl(mexico_unemployment_rate, .f = mean, .before = 5),
               canada_unemployment_rate_ma_6 = slide_dbl(canada_unemployment_rate, .f = mean, .before = 5),
               us_unemployment_rate_diff_6 = us_unemployment_rate - lag(us_unemployment_rate, n = 6),
               mexico_unemployment_rate_diff_6 = mexico_unemployment_rate - lag(mexico_unemployment_rate, n = 6),
               canada_unemployment_rate_diff_6 = canada_unemployment_rate - lag(canada_unemployment_rate, n = 6),
               
               us_canada_gdp_per_capita_gap = us_gdp_per_capita_monthly - canada_gdp_per_capita_monthly,
               us_mexico_gdp_per_capita_gap = us_gdp_per_capita_monthly - mexico_gdp_per_capita_monthly,
               us_canada_gdp_per_capita_gap_ma_6 = slide_dbl(us_canada_gdp_per_capita_gap, .f = mean, .before = 5),
               us_mexico_gdp_per_capita_gap_ma_6 = slide_dbl(us_mexico_gdp_per_capita_gap, .f = mean, .before = 5),
               us_canada_gdp_per_capita_gap_diff_6 = us_canada_gdp_per_capita_gap - 
                       lag(us_canada_gdp_per_capita_gap, n = 6),
               us_mexico_gdp_per_capita_gap_diff_6 = us_mexico_gdp_per_capita_gap -
                       lag(us_canada_gdp_per_capita_gap, n = 6)) %>%
        arrange(date) %>%
        fill(monthly_count_diff_13, .direction = "up")

# save border_data_3 ####
border_data_3 %>% write_csv(file = "data/border_data_3.csv")

# load
border_data_3 <- read_csv(file = "data/border_data_3.csv")


#///////////////////               


border_data_3
border_data_3 %>% glimpse()
border_data_3 %>% skim()
border_data_3 %>%
        filter(year(date) >= 1997) %>%
        skim()

border_data_3 %>%
        select(date, matches(match = ".*gdp_per_capita.*")) %>%
        slice(1:20)

# note that first year of data will be cutoff once arima_forecast covariates are created
# so that lagged variables don't have NAs; for now the lagged vars still have some NAs

# after dropping last year, only exception is monthly_count_diff_13 
# which was missing a single value even, so i filled it backward
# only remaining NA values are for recent gdp_per_capita, which isn't problem as explained below
border_data_2 %>% count(date)
border_data_3 %>% count(date)
border_data_3 %>% count(date) %>% arrange(desc(date))

border_data_3 %>% count(year) %>% print(n = nrow(.))

# note that gdp_per_capita data only goes up to 2023-01-01, but thats fine
# because 2024 data will be cut off as testing, and so will predict 
# with arima-forecasted covariates only
border_data_3 %>% 
        filter(is.na(us_canada_gdp_per_capita_gap)) %>%
        select(date, us_canada_gdp_per_capita_gap) %>% 
        count(date)


#//////////////////


# plot
border_data_3 %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line()

border_data_3 %>%
        filter(measure == "Personal vehicles") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_3 %>%
        filter(measure == "Commercial trucks") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


border_data_3 %>%
        filter(measure == "Pedestrians") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_data ####

trucks_training_data <- border_data_3 %>%
        filter(date >= "1997-01-01" & date <= "2015-12-01",
               measure == "Commercial trucks") %>%
        select(-c(us_unemployment_rate, canada_unemployment_rate, mexico_unemployment_rate,
                  us_gdp_per_capita_monthly, canada_gdp_per_capita_monthly, mexico_gdp_per_capita_monthly,
                  us_canada_gdp_per_capita_gap, us_mexico_gdp_per_capita_gap))


#//////////////////////


trucks_training_data
trucks_training_data %>% glimpse()
trucks_training_data %>% skim()
trucks_training_data %>% count(date) %>% print(n = nrow(.))
trucks_training_data %>% count(date) %>% arrange(desc(date))

trucks_training_data %>% count(year) %>% print(n = nrow(.))
border_data_3 %>% count(year) %>% print(n = nrow(.))


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# create get_arima_forecast() ####

get_arima_forecast <- function(training_data, current_var, forecast_h){
        
        current_var_output <- str_c(current_var, "_arima")
        
        arima_forecast <- training_data %>% 
                mutate(year_month = yearmonth(date)) %>%
                as_tsibble(index = year_month) %>%
                mutate(current_var = !!sym(current_var)) %>%
                model(ARIMA(formula = current_var, stepwise = FALSE)) %>%
                forecast(h = forecast_h) %>%
                as_tibble() %>%
                rename(!!sym(current_var_output) := .mean) %>%
                mutate(date = ym(year_month)) %>%
                select(date, !!sym(current_var_output)) 
        
        return(arima_forecast)
        
}


#///////////////////


# test
get_arima_forecast(training_data = trucks_training_data, current_var = "monthly_count", forecast_h = 3)


current_var <- "monthly_count"
current_var_output <- str_c(current_var, "_arima")
training_data <- trucks_training_data
forecast_h <- 3

training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        mutate(current_var = !!sym(current_var)) %>%
        model(ARIMA(formula = current_var, stepwise = FALSE)) %>%
        forecast(h = forecast_h) %>%
        as_tibble() %>%
        rename(!!sym(current_var_output) := .mean) %>%
        mutate(date = ym(year_month)) %>%
        select(date, !!sym(current_var_output)) 


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_arima_forecast_vars ####

trucks_arima_forecast_vars <- map(.x = c("monthly_count", "us_unemployment_rate", "canada_unemployment_rate", "mexico_unemployment_rate",
           "us_gdp_per_capita_monthly", "canada_gdp_per_capita_monthly", "mexico_gdp_per_capita_monthly"), 
    .f = ~ get_arima_forecast(training_data = border_data_3 %>%
                                      filter(date <= "2015-12-01",
                                             measure == "Commercial trucks"), 
                              current_var = .x, forecast_h = 48)) %>%
        bind_cols() %>%
        rename(date = date...1) %>%
        select(-matches("date..."))

# save trucks_arima_forecast_vars ####
trucks_arima_forecast_vars %>% write_csv(file = "data/trucks_arima_forecast_vars.csv")

# load
trucks_arima_forecast_vars <- read_csv(file = "data/trucks_arima_forecast_vars.csv")


#///////////////////


trucks_arima_forecast_vars 
trucks_arima_forecast_vars %>% glimpse()
trucks_arima_forecast_vars %>% skim()
trucks_arima_forecast_vars %>% count(date)
trucks_arima_forecast_vars %>% count(date) %>% arrange(desc(date))
        

#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_testing_data ####

trucks_testing_data <- border_data_3 %>%
        filter(measure == "Commercial trucks") %>%
        left_join(., trucks_arima_forecast_vars, ., by = c("date" = "date")) %>%
        mutate(monthly_count_arima = case_when(date <= "2015-12-01" ~ monthly_count, 
                                               TRUE ~ monthly_count_arima),
               us_unemployment_rate_arima = case_when(date <= "2015-12-01" ~ us_unemployment_rate, 
                                               TRUE ~ us_unemployment_rate_arima),
               canada_unemployment_rate_arima = case_when(date <= "2015-12-01" ~ canada_unemployment_rate, 
                                                TRUE ~ canada_unemployment_rate_arima),
               mexico_unemployment_rate_arima = case_when(date <= "2015-12-01" ~ mexico_unemployment_rate, 
                                                          TRUE ~ mexico_unemployment_rate_arima),
               us_gdp_per_capita_monthly_arima = case_when(date <= "2015-12-01" ~ us_gdp_per_capita_monthly, 
                                                          TRUE ~ us_gdp_per_capita_monthly_arima),
               canada_gdp_per_capita_monthly_arima = case_when(date <= "2015-12-01" ~ canada_gdp_per_capita_monthly, 
                                                           TRUE ~ canada_gdp_per_capita_monthly_arima),
               mexico_gdp_per_capita_monthly_arima = case_when(date <= "2015-12-01" ~ mexico_gdp_per_capita_monthly, 
                                                           TRUE ~ mexico_gdp_per_capita_monthly_arima),
               
               monthly_count_lag_1 = lag(monthly_count_arima, n = 1),
               monthly_count_lag_2 = lag(monthly_count_arima, n = 2),
               monthly_count_lag_3 = lag(monthly_count_arima, n = 3),
               monthly_count_lag_4 = lag(monthly_count_arima, n = 4),
               monthly_count_lag_5 = lag(monthly_count_arima, n = 5),
               monthly_count_lag_6 = lag(monthly_count_arima, n = 6),
               monthly_count_lag_7 = lag(monthly_count_arima, n = 7),
               monthly_count_lag_8 = lag(monthly_count_arima, n = 8),
               monthly_count_lag_9 = lag(monthly_count_arima, n = 9),
               monthly_count_lag_10 = lag(monthly_count_arima, n = 10),
               monthly_count_lag_11 = lag(monthly_count_arima, n = 11),
               monthly_count_lag_12 = lag(monthly_count_arima, n = 12),
               monthly_count_diff_2 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 2),
               monthly_count_diff_3 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 3),
               monthly_count_diff_4 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 4),
               monthly_count_diff_5 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 5),
               monthly_count_diff_6 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 6),
               monthly_count_diff_7 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 7),
               monthly_count_diff_8 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 8),
               monthly_count_diff_9 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 9),
               monthly_count_diff_10 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 10),
               monthly_count_diff_11 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 11),
               monthly_count_diff_12 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 12),
               monthly_count_diff_13 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 13),
               
               year = year(date),
               month = month(date),
               # pandemic_flag = case_when(measure %in% c("Personal vehicles") & year %in% c(2020, 2021, 2022) ~ 1, 
               #                           measure %in% c("Pedestrians") & year %in% c(2020, 2021) ~ 1,
               #                           measure == "Commercial trucks" & year == 2020 & month %in% (1:6) ~ 1,
               #                           TRUE ~ 0),
               # post_pandemic_flag = case_when(year >= 2020 ~ 1, TRUE ~ 0),
               financial_crisis_flag = case_when(year == 2008 ~ 1, TRUE ~ 0),
               post_financial_crisis_flag = case_when(year >= 2008 ~ 1, TRUE ~ 0),
               post_911_flag = case_when(date >= "2001-09-01" ~ 1, TRUE ~ 0),
               
               us_unemployment_rate_ma_6 = slide_dbl(us_unemployment_rate_arima, .f = mean, .before = 5),
               mexico_unemployment_rate_ma_6 = slide_dbl(mexico_unemployment_rate_arima, .f = mean, .before = 5),
               canada_unemployment_rate_ma_6 = slide_dbl(canada_unemployment_rate_arima, .f = mean, .before = 5),
               us_unemployment_rate_diff_6 = us_unemployment_rate_arima - 
                       lag(us_unemployment_rate_arima, n = 6),
               mexico_unemployment_rate_diff_6 = mexico_unemployment_rate_arima - 
                       lag(mexico_unemployment_rate_arima, n = 6),
               canada_unemployment_rate_diff_6 = canada_unemployment_rate_arima - 
                       lag(canada_unemployment_rate_arima, n = 6),
               
               us_canada_gdp_per_capita_gap = us_gdp_per_capita_monthly_arima - canada_gdp_per_capita_monthly_arima,
               us_mexico_gdp_per_capita_gap = us_gdp_per_capita_monthly_arima - mexico_gdp_per_capita_monthly_arima,
               us_canada_gdp_per_capita_gap_ma_6 = slide_dbl(us_canada_gdp_per_capita_gap, .f = mean, .before = 5),
               us_mexico_gdp_per_capita_gap_ma_6 = slide_dbl(us_mexico_gdp_per_capita_gap, .f = mean, .before = 5),
               us_canada_gdp_per_capita_gap_diff_6 = us_canada_gdp_per_capita_gap - 
                       lag(us_canada_gdp_per_capita_gap, n = 6),
               us_mexico_gdp_per_capita_gap_diff_6 = us_mexico_gdp_per_capita_gap -
                       lag(us_canada_gdp_per_capita_gap, n = 6)) %>%
        select(-c(us_unemployment_rate, canada_unemployment_rate, mexico_unemployment_rate,
                  us_gdp_per_capita_monthly, canada_gdp_per_capita_monthly, mexico_gdp_per_capita_monthly,
                  us_canada_gdp_per_capita_gap, us_mexico_gdp_per_capita_gap)) %>%
        filter(date >= "2016-01-01" & date <= "2019-12-01")


#//////////////////////


trucks_testing_data
trucks_testing_data %>% glimpse()
trucks_testing_data %>% skim()

trucks_testing_data %>% count(date) %>% print(n = nrow(.))
trucks_testing_data %>% count(date) %>% arrange(desc(date))

trucks_training_data %>% count(year) %>% print(n = nrow(.))
trucks_testing_data %>% count(year) %>% print(n = nrow(.))

trucks_testing_data %>%
        filter(is.na(monthly_count_lag_3)) %>% count(date)
trucks_testing_data %>%
        # filter(is.na(monthly_count_lag_3)) %>%
        filter(date >= "2022-11-01" & date <= "2023-02-01") %>%
        # filter(date %in% c("2022-12-01", "2023-01-01", "2023-02-01")) %>%
        glimpse()


#////////////////////////////////////////////////////////////////////////////////////////////


# save trucks_training/testing_data ####

trucks_training_data %>% write_csv(file = "data/trucks_training_data.csv")
trucks_testing_data %>% write_csv(file = "data/trucks_testing_data.csv")


# load training/testing_data
trucks_training_data <- read_csv(file = "data/trucks_training_data.csv")
trucks_testing_data <- read_csv(file = "data/trucks_testing_data.csv")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_tscv_splits_initial ####


# note that trucks_training_tscv_splits_initial is only used as convenience to 
# get date ranges for trucks_training_tscv_splits_synthetic, which
# will contain real covariates in analysis data and arima-forecasted covariates in assessment data
# this is important so that when tuning models and evaluating w tscv the assessment data
# does not have real lagged covariates, which would unrealistically improve their predictions
# eg is linear regression model had real lagged covariates out 48 months in assessment data
# it's "prediction" for month 48 could be using the real lag(monthly_count, n = 1), etc
# with synthetic data though, predictions based on assessment data will use 
# synthetic covariates that were arima_forecast 48 months out from end of analysis data,
# which is more realistic 

# training data has 19 years (1997-2015))
# first 10 of 19 years in training data (1997-2006) are used as initial analysis, 
# w/ next 4 years for assessment data (2007-2010)
# then each resample slides ahead 12 months, so there are 6 resamples, 
# with the 6 assessment data resamples starting with each of 2007-2012, respectively
# and assessment data resamples ending with each of 2010-2015, respectively
trucks_training_tscv_splits_initial <- trucks_training_data %>% 
        arrange(date) %>%
        select(-measure) %>%
        sliding_window(lookback = Inf, skip = (12*10)-1, assess_start = 1, assess_stop = 48, step = 12, complete = TRUE)


#/////////////////////


# inspect
trucks_training_data %>% nrow()
trucks_training_data %>% count(year) %>% print(n = nrow(.))

trucks_training_tscv_splits_initial
analysis(trucks_training_tscv_splits_initial$splits[[1]])
assessment(trucks_training_tscv_splits_initial$splits[[1]])

analysis(trucks_training_tscv_splits_initial$splits[[1]]) %>% nrow()
assessment(trucks_training_tscv_splits_initial$splits[[1]]) %>% nrow()
analysis(trucks_training_tscv_splits_initial$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_initial$splits[[1]]) %>% count(year)

analysis(trucks_training_tscv_splits_initial$splits[[6]]) %>% nrow()
assessment(trucks_training_tscv_splits_initial$splits[[6]]) %>% nrow()
analysis(trucks_training_tscv_splits_initial$splits[[6]]) %>% count(year)
assessment(trucks_training_tscv_splits_initial$splits[[6]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////


# create get_tscv_split_spec() ####

get_tscv_split_spec <- function(split_obj, i) {
        
        analysis_nrow <- analysis(split_obj$splits[[i]]) %>% nrow()
        
        analysis_start_date <- analysis(split_obj$splits[[i]]) %>% 
                arrange(date) %>%
                slice(1) %>%
                pull(date)
        
        analysis_end_date <- analysis(split_obj$splits[[i]]) %>% 
                arrange(desc(date)) %>%
                slice(1) %>%
                pull(date)
        
        assessment_start_date <- assessment(split_obj$splits[[i]]) %>% 
                arrange(date) %>%
                slice(1) %>%
                pull(date)
        
        assessment_end_date <- assessment(split_obj$splits[[i]]) %>% 
                arrange(desc(date)) %>%
                slice(1) %>%
                pull(date)
        
        return(tibble(split = i, 
               analysis_nrow = analysis_nrow,
               analysis_start_date = analysis_start_date,
               analysis_end_date = analysis_end_date,
               assessment_start_date = assessment_start_date,
               assessment_end_date = assessment_end_date))
}


#/////////////////////////////////


# test

split_obj <- trucks_training_tscv_splits_initial
i <- 1

map(.x = 1:nrow(trucks_training_tscv_splits_initial),
        .f = ~ get_tscv_split_spec(split_obj = trucks_training_tscv_splits_initial, i = .x)) %>%
        bind_rows()


#////////////////////////////////////////////////////////////////////////////////////////////


# get tscv_split_spec
tscv_split_spec <- map(.x = 1:nrow(trucks_training_tscv_splits_initial),
        .f = ~ get_tscv_split_spec(split_obj = trucks_training_tscv_splits_initial, i = .x)) %>%
        bind_rows()

tscv_split_spec 


#////////////////////////////////////////////////////////////////////////////////////////////


# create get_tscv_splits_synthetic() ####

get_tscv_splits_synthetic <- function(i) {
        
        analysis_end_date_i <- tscv_split_spec %>% slice(i) %>% pull(analysis_end_date)
        assessment_start_date_i <- tscv_split_spec %>% slice(i) %>% pull(assessment_start_date)
        assessment_end_date_i <- tscv_split_spec %>% slice(i) %>% pull(assessment_end_date)
        analysis_nrow_i <- tscv_split_spec %>% slice(i) %>% pull(analysis_nrow)
        
        
        #//////////////////////////////////////////////////////////////////////////////////
        
        
        
        trucks_arima_forecast_vars_i <- map(.x = c("monthly_count", "us_unemployment_rate", "canada_unemployment_rate", "mexico_unemployment_rate",
                                               "us_gdp_per_capita_monthly", "canada_gdp_per_capita_monthly", "mexico_gdp_per_capita_monthly"), 
                .f = ~ get_arima_forecast(training_data = border_data_3 %>%
                                  filter(date >= "1997-01-01",
                                        date <= analysis_end_date_i,
                                        measure == "Commercial trucks"), 
                                          current_var = .x, forecast_h = 48)) %>%
                bind_cols() %>%
                rename(date = date...1) %>%
                select(-matches("date..."))
        
        
        #//////////////////////////////////////////////////////////////////////////////////
        
        
        trucks_training_data_synthetic_i <- border_data_3 %>%
                filter(date >= "1997-01-01",
                       measure == "Commercial trucks") %>%
                left_join(., trucks_arima_forecast_vars_i, ., by = c("date" = "date")) %>%
                mutate(monthly_count_arima = case_when(date <= analysis_end_date_i ~ monthly_count, 
                                                       TRUE ~ monthly_count_arima),
                       us_unemployment_rate_arima = case_when(date <= analysis_end_date_i ~ us_unemployment_rate, 
                                                              TRUE ~ us_unemployment_rate_arima),
                       canada_unemployment_rate_arima = case_when(date <= analysis_end_date_i ~ canada_unemployment_rate, 
                                                                  TRUE ~ canada_unemployment_rate_arima),
                       mexico_unemployment_rate_arima = case_when(date <= analysis_end_date_i ~ mexico_unemployment_rate, 
                                                                  TRUE ~ mexico_unemployment_rate_arima),
                       us_gdp_per_capita_monthly_arima = case_when(date <= analysis_end_date_i ~ us_gdp_per_capita_monthly, 
                                                                   TRUE ~ us_gdp_per_capita_monthly_arima),
                       canada_gdp_per_capita_monthly_arima = case_when(date <= analysis_end_date_i ~ canada_gdp_per_capita_monthly, 
                                                                       TRUE ~ canada_gdp_per_capita_monthly_arima),
                       mexico_gdp_per_capita_monthly_arima = case_when(date <= analysis_end_date_i ~ mexico_gdp_per_capita_monthly, 
                                                                       TRUE ~ mexico_gdp_per_capita_monthly_arima),
                       
                       monthly_count_lag_1 = lag(monthly_count_arima, n = 1),
                       monthly_count_lag_2 = lag(monthly_count_arima, n = 2),
                       monthly_count_lag_3 = lag(monthly_count_arima, n = 3),
                       monthly_count_lag_4 = lag(monthly_count_arima, n = 4),
                       monthly_count_lag_5 = lag(monthly_count_arima, n = 5),
                       monthly_count_lag_6 = lag(monthly_count_arima, n = 6),
                       monthly_count_lag_7 = lag(monthly_count_arima, n = 7),
                       monthly_count_lag_8 = lag(monthly_count_arima, n = 8),
                       monthly_count_lag_9 = lag(monthly_count_arima, n = 9),
                       monthly_count_lag_10 = lag(monthly_count_arima, n = 10),
                       monthly_count_lag_11 = lag(monthly_count_arima, n = 11),
                       monthly_count_lag_12 = lag(monthly_count_arima, n = 12),
                       monthly_count_diff_2 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 2),
                       monthly_count_diff_3 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 3),
                       monthly_count_diff_4 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 4),
                       monthly_count_diff_5 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 5),
                       monthly_count_diff_6 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 6),
                       monthly_count_diff_7 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 7),
                       monthly_count_diff_8 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 8),
                       monthly_count_diff_9 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 9),
                       monthly_count_diff_10 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 10),
                       monthly_count_diff_11 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 11),
                       monthly_count_diff_12 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 12),
                       monthly_count_diff_13 = lag(monthly_count_arima, n = 1) - lag(monthly_count_arima, n = 13),
                       
                       year = year(date),
                       month = month(date),
                       # pandemic_flag = case_when(measure %in% c("Personal vehicles") & year %in% c(2020, 2021, 2022) ~ 1, 
                       #                           measure %in% c("Pedestrians") & year %in% c(2020, 2021) ~ 1,
                       #                           measure == "Commercial trucks" & year == 2020 & month %in% (1:6) ~ 1,
                       #                           TRUE ~ 0),
                       # post_pandemic_flag = case_when(year >= 2020 ~ 1, TRUE ~ 0),
                       financial_crisis_flag = case_when(year == 2008 ~ 1, TRUE ~ 0),
                       post_financial_crisis_flag = case_when(year >= 2008 ~ 1, TRUE ~ 0),
                       post_911_flag = case_when(date >= "2001-09-01" ~ 1, TRUE ~ 0),
                       
                       us_unemployment_rate_ma_6 = slide_dbl(us_unemployment_rate_arima, .f = mean, .before = 5),
                       mexico_unemployment_rate_ma_6 = slide_dbl(mexico_unemployment_rate_arima, .f = mean, .before = 5),
                       canada_unemployment_rate_ma_6 = slide_dbl(canada_unemployment_rate_arima, .f = mean, .before = 5),
                       us_unemployment_rate_diff_6 = us_unemployment_rate_arima - 
                               lag(us_unemployment_rate_arima, n = 6),
                       mexico_unemployment_rate_diff_6 = mexico_unemployment_rate_arima - 
                               lag(mexico_unemployment_rate_arima, n = 6),
                       canada_unemployment_rate_diff_6 = canada_unemployment_rate_arima - 
                               lag(canada_unemployment_rate_arima, n = 6),
                       
                       us_canada_gdp_per_capita_gap = us_gdp_per_capita_monthly_arima - canada_gdp_per_capita_monthly_arima,
                       us_mexico_gdp_per_capita_gap = us_gdp_per_capita_monthly_arima - mexico_gdp_per_capita_monthly_arima,
                       us_canada_gdp_per_capita_gap_ma_6 = slide_dbl(us_canada_gdp_per_capita_gap, .f = mean, .before = 5),
                       us_mexico_gdp_per_capita_gap_ma_6 = slide_dbl(us_mexico_gdp_per_capita_gap, .f = mean, .before = 5),
                       us_canada_gdp_per_capita_gap_diff_6 = us_canada_gdp_per_capita_gap - 
                               lag(us_canada_gdp_per_capita_gap, n = 6),
                       us_mexico_gdp_per_capita_gap_diff_6 = us_mexico_gdp_per_capita_gap -
                               lag(us_canada_gdp_per_capita_gap, n = 6)) %>%
                select(-c(us_unemployment_rate, canada_unemployment_rate, mexico_unemployment_rate,
                          us_gdp_per_capita_monthly, canada_gdp_per_capita_monthly, mexico_gdp_per_capita_monthly,
                          us_canada_gdp_per_capita_gap, us_mexico_gdp_per_capita_gap,
                          
                          measure,
                          monthly_count_arima, us_unemployment_rate_arima, 
                          canada_unemployment_rate_arima, mexico_unemployment_rate_arima,
                          us_gdp_per_capita_monthly_arima, canada_gdp_per_capita_monthly_arima,
                          mexico_gdp_per_capita_monthly_arima)) %>%
                filter(date <= assessment_end_date_i) %>%
                arrange(desc(date)) %>%
                fill(monthly_count_lag_1, .direction = "down") %>%
                fill(monthly_count_lag_2, .direction = "down") %>%
                fill(monthly_count_lag_3, .direction = "down") %>%
                fill(monthly_count_lag_4, .direction = "down") %>%
                fill(monthly_count_lag_5, .direction = "down") %>%
                fill(monthly_count_lag_6, .direction = "down") %>%
                fill(monthly_count_lag_7, .direction = "down") %>%
                fill(monthly_count_lag_8, .direction = "down") %>%
                fill(monthly_count_lag_9, .direction = "down") %>%
                fill(monthly_count_lag_10, .direction = "down") %>%
                fill(monthly_count_lag_11, .direction = "down") %>%
                fill(monthly_count_lag_12, .direction = "down") %>%
                fill(monthly_count_diff_2, .direction = "down") %>%
                fill(monthly_count_diff_3, .direction = "down") %>%
                fill(monthly_count_diff_4, .direction = "down") %>%
                fill(monthly_count_diff_5, .direction = "down") %>%
                fill(monthly_count_diff_6, .direction = "down") %>%
                fill(monthly_count_diff_7, .direction = "down") %>%
                fill(monthly_count_diff_8, .direction = "down") %>%
                fill(monthly_count_diff_9, .direction = "down") %>%
                fill(monthly_count_diff_10, .direction = "down") %>%
                fill(monthly_count_diff_11, .direction = "down") %>%
                fill(monthly_count_diff_12, .direction = "down") %>%
                fill(monthly_count_diff_13, .direction = "down") %>%
                fill(us_unemployment_rate_diff_6, .direction = "down") %>%
                fill(mexico_unemployment_rate_diff_6, .direction = "down") %>%
                fill(canada_unemployment_rate_diff_6, .direction = "down") %>%
                fill(us_canada_gdp_per_capita_gap_diff_6, .direction = "down") %>%
                fill(us_mexico_gdp_per_capita_gap_diff_6, .direction = "down") %>%
                arrange(date) 
        
        
        #//////////////////////////////////////////////////////////////////////////////////
        
        
        trucks_training_tscv_splits_i <- trucks_training_data_synthetic_i %>% 
                arrange(date) %>%
                sliding_window(lookback = Inf, skip = analysis_nrow_i - 1, 
                               assess_start = 1, assess_stop = 48, step = 1, complete = TRUE)
        
        return(trucks_training_tscv_splits_i)
        
}
        

#////////////////////////////////


# check

# i <- 6
# trucks_training_tscv_splits_synthetic_6 <- analysis(trucks_training_tscv_splits_synthetic$splits[[i]]) %>% 
#         bind_rows(., assessment(trucks_training_tscv_splits_synthetic$splits[[i]])) %>%
#         arrange(desc(date)) %>%
#         fill(monthly_count_lag_1, .direction = "down") %>%
#         fill(monthly_count_lag_2, .direction = "down") %>%
#         fill(monthly_count_lag_3, .direction = "down") %>%
#         fill(monthly_count_lag_4, .direction = "down") %>%
#         fill(monthly_count_lag_5, .direction = "down") %>%
#         fill(monthly_count_lag_6, .direction = "down") %>%
#         fill(monthly_count_lag_7, .direction = "down") %>%
#         fill(monthly_count_lag_8, .direction = "down") %>%
#         fill(monthly_count_lag_9, .direction = "down") %>%
#         fill(monthly_count_lag_10, .direction = "down") %>%
#         fill(monthly_count_lag_11, .direction = "down") %>%
#         fill(monthly_count_lag_12, .direction = "down") %>%
#         fill(monthly_count_diff_2, .direction = "down") %>%
#         fill(monthly_count_diff_3, .direction = "down") %>%
#         fill(monthly_count_diff_4, .direction = "down") %>%
#         fill(monthly_count_diff_5, .direction = "down") %>%
#         fill(monthly_count_diff_6, .direction = "down") %>%
#         fill(monthly_count_diff_7, .direction = "down") %>%
#         fill(monthly_count_diff_8, .direction = "down") %>%
#         fill(monthly_count_diff_9, .direction = "down") %>%
#         fill(monthly_count_diff_10, .direction = "down") %>%
#         fill(monthly_count_diff_11, .direction = "down") %>%
#         fill(monthly_count_diff_12, .direction = "down") %>%
#         fill(monthly_count_diff_13, .direction = "down") %>%
#         fill(us_unemployment_rate_diff_6, .direction = "down") %>%
#         fill(mexico_unemployment_rate_diff_6, .direction = "down") %>%
#         fill(canada_unemployment_rate_diff_6, .direction = "down") %>%
#         fill(us_canada_gdp_per_capita_gap_diff_6, .direction = "down") %>%
#         fill(us_mexico_gdp_per_capita_gap_diff_6, .direction = "down") %>%
#         select(-c(measure,
#                   monthly_count_arima, us_unemployment_rate_arima, 
#                   canada_unemployment_rate_arima, mexico_unemployment_rate_arima,
#                   us_gdp_per_capita_monthly_arima, canada_gdp_per_capita_monthly_arima,
#                   mexico_gdp_per_capita_monthly_arima)) %>%
#         arrange(date) %>%
#         sliding_window(lookback = Inf, skip = tscv_split_spec %>% slice(i) %>% pull(analysis_nrow) - 1, 
#                        assess_start = 1, assess_stop = 48, step = 1, complete = TRUE)
# 
# trucks_training_tscv_splits_synthetic_tbl <- trucks_training_tscv_splits_synthetic_1 %>%
#         bind_rows(., trucks_training_tscv_splits_synthetic_2) %>%
#         bind_rows(., trucks_training_tscv_splits_synthetic_3) %>%
#         bind_rows(., trucks_training_tscv_splits_synthetic_4) %>%
#         bind_rows(., trucks_training_tscv_splits_synthetic_5) %>%
#         bind_rows(., trucks_training_tscv_splits_synthetic_6)
# 
# current_split <- 1
# analysis(cars_training_tscv_splits_synthetic_1$splits[[current_split]]) %>% count(year)
# assessment(cars_training_tscv_splits_synthetic_1$splits[[current_split]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_tscv_splits_synthetic ####

trucks_training_tscv_splits_synthetic_tbl <- map(.x = 1:nrow(tscv_split_spec), 
                             .f = ~ get_tscv_splits_synthetic(i = .x)) %>%
        bind_rows()

# convert to rset obj
trucks_training_tscv_splits_synthetic <- manual_rset(splits = trucks_training_tscv_splits_synthetic_tbl %>% select(splits), 
            ids = trucks_training_tscv_splits_synthetic_tbl %>% select(id)) 

# save trucks_training_tscv_splits_synthetic ####
trucks_training_tscv_splits_synthetic %>% saveRDS(object = ., file = "data/trucks_training_tscv_splits_synthetic.rds")

# load
trucks_training_tscv_splits_synthetic <- readRDS(file = "data/trucks_training_tscv_splits_synthetic.rds")


#///////////////////////////////


trucks_training_tscv_splits_synthetic
trucks_training_tscv_splits_initial

analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% glimpse()
analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% skim()
assessment(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% skim()

analysis(trucks_training_tscv_splits_synthetic$splits[[2]]) %>% skim()


analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)
analysis(trucks_training_tscv_splits_initial$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_initial$splits[[1]]) %>% count(year)

analysis(trucks_training_tscv_splits_synthetic$splits[[6]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[6]]) %>% count(year)
analysis(trucks_training_tscv_splits_initial$splits[[6]]) %>% count(year)
assessment(trucks_training_tscv_splits_initial$splits[[6]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get benchmarks ####

trucks_testing_drift_benchmark <- trucks_training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
        forecast(h = 48) %>%
        as_tibble() %>%
        mutate(date = ym(year_month),
               drift_pred = .mean)

trucks_testing_drift_benchmark 
trucks_testing_drift_benchmark %>%
        ggplot(data = ., mapping = aes(x = year_month, y = .mean)) + geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


trucks_testing_naive_benchmark <- trucks_training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(naive = NAIVE(monthly_count)) %>%
        forecast(h = 48) %>%
        as_tibble()%>%
        mutate(date = ym(year_month),
               naive_pred = .mean)

trucks_testing_naive_benchmark
trucks_testing_naive_benchmark %>%
        ggplot(data = ., mapping = aes(x = year_month, y = .mean)) + geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


trucks_testing_snaive_benchmark <- trucks_training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(snaive = SNAIVE(monthly_count ~ lag("year"))) %>%
        forecast(h = 48) %>%
        as_tibble()%>%
        mutate(date = ym(year_month),
               snaive_pred = .mean)

trucks_testing_snaive_benchmark %>% print(n = 48)
trucks_testing_snaive_benchmark %>%
        ggplot(data = ., mapping = aes(x = year_month, y = .mean)) + geom_line()


#/////////////////////


# plot
trucks_training_data %>%
        select(date, monthly_count) %>%
        mutate(origin = "truth") %>%
        bind_rows(., trucks_testing_drift_benchmark %>%
                          mutate(date = ym(year_month),
                                 origin = "drift") %>%
                          select(-monthly_count) %>%
                          rename(monthly_count = drift_pred) %>%
                          select(date, monthly_count, origin)) %>%
        bind_rows(., trucks_testing_naive_benchmark %>%
                          mutate(date = ym(year_month),
                                 origin = "naive") %>%
                          select(-monthly_count) %>%
                          rename(monthly_count = naive_pred) %>%
                          select(date, monthly_count, origin)) %>%
        bind_rows(., trucks_testing_snaive_benchmark %>%
                mutate(date = ym(year_month),
                       origin = "snaive") %>%
                select(-monthly_count) %>%
                rename(monthly_count = snaive_pred) %>%
                select(date, monthly_count, origin)) %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = origin)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get ols model ####

ols_spec <- linear_reg() %>% set_engine(engine = "lm")
ols_spec

trucks_ols_fit <- ols_spec %>% fit(monthly_count ~ ., data = trucks_training_data %>% select(-c(date, measure)))

# save trucks_ols_fit ####
trucks_ols_fit %>% saveRDS(object = ., file = "data/trucks_ols_fit.rds")

# load
trucks_ols_fit <- readRDS(file = "data/trucks_ols_fit.rds")


#////////////////////////


trucks_ols_fit
trucks_ols_fit %>% tidy() %>% print(n = nrow(.))
trucks_ols_fit %>% summary()


#////////////////////////////////////////////////////////////////////////////////////////////


# fit on synthetic resamples for later use in ensemble stack
# note the other models with tuning fit on the synthetic resamples as part of tuning
# and that tuning output for them will be used in the ensemble stack

ols_workflow <- workflow() %>%
        add_formula(monthly_count ~ .) %>%
        add_model(ols_spec)

ols_workflow


#////////////////////////////////////////////////////////////////////////////////////////////


doParallel::registerDoParallel()
set.seed(123)
ols_fit_resamples_results <- fit_resamples(object = ols_workflow,
                        resamples = trucks_training_tscv_splits_synthetic,
                        metrics = metric_set(rmse),
                        control = control_stack_resamples())

# save ols_fit_resamples_results ####
ols_fit_resamples_results %>% saveRDS(object = ., file = "data/ols_fit_resamples_results.rds")

# load
ols_fit_resamples_results <- readRDS(file = "data/ols_fit_resamples_results.rds")


#////////////////////////////////////////////////////////////////////////////////////////////


trucks_ols_training_results <- trucks_training_data %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_training_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_training_data, type = "conf_int")) %>%
        rename(pred_lower = .pred_lower,
               pred_upper = .pred_upper) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100)
       
trucks_ols_training_results %>% glimpse() 
trucks_ols_training_results %>% rmse(truth = monthly_count, estimate = pred)
trucks_ols_training_results %>% mape(truth = monthly_count, estimate = pred)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_ols_testing_results_short_term ####

trucks_ols_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        rename(pred_lower = .pred_lower, 
               pred_upper = .pred_upper) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ols_testing_results_short_term ####
trucks_ols_testing_results_short_term %>% 
        write_csv(file = "data/trucks_ols_testing_results_short_term.csv")


#/////////////////////


trucks_ols_testing_results_short_term %>% glimpse() 
trucks_ols_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_ols_testing_results_short_term %>% rmse(truth = monthly_count, estimate = pred)
trucks_ols_testing_results_short_term %>% rmse(truth = monthly_count, estimate = snaive_pred)
trucks_ols_testing_results_short_term %>% rmse(truth = monthly_count, estimate = drift_pred)

trucks_ols_testing_results_short_term %>% mape(truth = monthly_count, estimate = pred)
trucks_ols_testing_results_short_term %>% mape(truth = monthly_count, estimate = snaive_pred)
trucks_ols_testing_results_short_term %>% mape(truth = monthly_count, estimate = drift_pred)

trucks_ols_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_ols_testing_results_medium_term ####

trucks_ols_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        rename(pred_lower = .pred_lower, 
               pred_upper = .pred_upper) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ols_testing_results_medium_term ####
trucks_ols_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_ols_testing_results_medium_term.csv")


#/////////////////////


trucks_ols_testing_results_medium_term
trucks_ols_testing_results_medium_term %>% glimpse() 
trucks_ols_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_ols_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////
        
        
# get trucks_ols_testing_results_long_term #### 

trucks_ols_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        rename(pred_lower = .pred_lower, 
               pred_upper = .pred_upper) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
                rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ols_testing_results_long_term ####
trucks_ols_testing_results_long_term %>% 
        write_csv(file = "data/trucks_ols_testing_results_long_term.csv")


#/////////////////////


trucks_ols_testing_results_long_term %>% glimpse() 
trucks_ols_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

# check why ols underperforms drift over long_term
trucks_ols_testing_results_long_term %>%
        mutate(diff_sq_error = pred_sq_error - drift_pred_sq_error) %>%
        arrange(desc(diff_sq_error)) %>%
        select(date, monthly_count, pred, drift_pred, pred_sq_error, drift_pred_sq_error)
trucks_ols_testing_results_long_term %>%
        summarize(pred_sq_error_sum = sum(pred_sq_error),
                  drift_pred_sq_error_sum = sum(drift_pred_sq_error))

trucks_ols_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get ols_testing_results_combined 

ols_testing_results_combined <- trucks_ols_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_ols_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_ols_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

ols_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# ols ts cross-validation ####


# create get_ols_tscv_error
get_ols_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        ols_spec <- linear_reg() %>% set_engine(engine = "lm")
        current_ols_fit <- ols_spec %>% fit(monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
                
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                bind_cols(., trucks_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                rename(pred_lower = .pred_lower,
                       pred_upper = .pred_upper) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                      mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                      snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                      snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                      drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                      drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "ols") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                bind_cols(., trucks_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "conf_int")) %>%
                rename(pred_lower = .pred_lower,
                       pred_upper = .pred_upper) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "ols") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                bind_cols(., trucks_ols_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "conf_int")) %>%
                rename(pred_lower = .pred_lower,
                       pred_upper = .pred_upper) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "ols") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#//////////////////////


# inspect

get_ols_tscv_error(tscv_splits = trucks_training_tscv_splits, current_split = 1)

current_split <- 1
current_analysis_data <- analysis(trucks_training_tscv_splits_synthetic$splits[[current_split]])
current_assessment_data <- assessment(trucks_training_tscv_splits$splits[[current_split]])

current_ols_fit <- ols_spec %>% fit(monthly_count ~ ., data = current_analysis_data %>% select(-c(date, measure)))
current_assessment_results <- current_assessment_data %>%
        slice(1:3) %>%
        bind_cols(., current_ols_fit %>%
                          predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
        rename(pred = .pred) %>%
        bind_cols(., trucks_ols_fit %>%
                          predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
        rename(pred_lower = .pred_lower,
               pred_upper = .pred_upper) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100, 
               split = current_split,
               analysis_split_start_date = min(current_analysis_data %>% pull(date)),
               analysis_split_end_date = max(current_analysis_data %>% pull(date)),
               assessment_split_start_date = min(date),
               assessment_split_end_date = max(date),
               horizon = 3) %>%
        select(horizon, split, analysis_split_start_date, analysis_split_end_date, 
                assessment_split_start_date, assessment_split_end_date, rmse, mape) %>%
        slice(1)

current_assessment_results %>% glimpse()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_ols_tscv_error ####
trucks_training_ols_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
    .f = ~ get_ols_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_ols_tscv_error ####
trucks_training_ols_tscv_error %>% write_csv(file = "data/trucks_training_ols_tscv_error.csv")


#////////////////////////


trucks_training_ols_tscv_error
trucks_training_ols_tscv_error %>% glimpse()

trucks_training_ols_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_ols_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_ols_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get arima model ####

trucks_arima_fit <- trucks_training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(ARIMA(formula = monthly_count, stepwise = FALSE)) 
      
trucks_arima_fit  


# save trucks_arima_fit  ####
trucks_arima_fit %>% saveRDS(object = ., file = "data/trucks_arima_fit.rds")

# load
trucks_arima_fit <- readRDS(file = "data/trucks_arima_fit.rds")


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_arima_testing_results_short_term ####

trucks_testing_data %>% nrow()

trucks_arima_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_arima_fit %>%
                          forecast(h = 48) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_arima_testing_results_short_term ####
trucks_arima_testing_results_short_term %>% 
        write_csv(file = "data/trucks_arima_testing_results_short_term.csv")


#/////////////////////


trucks_arima_testing_results_short_term
trucks_arima_testing_results_short_term %>% glimpse()
trucks_arima_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)


trucks_arima_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_arima_testing_results_medium_term ####

trucks_arima_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_arima_fit %>%
                          forecast(h = 48) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_arima_testing_results_medium_term ####
trucks_arima_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_arima_testing_results_medium_term.csv")


#/////////////////////


trucks_arima_testing_results_medium_term
trucks_arima_testing_results_medium_term %>% glimpse()
trucks_arima_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_arima_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_arima_testing_results_long_term ####

trucks_arima_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_arima_fit %>%
                          forecast(h = 48) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_arima_testing_results_long_term ####
trucks_arima_testing_results_long_term %>% 
        write_csv(file = "data/trucks_arima_testing_results_long_term.csv")


#/////////////////////


trucks_arima_testing_results_long_term
trucks_arima_testing_results_long_term %>% glimpse()
trucks_arima_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_arima_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get arima_testing_results_combined

arima_testing_results_combined <- trucks_arima_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "arima", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_arima_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "arima", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_arima_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "arima", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

arima_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# arima ts cross-validation ####


# create get_arima_tscv_error
get_arima_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])

        current_arima_fit <- current_analysis_data %>% 
                mutate(year_month = yearmonth(date)) %>%
                as_tsibble(index = year_month) %>%
                model(ARIMA(formula = monthly_count, stepwise = FALSE)) 
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_arima_fit %>%
                                  forecast(h = 3) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_arima_fit %>%
                                  forecast(h = 12) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_arima_fit %>%
                                  forecast(h = 48) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_arima_tscv_error
trucks_training_arima_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                .f = ~ get_arima_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_arima_tscv_error ####
trucks_training_arima_tscv_error %>% write_csv(file = "data/trucks_training_arima_tscv_error.csv")


#/////////////////////////


trucks_training_arima_tscv_error
trucks_training_arima_tscv_error %>% glimpse()

trucks_training_arima_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_arima_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_arima_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

# note that 3rd split where rmse spikes is when assessment data starts with 2009, post-financial crisis
# but if 3rd split is ignored, arima consistently outperforms snaive and drift
analysis(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)


#/////////////////////////////////////////////////////////////////////////////////////////


# check sample 3


current_analysis_data <- analysis(trucks_training_tscv_splits_synthetic$splits[[3]])
current_assessment_data <- assessment(trucks_training_tscv_splits_synthetic$splits[[3]])

current_arima_fit <- current_analysis_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(ARIMA(formula = monthly_count, stepwise = FALSE)) 

check_results_sample_3 <- current_assessment_data %>%
        slice(1:48) %>%
        bind_cols(., current_arima_fit %>%
                          forecast(h = 48) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., current_analysis_data %>% 
                          mutate(year_month = yearmonth(date)) %>%
                          as_tsibble(index = year_month) %>%
                          model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                          forecast(h = 48) %>%
                          as_tibble() %>%
                          mutate(date = ym(year_month),
                                 drift_pred = .mean) %>%
                          select(date, drift_pred),
                  by = c("date" = "date")) %>%
        left_join(., current_analysis_data %>% 
                          mutate(year_month = yearmonth(date)) %>%
                          as_tsibble(index = year_month) %>%
                          model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                          forecast(h = 48) %>%
                          as_tibble() %>%
                          mutate(date = ym(year_month),
                                 snaive_pred = .mean) %>%
                          select(date, snaive_pred),
                  by = c("date" = "date"))

check_results_sample_3
check_results_sample_3 %>% glimpse()

check_results_sample_3 %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#/////////////////////////////////////////////////////////////////////////////////////////


# check sample 4


current_analysis_data <- analysis(trucks_training_tscv_splits_synthetic$splits[[4]])
current_assessment_data <- assessment(trucks_training_tscv_splits_synthetic$splits[[4]])

current_arima_fit <- current_analysis_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        model(ARIMA(formula = monthly_count, stepwise = FALSE)) 

check_results_sample_4 <- current_assessment_data %>%
        slice(1:48) %>%
        bind_cols(., current_arima_fit %>%
                          forecast(h = 48) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., current_analysis_data %>% 
                          mutate(year_month = yearmonth(date)) %>%
                          as_tsibble(index = year_month) %>%
                          model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                          forecast(h = 48) %>%
                          as_tibble() %>%
                          mutate(date = ym(year_month),
                                 drift_pred = .mean) %>%
                          select(date, drift_pred),
                  by = c("date" = "date")) %>%
        left_join(., current_analysis_data %>% 
                          mutate(year_month = yearmonth(date)) %>%
                          as_tsibble(index = year_month) %>%
                          model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                          forecast(h = 48) %>%
                          as_tibble() %>%
                          mutate(date = ym(year_month),
                                 snaive_pred = .mean) %>%
                          select(date, snaive_pred),
                  by = c("date" = "date"))

check_results_sample_4
check_results_sample_4 %>% glimpse()

check_results_sample_4 %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get regarima model ####

trucks_regarima_fit <- trucks_training_data %>% 
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        select(-c(date, measure)) %>%
        model(ARIMA(formula = monthly_count ~ 
                year + month +
                us_unemployment_rate_ma_6 + mexico_unemployment_rate_ma_6 + canada_unemployment_rate_ma_6 +
                us_unemployment_rate_diff_6 + mexico_unemployment_rate_diff_6 + canada_unemployment_rate_diff_6 +
                us_canada_gdp_per_capita_gap_ma_6 + us_mexico_gdp_per_capita_gap_ma_6 +
                us_canada_gdp_per_capita_gap_diff_6 + us_mexico_gdp_per_capita_gap_diff_6,
                stepwise = FALSE)) 

# save trucks_regarima_fit  ####
trucks_regarima_fit %>% saveRDS(object = ., file = "data/trucks_regarima_fit.rds")

# load
trucks_regarima_fit <- readRDS(file = "data/trucks_regarima_fit.rds")


#//////////////////////////


trucks_regarima_fit  
trucks_regarima_fit %>% tidy() %>% select(-.model)


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_regarima_testing_results_short_term ####


trucks_regarima_testing_covariates <- trucks_testing_data %>%
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        select(-c(date, measure))

trucks_regarima_testing_covariates
trucks_regarima_testing_covariates %>% glimpse()

trucks_regarima_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_regarima_fit %>%
                          forecast(new_data = trucks_regarima_testing_covariates) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_regarima_testing_results_short_term ####
trucks_regarima_testing_results_short_term %>% 
        write_csv(file = "data/trucks_regarima_testing_results_short_term.csv")


#/////////////////////


trucks_regarima_testing_results_short_term
trucks_regarima_testing_results_short_term %>% glimpse()
trucks_regarima_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)


trucks_regarima_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_regarima_testing_results_medium_term ####

trucks_regarima_testing_covariates <- trucks_testing_data %>%
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        select(-c(date, measure))

trucks_regarima_testing_covariates
trucks_regarima_testing_covariates %>% glimpse()

trucks_regarima_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_regarima_fit %>%
                          forecast(new_data = trucks_regarima_testing_covariates) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_regarima_testing_results_medium_term ####
trucks_regarima_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_regarima_testing_results_medium_term.csv")


#/////////////////////


trucks_regarima_testing_results_medium_term
trucks_regarima_testing_results_medium_term %>% glimpse()
trucks_regarima_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_regarima_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_regarima_testing_results_long_term ####

trucks_regarima_testing_covariates <- trucks_testing_data %>%
        mutate(year_month = yearmonth(date)) %>%
        as_tsibble(index = year_month) %>%
        select(-c(date, measure))

trucks_regarima_testing_covariates
trucks_regarima_testing_covariates %>% glimpse()

trucks_regarima_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_regarima_fit %>%
                          forecast(new_data = trucks_regarima_testing_covariates) %>%
                          hilo(level = c(95)) %>%
                          as_tibble() %>%
                          unpack_hilo('95%') %>%
                          rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                          mutate(pred = .mean) %>%
                          select(pred, pred_lower, pred_upper)) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_regarima_testing_results_long_term ####
trucks_regarima_testing_results_long_term %>% 
        write_csv(file = "data/trucks_regarima_testing_results_long_term.csv")


#/////////////////////


trucks_regarima_testing_results_long_term
trucks_regarima_testing_results_long_term %>% glimpse()
trucks_regarima_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_regarima_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get regarima_testing_results_combined

regarima_testing_results_combined <- trucks_regarima_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "regarima", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_regarima_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "regarima", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_regarima_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "regarima", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

regarima_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# regarima ts cross-validation ####


# create get_regarima_tscv_error
get_regarima_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        current_regarima_fit <- current_analysis_data %>% 
                mutate(year_month = yearmonth(date)) %>%
                as_tsibble(index = year_month) %>%
                select(-c(date)) %>%
                model(ARIMA(formula = monthly_count ~ 
                                    year + month +
                                    us_unemployment_rate_ma_6 + mexico_unemployment_rate_ma_6 + canada_unemployment_rate_ma_6 +
                                    us_unemployment_rate_diff_6 + mexico_unemployment_rate_diff_6 + canada_unemployment_rate_diff_6 +
                                    us_canada_gdp_per_capita_gap_ma_6 + us_mexico_gdp_per_capita_gap_ma_6 +
                                    us_canada_gdp_per_capita_gap_diff_6 + us_mexico_gdp_per_capita_gap_diff_6,
                            stepwise = FALSE)) 
        
        trucks_regarima_assessment_covariates <- current_assessment_data %>%
                mutate(year_month = yearmonth(date)) %>%
                as_tsibble(index = year_month) %>%
                select(-c(date))
                                  
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_regarima_fit %>%
                                  forecast(new_data = trucks_regarima_assessment_covariates) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper) %>%
                                  slice(1:3)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_regarima_fit %>%
                                  forecast(new_data = trucks_regarima_assessment_covariates) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper) %>%
                                  slice(1:12)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_regarima_fit %>%
                                  forecast(new_data = trucks_regarima_assessment_covariates) %>%
                                  hilo(level = c(95)) %>%
                                  as_tibble() %>%
                                  unpack_hilo('95%') %>%
                                  rename(pred_lower = '95%_lower', pred_upper = '95%_upper') %>%
                                  mutate(pred = .mean) %>%
                                  select(pred, pred_lower, pred_upper) %>%
                                  slice(1:48)) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "arima") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_regarima_tscv_error ####
trucks_training_regarima_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                      .f = ~ get_regarima_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_regarima_tscv_error ####
trucks_training_regarima_tscv_error %>% write_csv(file = "data/trucks_training_regarima_tscv_error.csv")


#/////////////////////////


trucks_training_regarima_tscv_error
trucks_training_regarima_tscv_error %>% glimpse()

trucks_training_regarima_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_regarima_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_regarima_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")


analysis(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get random_forest model ####

# get recipe
random_forest_recipe <- recipe(formula = monthly_count ~ ., 
                               data = trucks_training_data %>% select(-c(date, measure))) 

random_forest_recipe


#////////////////////////////////////////////////////////////////////////////////////////////


# get model tune_spec
random_forest_tune_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
        set_mode("regression") %>%
        set_engine("ranger")

random_forest_tune_spec


#////////////////////////////////////////////////////////////////////////////////////////////


# get workflow
random_forest_tune_workflow <- workflow() %>%
        add_recipe(random_forest_recipe) %>%
        add_model(random_forest_tune_spec)

random_forest_tune_workflow


#////////////////////////////////////////////////////////////////////////////////////////////


# get approximate tuning results using grid = 20 automated parameter combos
doParallel::registerDoParallel()
set.seed(123)
random_forest_tune_results_1 <- tune_grid(object = random_forest_tune_workflow, 
                                          resamples = trucks_training_tscv_splits_synthetic, grid = 20)

# save random_forest_tune_results_1 ####
random_forest_tune_results_1 %>% saveRDS(object = ., file = "data/random_forest_tune_results_1.rds")

# load
random_forest_tune_results_1 <- readRDS(file = "data/random_forest_tune_results_1.rds")


#///////////////////


random_forest_tune_results_1 
random_forest_tune_results_1 %>% slice(1) %>% select(.metrics) %>% unnest(cols = .metrics)
random_forest_tune_results_1  %>% collect_metrics()
random_forest_tune_results_1  %>% collect_metrics() %>% count(.metric)
random_forest_tune_results_1  %>% collect_metrics() %>% count(mtry, min_n)
random_forest_tune_results_1  %>% collect_metrics() %>% count(.config)

random_forest_tune_results_1 %>% show_best(metric = "rmse")
random_forest_tune_results_1 %>% select_best(metric = "rmse")

# plot
random_forest_tune_results_1 %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, mtry, min_n) %>%
        pivot_longer(cols = c(mtry, min_n), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(facets = vars(var), scales = "free_x") +
        labs(x = NULL, y = "rmse")
        

#////////////////////////////////////////////////////////////////////////////////////////////


# get refined tune_results
random_forest_grid <- grid_regular(mtry(range = c(20, 40)), min_n(range = c(5, 20)), levels = 7)

random_forest_grid %>% print(n = nrow(.))


#////////////////////////////////////////////////////////////////////////////////////////////


doParallel::registerDoParallel()
set.seed(123)
random_forest_tune_results_2 <- tune_grid(object = random_forest_tune_workflow, 
                         resamples = trucks_training_tscv_splits_synthetic, 
                         grid = random_forest_grid,
                         control = control_stack_grid())

# save random_forest_tune_results ####
random_forest_tune_results_2 %>% saveRDS(object = ., file = "data/random_forest_tune_results_2.rds")

# load
random_forest_tune_results_2 <- readRDS(file = "data/random_forest_tune_results_2.rds")


#////////////////////////////


random_forest_tune_results_2
random_forest_tune_results_2 %>% slice(1) %>% select(.metrics) %>% unnest(cols = .metrics)
random_forest_tune_results_2 %>% collect_metrics()

random_forest_tune_results_2 %>% show_best(metric = "rmse")
random_forest_tune_results_2 %>% select_best(metric = "rmse")

# plot
random_forest_tune_results_2 %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        mutate(min_n = factor(min_n)) %>%
        ggplot(data = ., mapping = aes(x = mtry, y = mean, color = min_n)) +
        geom_line() 


#//////////////////////////////////////////////////////////////////////////////////////////


# get model spec
random_forest_spec <- rand_forest(mtry = random_forest_tune_results_2 %>%
                                          select_best(metric = "rmse") %>%
                                          pull(mtry),
                                trees = 1000,
                                  min_n = random_forest_tune_results_2 %>%
                                          select_best(metric = "rmse") %>%
                                          pull(min_n)) %>%
        set_mode("regression") %>%
        set_engine("ranger", importance = "permutation")

random_forest_spec


#//////////////////////////////////////////////////////////////////////////////////////////


trucks_random_forest_fit <- random_forest_spec %>% 
        fit(formula = monthly_count ~ ., data = trucks_training_data %>% select(-c(date, measure)))


# save trucks_random_forest_fit ####
trucks_random_forest_fit %>% saveRDS(object = ., file = "data/trucks_random_forest_fit.rds")

# load
trucks_random_forest_fit <- readRDS(file = "data/trucks_random_forest_fit.rds")


#/////////////////////////////


trucks_random_forest_fit
trucks_random_forest_fit %>% vip(geom = "point")


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_random_forest_testing_results_short_term ####

trucks_random_forest_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_random_forest_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_random_forest_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NA, pred_upper = NA) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_random_forest_testing_results_short_term ####
trucks_random_forest_testing_results_short_term %>% 
        write_csv(file = "data/trucks_random_forest_testing_results_short_term.csv")


#/////////////////////


trucks_random_forest_testing_results_short_term %>% glimpse() 
trucks_random_forest_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_random_forest_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_random_forest_testing_results_medium_term ####

trucks_random_forest_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_random_forest_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_random_forest_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NA, pred_upper = NA) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_random_forest_testing_results_medium_term ####
trucks_random_forest_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_random_forest_testing_results_medium_term.csv")


#/////////////////////


trucks_random_forest_testing_results_medium_term %>% glimpse() 
trucks_random_forest_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_random_forest_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_random_forest_testing_results_long_term ####

trucks_random_forest_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_random_forest_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_random_forest_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NA, pred_upper = NA) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
               rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)


# save trucks_random_forest_testing_results_long_term ####
trucks_random_forest_testing_results_long_term %>% 
        write_csv(file = "data/trucks_random_forest_testing_results_long_term.csv")


#/////////////////////


trucks_random_forest_testing_results_long_term %>% glimpse() 
trucks_random_forest_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_random_forest_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get random_forest_testing_results_combined

random_forest_testing_results_combined <- trucks_random_forest_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_random_forest_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_random_forest_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

random_forest_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# random_forest ts cross-validation ####


# create get_random_forest_tscv_error
get_random_forest_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        random_forest_spec <- rand_forest(mtry = 13, trees = 1000, min_n = 5) %>%
                set_mode("regression") %>%
                set_engine("ranger", importance = "permutation")
        
        current_random_forest_fit <- random_forest_spec %>% 
                fit(formula = monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_random_forest_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_random_forest_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "random_forest") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_random_forest_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_random_forest_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "random_forest") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_random_forest_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_random_forest_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "random_forest") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_random_forest_tscv_error
trucks_training_random_forest_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                    .f = ~ get_random_forest_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_random_forest_tscv_error ####
trucks_training_random_forest_tscv_error %>% write_csv(file = "data/trucks_training_random_forest_tscv_error.csv")


#////////////////////////


trucks_training_random_forest_tscv_error
trucks_training_random_forest_tscv_error %>% glimpse()

trucks_training_random_forest_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_random_forest_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_random_forest_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get xgb model ####

# get model tune_spec
xgb_tune_spec <- boost_tree(trees = 1000,
        tree_depth = tune(), 
        min_n = tune(),
        loss_reduction = tune(),                     
        sample_size = tune(), 
        mtry = tune(),        
        learn_rate = tune()) %>%
        set_engine("xgboost") %>%
        set_mode("regression")

xgb_tune_spec


#////////////////////////////////////////////////////////////////////////////////////////////


# get tune grid
xgb_tune_grid_1 <- grid_space_filling(tree_depth(),
        min_n(),
        loss_reduction(),
        sample_size = sample_prop(),
        finalize(mtry(), trucks_training_data %>% select(-c(date, measure))),
        learn_rate(),
        size = 30)

xgb_tune_grid_1


#////////////////////////////////////////////////////////////////////////////////////////////


# get tune workflow
xgb_tune_workflow <- workflow() %>%
        add_formula(monthly_count ~ .) %>%
        add_model(xgb_tune_spec)

xgb_tune_workflow


#////////////////////////////////////////////////////////////////////////////////////////////


# get tune_results

doParallel::registerDoParallel()
set.seed(123)
xgb_tune_results <- tune_grid(object = xgb_tune_workflow,
        resamples = trucks_training_tscv_splits_synthetic,
        grid = xgb_tune_grid_1,
        control = control_stack_grid())

# save xgb_tune_results ####
xgb_tune_results %>% saveRDS(object = ., file = "data/xgb_tune_results.rds")

# load
xgb_tune_results <- readRDS(file = "data/xgb_tune_results.rds")


#///////////////////////


xgb_tune_results_1
xgb_tune_results_1 %>% slice(1) %>% select(.metrics) %>% unnest(cols = .metrics)
xgb_tune_results_1 %>% collect_metrics()
xgb_tune_results_1 %>% collect_metrics() %>% count(.metric)
xgb_tune_results_1 %>% collect_metrics() %>% 
        count(mtry, min_n, tree_depth, learn_rate, loss_reduction, sample_size)
xgb_tune_results_1 %>% collect_metrics() %>% count(.config)

xgb_tune_results_1 %>% show_best(metric = "rmse")
xgb_tune_results_1 %>% select_best(metric = "rmse")

# plot
xgb_tune_results_1 %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, mtry, min_n, tree_depth, learn_rate, loss_reduction, sample_size) %>%
        pivot_longer(cols = mtry:sample_size, names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(facets = vars(var), scales = "free_x") +
        labs(x = NULL, y = "rmse")


#////////////////////////////////////////////////////////////////////////////////////////////


# get model_spec
xgb_spec <- boost_tree(trees = 1000,
                            tree_depth = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(tree_depth), 
                            min_n = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(min_n),
                            loss_reduction = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(loss_reduction),                     
                            sample_size = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(sample_size), 
                            mtry = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(mtry),        
                            learn_rate = xgb_tune_results_1 %>% 
                               select_best(metric = "rmse") %>% 
                               pull(learn_rate)) %>%
        set_engine("xgboost") %>%
        set_mode("regression")

xgb_spec


#////////////////////////////////////////////////////////////////////////////////////////////


# fit model

trucks_xgb_fit <- xgb_spec %>% 
        fit(formula = monthly_count ~ ., data = trucks_training_data %>% select(-c(date, measure)))

# save trucks_xgb_fit####
trucks_xgb_fit %>% saveRDS(object = ., file = "data/trucks_xgb_fit.rds")

# load
trucks_xgb_fit <- readRDS(file = "data/trucks_xgb_fit.rds")


#/////////////////////


trucks_xgb_fit
trucks_xgb_fit %>% vip(geom = "point")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_xgb_testing_results_short_term ####

trucks_xgb_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_xgb_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_xgb_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_xgb_testing_results_short_term ####
trucks_xgb_testing_results_short_term %>% 
        write_csv(file = "data/trucks_xgb_testing_results_short_term.csv")


#/////////////////////


trucks_xgb_testing_results_short_term
trucks_xgb_testing_results_short_term %>% glimpse() 
trucks_xgb_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_xgb_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_xgb_testing_results_medium_term ####

trucks_xgb_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_xgb_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_xgb_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_xgb_testing_results_medium_term ####
trucks_xgb_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_xgb_testing_results_medium_term.csv")


#/////////////////////


trucks_xgb_testing_results_medium_term
trucks_xgb_testing_results_medium_term %>% glimpse() 
trucks_xgb_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_xgb_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_xgb_testing_results_long_term #### 

trucks_xgb_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_xgb_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_xgb_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
               rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_xgb_testing_results_long_term ####
trucks_xgb_testing_results_long_term %>% 
        write_csv(file = "data/trucks_xgb_testing_results_long_term.csv")


#/////////////////////


trucks_xgb_testing_results_long_term %>% glimpse() 
trucks_xgb_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_xgb_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get xgb_testing_results_combined 

xgb_testing_results_combined <- trucks_xgb_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_xgb_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_xgb_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

xgb_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# xgb ts cross-validation ####


# create get_xgb_tscv_error
get_xgb_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        xgb_spec <- xgb_spec <- boost_tree(trees = 1000,
                                           tree_depth = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(tree_depth), 
                                           min_n = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(min_n),
                                           loss_reduction = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(loss_reduction),                     
                                           sample_size = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(sample_size), 
                                           mtry = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(mtry),        
                                           learn_rate = xgb_tune_results_1 %>% 
                                                   select_best(metric = "rmse") %>% 
                                                   pull(learn_rate)) %>%
                set_engine("xgboost") %>%
                set_mode("regression")
        
        current_xgb_fit <- xgb_spec %>% 
                fit(formula = monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_xgb_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_xgb_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "xgb") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_xgb_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_xgb_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "xgb") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_xgb_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_xgb_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "xgb") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_xgb_tscv_error
trucks_training_xgb_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                              .f = ~ get_xgb_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_xgb_tscv_error ####
trucks_training_xgb_tscv_error %>% write_csv(file = "data/trucks_training_xgb_tscv_error.csv")


#////////////////////////


trucks_training_xgb_tscv_error
trucks_training_xgb_tscv_error %>% glimpse()

trucks_training_xgb_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_xgb_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_xgb_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[1]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get lasso model ####


# get model tune_spec
lasso_tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
        set_engine("glmnet")

lasso_tune_spec


#////////////////////////////////////////////////////////////////////////////////////////////


# get workflow
lasso_workflow <- workflow() %>%
        add_formula(monthly_count ~ .) %>%
        add_model(lasso_tune_spec)

lasso_workflow


#////////////////////////////////////////////////////////////////////////////////////////////


# get lasso_grid
lasso_grid <- grid_regular(penalty(), levels = 50)

lasso_grid


#////////////////////////////////////////////////////////////////////////////////////////////


# tune
doParallel::registerDoParallel()
set.seed(123)
lasso_tune_results <- tune_grid(object = lasso_workflow, 
                        resamples = trucks_training_tscv_splits_synthetic, 
                        grid = lasso_grid,
                        control = control_stack_grid())

# save lasso_tune_results ####
lasso_tune_results %>% saveRDS(object = ., file = "data/lasso_tune_results.rds")

# load
lasso_tune_results <- readRDS(file = "data/lasso_tune_results.rds")


#/////////////////////////


lasso_tune_results 
lasso_tune_results %>% slice(1) %>% select(.metrics) %>% unnest(cols = .metrics)
lasso_tune_results %>% collect_metrics()
lasso_tune_results %>% collect_metrics() %>% count(.metric)
lasso_tune_results %>% collect_metrics() %>% count(penalty)
lasso_tune_results %>% collect_metrics() %>% count(.config)

lasso_tune_results %>% show_best(metric = "rmse")
lasso_tune_results %>% select_best(metric = "rmse")

# plot
lasso_tune_results %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, penalty) %>%
        pivot_longer(cols = c(penalty), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(alpha = 0.8, show.legend = FALSE) +
        labs(x = NULL, y = "rmse")


#//////////////////////////////////////////////////////////////////////////////////////////


# get model spec
lasso_spec <- linear_reg(penalty = lasso_tune_results %>% 
                                         select_best(metric = "rmse") %>%
                                         pull(penalty), 
                                 mixture = 1) %>%
        set_engine("glmnet")

lasso_spec


#//////////////////////////////////////////////////////////////////////////////////////////


trucks_lasso_fit <- lasso_spec %>% 
        fit(formula = monthly_count ~ ., data = trucks_training_data %>% select(-c(date, measure)))

# save trucks_lasso_fit ####
trucks_lasso_fit %>% saveRDS(object = ., file = "data/trucks_lasso_fit.rds")

# load
trucks_lasso_fit <- readRDS(file = "data/trucks_lasso_fit.rds")


#/////////////////////////////


trucks_lasso_fit
trucks_lasso_fit %>% tidy()
trucks_lasso_fit %>% vip(geom = "point")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_lasso_testing_results_short_term ####

trucks_lasso_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_lasso_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_lasso_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_lasso_testing_results_short_term ####
trucks_lasso_testing_results_short_term %>% 
        write_csv(file = "data/trucks_lasso_testing_results_short_term.csv")


#/////////////////////


trucks_lasso_testing_results_short_term
trucks_lasso_testing_results_short_term %>% glimpse() 
trucks_lasso_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_lasso_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_lasso_testing_results_medium_term ####

trucks_lasso_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_lasso_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_lasso_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_lasso_testing_results_medium_term ####
trucks_lasso_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_lasso_testing_results_medium_term.csv")


#/////////////////////


trucks_lasso_testing_results_medium_term
trucks_lasso_testing_results_medium_term %>% glimpse() 
trucks_lasso_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_lasso_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_lasso_testing_results_long_term #### 

trucks_lasso_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_lasso_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_lasso_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
               rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_lasso_testing_results_long_term ####
trucks_lasso_testing_results_long_term %>% 
        write_csv(file = "data/trucks_lasso_testing_results_long_term.csv")


#/////////////////////


trucks_lasso_testing_results_long_term %>% glimpse() 
trucks_lasso_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_lasso_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get lasso_testing_results_combined 

lasso_testing_results_combined <- trucks_lasso_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_lasso_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_lasso_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

lasso_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# lasso ts cross-validation ####


# create get_lasso_tscv_error
get_lasso_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        lasso_spec <- linear_reg(penalty = lasso_tune_results %>% 
                                         select_best(metric = "rmse") %>%
                                         pull(penalty), 
                                 mixture = 1) %>%
                set_engine("glmnet")
        
        current_lasso_fit <- lasso_spec %>% 
                fit(formula = monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_lasso_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_lasso_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "lasso") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_lasso_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_lasso_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "lasso") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_lasso_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_lasso_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "lasso") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_lasso_tscv_error
trucks_training_lasso_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                      .f = ~ get_lasso_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_lasso_tscv_error ####
trucks_training_lasso_tscv_error %>% write_csv(file = "data/trucks_training_lasso_tscv_error.csv")


#////////////////////////


trucks_training_lasso_tscv_error
trucks_training_lasso_tscv_error %>% glimpse()

trucks_training_lasso_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_lasso_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_lasso_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get cubist model ####

# get model tune_spect
cubist_tune_spec <- cubist_rules(committees = tune(), neighbors = tune(),
                                   mode = "regression") %>%
        set_engine("Cubist")

cubist_tune_spec


#////////////////////////////////////////////////////////////////////////////////////////////


# get workflow
cubist_workflow <- workflow() %>%
        add_formula(monthly_count ~ .) %>%
        add_model(cubist_tune_spec)

cubist_workflow


#////////////////////////////////////////////////////////////////////////////////////////////


# get cubist_grid
cubist_grid <- expand.grid(committees = 1:30, 
                           neighbors = c(1, 3, 5, 7, 9))

cubist_grid


#////////////////////////////////////////////////////////////////////////////////////////////


# tune
doParallel::registerDoParallel()
set.seed(123)
cubist_tune_results <- tune_grid(object = cubist_workflow, 
                                resamples = trucks_training_tscv_splits_synthetic, 
                                grid = cubist_grid,
                                control = control_stack_grid())

# save cubist_tune_results ####
cubist_tune_results %>% saveRDS(object = ., file = "data/cubist_tune_results.rds")

# load
cubist_tune_results <- readRDS(file = "data/cubist_tune_results.rds")


#/////////////////////////


cubist_tune_results 
cubist_tune_results %>% slice(1) %>% select(.metrics) %>% unnest(cols = .metrics)
cubist_tune_results %>% collect_metrics()
cubist_tune_results %>% collect_metrics() %>% count(.metric)
cubist_tune_results %>% collect_metrics() %>% count(penalty)
cubist_tune_results %>% collect_metrics() %>% count(.config)

cubist_tune_results %>% show_best(metric = "rmse")
cubist_tune_results %>% select_best(metric = "rmse")

# plot
cubist_tune_results %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, committees, neighbors) %>%
        pivot_longer(cols = c(committees, neighbors), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(alpha = 0.8, show.legend = FALSE) +
        labs(x = NULL, y = "rmse") + 
        facet_wrap(facets = vars(var))


#//////////////////////////////////////////////////////////////////////////////////////////


# get model spec
cubist_spec <- cubist_rules(committees = cubist_tune_results %>% 
                                    select_best(metric = "rmse") %>%
                                    pull(committees), 
                            neighbors = cubist_tune_results %>% 
                                    select_best(metric = "rmse") %>%
                                    pull(neighbors),
                            mode = "regression") %>%
        set_engine("Cubist")

cubist_spec


#//////////////////////////////////////////////////////////////////////////////////////////


trucks_cubist_fit <- cubist_spec %>% 
        fit(formula = monthly_count ~ ., data = trucks_training_data %>% select(-c(date, measure)))

# save trucks_cubist_fit ####
trucks_cubist_fit %>% saveRDS(object = ., file = "data/trucks_cubist_fit.rds")

# load
trucks_cubist_fit <- readRDS(file = "data/trucks_cubist_fit.rds")


#/////////////////////////////


trucks_cubist_fit
trucks_cubist_fit %>% tidy()
trucks_cubist_fit %>% vip(geom = "point")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_cubist_testing_results_short_term ####

trucks_cubist_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_cubist_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_cubist_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_cubist_testing_results_short_term ####
trucks_cubist_testing_results_short_term %>% 
        write_csv(file = "data/trucks_cubist_testing_results_short_term.csv")


#/////////////////////


trucks_cubist_testing_results_short_term
trucks_cubist_testing_results_short_term %>% glimpse() 
trucks_cubist_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_cubist_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_cubist_testing_results_medium_term ####

trucks_cubist_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_cubist_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_cubist_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_cubist_testing_results_medium_term ####
trucks_cubist_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_cubist_testing_results_medium_term.csv")


#/////////////////////


trucks_cubist_testing_results_medium_term
trucks_cubist_testing_results_medium_term %>% glimpse() 
trucks_cubist_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_cubist_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_cubist_testing_results_long_term #### 

trucks_cubist_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_cubist_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_cubist_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
               rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_cubist_testing_results_long_term ####
trucks_cubist_testing_results_long_term %>% 
        write_csv(file = "data/trucks_cubist_testing_results_long_term.csv")


#/////////////////////


trucks_cubist_testing_results_long_term %>% glimpse() 
trucks_cubist_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_cubist_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get cubist_testing_results_combined 

cubist_testing_results_combined <- trucks_cubist_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_cubist_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_cubist_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

cubist_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# cubist ts cross-validation ####


# create get_cubist_tscv_error
get_cubist_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        cubist_spec <- cubist_rules(committees = cubist_tune_results %>% 
                                            select_best(metric = "rmse") %>%
                                            pull(committees), 
                                    neighbors = cubist_tune_results %>% 
                                            select_best(metric = "rmse") %>%
                                            pull(neighbors),
                                    mode = "regression") %>%
                set_engine("Cubist")
        
        current_cubist_fit <- cubist_spec %>% 
                fit(formula = monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_cubist_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_cubist_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "cubist") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_cubist_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_cubist_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "cubist") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_cubist_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_cubist_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "cubist") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_cubist_tscv_error
trucks_training_cubist_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                        .f = ~ get_cubist_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_cubist_tscv_error ####
trucks_training_cubist_tscv_error %>% write_csv(file = "data/trucks_training_cubist_tscv_error.csv")


#////////////////////////


trucks_training_cubist_tscv_error
trucks_training_cubist_tscv_error %>% glimpse()

trucks_training_cubist_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_cubist_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_cubist_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get ensemble model ####

# load tuning/fit_resample_results
ols_fit_resamples_results <- readRDS(file = "data/ols_fit_resamples_results.rds")
random_forest_tune_results_2 <- readRDS(file = "data/random_forest_tune_results_2.rds")
xgb_tune_results <- readRDS(file = "data/xgb_tune_results.rds")
lasso_tune_results <- readRDS(file = "data/lasso_tune_results.rds")
cubist_tune_results <- readRDS(file = "data/cubist_tune_results.rds")


#////////////////////////////////////////////////////////////////////////////////////////////


# get ensemble_stack
trucks_ensemble_stack <- stacks() %>%
        add_candidates(ols_fit_resamples_results) %>%
        add_candidates(random_forest_tune_results_2) %>%
        add_candidates(xgb_tune_results) %>%
        add_candidates(lasso_tune_results) %>%
        add_candidates(cubist_tune_results)


#///////////////////////


trucks_ensemble_stack
trucks_ensemble_stack %>% as_tibble()
trucks_ensemble_stack %>% as_tibble() %>% glimpse()


#////////////////////////////////////////////////////////////////////////////////////////////


# blend_predictions
trucks_ensemble_model <- trucks_ensemble_stack %>% blend_predictions()
trucks_ensemble_model


#////////////////////////////////////////////////////////////////////////////////////////////


# fit ensemble 
trucks_ensemble_fit <- trucks_ensemble_model %>% fit_members()


#//////////////////////////


trucks_ensemble_fit

trucks_ensemble_fit %>% collect_parameters("ols_fit_resamples_results")
trucks_ensemble_fit %>% collect_parameters("random_forest_tune_results_2")
trucks_ensemble_fit %>% collect_parameters("xgb_tune_results")
trucks_ensemble_fit %>% collect_parameters("lasso_tune_results")
trucks_ensemble_fit %>% collect_parameters("cubist_tune_results")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_ensemble_testing_results_short_term ####

trucks_ensemble_testing_results_short_term <- trucks_testing_data %>%
        bind_cols(., trucks_ensemble_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_ensemble_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:3) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ensemble_testing_results_short_term ####
trucks_ensemble_testing_results_short_term %>% 
        write_csv(file = "data/trucks_ensemble_testing_results_short_term.csv")


#/////////////////////


trucks_ensemble_testing_results_short_term
trucks_ensemble_testing_results_short_term %>% glimpse() 
trucks_ensemble_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_ensemble_testing_results_short_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_ensemble_testing_results_medium_term ####

trucks_ensemble_testing_results_medium_term <- trucks_testing_data %>%
        bind_cols(., trucks_ensemble_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_ensemble_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:12) %>%
        mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ensemble_testing_results_medium_term ####
trucks_ensemble_testing_results_medium_term %>% 
        write_csv(file = "data/trucks_ensemble_testing_results_medium_term.csv")


#/////////////////////


trucks_ensemble_testing_results_medium_term
trucks_ensemble_testing_results_medium_term %>% glimpse() 
trucks_ensemble_testing_results_medium_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_ensemble_testing_results_medium_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line()


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_ensemble_testing_results_long_term #### 

trucks_ensemble_testing_results_long_term <- trucks_testing_data %>%
        bind_cols(., trucks_ensemble_fit %>%
                          predict(object = ., new_data = trucks_testing_data, type = "numeric")) %>%
        rename(pred = .pred) %>%
        # bind_cols(., trucks_ensemble_fit %>%
        #                   predict(object = ., new_data = trucks_testing_data, type = "conf_int")) %>%
        # rename(pred_lower = .pred_lower, 
        #        pred_upper = .pred_upper) %>%
        mutate(pred_lower = NULL, 
               pred_uper = NULL) %>%
        left_join(., trucks_testing_snaive_benchmark %>% select(date, snaive_pred), by = c("date" = "date")) %>%
        left_join(., trucks_testing_drift_benchmark %>% select(date, drift_pred), by = c("date" = "date")) %>%
        arrange(date) %>%
        slice(1:48) %>%
        mutate(pred_sq_error = (monthly_count - pred)^2,
               drift_pred_sq_error = (monthly_count - drift_pred)^2,
               rmse = sqrt(mean((monthly_count - pred)^2)),
               mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
               snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
               snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
               drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
               drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100)

# save trucks_ensemble_testing_results_long_term ####
trucks_ensemble_testing_results_long_term %>% 
        write_csv(file = "data/trucks_ensemble_testing_results_long_term.csv")


#/////////////////////


trucks_ensemble_testing_results_long_term %>% glimpse() 
trucks_ensemble_testing_results_long_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)

trucks_ensemble_testing_results_long_term %>% 
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        filter(var %in% c("monthly_count", "drift_pred", "pred", "snaive_pred")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 month") +
        theme(
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1)
        )


#////////////////////////////////////////////////////////////////////////////////////////////


# get ensemble_testing_results_combined 

ensemble_testing_results_combined <- trucks_ensemble_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        mutate(model = "ols", horizon = "3_month") %>%
        relocate(c(model, horizon), .before = everything()) %>%
        bind_rows(., 
                  trucks_ensemble_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "12_month") %>%
                          relocate(c(model, horizon), .before = everything())) %>%
        bind_rows(., 
                  trucks_ensemble_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                          mutate(model = "ols", horizon = "48_month") %>%
                          relocate(c(model, horizon), .before = everything()))

ensemble_testing_results_combined


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# ensemble ts cross-validation ####


# create get_ensemble_tscv_error
get_ensemble_tscv_error <- function(tscv_splits, current_split) {
        
        print(str_glue("current_split is {current_split}"))
        
        current_analysis_data <- analysis(tscv_splits$splits[[current_split]])
        current_assessment_data <- assessment(tscv_splits$splits[[current_split]])
        
        ensemble_spec <- ensemble_rules(committees = ensemble_tune_results %>% 
                                            select_best(metric = "rmse") %>%
                                            pull(committees), 
                                    neighbors = ensemble_tune_results %>% 
                                            select_best(metric = "rmse") %>%
                                            pull(neighbors),
                                    mode = "regression") %>%
                set_engine("ensemble")
        
        current_ensemble_fit <- ensemble_spec %>% 
                fit(formula = monthly_count ~ ., data = current_analysis_data %>% select(-c(date)))
        
        current_assessment_3_month_results <- current_assessment_data %>%
                slice(1:3) %>%
                bind_cols(., current_ensemble_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_ensemble_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 3) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 3,
                       model = "ensemble") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_12_month_results <- current_assessment_data %>%
                slice(1:12) %>%
                bind_cols(., current_ensemble_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:12), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_ensemble_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 12) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 12,
                       model = "ensemble") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_48_month_results <- current_assessment_data %>%
                slice(1:48) %>%
                bind_cols(., current_ensemble_fit %>%
                                  predict(object = ., new_data = current_assessment_data %>% slice(1:48), type = "numeric")) %>%
                rename(pred = .pred) %>%
                # bind_cols(., trucks_ensemble_fit %>%
                #                   predict(object = ., new_data = current_assessment_data %>% slice(1:3), type = "conf_int")) %>%
                # rename(pred_lower = .pred_lower,
                #        pred_upper = .pred_upper) %>%
                mutate(pred_lower = NA, pred_upper = NA) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = RW(monthly_count ~ drift(drift = TRUE))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         drift_pred = .mean) %>%
                                  select(date, drift_pred),
                          by = c("date" = "date")) %>%
                left_join(., current_analysis_data %>% 
                                  mutate(year_month = yearmonth(date)) %>%
                                  as_tsibble(index = year_month) %>%
                                  model(drift = SNAIVE(monthly_count ~ lag("year"))) %>%
                                  forecast(h = 48) %>%
                                  as_tibble() %>%
                                  mutate(date = ym(year_month),
                                         snaive_pred = .mean) %>%
                                  select(date, snaive_pred),
                          by = c("date" = "date")) %>%
                mutate(rmse = sqrt(mean((monthly_count - pred)^2)),
                       mape = mean(abs((monthly_count - pred) / monthly_count)) * 100,
                       snaive_rmse = sqrt(mean((monthly_count - snaive_pred)^2)),
                       snaive_mape = mean(abs((monthly_count - snaive_pred) / monthly_count)) * 100,
                       drift_rmse = sqrt(mean((monthly_count - drift_pred)^2)),
                       drift_mape = mean(abs((monthly_count - drift_pred) / monthly_count)) * 100, 
                       split = current_split,
                       analysis_split_start_date = min(current_analysis_data %>% pull(date)),
                       analysis_split_end_date = max(current_analysis_data %>% pull(date)),
                       assessment_split_start_date = min(date),
                       assessment_split_end_date = max(date),
                       horizon = 48,
                       model = "ensemble") %>%
                select(model, horizon, split, analysis_split_start_date, analysis_split_end_date, 
                       assessment_split_start_date, assessment_split_end_date, 
                       rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
                slice(1)
        
        current_assessment_results <- current_assessment_3_month_results %>%
                bind_rows(., current_assessment_12_month_results) %>%
                bind_rows(., current_assessment_48_month_results)
        
        return(current_assessment_results)
}


#////////////////////////////////////////////////////////////////////////////////////////////


# get trucks_training_ensemble_tscv_error
trucks_training_ensemble_tscv_error <- map(.x = 1:nrow(trucks_training_tscv_splits_synthetic), 
                                         .f = ~ get_ensemble_tscv_error(tscv_splits = trucks_training_tscv_splits_synthetic, current_split = .x)) %>%
        bind_rows()

# save trucks_training_ensemble_tscv_error ####
trucks_training_ensemble_tscv_error %>% write_csv(file = "data/trucks_training_ensemble_tscv_error.csv")


#////////////////////////


trucks_training_ensemble_tscv_error
trucks_training_ensemble_tscv_error %>% glimpse()

trucks_training_ensemble_tscv_error %>%
        group_by(model, horizon) %>%
        summarize(rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape))

trucks_training_ensemble_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon))

trucks_training_ensemble_tscv_error %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = factor(horizon), y = values, fill = var)) + 
        geom_boxplot(position = "dodge")

analysis(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)
assessment(trucks_training_tscv_splits_synthetic$splits[[3]]) %>% count(year)


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# compare tscv_error ####


# load tscv_error
trucks_training_ols_tscv_error <- read_csv(file = "data/trucks_training_ols_tscv_error.csv")
trucks_training_arima_tscv_error <- read_csv(file = "data/trucks_training_arima_tscv_error.csv")
trucks_training_regarima_tscv_error <- read_csv(file = "data/trucks_training_regarima_tscv_error.csv")
trucks_training_random_forest_tscv_error <- read_csv(file = "data/trucks_training_random_forest_tscv_error.csv")
trucks_training_xgb_tscv_error <- read_csv(file = "data/trucks_training_xgb_tscv_error.csv")
trucks_training_lasso_tscv_error <- read_csv(file = "data/trucks_training_lasso_tscv_error.csv")
trucks_training_cubist_tscv_error <- read_csv(file = "data/trucks_training_cubist_tscv_error.csv")


#////////////////////////////////////////////////////////////////////////////////////////////////////


# note that testing_error is generally lower than tscv_error, which makes sense because
# the models used for testing_error were training on more data, 
# had financial crisis in training_data.

# also note that ols does better on tscv_error probably because the covariates when predicting the 
# assessment data are actual values, whereas for testing_error the covariates are arima_forecasts
trucks_training_ols_tscv_error %>%
        group_by(horizon) %>%
        summarize(ols_rmse_mean = mean(rmse),
                  snaive_rmse_mean = mean(snaive_rmse),
                  drift_rmse_mean = mean(drift_rmse),
                  ols_mape_mean = mean(mape),
                  snaive_mape_mean = mean(snaive_mape),
                  drift_mape_mean = mean(drift_mape)) %>%
        left_join(., trucks_training_arima_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(arima_rmse_mean = mean(rmse),
                                    arima_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_training_regarima_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(regarima_rmse_mean = mean(rmse),
                                    regarima_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_training_random_forest_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(random_forest_rmse_mean = mean(rmse),
                                    random_forest_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_training_xgb_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(xgb_rmse_mean = mean(rmse),
                                    xgb_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_training_lasso_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(lasso_rmse_mean = mean(rmse),
                                    lasso_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_training_cubist_tscv_error %>%
                          group_by(horizon) %>%
                          summarize(cubist_rmse_mean = mean(rmse),
                                    cubist_mape_mean = mean(mape)),
                  by = c("horizon" = "horizon")) %>%
        mutate(type = "tscv_error") %>%
        # select(type, horizon, ols_rmse_mean, arima_rmse_mean, regarima_rmse_mean,
        #        random_forest_rmse_mean, xgb_rmse_mean, lasso_rmse_mean, cubist_rmse_mean,
        # drift_rmse_mean, snaive_rmse_mean)
        select(type, horizon, ols_mape_mean, arima_mape_mean, regarima_mape_mean,
                random_forest_mape_mean, xgb_mape_mean, lasso_mape_mean, cubist_mape_mean,
               drift_mape_mean, snaive_mape_mean)


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# compare testing_data error ####


# load testing_data_results
trucks_ols_testing_results_short_term <- read_csv(file = "data/trucks_ols_testing_results_short_term.csv")
trucks_ols_testing_results_medium_term <- read_csv(file = "data/trucks_ols_testing_results_medium_term.csv")
trucks_ols_testing_results_long_term <- read_csv(file = "data/trucks_ols_testing_results_long_term.csv")

trucks_arima_testing_results_short_term <- read_csv(file = "data/trucks_arima_testing_results_short_term.csv")
trucks_arima_testing_results_medium_term <- read_csv(file = "data/trucks_arima_testing_results_medium_term.csv")
trucks_arima_testing_results_long_term <- read_csv(file = "data/trucks_arima_testing_results_long_term.csv")

trucks_regarima_testing_results_short_term <- read_csv(file = "data/trucks_regarima_testing_results_short_term.csv")
trucks_regarima_testing_results_medium_term <- read_csv(file = "data/trucks_regarima_testing_results_medium_term.csv")
trucks_regarima_testing_results_long_term <- read_csv(file = "data/trucks_regarima_testing_results_long_term.csv")

trucks_random_forest_testing_results_short_term <- read_csv(file = "data/trucks_random_forest_testing_results_short_term.csv")
trucks_random_forest_testing_results_medium_term <- read_csv(file = "data/trucks_random_forest_testing_results_medium_term.csv")
trucks_random_forest_testing_results_long_term <- read_csv(file = "data/trucks_random_forest_testing_results_long_term.csv")

trucks_xgb_testing_results_short_term <- read_csv(file = "data/trucks_xgb_testing_results_short_term.csv")
trucks_xgb_testing_results_medium_term <- read_csv(file = "data/trucks_xgb_testing_results_medium_term.csv")
trucks_xgb_testing_results_long_term <- read_csv(file = "data/trucks_xgb_testing_results_long_term.csv")

trucks_lasso_testing_results_short_term <- read_csv(file = "data/trucks_lasso_testing_results_short_term.csv")
trucks_lasso_testing_results_medium_term <- read_csv(file = "data/trucks_lasso_testing_results_medium_term.csv")
trucks_lasso_testing_results_long_term <- read_csv(file = "data/trucks_lasso_testing_results_long_term.csv")

trucks_cubist_testing_results_short_term <- read_csv(file = "data/trucks_cubist_testing_results_short_term.csv")
trucks_cubist_testing_results_medium_term <- read_csv(file = "data/trucks_cubist_testing_results_medium_term.csv")
trucks_cubist_testing_results_long_term <- read_csv(file = "data/trucks_cubist_testing_results_long_term.csv")


#////////////////////////////////////////////////////////////////////////////////////////////////////


# note that testing_error is generally lower than tscv_error, which makes sense because
# the models used for testing_error were training on more data, 
# had financial crisis in training_data.

# also note that ols does better on tscv_error probably because the covariates when predicting the 
# assessment data are actual values, whereas for testing_error the covariates are arima_forecasts
trucks_ols_testing_results_short_term %>% 
        distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape) %>%
        bind_rows(., trucks_ols_testing_results_medium_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)) %>%
        bind_rows(., trucks_ols_testing_results_long_term %>% 
                          distinct(rmse, snaive_rmse, drift_rmse, mape, snaive_mape, drift_mape)) %>%
        mutate(horizon = c(3, 12, 48)) %>%
        rename(ols_rmse = rmse, ols_mape = mape) %>%
        left_join(., trucks_arima_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_arima_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_arima_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(arima_rmse = rmse, arima_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_regarima_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_regarima_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_regarima_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(regarima_rmse = rmse, regarima_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_random_forest_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_random_forest_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_random_forest_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(random_forest_rmse = rmse, random_forest_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_xgb_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_xgb_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_xgb_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(xgb_rmse = rmse, xgb_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_lasso_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_lasso_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_lasso_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(lasso_rmse = rmse, lasso_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        left_join(., trucks_cubist_testing_results_short_term %>% 
                          distinct(rmse, mape) %>%
                          bind_rows(., trucks_cubist_testing_results_medium_term %>% 
                                            distinct(rmse, mape)) %>%
                          bind_rows(., trucks_cubist_testing_results_long_term %>% 
                                            distinct(rmse, mape)) %>%
                          mutate(horizon = c(3, 12, 48)) %>%
                          rename(cubist_rmse = rmse, cubist_mape = mape),
                  by = c("horizon" = "horizon")) %>%
        mutate(type = "testing_error") %>%
        # select(type, horizon, ols_rmse, arima_rmse, regarima_rmse,
        #        random_forest_rmse, xgb_rmse, lasso_rmse, cubist_rmse,
        # drift_rmse, snaive_rmse)
        select(type, horizon, ols_mape, arima_mape, regarima_mape,
               random_forest_mape, xgb_mape, lasso_mape, cubist_mape,
               drift_mape, snaive_mape)
        





library(tidyverse)
library(fs)
library(janitor)
library(skimr)
library(officer)
library(devEMF)
library(openxlsx)


options(scipen = 999)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS785")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# load border_data_3 ####
border_data_3 <- read_csv(file = "data/border_data_3.csv")

# load training/testing_data
trucks_training_data <- read_csv(file = "data/trucks_training_data.csv")
trucks_testing_data <- read_csv(file = "data/trucks_testing_data.csv")


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


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# trucks_line_chart ####

trucks_line_chart <- border_data_3 %>%
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        ggplot(data = ., mapping = aes(x = date, y = monthly_count, color = measure)) +
        geom_line(color = "#2474B6") +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .01/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "none",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

trucks_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(trucks_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/trucks_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# trucks_box_plot ####

trucks_box_plot <- border_data_3 %>%
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        ggplot(data = ., mapping = aes(x = fct_reorder(.f = factor(month), .x = month, .desc = FALSE), 
                                       y = monthly_count)) +
        geom_boxplot(fill = "#FFFFFF", color = "#2474B6") +
        # scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Month", y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .000015/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "none",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

trucks_box_plot


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(trucks_box_plot)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/trucks_box_plot.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# unemployment_line_chart ####

unemployment_line_chart <- border_data_3 %>%
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        select(date, us_unemployment_rate, canada_unemployment_rate, mexico_unemployment_rate) %>%
        pivot_longer(cols = c(us_unemployment_rate, canada_unemployment_rate, mexico_unemployment_rate),
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "us_unemployment_rate" ~ "US",
                               var == "canada_unemployment_rate" ~ "Canada",
                               var == "mexico_unemployment_rate" ~ "Mexico")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = number_format()) +
        labs(x = NULL, y = "Unemployment rate", color = NULL) +
        coord_fixed(ratio = 400/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

unemployment_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(unemployment_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/unemployment_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# gdp_per_capita_line_chart ####

gdp_per_capita_line_chart <- border_data_3 %>%
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        select(date, us_gdp_per_capita_monthly, canada_gdp_per_capita_monthly, mexico_gdp_per_capita_monthly) %>%
        pivot_longer(cols = c(us_gdp_per_capita_monthly, canada_gdp_per_capita_monthly, mexico_gdp_per_capita_monthly),
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "us_gdp_per_capita_monthly" ~ "US",
                               var == "canada_gdp_per_capita_monthly" ~ "Canada",
                               var == "mexico_gdp_per_capita_monthly" ~ "Mexico")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = dollar_format()) +
        labs(x = NULL, y = "GDP per capita", color = NULL) +
        coord_fixed(ratio = .07/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

gdp_per_capita_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(gdp_per_capita_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/gdp_per_capita_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# count_by_month_summary_table ####

border_data_3 %>% glimpse()

count_by_month_summary_table <- border_data_3 %>% 
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        select(month, monthly_count) %>%
        group_by(month) %>%
        skim() %>%
        as_tibble() %>%
        select(month, n_missing, numeric.mean:numeric.p100) %>%
        rename(Month = month,
               N_missing = n_missing,
               Mean = numeric.mean, 
               SD = numeric.sd,
               p0 = numeric.p0,
               p25 = numeric.p25,
               p50 = numeric.p50,
               p75 = numeric.p75,
               p100 = numeric.p100)

count_by_month_summary_table 

# save 
count_by_month_summary_table %>% write_csv(file = "output/tables/count_by_month_summary_table.csv")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# count_by_year_summary_table ####

border_data_3 %>% glimpse()

count_by_year_summary_table <- border_data_3 %>% 
        filter(date >= "1997-01-01" & date <= "2019-12-01",
               measure == "Commercial trucks") %>%
        select(year, monthly_count) %>%
        group_by(year) %>%
        skim() %>%
        as_tibble() %>%
        select(year, n_missing, numeric.mean:numeric.p100) %>%
        rename(Year = year,
               N_missing = n_missing,
               Mean = numeric.mean, 
               SD = numeric.sd,
               p0 = numeric.p0,
               p25 = numeric.p25,
               p50 = numeric.p50,
               p75 = numeric.p75,
               p100 = numeric.p100)

count_by_year_summary_table 

# save 
count_by_year_summary_table %>% write_csv(file = "output/tables/count_by_year_summary_table.csv")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# arima_testing_pred_line_chart ####


arima_testing_pred_line_chart <- read_csv(file = "data/trucks_arima_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "ARIMA",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

arima_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(arima_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/arima_testing_pred_line_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# regarima_testing_pred_line_chart ####


regarima_testing_pred_line_chart <- read_csv(file = "data/trucks_regarima_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "REGARIMA",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

regarima_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(regarima_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/regarima_testing_pred_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# ols_testing_pred_line_chart ####


ols_testing_pred_line_chart <- read_csv(file = "data/trucks_ols_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "OLS",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

ols_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ols_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/ols_testing_pred_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# lasso_testing_pred_line_chart ####


lasso_testing_pred_line_chart <- read_csv(file = "data/trucks_lasso_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "LASSO",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

lasso_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(lasso_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/lasso_testing_pred_line_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# random_forest_testing_pred_line_chart ####


random_forest_testing_pred_line_chart <- read_csv(file = "data/trucks_random_forest_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "Random Forest",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

random_forest_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(random_forest_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/random_forest_testing_pred_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# xgb_testing_pred_line_chart ####


xgb_testing_pred_line_chart <- read_csv(file = "data/trucks_xgb_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "XGBoost",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

xgb_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(xgb_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/xgb_testing_pred_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# cubist_testing_pred_line_chart ####


cubist_testing_pred_line_chart <- read_csv(file = "data/trucks_cubist_testing_results_long_term.csv") %>%
        select(date, monthly_count, pred, snaive_pred, drift_pred) %>%
        pivot_longer(cols = c(monthly_count, pred, snaive_pred, drift_pred), 
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "pred" ~ "Cubist",
                               var == "snaive_pred" ~ "Seas. Naive", 
                               var == "drift_pred" ~ "Drift",
                               var == "monthly_count" ~ "Actual")) %>%
        ggplot(data = ., mapping = aes(x = date, y = values, color = var)) +
        geom_line() +
        scale_x_date(date_breaks = "1 year") +
        scale_y_continuous(labels = comma_format()) +
        labs(x = NULL, y = "Monthly count", color = NULL) +
        coord_fixed(ratio = .003/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = -10, l = 0), angle = 45, hjust = 1),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

cubist_testing_pred_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(cubist_testing_pred_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/cubist_testing_pred_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# arima_tscv_error_line_chart ####

arima_tscv_error_line_chart <- read_csv(file = "data/trucks_training_arima_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                       horizon == 12 ~ "12 month",
                                       horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "ARIMA",
                                var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00006/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

arima_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(arima_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/arima_tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# regarima_tscv_error_line_chart ####

regarima_tscv_error_line_chart <- read_csv(file = "data/trucks_training_regarima_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "REGARIMA",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00006/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

regarima_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(regarima_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/regarima_tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# ols_tscv_error_line_chart ####

ols_tscv_error_line_chart <- read_csv(file = "data/trucks_training_ols_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "OLS",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00002/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

ols_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ols_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/ols_tscv_error_line_chart.docx")

#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# lasso_tscv_error_line_chart ####

lasso_tscv_error_line_chart <- read_csv(file = "data/trucks_training_lasso_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "LASSO",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00002/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

lasso_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(lasso_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/lasso_tscv_error_line_chart.docx")



#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# random_forest_tscv_error_line_chart ####

random_forest_tscv_error_line_chart <- read_csv(file = "data/trucks_training_random_forest_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "Random Forest",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00006/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

random_forest_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(random_forest_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/random_forest_tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# xgb_tscv_error_line_chart ####

xgb_tscv_error_line_chart <- read_csv(file = "data/trucks_training_xgb_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "XGBoost",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00004/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

xgb_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(xgb_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/xgb_tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# cubist_tscv_error_line_chart ####

cubist_tscv_error_line_chart <- read_csv(file = "data/trucks_training_cubist_tscv_error.csv") %>%
        pivot_longer(cols = c(rmse, snaive_rmse, drift_rmse), names_to = "var", values_to = "values") %>%
        mutate(horizon_label = case_when(horizon == 3 ~ "3 month", 
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month"),
               horizon_label = fct_reorder(.f = factor(horizon_label), .x = horizon, .desc = FALSE),
               var = case_when(var == "rmse" ~ "Cubist",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = split, y = values, color = var)) +
        geom_line() +
        facet_wrap(facets = vars(horizon_label)) +
        scale_y_continuous(labels = comma_format()) +
        labs(x = "Split #", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00004/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

cubist_tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(cubist_tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/cubist_tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# random_forest_tune_results_scatterplot ####

random_forest_tune_results_scatterplot <- readRDS(file = "data/random_forest_tune_results_1.rds") %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, mtry, min_n) %>%
        pivot_longer(cols = c(mtry, min_n), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(labels = comma_format()) +
        facet_wrap(facets = vars(var)) +
        labs(x = NULL, y = "RMSE") +
        coord_fixed(ratio = .005/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

random_forest_tune_results_scatterplot 


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(random_forest_tune_results_scatterplot )
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/random_forest_tune_results_scatterplot .docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# lasso_tune_results_scatterplot ####

lasso_tune_results_scatterplot <- readRDS(file = "data/lasso_tune_results.rds") %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, penalty) %>%
        pivot_longer(cols = c(penalty), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        facet_wrap(facets = vars(var)) +
        labs(x = NULL, y = "RMSE") +
        coord_fixed(ratio = 5/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

lasso_tune_results_scatterplot 


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(lasso_tune_results_scatterplot )
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/lasso_tune_results_scatterplot .docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# xgb_tune_results_scatterplot ####

xgb_tune_results_scatterplot <- readRDS(file = "data/xgb_tune_results.rds") %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, mtry, min_n, tree_depth, learn_rate, loss_reduction, sample_size) %>%
        pivot_longer(cols = mtry:sample_size, names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        facet_wrap(facets = vars(var)) +
        labs(x = NULL, y = "RMSE") +
        coord_fixed(ratio = .00005/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

xgb_tune_results_scatterplot 


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(xgb_tune_results_scatterplot )
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/xgb_tune_results_scatterplot .docx")



#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# cubist_tune_results_scatterplot ####

cubist_tune_results_scatterplot <- readRDS(file = "data/cubist_tune_results.rds") %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        select(mean, committees, neighbors) %>%
        pivot_longer(cols = c(committees, neighbors), names_to = "var", values_to = "values") %>%
        ggplot(data = ., mapping = aes(x = values, y = mean, color = var)) +
        geom_point(show.legend = FALSE) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        facet_wrap(facets = vars(var)) +
        labs(x = NULL, y = "RMSE") +
        coord_fixed(ratio = .0005/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

cubist_tune_results_scatterplot 


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(cubist_tune_results_scatterplot)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/cubist_tune_results_scatterplot.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# tscv_error_line_chart ####

# load tscv_error
trucks_training_ols_tscv_error <- read_csv(file = "data/trucks_training_ols_tscv_error.csv")
trucks_training_arima_tscv_error <- read_csv(file = "data/trucks_training_arima_tscv_error.csv")
trucks_training_regarima_tscv_error <- read_csv(file = "data/trucks_training_regarima_tscv_error.csv")
trucks_training_random_forest_tscv_error <- read_csv(file = "data/trucks_training_random_forest_tscv_error.csv")
trucks_training_xgb_tscv_error <- read_csv(file = "data/trucks_training_xgb_tscv_error.csv")
trucks_training_lasso_tscv_error <- read_csv(file = "data/trucks_training_lasso_tscv_error.csv")
trucks_training_cubist_tscv_error <- read_csv(file = "data/trucks_training_cubist_tscv_error.csv")


#////////////////////////////////////////////////////////////////////////////////////////////////////



tscv_error_line_chart <- trucks_training_ols_tscv_error %>%
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
        select(type, horizon, ols_rmse_mean, arima_rmse_mean, regarima_rmse_mean,
               random_forest_rmse_mean, xgb_rmse_mean, lasso_rmse_mean, cubist_rmse_mean,
               drift_rmse_mean, snaive_rmse_mean) %>%
        pivot_longer(cols = c(ols_rmse_mean, arima_rmse_mean, regarima_rmse_mean, 
                              random_forest_rmse_mean, xgb_rmse_mean, lasso_rmse_mean, cubist_rmse_mean,
                              drift_rmse_mean, snaive_rmse_mean),
                     names_to = "var", values_to = "values") %>%
        mutate(horizon_label = "Horizon months",
               var = case_when(var == "ols_rmse_mean" ~ "OLS",
                               var == "arima_rmse_mean" ~ "ARIMA",
                               var == "regarima_rmse_mean" ~ "REGARIMA",
                               var == "random_forest_rmse_mean" ~ "Random Forest",
                               var == "xgb_rmse_mean" ~ "XGBoost",
                               var == "lasso_rmse_mean" ~ "LASSO",
                               var == "cubist_rmse_mean" ~ "Cubist",
                               var == "drift_rmse_mean" ~ "Drift",
                               var == "snaive_rmse_mean" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = horizon, y = values, color = var)) +
        geom_line(show.legend = FALSE, size = 1) +
        geom_point(size = 2) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        labs(x = "Forecast horizon months", y = "TSCV RMSE", color = NULL) +
        coord_fixed(ratio = .0002/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

tscv_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(tscv_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/tscv_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# testing_error_line_chart ####

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
testing_error_line_chart <- trucks_ols_testing_results_short_term %>% 
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
        select(type, horizon, ols_rmse, arima_rmse, regarima_rmse,
               random_forest_rmse, xgb_rmse, lasso_rmse, cubist_rmse,
                drift_rmse, snaive_rmse) %>%
        pivot_longer(cols = c(ols_rmse, arima_rmse, regarima_rmse, 
                              random_forest_rmse, xgb_rmse, lasso_rmse, cubist_rmse,
                              drift_rmse, snaive_rmse),
                     names_to = "var", values_to = "values") %>%
        mutate(var = case_when(var == "ols_rmse" ~ "OLS",
                               var == "arima_rmse" ~ "ARIMA",
                               var == "regarima_rmse" ~ "REGARIMA",
                               var == "random_forest_rmse" ~ "Random Forest",
                               var == "xgb_rmse" ~ "XGBoost",
                               var == "lasso_rmse" ~ "LASSO",
                               var == "cubist_rmse" ~ "Cubist",
                               var == "drift_rmse" ~ "Drift",
                               var == "snaive_rmse" ~ "Seas. Naive")) %>%
        ggplot(data = ., mapping = aes(x = horizon, y = values, color = var)) +
        geom_line(show.legend = FALSE, size = 1) +
        geom_point(size = 2) +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        labs(x = "Forecast horizon months", y = "RMSE", color = NULL) +
        coord_fixed(ratio = .00025/1, clip = "off") +
        theme_bw() +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#DDDDDD"),
                panel.border = element_rect(color = "#DDDDDD", size = .5),
                strip.background = element_blank(),
                strip.text = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333"),
                axis.ticks.y = element_blank(),
                axis.ticks.x = element_line(size = .5),
                # axis.ticks.length.y.left = unit(.2, "cm"),
                axis.ticks.length.x.bottom = unit(.1, "cm"),
                axis.text.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 3, r = 0, b = -10, l = 0), angle = 0, hjust = .5),
                axis.text.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                           margin = margin(t = 0, r = 0, b = 0, l = 0)),
                axis.line.x.bottom = element_line(color = "#333333"),
                # axis.line.x.bottom = element_blank(),
                axis.line.y.left = element_blank(),
                axis.title.x = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333", 
                                            margin = margin(t = 13, r = 0, b = 5, l = 0)),
                axis.title.y = element_text(family = "Calibri", face = "plain", size = 10, color = "#333333",
                                            margin = margin(t = 0, r = 3, b = 0, l = 0)),
                # axis.title.y = element_blank(),
                # axis.text.y = element_blank(),
                plot.title = element_text(size = 10, face = "plain", hjust = .5, family = "Calibri", color = "#333333", 
                                          margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                legend.position = "right",
                # legend.key.size = unit(2, "mm"), 
                legend.title = element_text(size = 10, family = "Calibri", face = "plain", color = "#333333"),
                legend.text = element_text(size = 10, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
                # legend.spacing.y = unit(5.5, "cm"),
                # legend.key = element_rect(size = 5),
                # legend.key.size = unit(2, 'lines')
        )

testing_error_line_chart


#///////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(testing_error_line_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "output/charts/testing_error_line_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


# tscv_error_summary_table ####


# load tscv_error
trucks_training_ols_tscv_error <- read_csv(file = "data/trucks_training_ols_tscv_error.csv")
trucks_training_arima_tscv_error <- read_csv(file = "data/trucks_training_arima_tscv_error.csv")
trucks_training_regarima_tscv_error <- read_csv(file = "data/trucks_training_regarima_tscv_error.csv")
trucks_training_random_forest_tscv_error <- read_csv(file = "data/trucks_training_random_forest_tscv_error.csv")
trucks_training_xgb_tscv_error <- read_csv(file = "data/trucks_training_xgb_tscv_error.csv")
trucks_training_lasso_tscv_error <- read_csv(file = "data/trucks_training_lasso_tscv_error.csv")
trucks_training_cubist_tscv_error <- read_csv(file = "data/trucks_training_cubist_tscv_error.csv")


#////////////////////////////////////////////////////////////////////////////////////////////////////


tscv_error_summary_table <- trucks_training_ols_tscv_error %>%
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
        mutate(type = "TSCV error",
               horizon_label = case_when(horizon == 3 ~ "3 month",
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month")) %>%
        select(type, horizon_label, arima_rmse_mean, regarima_rmse_mean,
               ols_rmse_mean, lasso_rmse_mean, 
               random_forest_rmse_mean, xgb_rmse_mean, cubist_rmse_mean,
               drift_rmse_mean, snaive_rmse_mean) %>%
        rename(Type = type, "Forecast horizon" = horizon_label, "OLS RMSE" = ols_rmse_mean, 
               "ARIMA RMSE" = arima_rmse_mean,
               "REGARIMA RMSE" = regarima_rmse_mean, "Random Forest RMSE" = random_forest_rmse_mean, 
               "XGBoost RMSE" = xgb_rmse_mean,
               "Cubist RMSE" = cubist_rmse_mean, "LASSO RMSE" = lasso_rmse_mean, 
               "Drift RMSE" = drift_rmse_mean,
               "Seas. Naive RMSE" = snaive_rmse_mean)
        
        

tscv_error_summary_table

tscv_error_summary_table %>% write_csv(file = "output/tables/tscv_error_summary_table.csv")


#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////


# testing_error_summary_table  ####


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
testing_error_summary_table <- trucks_ols_testing_results_short_term %>% 
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
        mutate(type = "Testing error",
               horizon_label = case_when(horizon == 3 ~ "3 month",
                                         horizon == 12 ~ "12 month",
                                         horizon == 48 ~ "48 month")) %>%
        select(type, horizon_label, arima_rmse, regarima_rmse,
               ols_rmse, lasso_rmse, 
               random_forest_rmse, xgb_rmse, cubist_rmse,
               drift_rmse, snaive_rmse) %>%
        rename(Type = type, "Forecast horizon" = horizon_label, "OLS RMSE" = ols_rmse, 
               "ARIMA RMSE" = arima_rmse,
               "REGARIMA RMSE" = regarima_rmse, "Random Forest RMSE" = random_forest_rmse, 
               "XGBoost RMSE" = xgb_rmse,
               "Cubist RMSE" = cubist_rmse, "LASSO RMSE" = lasso_rmse, 
               "Drift RMSE" = drift_rmse,
               "Seas. Naive RMSE" = snaive_rmse)



testing_error_summary_table

testing_error_summary_table %>% write_csv(file = "output/tables/testing_error_summary_table.csv")
        






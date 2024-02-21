
# Install & Load ----------------------------------------------------------

install.packages("EEAaq")
install.packages("tidyverse")
install.packages("skimr")
install.packages("patchwork")
install.packages("timetk")

library(EEAaq)
library(tidyverse)
library(skimr)
library(patchwork)
library(timetk)



# Data --------------------------------------------------------------------

str(pollutants)
View(pollutants)
polluts <- c("PM10")

metadata <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)

str(metadata)
metadata |> count(ISO) |> arrange(desc(n)) |> head(20)

iso <- "IT"
metadata |> dplyr::filter(ISO %in% iso) |> count(NUTS1)
metadata |> dplyr::filter(ISO %in% iso) |> count(NUTS2) |> arrange(desc(n)) |> head(20)
zones <- c("Lombardia")
metadata |> dplyr::filter(NUTS2 %in% zones) |> count(NUTS3) |> pull(n) |> sum()

EEAaq_map_stations(
	zone_name = zones, NUTS_level = "NUTS2", bounds_level = "NUTS3", 
	pollutant = polluts, color = TRUE, dynamic = FALSE
)

data <- EEAaq_get_data(
	zone_name = zones, NUTS_level = "NUTS2", pollutant = polluts,
	from = 2017, to = 2019, verbose = TRUE
)

save(data, file = "airpol_lomb_pm10.RData")



# Data Aggregation --------------------------------------------------------

load("data/airpol_lomb_pm10.RData")

EEAaq_map_stations(
	data = data, NUTS_level = "NUTS2", bounds_level = "NUTS3", 
	color = TRUE, dynamic = FALSE
)

summ <- EEAaq_summary(data)
summ$Summary
summ$Summary_byStat

data_month <- EEAaq_time_aggregate(data, frequency = "monthly", aggr_fun = "mean")
skim(data_month)

data_year <- EEAaq_time_aggregate(data, frequency = "yearly", aggr_fun = "mean")
skim(data_year)



# EDA ---------------------------------------------------------------------


# * Maps ------------------------------------------------------------------

# map_year <- data_year |> 
# 	EEAaq_idw_map(
# 		pollutant = "PM10", aggr_fun = "mean", 
# 		distinct = FALSE, gradient = TRUE, idp = 2
# 	)
# save(map_year, file = "data/map_year.RData")
# 
# map_month <- data_month |> 
# 	EEAaq_idw_map(
# 		pollutant = "PM10", aggr_fun = "mean", 
# 		distinct = TRUE, gradient = TRUE, idp = 2
# 	)
# save(map_month, file = "data/map_month.RData")

load("data/map_year.RData")
map_year

load("data/map_month.RData")
map_month[[1]]

i = 1 # 1, 7, 13, 19, 25, 31
map_month[[i]] + map_month[[i + 1]] + map_month[[i + 2]] + 
	map_month[[i + 3]] + map_month[[i + 4]] + map_month[[i + 5]]



# * Time Series Plots -----------------------------------------------------

# ** Decomposition --------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |>
	summarise(pm10 = mean(pm10)) |>
	plot_stl_diagnostics(
		.date_var = date, .value = pm10, 
		.interactive = FALSE,
		.feature_set = c("observed", "season", "trend", "remainder"),
		.title = "PM10 Monthly Time Series"
	)


# ** Trend ----------------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series(
		.date_var = date, .value = pm10, 
		.smooth = TRUE, .interactive = FALSE,
		.title = "PM10 Monthly Time Series"
	)

data_year |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series(
		.date_var = date, .value = pm10, 
		.smooth = FALSE, .interactive = FALSE,
		.title = "PM10 Monthly Time Series"
	)


# ** Seasonality ----------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	plot_time_series_boxplot(
		.date_var = date, .value = pm10, .period = "1 month",
		.smooth = TRUE, .interactive = FALSE,
		.title = "PM10 Monthly Time Series"
	)

data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	plot_seasonal_diagnostics(
		.date_var = date, .value = pm10,
		.interactive = FALSE,
		.title = "PM10 Monthly Time Series"
	)


# ** Autocorrelation ------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_acf_diagnostics(
		.date_var = date, .value = pm10,
		.interactive = FALSE,
		.title = "PM10 Monthly Time Series"
	)


# ** Anomalies ------------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_anomaly_diagnostics(
		.date_var = date, .value = pm10, 
		.interactive = FALSE, .alpha = 0.05, .max_anomalies = 0.1,
		.title = "PM10 Monthly Time Series"
	)


# ** Regression -----------------------------------------------------------
data_month |> 
	set_names(c("code", "station", "date", "pm10")) |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series_regression(
		.date_var = date, .formula = pm10 ~ date + lag(pm10, 1) + lag(pm10, 12),
		.interactive = FALSE, .show_summary = TRUE,
		.title = "PM10 Monthly Time Series"
	)


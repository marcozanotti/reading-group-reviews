
# Install & Load ----------------------------------------------------------

install.packages("EEAaq")
install.packages("tidyverse")
install.packages("skimr")
install.packages("patchwork")
install.packages("timetk")
install.packages("caret")
install.packages("CAST")
install.packages("gstat")
install.packages("sf")
install.packages("sp")
install.packages("spacetime")

library(EEAaq)
library(tidyverse)
library(skimr)
library(patchwork)
library(timetk)
library(caret)
library(CAST)
library(gstat)
library(sf)
library(sp)
library(spacetime)



# Data --------------------------------------------------------------------

str(pollutants)
View(pollutants)
polluts <- c("PM10")

metadata <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)

str(metadata)
metadata |> count(ISO) |> arrange(desc(n)) |> head(20)

iso <- "DE"
metadata |> dplyr::filter(ISO %in% iso) |> count(NUTS1)
metadata |> dplyr::filter(ISO %in% iso) |> count(NUTS2) |> arrange(desc(n)) |> head(20)
zones <- c("Nordrhein-Westfalen")
metadata |> dplyr::filter(NUTS1 %in% zones) |> count(NUTS3) |> pull(n) |> sum()

EEAaq_map_stations(
	zone_name = zones, NUTS_level = "NUTS1", bounds_level = "NUTS3", 
	pollutant = polluts, color = TRUE, dynamic = FALSE
)

data <- EEAaq_get_data(
	zone_name = zones, NUTS_level = "NUTS2", pollutant = polluts,
	from = 2017, to = 2019, verbose = TRUE
)

save(data, file = "airpol_nordwestfalen_pm10.RData")



# Data Aggregation --------------------------------------------------------

load("data/airpol_nordwestfalen_pm10.RData")
iso <- "DE"
zones <- c("Nordrhein-Westfalen")

EEAaq_map_stations(
	data = data, NUTS_level = "NUTS2", bounds_level = "NUTS3", 
	color = TRUE, dynamic = FALSE
)

summ <- EEAaq_summary(data)
summ$Summary
summ$Summary_byStat

str(stations)
names(stations)

stations_selected <- stations |> 
	dplyr::filter(NUTS1 %in% zones) |> 
	select(AirQualityStationEoICode, AirQualityStationName, Latitude, Longitude, Altitude) |> 
	distinct(.keep_all = TRUE)
table(stations_selected$AirQualityStationEoICode)

data_month <- EEAaq_time_aggregate(data, frequency = "monthly", aggr_fun = "mean") |> 
	left_join(stations_selected, by = c("AirQualityStationEoICode", "AirQualityStationName")) |> 
	set_names(c("code", "station", "date", "pm10", "lat", "long", "alt"))
code_to_rm <- data_month |> 
	count(code) |> 
	dplyr::filter(n < 12 * 3) |> 
	pull(code) |> 
	c(c("DENW381", "DENW382"))
data_month <- data_month |> dplyr::filter(!code %in% code_to_rm)
skim(data_month)
View(data_month)

data_year <- EEAaq_time_aggregate(data, frequency = "yearly", aggr_fun = "mean") |> 
	left_join(stations_selected, by = c("AirQualityStationEoICode", "AirQualityStationName")) |> 
	set_names(c("code", "station", "date", "pm10", "lat", "long", "alt"))
code_to_rm <- data_year |> 
	count(code) |> 
	dplyr::filter(n < 1 * 3) |> 
	pull(code) |> 
	c(c("DENW381", "DENW382"))
data_year <- data_year |> dplyr::filter(!code %in% code_to_rm)
skim(data_year)



# EDA ---------------------------------------------------------------------

# * Time Series Plots -----------------------------------------------------

# ** Decomposition --------------------------------------------------------
data_month |> 
	group_by(date) |>
	summarise(pm10 = mean(pm10)) |>
	plot_stl_diagnostics(
		.date_var = date, .value = pm10, 
		.interactive = FALSE,
		.title = "PM10 Decomposition - by month"
	)

# ** Trend ----------------------------------------------------------------
data_month |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series(
		.date_var = date, .value = pm10, 
		.smooth = TRUE, .interactive = FALSE,
		.title = "PM10 Time Series - by month"
	)

data_month |> 
	plot_time_series(
		.date_var = date, .value = pm10, .color_var = station, 
		.smooth = FALSE, .interactive = FALSE, .legend_show = FALSE,
		.title = "PM10 Time Series - by station and month"
	)

data_month |> 
	mutate(alt_class = case_when(
		alt < 300 ~ "plain",
		alt >= 300 & alt < 600 ~ "hill",
		alt >= 600 ~ "mountain", 
		TRUE ~ NA_character_
	)) |> 
	group_by(date, alt_class) |> 
	summarise(pm10 = mean(pm10)) |>
	ggplot(aes(x = date, y = pm10, col = alt_class)) + 
	geom_line() + 
	labs(title = "PM10 Time Series - by station and month", x = NULL, y = NULL) +
	timetk:::theme_tq()

data_month |> 
	mutate(alt_class = case_when(
		alt < 300 ~ "plain",
		alt >= 300 & alt < 600 ~ "hill",
		alt >= 600 ~ "mountain", 
		TRUE ~ NA_character_
	)) |> 
	ggplot(aes(x = date, y = pm10, group = station, col = alt_class)) + 
	geom_line() + 
	labs(title = "PM10 Time Series - by station and month", x = NULL, y = NULL) +
	timetk:::theme_tq()

data_year |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series(
		.date_var = date, .value = pm10, 
		.smooth = FALSE, .interactive = FALSE,
		.title = "PM10 Time Series - by year"
	)

data_year |> 
	plot_time_series(
		.date_var = date, .value = pm10, .color_var = station, 
		.smooth = FALSE, .interactive = FALSE, .legend_show = FALSE,
		.title = "PM10 Time Series - by station and year"
	)

data_year |> 
	mutate(alt_class = case_when(
		alt < 300 ~ "plain",
		alt >= 300 & alt < 600 ~ "hill",
		alt >= 600 ~ "mountain", 
		TRUE ~ NA_character_
	)) |> 
	group_by(date, alt_class) |> 
	summarise(pm10 = mean(pm10)) |>
	ggplot(aes(x = date, y = pm10, col = alt_class)) + 
	geom_line() + 
	labs(title = "PM10 Time Series - by station and year", x = NULL, y = NULL) +
	timetk:::theme_tq()

data_year |> 
	mutate(alt_class = case_when(
		alt < 300 ~ "plain",
		alt >= 300 & alt < 600 ~ "hill",
		alt >= 600 ~ "mountain", 
		TRUE ~ NA_character_
	)) |> 
	ggplot(aes(x = date, y = pm10, group = station, col = alt_class)) + 
	geom_line() + 
	labs(title = "PM10 Time Series - by station and year", x = NULL, y = NULL) +
	timetk:::theme_tq()

# ** Seasonality ----------------------------------------------------------
data_month |> 
	plot_time_series_boxplot(
		.date_var = date, .value = pm10, .period = "1 month",
		.smooth = TRUE, .interactive = FALSE,
		.title = "PM10 Seasonality - by month"
	)

data_month |> 
	plot_seasonal_diagnostics(
		.date_var = date, .value = pm10,
		.interactive = FALSE,
		.title = "PM10 Seasonality"
	)

# ** Autocorrelation ------------------------------------------------------
data_month |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_acf_diagnostics(
		.date_var = date, .value = pm10,
		.interactive = FALSE,
		.title = "PM10 Autocorrelation"
	)

# ** Anomalies ------------------------------------------------------------
data_month |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_anomaly_diagnostics(
		.date_var = date, .value = pm10, 
		.interactive = FALSE, .alpha = 0.025, .max_anomalies = 0.1,
		.title = "PM10 Anomalies"
	)


# ** Stationarity ---------------------------------------------------------
data_month |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |> 
	pull(pm10) |> 
	tseries::adf.test()

# ** Regression -----------------------------------------------------------
data_month |> 
	group_by(date) |> 
	summarise(pm10 = mean(pm10)) |>
	plot_time_series_regression(
		.date_var = date, .formula = pm10 ~ date + lag(pm10, 1),
		.interactive = FALSE, .show_summary = TRUE,
		.title = "PM10 Regression"
	)


# * Space Plots -----------------------------------------------------------
data_month |> 
	st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
	ggplot() + 
	geom_sf(mapping = aes(col = pm10)) + 
	scale_colour_gradient(low = "green", high = "red") +
	facet_wrap(~ date) + 
	labs(title = NULL, x = "", y = "")

data_month |> 
	st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
	group_by(station) |> 
	summarize(pm10 = mean(pm10)) |> 
	ggplot() + 
	geom_sf(mapping = aes(col = pm10)) + 
	scale_colour_gradient(low = "green", high = "red") +
	labs(title = "PM10 - by station", x = "", y = "")

data_month |> 
	st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
	mutate(month = factor(lubridate::month(date), ordered = TRUE)) |> 
	group_by(station, month) |> 
	summarize(pm10 = mean(pm10)) |> 
	ggplot() + 
	geom_sf(mapping = aes(col = pm10)) + 
	scale_colour_gradient(low = "green", high = "red") +
	facet_wrap(~ month) + 
	labs(title = "PM10 - by station", x = "", y = "")


# * Distribution ----------------------------------------------------------
data_month |> 
	ggplot(aes(x = pm10)) + 
	geom_histogram(binwidth = 2, fill = "lightblue") + 
	labs(title = "PM10 Distribution", x = "PM10", y = "Frequency") + 
	timetk:::theme_tq()



# Modelling ---------------------------------------------------------------

# * Data setup ------------------------------------------------------------
data_panel <- data_month |> 
	dplyr::filter(code != "IT0776A") |> # rimosso perchè le geometry sono uguali a un altro quindi fanculo
	st_as_sf(coords = c("long", "lat"), crs = 3003) |>
	select(date, code, pm10) |>
	arrange(date, code)
dim(data_panel)

data_sp <- as_Spatial(data_panel) # sp object
dim(data_sp)

space <- SpatialPoints(unique(data_sp@coords), CRS("+init=epsg:3003"))
time <- as.Date(unique(data_sp$date))
pm10_df <- data.frame("pm10" = data_sp$pm10)
length(space); length(time); nrow(pm10_df); length(space) * length(time)
data_st <- STFDF(sp = space, time = time, data = pm10_df) # st object
dim(data_st)
str(data_st)
stplot(data_st)


# * Spatio-Temporal Variogram ---------------------------------------------

# empirical variogram
variog <- variogramST(
	pm10 ~ 1, tlags = 0:12, data = data_st,
	tunit = "months", assumeRegular = TRUE
)
plot(variog, map = FALSE)
plot(variog, map = TRUE)
plot(variog, wireframe = TRUE)

# separable variogram
variog_sep <- vgmST(
	stModel = "separable",
	space = vgm(psill = 20, "Exp", range = 100, nugget = 10),
	time = vgm(psill = 12, "Exp", range = 100, nugget = 10),
	sill = 20
)
plot(variog, variog_sep, map = FALSE)
plot(variog, variog_sep, map = TRUE)
vgm_sep <- fit.StVariogram(variog, variog_sep)

# product-sum variogram
variog_prodsum <- vgmST(
	stModel = "productSum",
	space = vgm(psill = 20, "Exp", range = 100, nugget = 10),
	time = vgm(psill = 12, "Exp", range = 100, nugget = 10),
	k = 2
)
plot(variog, variog_prodsum, map = FALSE)
plot(variog, variog_prodsum,map = TRUE)
vgm_prodsum <- fit.StVariogram(variog, variog_prodsum)

sqrt(attr(vgm_sep, "MSE"));	sqrt(attr(vgm_prodsum, "MSE"))
extractPar(vgm_sep); extractPar(vgm_prodsum)



# * Spatio-Temporal Validation --------------------------------------------

# ** Train - Test Split ---------------------------------------------------
tr <- data_panel |> dplyr::filter(date < "2019-10-01") # train
te <- data_panel |> dplyr::filter(date >= "2019-10-01") # test

# ** Validation Split -----------------------------------------------------
set.seed(1992)
k_space <- trunc(tr$code |> unique() |> length() * (1/3))
k_time <- trunc(tr$date |> unique() |> length() * (1/3))
k_spacetime <- min(k_space, k_time)
idx_llo <- CreateSpacetimeFolds(tr, spacevar = "code", k = k_space) # Leave-Location-Out CV (LLOCV)
idx_lto <- CreateSpacetimeFolds(tr, timevar = "date", k = k_time) # Leave-Time-Out CV (LLOCV)
idx_llto <- CreateSpacetimeFolds(tr, spacevar = "code", timevar = "date", k = k_spacetime) # Leave-Location-Time-Out CV (LLTOCV)
# tr[idx_llo$indexOut[[1]], ] |> View()
# tr[idx_lto$indexOut[[2]], ] |> View()
# tr[idx_llto$indexOut[[1]], ] |> View()


# ** Model Validation -----------------------------------------------------
variog_validation <- function(variog_model, train_set, valid_index) {
	
	RMSE <- numeric(length = length(valid_index$index))

	for (f in 1:length(valid_index$index)) {
		
		t1 <- Sys.time()
		if (f == 1) {
			cat(paste0("First iteration started at ", t1, "\n"))
		}
		
		Train_panel_fold <- train_set[valid_index$index[[f]], ]
		Train_panel_sp_fold <- as_Spatial(Train_panel_fold)
		space_fold <- SpatialPoints(unique(Train_panel_sp_fold@coords), CRS("+init=epsg:3003"))
		time_fold <- as.Date(unique(Train_panel_sp_fold$date))
		pm10_fold <- data.frame("pm10" = Train_panel_sp_fold$pm10)
		PM10_st_fold <- STFDF(sp = space_fold, time = time_fold, data = pm10_fold)
		
		Test_panel_fold <- train_set[valid_index$indexOut[[f]], ]
		Test_panel_sp_fold <- as_Spatial(Test_panel_fold)
		Test_space_fold <- SpatialPoints(unique(Test_panel_sp_fold@coords), CRS("+init=epsg:3003"))
		Test_time_fold <- as.Date(unique(Test_panel_sp_fold$date))
		Test_pm10_fold <- data.frame("pm10" = Test_panel_sp_fold$pm10)
		Test_PM10_st_fold <- STFDF(sp = Test_space_fold, time = Test_time_fold, data = Test_pm10_fold)
		
		krg.or <- krigeST(
			pm10 ~ 1, data = PM10_st_fold, 
			newdata = Test_PM10_st_fold, 
			modelList = variog_model
		)
		
		RMSE[f] <- sqrt(mean(unlist((krg.or@data - Test_PM10_st_fold@data$pm10) ^ 2)))
		
		t2 <- Sys.time()
		cat(
			paste0(
				"Iteration ", f, " of ", length(valid_index$index), 
				" completed in ", round(t2 - t1, 2), " seconds.\n"
			)
		)
		if (f == length(valid_index$index)) {
			cat(paste0("Last iteration ended at ", t2, "\n"))
		}
		
	}
	
	return(RMSE)
	
}

# separable model
variog_sep <- vgmST(
	stModel = "separable",
	space = vgm(psill = 20, "Exp", range = 100, nugget = 10),
	time = vgm(psill = 12, "Exp", range = 100, nugget = 10),
	sill = 20
)

res_llo_sep <- variog_validation(variog_sep, tr, idx_llo)
res_lto_sep <- variog_validation(variog_sep, tr, idx_lto)
res_llto_sep <- variog_validation(variog_sep, tr, idx_lto)

RMSE_models_sep <- rbind(
	data.frame(CV = "LLO", RMSE = res_llo_sep, Model = "separable"),
	data.frame(CV = "LTO", RMSE = res_lto_sep, Model = "separable"),
	data.frame(CV = "LLTO", RMSE = res_llto_sep, Model = "separable")
)

# product-sum model
variog_prodsum <- vgmST(
	stModel = "productSum",
	space = vgm(psill = 20, "Exp", range = 100, nugget = 10),
	time = vgm(psill = 12, "Exp", range = 100, nugget = 10),
	k = 2
)

res_llo_prod <- variog_validation(variog_prodsum, tr, idx_llo)
res_lto_prod <- variog_validation(variog_prodsum, tr, idx_lto)
res_llto_prod <- variog_validation(variog_prodsum, tr, idx_lto)

RMSE_models_prod <- rbind(
	data.frame(CV = "LLO", RMSE = res_llo_prod, Model = "product-sum"),
	data.frame(CV = "LTO", RMSE = res_lto_prod, Model = "product-sum"),
	data.frame(CV = "LLTO", RMSE = res_llto_prod, Model = "product-sum")
)


RMSE_models <- bind_rows(RMSE_models_sep, RMSE_models_prod)

RMSE_models |>
	ggplot(aes(y = RMSE, fill = Model)) + 
	geom_boxplot() + 
	labs(title = "RMSE by Model") + 
	timetk:::theme_tq()

RMSE_models |>
	ggplot(aes(y = RMSE, fill = CV)) + 
	geom_boxplot() + 
	labs(title = "RMSE by CV strategy") + 
	timetk:::theme_tq()

RMSE_models |>
	ggplot(aes(y = RMSE, fill = CV)) + 
	geom_boxplot() + 
	facet_wrap(~ Model) + 
	labs(title = "RMSE by CV strategy") + 
	timetk:::theme_tq()

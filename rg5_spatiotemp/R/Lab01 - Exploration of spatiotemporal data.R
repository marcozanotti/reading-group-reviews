#########################################################################################
############### Ph.D. program in Economics, Statistics and Data Science   ###############
###############                         Reading group on                  ###############
###############     "Spatio-temporal data analysis and cross-validation"  ###############
###############             Lab01 - Exploring spatio-temporal data        ###############
#########################################################################################


#############################
########### Setup ###########
#############################

## Working directory
setwd("H:/.shortcut-targets-by-id/1BNb3hsFKyndDWlPDs4JTjbrSnI4UljMj/RG-Space_time-Condivisione/Codici_Paolo")

## Packages
library(gstat)
library(ggplot2)
library(tidyverse)
library(sf)
library(plotly)
library(ggpubr)
library(tsbox)
library(forecast)
library(lubridate)
library(sp)
library(spacetime)





##############################################
########### Load AgrImOnIA dataset ###########
##############################################
load("../RG_ST_pointdataset.RData")
Data <- Agrimonia_month_full

#### Observe the structure of the dataset
str(Data)
# 101 stations
table(Data$Time)
# 36 months (3 years)
table(Data$IDStations)
# No missing data
table(Data$Time,Data$IDStations) %>% View()





###################################################################
########### Time series analysis of PM10 concentrations ###########
###################################################################

#### Filter observations: only stations 1264 and 1265
Data_red <- Data %>%
  filter(IDStations %in% c(1264,1265)) %>%
  arrange(IDStations)

#### Transform reduced dataset to time-series object
Data_red_ts <- Data_red %>%
  select(Time,IDStations,AQ_PM10) %>%
  tsbox::ts_ts() %>%
  tsbox::ts_frequency(to = "month")

#### Time series plot (only selected sites)
autoplot(Data_red_ts, facets=TRUE) +
  labs(title = "PM10 at sites 1264 and 1265",
       x = "Year", y = latex2exp::TeX("$\\mu g/m^3$"))

autoplot(Data_red_ts, facets=FALSE) +
  labs(title = "PM10 at sites 1264 and 1265",
       x = "Year", y = latex2exp::TeX("$\\mu g/m^3$"))

#### Time series plot (full dataset)
Data %>%
  select(Time,IDStations,AQ_PM10) %>%
  tsbox::ts_ts() %>%
  tsbox::ts_frequency(to = "month") %>%
  autoplot(facets = FALSE) +
  labs(title = "PM10 across Lombardy", x = "Year", y = latex2exp::TeX("$\\mu g/m^3$")) + 
  theme(legend.position = "")

#### Seasonal box-plot (full dataset)
Data %>%
  select(Time,IDStations,AQ_PM10) %>%
  mutate(Month = factor(lubridate::month(Time),ordered = T)) %>%
  ggplot(mapping = aes(y = AQ_PM10, x = Month, group = Month)) + 
  geom_boxplot() + 
  labs(title = "Intra-yearly variation of PM10 across Lombardy",
       x = "Month", y = latex2exp::TeX("$\\mu g/m^3$"))
  
#### Persistence analysis: ACF and PACF
ggAcf(Data_red_ts) + 
  labs(title = "Auto-and-Cross correlations of PM10 at sites 1264 and 1265")
ggPacf(Data_red_ts) + 
  labs(title = "Partial Auto-and-Cross correlations of PM10 at sites 1264 and 1265")

#### Persistence analysis: lag-plot
gglagplot(Data_red_ts[,1],set.lags = c(1,2,3,6,12,18)) + 
  labs(title = "Lag-plot", subtitle = "PM10 at site 1264")
gglagplot(Data_red_ts[,1],set.lags = c(1,2,3,6,12,18)) + 
  labs(title = "Lag-plot", subtitle = "PM10 at site 1265")

#### Distribution analysis: histogram and boxplots
# Define colors
cc <- c("Dens"="#FF0000","Norm"="blue")
# Histogram
Data_red %>%
  ggplot(data = ., aes(x = AQ_PM10)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(Data_red$AQ_PM10,na.rm=T),
                            sd = sd(Data_red$AQ_PM10,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  facet_wrap(~ IDStations) + 
  labs(title = "Empirical distribution of simulated values",
       x = "Values",
       y = "Density") + 
  scale_color_manual("Curve",
                     values = cc,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussian"))
# Boxplot by station (reduced)
Data_red %>%
  ggplot(mapping = aes(y = AQ_PM10, x = IDStations)) + 
  geom_boxplot() + 
  labs(title = "PM10 at sites 1264 and 1265",
       x = "Station", y = latex2exp::TeX("$\\mu g/m^3$"))



###################################################################
########### Exploration according to several dimensions ###########
###################################################################

#### Boxplot by station (full sample)
Data %>%
  ggplot(mapping = aes(y = AQ_PM10, x = IDStations)) + 
  geom_boxplot() + 
  labs(title = "PM10 across Lombardy",
       x = "Station", y = latex2exp::TeX("$\\mu g/m^3$"))

#### Box-plot by station and altitude
Data %>%
  mutate(Alt_class = case_when(Altitude < 300 ~ "Plain",
                               Altitude >= 300 & Altitude < 600 ~ "Hill",
                               Altitude >= 600 ~ "Mountain",
                               TRUE ~ NA_character_)) %>%
  ggplot(mapping = aes(y = AQ_PM10, x = IDStations, fill = Alt_class)) + 
  geom_boxplot() + 
  labs(title = "PM10 across Lombardy",
       x = "Station", y = latex2exp::TeX("$\\mu g/m^3$"))

#### Average time series by altitude
Data %>%
  mutate(Alt_class = case_when(Altitude < 300 ~ "Plain",
                               Altitude >= 300 & Altitude < 600 ~ "Hill",
                               Altitude >= 600 ~ "Mountain",
                               TRUE ~ NA_character_)) %>%
  group_by(Time,Alt_class) %>%
  summarize(PM10_m = mean(AQ_PM10, na.rm=T),
            PM10_sd = sd(AQ_PM10, na.rm=T)) %>%
  ggplot(mapping = aes(col = Alt_class,x = Time)) + 
  geom_ribbon(aes(ymin = PM10_m - PM10_sd, ymax = PM10_m + PM10_sd, fill = Alt_class, alpha = 0.5)) + 
  geom_line(mapping = aes(y = PM10_m)) + 
  labs(title = "PM10 by altitude", x = "", y = latex2exp::TeX("$\\mu g/m^3$"))

#### Seasonal box-plot by altitude
Data %>%
  mutate(Alt_class = case_when(Altitude < 300 ~ "Plain",
                               Altitude >= 300 & Altitude < 600 ~ "Hill",
                               Altitude >= 600 ~ "Mountain",
                               TRUE ~ NA_character_)) %>%
  mutate(Month = factor(lubridate::month(Time),ordered = T)) %>%
  ggplot(mapping = aes(y = AQ_PM10, x = Month, group = Month, fill = Alt_class)) + 
  geom_boxplot() + 
  facet_wrap(~ Alt_class) + 
  labs(title = "Intra-yearly variation of PM10 by altitude",
       x = "Month", y = latex2exp::TeX("$\\mu g/m^3$"))





#########################################################
########### Exploration across space and time ###########
#########################################################

#### PM10 concentrations across space and time (full sample)
Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  ggplot() + 
  geom_sf(mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  facet_wrap(~ Time) + 
  labs(title =  latex2exp::TeX("PM$_{10}$ ($\\mu g/m^3$) in the extended Lombardy region"),
       x = "", y = "")

#### Average PM10 concentrations across space and time (full sample)
Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  group_by(IDStations) %>%
  summarize(AQ_PM10 = mean(AQ_PM10,na.rm=T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  labs(title =  latex2exp::TeX("Average PM$_{10}$ ($\\mu g/m^3$) concentrations in the extended Lombardy region"),
       x = "", y = "")

#### Variability of PM10 concentrations across space and time (full sample)
Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  group_by(IDStations) %>%
  summarize(AQ_PM10 = sd(AQ_PM10,na.rm=T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  labs(title =  latex2exp::TeX("Standard deviation of PM$_{10}$ ($\\mu g/m^3$) concentrations in the extended Lombardy region"),
       x = "", y = "")

#### Seasonal average PM10 concentrations across space and time (full sample)
Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  mutate(Month = factor(lubridate::month(Time),ordered = T)) %>%
  group_by(IDStations,Month) %>%
  summarize(AQ_PM10 = mean(AQ_PM10,na.rm=T)) %>%
  ggplot() + 
  geom_sf(mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  facet_wrap(~ Month) + 
  labs(title =  latex2exp::TeX("Seasonal average PM$_{10}$ ($\\mu g/m^3$) concentrations in the extended Lombardy region"),
       x = "", y = "")

#### PM10 concentrations across space and time and altitude (full sample)
Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  ggplot() + 
  geom_sf(mapping = aes(col = AQ_PM10, size = Altitude)) + 
  scale_colour_gradient(low="green",high="red") +
  facet_wrap(~ Time) + 
  labs(title =  latex2exp::TeX("PM$_{10}$ ($\\mu g/m^3$) by altitude in the extended Lombardy region"),
       x = "", y = "")



#####################################################################
########### Exploration across space and spatial modeling ###########
#####################################################################

#### Load the Lombardy shapefile
load("../Raw data/Shape_ExtendedLombardy.Rdata")

#### Aggregate observations by station (average)
Data_avg <- Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  group_by(IDStations) %>%
  summarize(AQ_PM10 = mean(AQ_PM10,na.rm=T))

#### Load and adjust the shape file of the Lombardy Region
lomb_poly <- st_read("../Shape/Lombardia.shp",quiet=TRUE)
lomb_poly <- st_set_crs(lomb_poly, value = 3003)
Data_avg_gb <- Data_avg %>%
  st_transform(crs = 3003)

#### Plot the PM10 sensors on the map
ggplot(data = st_boundary(lomb_poly)) + 
  geom_sf() + 
  geom_sf(data = Data_avg_gb,  size = 2.5,
          mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  theme_bw()  +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") + 
  labs(title = latex2exp::TeX("Average PM$_{10}$ ($\\mu g/m^3$) concentrations in the extended Lombardy region"),
       x = "", y = "")

#### Variogram estimation
# Empirical semivariogram
vgm <- variogram(AQ_PM10 ~ 1, data = Data_avg_gb)
plot(vgm)
vgm
# Estimate an Exponential model 
vgm.ex.fit = fit.variogram(vgm, model = vgm(psill=25, "Exp", range=80000, nugget=5), 
                           fit.method = 6)
# explore this function for other types of estimation methods
vgm.ex.fit
plot(vgm, vgm.ex.fit,ylim=c(0,60),lwd=4)


#### Make a grid for prediction
gr <- st_bbox(lomb_poly) %>%
  stars::st_as_stars(dx = 5500) %>%
  st_crop(lomb_poly)
gr <- st_transform(gr,crs = 3003)  # change system to GB
gr <- st_as_sf(gr) # change to a vector format
plot(gr, main="")

#### Prediction using ordinary kriging and mapping
krg.or <- krige(AQ_PM10 ~ 1, locations = Data_avg_gb,  newdata=gr, model = vgm.ex.fit)

#### Map the estimates on the map
ggplot(data = st_boundary(lomb_poly)) + 
  geom_sf() + 
  geom_sf(data = krg.or, mapping = aes(fill = var1.pred)) + 
  geom_sf(data = Data_avg_gb,  size = 5, col = "black") + 
  geom_sf(data = Data_avg_gb,  size = 2.5, mapping = aes(col = AQ_PM10)) + 
  scale_colour_gradient(low="green",high="red") +
  scale_fill_gradient(low="green",high="red") +
  theme_bw()  +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br") + 
  labs(title = latex2exp::TeX("Estimated average PM$_{10}$ ($\\mu g/m^3$) concentrations in the extended Lombardy region"),
       subtitle = "Exponential model with partial sill=25, range=80000, and nugget=5",
       x = "", y = "")




##########################################################
########## Build the spatio-temporal data.frame ##########
##########################################################

##### Trasform into long (panel) structure
Data_panel <- Data %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
  select(Time,IDStations,AQ_PM10,WE_Temp_2m) %>%
  arrange(Time,IDStations) %>%
  st_transform(crs = 3003)

##### Transform to spatial object (sp)
Data_panel_sp <- as_Spatial(Data_panel)

##### Build a ST object compatible with gstat package
space <- SpatialPoints(unique(Data_panel_sp@coords),CRS("+init=epsg:3003"))
time <- as.Date(unique(Data_panel_sp$Time))
pm10 <- data.frame(Data_panel_sp$AQ_PM10,Data_panel_sp$WE_Temp_2m)
PM10_st <- STFDF(sp = space,time = time,data = pm10)
str(PM10_st)
stplot(PM10_st)



###############################################
########## Spatio-temporal variogram ##########
###############################################

##### Empirical spatio-temporal variogram
var <- variogramST(Data_panel_sp.AQ_PM10 ~ 1,
                   tlags = 0:12,
                   data = PM10_st,
                   tunit = "months",
                   assumeRegular = T)
plot(var,map=F)
plot(var,map=T)
plot(var,wireframe=T)

##### Separable spatio-temporal covariance function
# Define the model
separable <- vgmST(stModel = "separable",
                   space = vgm(psill=25, "Exp", range=150, nugget=10),
                   time = vgm(psill=1, "Exp", range = 100, nugget = 0.50),
                   sill=200)
# Plot
plot(var,separable,map=F)
plot(var,separable,map=T)
# Estimation
separable_Vgm <- fit.StVariogram(var, separable)
sqrt(attr(separable_Vgm, "MSE"))
extractPar(separable_Vgm)

##### Product-Sum spatio-temporal covariance function
# Define the model
prodSumModel <- vgmST("productSum",
                      space = vgm(psill = 25, "Exp", range = 150, nugget = 10),
                      time = vgm(psill = 100, "Exp", range = 100, nugget = 0.50),
                      k = 2)
# Plot
plot(var,prodSumModel,map=F)
plot(var,prodSumModel,map=T)
# Estimation
prodSumModel_Vgm <- fit.StVariogram(var, prodSumModel)
sqrt(attr(prodSumModel_Vgm, "MSE"))
extractPar(prodSumModel_Vgm)



##############################################################
########## Spatio-temporal variogram with covariate ##########
##############################################################

##### Empirical spatio-temporal variogram
var_uk <- variogramST(Data_panel_sp.AQ_PM10 ~ Data_panel_sp$WE_Temp_2m,
                      tlags = 0:12,
                      data = PM10_st,
                      tunit = "months",
                      assumeRegular = T)
plot(var_uk,map=F)
plot(var_uk,map=T)
plot(var_uk,wireframe=T)

##### Separable spatio-temporal covariance function
# Define the model
separable <- vgmST(stModel = "separable",
                   space = vgm(psill=25, "Exp", range=150, nugget=10),
                   time = vgm(psill=1, "Exp", range = 100, nugget = 0.50),
                   sill=200)
# Plot
plot(var_uk,separable,map=F)
plot(var_uk,separable,map=T)
# Estimation
separable_Vgm_uk <- fit.StVariogram(var_uk, separable)
sqrt(attr(separable_Vgm_uk, "MSE"))
extractPar(separable_Vgm_uk)

##### Product-Sum spatio-temporal covariance function
# Define the model
prodSumModel <- vgmST("productSum",
                      space = vgm(psill = 25, "Exp", range = 150, nugget = 10),
                      time = vgm(psill = 100, "Exp", range = 100, nugget = 0.50),
                      k = 2)
# Plot
plot(var_uk,prodSumModel,map=F)
plot(var_uk,prodSumModel,map=T)
# Estimation
prodSumModel_Vgm_uk <- fit.StVariogram(var_uk, prodSumModel)
sqrt(attr(prodSumModel_Vgm_uk, "MSE"))
extractPar(prodSumModel_Vgm_uk)



#######################################
########## Models comparison ##########
#######################################

out <- cbind(
  rbind(sqrt(attr(separable_Vgm, "MSE")),
        sqrt(attr(prodSumModel_Vgm, "MSE")),
        sqrt(attr(separable_Vgm_uk, "MSE")),
        sqrt(attr(prodSumModel_Vgm_uk, "MSE"))),
  rbind(extractPar(separable_Vgm),
        extractPar(prodSumModel_Vgm),
        extractPar(separable_Vgm_uk),
        extractPar(prodSumModel_Vgm_uk))
)
colnames(out)[1] <- "RMSE"
rownames(out) <- c("separable","prodSum","separable_uk","prodSum_uk")
out <- data.frame(out)
out



######################################################
########## Spatio-temporal cross-validaiton ##########
######################################################
library(caret)
library(CAST)

##### Validation set strategy
# Train set from 01/01/2016 to 01/09/2018
tr <- Data_panel %>%
  filter(Time < "2018-10-01")
# Test set from 01/10/2018 to 01/12/2018
te <- Data_panel %>%
  filter(Time >= "2018-10-01")

##### Data partitioning
# Random 10-folds CV
idx_cv <- caret::createFolds(y = tr$AQ_PM10, k = 10)
# Leave-Location-Out CV (LLOCV)
idx_llo <- CreateSpacetimeFolds(tr, spacevar = "IDStations", k = 101)
tr[idx_llo$indexOut[[1]],] %>% View()
tr[idx_llo$indexOut[[100]],] %>% View()
# Leave-Time-Out CV (LLOCV)
idx_lto <- CreateSpacetimeFolds(tr, timevar = "Time", k = 33)
tr[idx_lto$indexOut[[1]],] %>% View()
tr[idx_lto$indexOut[[25]],] %>% View()
# Leave-Location-Time-Out CV (LLTOCV)
idx_llto <- CreateSpacetimeFolds(tr, spacevar = "IDStations",timevar = "Time", k = 10)
tr[idx_llto$indexOut[[1]],] %>% View()
tr[idx_llto$indexOut[[25]],] %>% View()

##### Out-of-sample validation of model performance
# Define the model (spatio-temporal variogram)
prodSumModel <- vgmST("productSum",
                      space = vgm(psill = 25, "Exp", range = 150, nugget = 10),
                      time = vgm(psill = 100, "Exp", range = 100, nugget = 0.50),
                      k = 2)

# Define the output of interest
LLTO_RMSE <- numeric(length = length(idx_llto$index))
LLO_RMSE <- numeric(length = length(idx_llto$index))
LTO_RMSE <- numeric(length = length(idx_llto$index))
Kfold_RMSE <- numeric(length = length(idx_llto$index))

# LLTO for loop
for (f in 1:length(idx_llto$index)) {
  
  t1 <- Sys.time()
  if (f == 1) {
    cat(paste0("LLTO: First iteration ended at ", t1, "\n"))
  }
  
  # Build training spatio-temporal dataset
  Train_panel_fold <- tr[idx_llto$index[[f]],]
  Train_panel_sp_fold <- as_Spatial(Train_panel_fold)
  space_fold <- SpatialPoints(unique(Train_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  time_fold <- as.Date(unique(Train_panel_sp_fold$Time))
  pm10_fold <- data.frame(Train_panel_sp_fold$AQ_PM10,Train_panel_sp_fold$WE_Temp_2m)
  PM10_st_fold <- STFDF(sp = space_fold,time = time_fold,data = pm10_fold)
  
  # Build validation spatio-temporal dataset
  Test_panel_fold <- tr[idx_llto$indexOut[[f]],]
  Test_panel_sp_fold <- as_Spatial(Test_panel_fold)
  Test_space_fold <- SpatialPoints(unique(Test_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  Test_time_fold <- as.Date(unique(Test_panel_sp_fold$Time))
  Test_pm10_fold <- data.frame(Test_panel_sp_fold$AQ_PM10,Test_panel_sp_fold$WE_Temp_2m)
  Test_PM10_st_fold <- STFDF(sp = Test_space_fold,time = Test_time_fold, data = Test_pm10_fold)
  
  # Model estimation and spatio-temporal prediction
  krg.or <- krigeST(Train_panel_sp_fold.AQ_PM10 ~ 1, data = PM10_st_fold,
                    newdata=Test_PM10_st_fold, modelList = prodSumModel)
  
  # Store RMSE
  LLTO_RMSE[f] <- sqrt(mean(unlist((krg.or@data - Test_PM10_st_fold@data$Test_panel_sp_fold.AQ_PM10)^2)))
  
  t2 <- Sys.time()

  cat(paste0("Iteration ",f, " of ",length(idx_llto$index), " completed in ",round(t2-t1,2)," seconds.\n"))
  if (f == length(idx_llto$index)) {
    cat(paste0("LLTO: Last iteration ended at ", t2, "\n"))
  }
}


# LLO for loop
for (f in 1:length(idx_llo$index)) {
  
  t1 <- Sys.time()
  if (f == 1) {
    cat(paste0("LLO: First iteration ended at ", t1, "\n"))
  }
  
  # Build training spatio-temporal dataset
  Train_panel_fold <- tr[idx_llo$index[[f]],]
  Train_panel_sp_fold <- as_Spatial(Train_panel_fold)
  space_fold <- SpatialPoints(unique(Train_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  time_fold <- as.Date(unique(Train_panel_sp_fold$Time))
  pm10_fold <- data.frame(Train_panel_sp_fold$AQ_PM10,Train_panel_sp_fold$WE_Temp_2m)
  PM10_st_fold <- STFDF(sp = space_fold,time = time_fold,data = pm10_fold)
  
  # Build validation spatio-temporal dataset
  Test_panel_fold <- tr[idx_llo$indexOut[[f]],]
  Test_panel_sp_fold <- as_Spatial(Test_panel_fold)
  Test_space_fold <- SpatialPoints(unique(Test_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  Test_time_fold <- as.Date(unique(Test_panel_sp_fold$Time))
  Test_pm10_fold <- data.frame(Test_panel_sp_fold$AQ_PM10,Test_panel_sp_fold$WE_Temp_2m)
  Test_PM10_st_fold <- STFDF(sp = Test_space_fold,time = Test_time_fold, data = Test_pm10_fold)
  
  # Model estimation and spatio-temporal prediction
  krg.or <- krigeST(Train_panel_sp_fold.AQ_PM10 ~ 1, data = PM10_st_fold,
                    newdata=Test_PM10_st_fold, modelList = prodSumModel)
  
  # Store RMSE
  LLO_RMSE[f] <- sqrt(mean(unlist((krg.or@data - Test_PM10_st_fold@data$Test_panel_sp_fold.AQ_PM10)^2)))
  
  t2 <- Sys.time()
  
  cat(paste0("Iteration ",f, " of ",length(idx_llo$index), " completed in ",round(t2-t1,2)," seconds.\n"))
  if (f == length(idx_llo$index)) {
    cat(paste0("LLO: Last iteration ended at ", t2, "\n"))
  }
}


# LTO for loop
for (f in 1:length(idx_lto$index)) {
  
  t1 <- Sys.time()
  if (f == 1) {
    cat(paste0("LTO: First iteration ended at ", t2, "\n"))
  }
  
  # Build training spatio-temporal dataset
  Train_panel_fold <- tr[idx_lto$index[[f]],]
  Train_panel_sp_fold <- as_Spatial(Train_panel_fold)
  space_fold <- SpatialPoints(unique(Train_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  time_fold <- as.Date(unique(Train_panel_sp_fold$Time))
  pm10_fold <- data.frame(Train_panel_sp_fold$AQ_PM10,Train_panel_sp_fold$WE_Temp_2m)
  PM10_st_fold <- STIDF(sp = space_fold,time = time_fold,data = pm10_fold)
  
  # Build validation spatio-temporal dataset
  Test_panel_fold <- tr[idx_lto$indexOut[[f]],]
  Test_panel_sp_fold <- as_Spatial(Test_panel_fold)
  Test_space_fold <- SpatialPoints(unique(Test_panel_sp_fold@coords),CRS("+init=epsg:3003"))
  Test_time_fold <- as.Date(unique(Test_panel_sp_fold$Time))
  Test_pm10_fold <- data.frame(Test_panel_sp_fold$AQ_PM10,Test_panel_sp_fold$WE_Temp_2m)
  Test_PM10_st_fold <- STIDF(sp = Test_space_fold,time = Test_time_fold, data = Test_pm10_fold)
  
  # Model estimation and spatio-temporal prediction
  krg.or <- krigeST(Train_panel_sp_fold.AQ_PM10 ~ 1, data = PM10_st_fold,
                    newdata=Test_PM10_st_fold, modelList = prodSumModel)
  
  # Store RMSE
  LTO_RMSE[f] <- sqrt(mean(unlist((krg.or@data - Test_PM10_st_fold@data$Test_panel_sp_fold.AQ_PM10)^2)))
  
  t2 <- Sys.time()
  
  cat(paste0("Iteration ",f, " of ",length(idx_lto$index), " completed in ",round(t2-t1,2)," seconds.\n"))
  if (f == length(idx_lto$index)) {
    cat(paste0("LTO: Last iteration ended at ", t2, "\n"))
  }
}


RMSE_models <- rbind(data.frame(RMSE = LTO_RMSE,Model = "LTO"),
                     data.frame(RMSE = LLO_RMSE,Model = "LLO"),
                     data.frame(RMSE = LLTO_RMSE,Model = "LLTO"))

RMSE_models %>%
  ggplot() + 
  geom_boxplot(mapping = aes(y = RMSE, fill = Model)) + 
  labs(title = "RMSE by CV strategy")




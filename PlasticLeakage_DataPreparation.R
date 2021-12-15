#################### Data Preparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
## R version: 4.1.1, Operating system: Windows 10
##


#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("cartography","dplyr","gdalUtils","ggplot2","raster","reproducible","rgeos","rgdal","rnaturalearth","sf","sp","SpaDES","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'C://Users//carob//Documents//WWF_PlastikLeakage_Vietnam//data'



### II. DATA IMPORT ####

## download country boundary of Vietnam from GADM via inbuilt function
#vietnam <- getData('GADM', country='VNM', level=0)
# downloaded from https://data.humdata.org/dataset/viet-nam-administrative-boundaries-polygon-polyline
vietnam <- readOGR(paste(dir, "/vietnam/vietnam.shp", sep = ""))


#### Plastic Leakage Factors

#### 1. Weather Data (Meteostat)
## downloaded via python API

## data downloaded for time period: 01.01.2016-31.12.2020

## b) Preciptation (mm)
prcp_meteostat <- read_csv(paste(dir, "/Meteostat_PrecipitationDaily.csv", sep = ""))


#### 2. Wind Speed (hourly) / Peak Wind Gust
wind_meteostat <- read_csv(paste(dir, "/Meteostat_WindHourly.csv", sep = ""))

meteostat <- read_csv(paste(dir, "/Meteostat_Daily.csv", sep = ""))

meteostat_daily <- meteostat %>% 
                    group_by(station, time) %>% 
                     dplyr::summarise(prcp_mean = mean(prcp, na.rm = T), wspd_mean = mean(wspd, na.rm = T))

meteostat_daily$year <- strftime(meteostat_daily$time, "%Y")
meteostat_daily$month <- strftime(meteostat_daily$time, "%m")
meteostat_daily$day <- strftime(meteostat_daily$time, "%d")


## divide data into years
meteostat_daily_16 <- meteostat_daily[meteostat_daily$year == 2016,]
meteostat_daily_17 <- meteostat_daily[meteostat_daily$year == 2017,]
meteostat_daily_18 <- meteostat_daily[meteostat_daily$year == 2018,]
meteostat_daily_19 <- meteostat_daily[meteostat_daily$year == 2019,]
meteostat_daily_20 <- meteostat_daily[meteostat_daily$year == 2020,]

# check data availability per year
sum(is.na(meteostat_daily_16))
sum(is.na(meteostat_daily_17))
sum(is.na(meteostat_daily_18))
sum(is.na(meteostat_daily_19)) # least NAs - take 2019 as reference year
sum(is.na(meteostat_daily_20))


## plot time series one year to check precipitation distribution
qplot(x=time, y=prcp_mean,
      data=meteostat_daily_19, na.rm=T)

## aggregate over years to reduce NAs
meteostat_yearly_mean <- meteostat_daily %>%
                          group_by(station, month, day) %>% 
                          dplyr::summarise(prcp_yearly_mean = mean(prcp_mean, na.rm = T), 
                                           wspd_yearly_mean = mean(wspd_mean, na.rm = T)) 

sum(is.na(meteostat_yearly_mean))




# ## take average of one year (from 5 years)
# # aggregate per year
# meteostat_yearly <- meteostat_daily %>% 
#   group_by(station, time) %>% 
#   dplyr::summarise(prcp_mean = mean(prcp, na.rm = T), wspd_mean = mean(wspd, na.rm = T))




#### 3. Water Areas (JRC Global Surface Water)
## downloaded via Google Earth Engine & processed in QGIS (faster & less memory)
jrc_water <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_30.tif", sep = ""))



#### 4. Natural Hazards (Flooding & Storm)

#### a) Flooding
## derived from JRC Global Surface Water Dataset


#### b) Storm
# may take some time (big dataset)
#storm <- readOGR(paste(dir, "/unisys_tracks_1956_2018dec31/UNISYS_tracks_1956_2018Dec31.shp", sep = ""), use_iconv = TRUE, encoding = "UTF-8")



#### 5. Topography - DEM (Digital Elevation Model)
## downloaded from: https://data.opendevelopmentmekong.net/en/dataset/digital-elevation-model-dem

# import DEM of Vietnam (30m) as RasterLayer
dem <- raster(paste(dir, "/dem/dem_compress_clipped.tif", sep = ""))

# remove NA values
# dem_subset <- dem
# dem_subset[dem_subset < 0] <- NA
# plot(dem_subset)

# calculate slope in degrees
slope <- terrain(dem, opt = 'slope', unit = 'degrees')

# add slope as band
#dem_stack <- stack(dem_subset, slope)

# rename band
#names(dem_stack)[[1]] <- "elevation"

plot(slope, main = "Slope (DEM)")
plot(vietnam, add=T)

# export slope as raster file
writeRaster(slope, paste(dir, "/dem/dem_slope.tif", sep = ""), overwrite = T)



#### 6. Waste Generation
#waste <- readOGR(paste(dir, "/gadm36_VNM_1_wasteperprovince_20032019_UTM48N.shp", sep = ""), use_iconv = TRUE, encoding = "UTF-8")



#### III. DATA CLEANING ####

#### 1. Precipitation (daily)

## rename columns
prcp_meteostat <- dplyr::rename(prcp_meteostat, date = time)


#### 2. Wind (hourly)

## rename columns
wind_meteostat <- dplyr::rename(wind_meteostat, datetime = time)

## no NAs or exact duplicates  (already checked in python download script)

## number of duplicates in station, datetime & wind speed
sum(duplicated(wind_meteostat[,1:3])) #0



#### 4. Natural Hazards (Flooding & Storm)

#### a) Flooding


#### b) Storm
## only keep needed rows
# storm <- storm[,names(storm) %in% c("ADV_DATE","ADV_HOUR","SPEED")]
# 
# ## convert data to simple feature object (sf) (for easier operation & later plotting with ggplot)
# storm_sf <- st_as_sf(storm)
# 
# ## first change CRS to WGS84
# crs(storm_sf)
# crs(vietnam)
# 
# storm_sf_wgs84 <- st_transform(storm_sf, crs(vietnam))
# 
# ## clip to outline of vietnam
# storm_vnm <- st_intersection(storm_sf_wgs84, st_as_sf(vietnam))
# 
# ## remove not needed columns
# storm_vnm$GID_0 <- NULL
# storm_vnm$NAME_0 <- NULL
# 
# plot(storm_vnm$geometry)
# plot(vietnam, add =T)



#### 6. Waste Generation
## remove not needed columns
# waste <- waste[,names(waste) %in% c("VARNAME_1","ENGTYPE_1","waste.t.y.","leakage...")]
# 
# ## rename columns
# names(waste) <- c("location","type","waste_t_y","leakage_perc")



#### IV. DATA ANALYSIS ####

## Heavy Rain days per station (>=100 mm rain per day)
heavyrain_stations <- meteostat_daily %>% group_by(station) %>% dplyr::summarise(heavyraindays = sum(prcp_mean >= 100, na.rm = T))

## Heavy Wind days per station (>39km/h)
heavywind_stations <- meteostat_daily %>% group_by(station) %>% dplyr::summarise(heavywindhours = sum(wspd_mean >= 39, na.rm = T))


## add coordinates to stations
station_data <- unique(meteostat[,c(1,5:7)])

## merge prcp & wspd data
climate_data <- full_join(heavyrain_stations, heavywind_stations, by ="station") 

## join extra station information to climate data
climate_data <- left_join(climate_data, station_data, by ="station")


## join extra station information to daily weather data
weather_daily <- left_join(meteostat_daily, station_data, by ="station")




#### V. create points from coordinates ####

#### 1. Climate Data

## create spatialpoints from lat, long coordinates
climate_stations <- SpatialPointsDataFrame(coords = c(climate_data[,c("longitude","latitude")]),
                                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                           data = climate_data)
plot(climate_stations)

## export as shapefile
shapefile(x = climate_stations, filename = paste(dir, "/climate_stations_VN.shp", sep = ""), overwrite=T)


#################### Data Import ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##
#####################################################



#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("dplyr","raster","sp","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'D:/Documents/WWF_PlastikLeakage_Vietnam/data'



### II. DATA IMPORT ####

## data downloaded for time period: 01.10.2016-01.10.2021


#### 1. Precipitation (daily)
 
## a) NOAA GHCN (daily)
# downloaded from: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/FIPS:VM/detail
# downloaded in metric units (mm)
prcp_noaa <- read_csv(paste(dir, "/2755974.csv", sep = ""))


## b) Meteostat
## downloaded via python API
prcp_meteostat <- read_csv(paste(dir, "/Meteostat_PrecipitationDaily.csv", sep = ""))


#### 2. Wind Speed (hourly)
## downloaded via python API
wind_meteostat <- read_csv(paste(dir, "/Meteostat_WindHourly.csv", sep = ""))



#### III. DATA CLEANING ####

#### 1. Precipitation (daily)

## a) NOAA GHCND
## remove not needed column
prcp_noaa$PRCP_ATTRIBUTES <- NULL
prcp_noaa$ELEVATION <- NULL

## rename columns to fit Meteostat data
names(prcp_noaa) <- tolower(names(prcp_noaa))

## remove NA values (where no precipitation value recorded)
prcp_noaa <- prcp_noaa[!is.na(prcp_noaa$prcp),]
  
## check length of station names
unique(prcp_noaa$station) # 5 digits

## extract station name
prcp_noaa$station <- substr(prcp_noaa$station, nchar(prcp_noaa$station)-4, nchar(prcp_noaa$station))


## b) Meteostat
## rename columns
prcp_meteostat <- rename(prcp_meteostat, date = time)


#### 2. Wind (hourly)

## a) Meteostat
## rename columns
wind_meteostat <- rename(wind_meteostat, datetime = time)

## no NAs or exact duplicates  (already checked in python download script)

## number of duplicates in station, datetime & wind speed
sum(duplicated(wind_meteostat[,1:3])) #0



#### IV. DATA MERGING ####

#### Precipitation (daily)

# first convert to same data type
typeof(prcp_noaa$station) # character
typeof(prcp_meteostat$station) # double
prcp_meteostat$station <- as.character(prcp_meteostat$station)

## merge data by station id
prcp_merge <- rbind(prcp_noaa[,c("station", "date", "prcp")], prcp_meteostat[,c("station", "date", "prcp")])

## remove exact duplicates
prcp_merge <- distinct(prcp_merge)

## number of duplicates in station & date
sum(duplicated(prcp_merge[,1:2])) #4443

## for multiple prcp values at same station & date - take average
prcp_merge <- prcp_merge %>% group_by(station, date) %>% summarize(prcp_mean=mean(prcp))



#### IV. DATA ANALYSIS ####

#### Heavy Rain days per station (>=100 mm rain per day)
heavyrain_stations <- prcp_merge %>% group_by(station) %>% summarise(heavyraindays = sum(prcp_mean >= 100))

#### Heavy Wind days per station (>39km/h)
heavywind_stations <- wind_meteostat %>% group_by(station) %>% summarise(heavywindhours = sum(wspd >= 39))

  

## add coordinates to stations

## station location data - from meteostat data (as noaa is only one of its sources)
station_data <- unique(prcp_meteostat[,c(1,4:6)])


## 1. Precipitation

## join extra station information to prcp data
prcp_data <- left_join(heavyrain_stations, station_data, by ="station")


## 2. Wind

# first convert to same data type
typeof(heavywind_stations$station) # double
typeof(station_data$station) # character

# change to character data type
heavywind_stations$station <- as.character(heavywind_stations$station)

wspd_data <- left_join(heavywind_stations, station_data, by ="station")


  


#### V. create polygons from coordinates ####

#### 1. Precipitation (daily)

## create spatialpoints from lat, long coordinates
prcp_stations <- SpatialPointsDataFrame(coords = c(prcp_data[,c("longitude","latitude")]),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                    data = prcp_data)
plot(prcp_stations)

## export as shapefile
shapefile(x = prcp_stations, file = paste(dir, "/precipitation_stations_VN.shp", sep = ""))


#### 2. Wind (hourly)

## create spatialpoints from lat, long coordinates
wspd_stations <- SpatialPointsDataFrame(coords = c(wspd_data[,c("longitude","latitude")]),
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                        data = wspd_data)
plot(wspd_stations)

shapefile(x = wspd_stations, file = paste(dir, "/windspeed_stations_VN.shp", sep = ""))


#### TODO: combine prcp & wspd into one shapefile?? ####


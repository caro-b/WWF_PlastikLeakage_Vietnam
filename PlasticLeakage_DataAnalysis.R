#################### Data Import ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##
#####################################################



#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("dplyr","tidyverse")
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
wind_meteostat <- rename(wind_meteostat, date = time)



#### IV. DATA MERGING ####

#### 1. Precipitation (daily)

# first convert to same data type
typeof(prcp_noaa$station) # character
typeof(prcp_meteostat$station) # double
prcp_meteostat$station <- as.character(prcp_meteostat$station)

## merge data by station id
prcp_merge <- rbind(prcp_noaa %>% select(station, date, prcp), prcp_meteostat %>% select(station, date, prcp))

## remove exact duplicates
prcp_merge <- distinct(prcp_merge)

## number of duplicates in station & date
sum(duplicated(prcp_merge[,1:2])) #4443

## for multiple prcp values at same station & date - take average
prcp_merge <- prcp_merge %>% group_by(station, date) %>% summarize(prcp_mean=mean(prcp))

## station location data - from meteostat data (as noaa is only one of its sources)
station_data <- unique(prcp_meteostat[,c(1,4:6)])

## join extra station information to prcp data
prcp_data <- left_join(prcp_merge, station_data, by ="station")


## No. of prcp stations
length(unique(prcp_merge$station)) #18




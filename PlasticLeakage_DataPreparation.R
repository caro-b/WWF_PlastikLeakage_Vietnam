#################### Data Preparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
## R version: 4.1.1, Operating system: Windows 10
##


#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("cartography","dplyr","gdalUtils","raster","reproducible","rgeos","rgdal","rnaturalearth","sf","sp","SpaDES","tidyverse")
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

## data downloaded for time period: 01.10.2016-01.10.2021

#### 1. Precipitation (daily)(mm)

## a) NOAA GHCN (daily)
# downloaded from: https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/FIPS:VM/detail
# downloaded as CSV
prcp_noaa <- read_csv(paste(dir, "/2814077.csv", sep = ""))


## b) Meteostat
## downloaded via python API
prcp_meteostat <- read_csv(paste(dir, "/Meteostat_PrecipitationDaily.csv", sep = ""))



#### 2. Wind Speed (hourly)
## downloaded via python API
wind_meteostat <- read_csv(paste(dir, "/Meteostat_WindHourly.csv", sep = ""))



#### 3. Water Areas (JRC Global Surface Water)
## downloaded via Google Earth Engine & processed in QGIS (faster & less memory)
jrc_water <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_30.tif", sep = ""))
#jrc_water_perm <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_perm.tif", sep = ""))
# reads proxy as actual raster to big
#jrc_water_perm <- read_stars(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_perm.tif", sep = ""), NA_value = 0)



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
prcp_meteostat <- dplyr::rename(prcp_meteostat, date = time)


#### 2. Wind (hourly)

## a) Meteostat
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
prcp_merge <- prcp_merge %>% group_by(station, date) %>% dplyr::summarise(prcp_mean=mean(prcp))



#### IV. DATA ANALYSIS ####

#### Heavy Rain days per station (>=100 mm rain per day)
heavyrain_stations <- prcp_merge %>% group_by(station) %>% dplyr::summarise(heavyraindays = sum(prcp_mean >= 100))

#### Heavy Wind days per station (>39km/h)
heavywind_stations <- wind_meteostat %>% group_by(station) %>% dplyr::summarise(heavywindhours = sum(wspd >= 39))


## add coordinates to stations
# station location data - from meteostat data (as noaa is only one of its sources)
station_data <- unique(prcp_meteostat[,c(1,4:6)])
## add noaa stations
station_data <- rbind(station_data, unique(prcp_noaa[,c(1:4)])[!unique(prcp_noaa$station) %in% station_data$station,])

# first convert to same data type
typeof(heavywind_stations$station) # double
typeof(station_data$station) # character

# change to character data type
heavywind_stations$station <- as.character(heavywind_stations$station)

## merge prcp & wspd data
climate_data <- full_join(heavyrain_stations, heavywind_stations, by ="station") 

## join extra station information to climate data
climate_data <- left_join(climate_data, station_data, by ="station")



#### V. create points from coordinates ####

#### 1. Climate Data

## create spatialpoints from lat, long coordinates
climate_stations <- SpatialPointsDataFrame(coords = c(climate_data[,c("longitude","latitude")]),
                                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                           data = climate_data)
plot(climate_stations)

## export as shapefile
shapefile(x = climate_stations, filename = paste(dir, "/climate_stations_VN.shp", sep = ""), overwrite=T)


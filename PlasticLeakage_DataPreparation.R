#################### Data Preparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
## R version: 4.1.1, Operating system: Windows 10
##


#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("basemaps","dplyr","gdalUtils","ggmap","raster","reproducible","rgeos","rgdal","sf","sp","SpaDES","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'D:/Documents/WWF_PlastikLeakage_Vietnam/data'



### II. DATA IMPORT ####

## download country boundary of Vietnam from GADM via inbuilt function
vietnam <- getData('GADM', country='VNM', level=0)


#### Plastic Leakage Factors

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



#### 3. Water Areas (JRC Global Surface Water)
## downloaded via Google Earth Engine & processed in QGIS (faster & less memory)
jrc_water <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_clipped.tif", sep = ""))
plot(jrc_water)
# jrc_water1 <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000000000-0000000000.tif", sep = ""))
# jrc_water2 <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000046592-0000000000.tif", sep = ""))

## merge tiles into one image
# may take some time to run
# jrc_water_merged <- do.call(merge, c(jrc_water1, jrc_water2))

## spatial subsetting (faster than masking complete dataset)
# jrc_water_masked <- jrc_water_merged[vietnam, ]
# plot(jrc_water_masked)
# 
# ## mask raster to outline of vietnam
# # faster than raster mask function (as big RasterLayer) (may take some time to run)
# m <- list(jrc_water_merged, vietnam)
# jrc_water_masked <- fastMask(jrc_water_merged, vietnam)
# plot(jrc_water_masked)



#### 4. Natural Hazards (Flooding & Storm)

#### a) Flooding


#### b) Storm
storm <- readOGR(paste(dir, "/unisys_tracks_1956_2018dec31/UNISYS_tracks_1956_2018Dec31.shp", sep = ""), use_iconv = TRUE, encoding = "UTF-8")

## first change CRS to WGS84
crs(storm)
crs(vietnam)
# may take some time
storm_wgs84 <- spTransform(storm, crs(vietnam))

## spatial subsetting (faster than masking complete dataset)
storm_vnm <- storm_wgs84[vietnam, ]

## mask raster to outline of vietnam
storm_vnm <- gIntersection(storm_vnm, vietnam)
plot(storm_vnm)
plot(vietnam, add =T)



#### 5. Topography - DEM (Digital Elevation Model)
# import DEM of Vietnam (30m) as RasterLayer
dem <- raster(paste(dir, "/dem/dem_compress.tif", sep = ""))

# calculate slope
slope <- terrain(dem, opt = 'slope')

# add slope as band
dem <- stack(dem, slope)

# rename band
names(dem)[[1]] <- "elevation"

plot(dem$elevation, main = "Elevation (DEM)")
plot(vietnam, add=T)

plot(dem$slope, main = "Slope (DEM)")
plot(vietnam, add=T)



#### 6. Waste Generation
waste <- readOGR(paste(dir, "/gadm36_VNM_1_wasterperprovince_2003_UTM48N.shp", sep = ""), use_iconv = TRUE, encoding = "UTF-8")
plot(waste)



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



#### 6. Waste Generation
## remove not needed columns
waste$GID_0 <- NULL
waste$NAME_0 <- NULL
waste$GID_1 <- NULL
waste$NAME_1 <- NULL
waste$TYPE_1 <- NULL
waste$areacode <- NULL

## rename columns
names(waste) <- c("location","type","waste_t_y")



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
# station location data - from meteostat data (as noaa is only one of its sources)
station_data <- unique(prcp_meteostat[,c(1,4:6)])

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


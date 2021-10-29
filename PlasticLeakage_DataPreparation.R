#################### Data Preparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##



#### I. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("basemaps","dplyr","gdalUtils","raster","rgdal","sp","SpaDES","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'D:/Documents/WWF_PlastikLeakage_Vietnam/data'



### II. DATA IMPORT ####

## data downloaded for time period: 01.10.2016-01.10.2021


#### 0. Country Boundaries

## download country boundary of Vietnam from GADM via inbuilt function
vietnam <- getData('GADM', country='VNM', level=0)



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
## downloaded via Google Earth Engine
jrc_water <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam_clipped.tif", sep = ""))
plot(jrc_water)
plot(vietnam, add=T)

basemap_raster(vietnam)

## mask image to extent of Vietnam
mask <- jrc_water
# 0 values to be NA so they don't get mapped
mask[mask == 0] <- NA
plot(mask)
#jrc_water_masked <- mask(x = jrc_water2, mask = mask)

#jrc_water1 <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000000000-0000000000.tif", sep = ""))
#jrc_water2 <- raster(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000046592-0000000000.tif", sep = ""))

## use gdal for merging (faster than raster function)
# all_my_rasts <- c(paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000000000-0000000000.tif", sep = ""),
#                    paste(dir, "/JRC_GlobalSurfaceWater_Vietnam-0000046592-0000000000.tif", sep = ""))
#                   
# template <- raster(vietnam)
# projection(template) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
# writeRaster(template, file="MyBigNastyRasty.tif", format="GTiff")
# mosaic_rasters(gdalfile=all_my_rasts,dst_dataset="MyBigNastyRasty.tif",of="GTiff", overwrite =T)
# gdalinfo("MyBigNastyRasty.tif")

# Mosaic/merge raster tiles into one image
#jrc_water <- merge(jrc_water1, jrc_water2, extent = extent(vietnam))

# # merge tiles into one image
# x <- list(jrc_water1, jrc_water2)
# jrc_water <- do.call(merge, x)
# plot(jrc_water)
# #writeRaster(jrc_water, file="jrc_water.tif", format="GTiff")

# ## trim raster
# jrc_water1_trim <- trim(jrc_water1, values=c(NA,0))
# plot(jrc_water1_trim)

# ## tile too big - first split raster into 2 tiles
# tiles <- splitRaster(jrc_water1, ny = 2)
# jrc_water1a <- tiles[[1]]
# jrc_water1b <- tiles[[2]]



#### 4. Natural Hazards (Flooding & Storm)

#### a) Flooding




#### b) Storm





#### 5. Topography - DEM (Digital Elevation Model)

# ## download SRTM 90m DEM tiles
# srtm1 <- getData('SRTM', lon=103, lat=10)
# srtm2 <- getData('SRTM', lon=106, lat=15)
# srtm3 <- getData('SRTM', lon=106, lat=20)
# 
# plot(srtm3)
# plot(vietnam, add=T)
# 
# # Mosaic/merge srtm tiles
# srtmmosaic <- mosaic(srtm, srtm2, srtm3, fun=mean)


# import DEM of Vietnam (30m) as RasterLayer
dem <- raster(paste(dir, "/dem/dem_compress.tif", sep = ""))

# calculate slope
dem_slope <- terrain(dem, opt = 'slope')

plot(dem, main = "Elevation (DEM)")
plot(vietnam, add=T)

plot(dem_slope, main = "Slope (DEM)")
plot(vietnam, add=T)




#### 6. Waste Generation




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

## join extra station information to wspd data
wspd_data <- left_join(heavywind_stations, station_data, by ="station")





#### V. create polygons from coordinates ####

#### 1. Precipitation (daily)

## create spatialpoints from lat, long coordinates
prcp_stations <- SpatialPointsDataFrame(coords = c(prcp_data[,c("longitude","latitude")]),
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                        data = prcp_data)
plot(prcp_stations)

## export as shapefile
shapefile(x = prcp_stations, filename = paste(dir, "/precipitation_stations_VN.shp", sep = ""), overwrite=T)


#### 2. Wind (hourly)

## create spatialpoints from lat, long coordinates
wspd_stations <- SpatialPointsDataFrame(coords = c(wspd_data[,c("longitude","latitude")]),
                                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),
                                        data = wspd_data)
plot(wspd_stations)

shapefile(x = wspd_stations, filename = paste(dir, "/windspeed_stations_VN.shp", sep = ""), overwrite=T)


#### TODO: combine prcp & wspd into one shapefile?? ####


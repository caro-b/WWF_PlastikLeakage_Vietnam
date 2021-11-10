#################### VariablePreparation ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("cartography","cluster","dplyr","factoextra","gdalUtils","ggmap","plyr","raster","reproducible","rgeos","rgdal","sf","sp","spatialEco","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

library(psych)
library(corrplot)
library(ggplot2)


#### I. Import landfill Locations ####
## import landfill polygons
landfills <- readOGR(paste(dir, "/OpenLandfills_Vietnam/OpenLandfills_Vietnam.shp", sep = ""), use_iconv = T, encoding = "UTF-8")
plot(landfills)
## access & plot first landfill
plot(landfills[1,])

## calulcate landfill area in ha (from meters) (polygon area)
landfills$area_ha <- area(landfills)/10000

## calculate centroids
# first convert data to simple feature object (sf) (for easier operation & later plotting with ggplot)
landfills_sf <- st_as_sf(landfills)

# calculate centroids of landfills for calculating distance to nearest points/polygons
landfills_sf_centroids <- st_centroid(landfills_sf)
plot(landfills_sf$geometry)
plot(landfills_sf_centroids$geometry, add = T)



#### II. Plastic Leakage Factors ####

#### 1. Climatic conditions (Precipitation (daily) & Wind Speed (hourly))
## point data --> find nearest station
climate_stations_sf <- st_as_sf(climate_stations)

## save results in spatialdataframe
landfills_factors <- landfills_sf_centroids
# initiate columns with dummy variable
landfills_factors$dist_station <- -1
landfills_factors$rain <- -1
landfills_factors$windspeed <- -1

## function to find nearest climate station & save corresponding data
nearest_climate_station <- function(landfills_factors) {
  
  ## calculate the distance matrix in meters using Great Circle distance (for lat/long data) from one landfill to all climate stations
  dist_climate <- st_distance(landfills_factors[i,]$geometry, climate_stations_sf$geometry)
  
  ## calculate minimum distance
  landfills_factors[i,]$dist_station <- min(dist_climate)
  
  ## find row associated to min distance
  dist_min_climate <- (which(dist_climate == min(dist_climate), arr.ind=TRUE))[[1,2]]
  
  ## find associated climate station & save climate values to sf object
  landfills_factors[i,]$rain <- climate_stations_sf[dist_min_climate,]$heavyraindays
  landfills_factors[i,]$windspeed <- climate_stations_sf[dist_min_climate,]$heavywindhours
  
  return(landfills_factors)
}

## test if nearest point found
# plot(landfills_sf_centroids$geometry)
# plot(climate_stations_sf, add=T, col="blue")
# plot(landfills_sf_centroids[1,]$geometry, add = T, col="green")
# plot(climate_stations_sf[dist_min_climate,]$geometry, col="pink", add=T)#works



#### 3. Water Areas (JRC) (& Flooding)
## raster with values if there is data (else na) --> nearest & average value?

# initiate columns with dummy variable
landfills_factors$dist_water <- -1
landfills_factors$dist_permwater <- -1
# % of area flooded
landfills_factors$flood_risk <- -1

## function to find nearest water & save corresponding data
nearest_water <- function(landfills_factors) {
  
  # get water in 1km buffer around landfill - then high flooding risk
  buffer <- st_buffer(landfills_sf_centroids[i,], dist = 1000) # dist in meters
  
  # intersect to get water area in buffer
  water <- intersect(jrc_water, buffer)
  ## flood risk (% flooded)
  landfills_factors[i,]$flood_risk <- sum(values(water), na.rm=T)/ length(values(water)) # na values counted as 0
  
  # get broader buffer for distances to water bodies
  buffer_broad <- st_buffer(landfills_sf_centroids[i,], dist = 10000) # 10km
  water_broad <- intersect(jrc_water, buffer_broad)
  # polygonize to calculate distance
  water_vector <- rasterToPolygons(water_broad, fun = function(x){x>0}, na.rm = T, dissolve = T)
  
  ## calculate minimum distance to closest water
  # account for no water in buffer
  if(is.null(water_vector)) {
    landfills_factors[i,]$dist_water <- NA
    landfills_factors[i,]$dist_permwater <- NA
  } else  { #if (!is.null(water_vector))
    
    landfills_factors[i,]$dist_water <- min(st_distance(st_as_sf(water_vector), landfills_sf_centroids[i,]))
    
    # calculate distance to closest permanent water (e.g. river)?? 
    water_perm <- water_vector[water_vector[[1]] >=50,]
    
    # account for no permanent water
    if (length(water_perm) != 0) {
      landfills_factors[i,]$dist_permwater <- min(st_distance(st_as_sf(water_perm), landfills_sf_centroids[i,]))
    } else {
      landfills_factors[i,]$dist_permwater <- NA
    }
  }
  return(landfills_factors)
}

## Distance to Ocean 

# download coastline from naturalearth
# coastline <- ne_download(scale = 10, type = 'coastline', category = 'physical')
# coastline_vnm <- intersect(coastline, vietnam)
#islands <- ne_download(scale = 10, type = 'minor_islands', category = 'physical')
# coastline_islands <- readOGR(paste(dir, "/ne_10m_minor_islands_coastline/ne_10m_minor_islands_coastline.shp", sep = ""))
# coastline_islands_vnm <- intersect(coastline_islands, vietnam)
# plot(coastline_vnm)

# download ocean from naturalearth
ocean <- ne_download(scale = 10, type = 'ocean', category = 'physical')
ocean_vnm <- intersect(ocean, vietnam)
plot(ocean_vnm)

landfills_factors$dist_ocean <- -1

## function to find distance to ocean
distance_ocean <- function(landfills_factors) {
  
  ## calculate the distance matrix in meters using Great Circle distance (for lat/long data)
  dist_ocean <- st_distance(landfills_factors[i,]$geometry, st_as_sf(ocean_vnm))
  
  ## calculate minimum distance
  landfills_factors[i,]$dist_ocean <- min(dist_ocean)
  
  return(landfills_factors)
}



#### 4. Natural Hazards (Flooding & Storm)
#### (a) Flood Proneness)


#### b) Storm Tracks

# initiate columns with dummy variable
landfills_factors$no_storms <- -1

## function to find nearest water & save corresponding data
find_storms <- function(landfills_factors) {
  
  # get water in 1km buffer around landfill - then high risk
  buffer <- st_buffer(landfills_sf_centroids[i,], dist = 1000) # 10km
  
  # intersect to get storm tracks in buffer
  storm <- st_intersection(storm_vnm, st_as_sf(buffer))
  
  # count number of storm tracks in buffer - higher risk
  landfills_factors[i,]$no_storms <- nrow(storm)
  
  return(landfills_factors)
}



#### 5. Topography - DEM (Digital Elevation Model)

landfills_factors$slope <- -1

## function to get DEM slope values per landfill
mean_slope <- function(landfills_factors) {
  
  slope_area <- intersect(slope, landfills[i,])
  landfills_factors[i,]$slope <- mean(values(slope_area))
  
  return(landfills_factors)
}



#### 6. Waste Generation
## polygon per province --> in which polygon landfill lies in
waste_sf <- st_as_sf(waste)

# convert CRS to get matching CRS
landfills_sf <- st_transform(landfills_sf, crs(waste_sf))

landfills_factors$waste <- -1

waste_province <- function(landfills_factors) {
  waste <- st_intersection(waste_sf, landfills_sf[i,], sparse=T)
  if (nrow(waste) != 0) {
    landfills_factors[i,]$waste <- waste$waste_t_y
  } else {
    landfills_factors[i,]$waste <- NA
  }
  return(landfills_factors)
}



## loop over landfills - take one landfill at once & find nearest climate station
# index for loop
i <- 1
while (i <= length(landfills_factors$geometry)) {
  # function to find nearest station & save climate attributes into sf object
  landfills_factors <- nearest_climate_station(landfills_factors)
  # function to find nearest water body
  landfills_factors <- nearest_water(landfills_factors)
  # function to count storms near landfill
  landfills_factors <- find_storms(landfills_factors)
  # function to get mean slope per landfill area
  landfills_factors <- mean_slope(landfills_factors)
  # function to test in which province landfill lies & save corresponding waste generation
  landfills_factors <- waste_province(landfills_factors)
  # distance to ocean
  landfills_factors <- distance_ocean(landfills_factors)
  ## increment i
  i <- i+1
}

## download dataframe as CSV
write.csv(landfills_factors, paste(dir, "/landfill_factors.csv", sep = ""), row.names = F)


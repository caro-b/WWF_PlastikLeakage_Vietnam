#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. Import landfill Locations
## import landfill polygons
landfills <- readOGR(paste(dir, "/OpenLandfills_Vietnam/OpenLandfills_Vietnam.shp", sep = ""), use_iconv = T, encoding = "UTF-8")
plot(landfills)
## access & plot first landfill
plot(landfills[1,])

## calculate centroids
# first convert data to simple feature object (sf) (for easier operation & later plotting with ggplot)
landfills_sf <- st_as_sf(landfills)

# calculate centroids of landfills for calculating distance to nearest points/polygons
landfills_sf_centroids <- st_centroid(landfills_sf)
plot(landfills_sf$geometry)
plot(landfills_sf_centroids$geometry, add = T)



#### Plastic Leakage Factors

#### 1. Climatic conditions (Precipitation (daily) & Wind Speed (hourly))
## point data --> find nearest station
climate_stations_sf <- st_as_sf(climate_stations)

## save results in spatialdataframe
landfills_factors <- landfills_sf_centroids
# initiate columns with dummy variable
landfills_factors$min_dist <- -1
landfills_factors$rain <- -1
landfills_factors$windspeed <- -1

## function to find nearest climate station & save corresponding data
nearest_climate_station <- function(landfills_factors) {
  ## calculate the distance matrix in meters using Great Circle distance (for lat/long data) from one landfill to all climate stations
  dist_climate <- st_distance(landfills_factors[i,]$geometry, climate_stations_sf$geometry)
  
  ## calculate minimum distance
  landfills_factors[i,]$min_dist <- min(dist_climate)
  
  ## find row associated to min distance
  dist_min_climate <- (which(dist_climate == min(dist_climate), arr.ind=TRUE))[[1,2]]
  
  ## find associated climate station & save climate values to sf object
  landfills_factors[i,]$rain <- climate_stations_sf[dist_min_climate,]$heavyraindays
  landfills_factors[i,]$windspeed <- climate_stations_sf[dist_min_climate,]$heavywindhours
  
  return(landfills_factors)
}


## loop over landfills - take one landfill at once & find nearest climate station
# index for loop
i <- 1
while (i <= length(landfills_factors$geometry)) {
  # use function to find nearest station & save climate attributes into sf object
  landfills_factors <- nearest_climate_station(landfills_factors)
  ## increment i
  i <-  i+1
}

## check if nearest point found
# plot(landfills_sf_centroids$geometry)
# plot(climate_stations_sf, add=T, col="blue")
# plot(landfills_sf_centroids[1,]$geometry, add = T, col="green")
# plot(climate_stations_sf[dist_min_climate,]$geometry, col="pink", add=T)#works



#### 3. Water Areas (JRC)
## raster with values if there is data (else na) --> nearest & average value?



#### 4. Natural Hazards (Flooding & Storm)
#### a) Flooding

#### b) Storm
## polygons of storm tracks --> nearest


#### 5. Topography - DEM (Digital Elevation Model)
## raster of whole vietnam - average value of area


#### 6. Waste Generation
## polygon per province --> in which polygon landfill lies in


#### TODO: function: create risk per landfill ####


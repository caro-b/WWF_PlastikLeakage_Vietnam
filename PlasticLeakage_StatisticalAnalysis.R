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
  
  # get water in 1km buffer around landfill - then high risk
  buffer <- buffer(landfills[i,], width = 0.005)
  # intersect to get water area in buffer
  water_first <- intersect(jrc_water, buffer)

  ## water occurrence value - higher flood risk
  landfills_factors[i,]$flood_risk <- sum(values(water_first), na.rm=T)/ length(values(water_first)) # na values counted as 0
  
  # polygonize to calculate distance
  water_vector <- rasterToPolygons(water_first)
  
  ## calculate minimum distance to closest water
  # account for no water in buffer
  if(is.null(water_vector)) {
    landfills_factors[i,]$dist_water <- NA
    landfills_factors[i,]$dist_permwater <- NA
  } else  { #if (!is.null(water_vector))
    #water_dist <- (st_distance(st_as_sf(water_vector), landfills_sf_centroids[i,]))
    landfills_factors[i,]$dist_water <- min(st_distance(st_as_sf(water_vector), landfills_sf_centroids[i,]))
    
    # calculate distance to closest permanent water (e.g. river)?? 
    water_perm <- water_vector[water_vector$JRC_GlobalSurfaceWater_Vietnam_clipped >=50,]
    
    # account for no permanent water
    if (length(water_perm) != 0) {
      landfills_factors[i,]$dist_permwater <- min(st_distance(st_as_sf(water_perm), landfills_sf_centroids[i,]))
    } else {
      landfills_factors[i,]$dist_permwater <- NA
    }
  }
  return(landfills_factors)
}


#### TODO: better to use river data ??? ####
#### TODO: vietnam outline for distance to ocean ####
#vietnam_sf <- st_as_sf(vietnam)
#ocean <- getBorders(vietnam)



## loop over landfills - take one landfill at once & find nearest climate station
# index for loop
i <- 1
while (i <= length(landfills_factors$geometry)) {
  # use function to find nearest station & save climate attributes into sf object
  landfills_factors <- nearest_climate_station(landfills_factors)
  landfills_factors <- nearest_water(landfills_factors)
  ## increment i
  i <- i+1
}




#### 4. Natural Hazards (Flooding & Storm)
#### a) Flood Proneness




#### b) Storm
## polygons of storm tracks --> nearest
storm_vnm



#### 5. Topography - DEM (Digital Elevation Model)
## raster of whole vietnam - average value of area
plot(slope)
plot(landfills_sf_centroids$geometry, add=T)
plot(landfills[1,])
first <- landfills[1,]
test <- intersect(slope, first)
slope_mean <- mean(values(test))



#### 6. Waste Generation
## polygon per province --> in which polygon landfill lies in


#### TODO: function: create risk per landfill ####


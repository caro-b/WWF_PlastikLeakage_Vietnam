#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. Import Landfill Locations
## import landfill polygons
landfills <- readOGR(paste(dir, "/WWF_Landfills_Vietnam/WWF_Landfills_Vietnam.shp", sep = ""), use_iconv = TRUE, encoding = "UTF-8")



#### Plastic Leakage Factors

#### 1. Climatic conditions (Precipitation (daily) & Wind Speed (hourly))
## point data --> nearest point



#### 3. Water Areas
## raster with values if there is data (else na) --> nearest & average value?



#### 4. Natural Hazards (Flooding & Storm)
#### a) Flooding

#### b) Storm
## polygons of storm tracks --> nearest


#### 5. Topography - DEM (Digital Elevation Model)
## raster of whole vietnam - average value of area


#### 6. Waste Generation
## polygon per province --> in which polygon landfill lies in






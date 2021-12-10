#################### Plotting Clustering Results ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##
## Goal of this script:
## Develop an interactive map to plot the clustering results of the landfills in regard to their plastic leakage risk
## Develop an interactive web app for users & stakeholders to get an overview over the risk assessment & the data included

# install required packages (if not installed yet)
packagelist <- c("dplyr","ggplot2","leafem","leaflet","RColorBrewer","readr","rgdal","sf","shiny","sp","stars","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/data'


#### 0. Import Data ####

## import landfills clusters spatial data
landfills_sf <- st_read(paste(dir,"/landfill_clusters.gpkg", sep= ""))

### import landfill polygons
landfills <- readOGR(paste(dir, "/landfills/OpenLandfills_Vietnam.shp", sep = ""), use_iconv = T, encoding = "UTF-8")

## import shapefile of vietnam
vietnam <- readOGR(paste(dir, "/vietnam/vietnam.shp", sep = ""))

## import data layers of web app



#### 1. Interactive Map (Leaflet) ####

## plot map with landfills colored by cluster
# e.g. plot water distance < 500m in red

# create color palette
cof <- colorFactor(c("red","green","blue"), domain=c("1","2","3"))

map <- leaflet(landfills_sf) %>%
  #addTiles() %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  #addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(lng = 105.48, lat = 15.54, zoom = 5) %>%
  addCircleMarkers(color = ~cof(km_cluster_unstand), radius = sqrt(landfills_sf$area_ha)*2, fillOpacity = 0.5) %>%
  addLegend("bottomright", colors= c("red","blue","green"), labels=c("high", "medium", "low"), title="Leakage Risk") 
map



#### add raster images as interactive layers
# may take some time to load due to big raster data sets
# for large rasters use stars package

## DEM
dem = read_stars("C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/data/dem/dem_compress_clipped.tif")
st_is_longlat(dem)

fl = tempfile(fileext = ".tif")
write_stars(dem, dsn = fl)

# color palette
#pal_dem <- hcl.colors(9, "terrain 2")
#pal_dem <- terrain.colors(5)
pal_dem <- RColorBrewer::brewer.pal(9, "Greys")

# define own color palette
#col <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")


## JRC Water
jrc = read_stars("C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/data/JRC_GlobalSurfaceWater_Vietnam_30.tif")
st_is_longlat(jrc)

## needs to be down-sampled else raster to big for R to handle
jrc_ds = stars:::st_downsample(jrc, n = 10)
dim(jrc_ds)

file = tempfile(fileext = ".tif")
write_stars(jrc_ds, dsn = file)

# remove 0 values for plotting
jrc_ds[jrc_ds == 0] <- NA
write_stars(jrc_ds, dsn = file)

# define own color palette
pal <- RColorBrewer::brewer.pal(9, "Blues")


map_raster <- leaflet(options = leafletOptions(noWrap = T)) %>%
  setView(lng = 105.48, lat = 15.54, zoom = 5) %>%
  # Base groups
  #addTiles(group = "OSM") %>% # Add OpenStreetMap map tiles
  addProviderTiles("OpenStreetMap", group = "OSM") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  # Add raster images (as overlay groups)
  # addGeoRaster for stars & star_proxy objects
  leafem:::addGeotiff(file = fl, group = "DEM", colorOptions = leafem:::colorOptions( 
    palette = pal_dem, na.color = "transparent")) %>%
  leafem:::addGeotiff(file = file, group = "Water", colorOptions = leafem:::colorOptions(
    palette = pal, na.color = "transparent")) %>%
  # Overlay groups
  addPolygons(data = vietnam, fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  addPolygons(data = landfills, fill = F, weight = 2, color = "#FFFFCC", group = "Landfills") %>%
  addCircleMarkers(data = landfills_sf, color = ~cof(km_cluster_unstand), radius = sqrt(landfills_sf$area_ha)*2, 
                   fillOpacity = 0.5, group = "Climate") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM", "Dark", "Satellite"),
    overlayGroups = c("DEM", "Water", "Climate", "Outline", "Landfills"),
    options = layersControlOptions(collapsed = F)
  )  
  addLegend("bottomright", colors= c("red","blue","green"), labels=c("high", "medium", "low"), title="Leakage Risk") 
map_raster

#### TODO: raster overlay only works for OSM basemap ?? ####
#### try addGeoRaster instead of addGeotiff

  

#### 2. Web App (Leaflet & Shiny) ####

## Define UI of the app
ui <- fluidPage(
  ## App title
  titlePanel("Plastic Leakage Risk of Landfills in Vietnam"),
  
  sidebarLayout(position = "right",
    sidebarPanel("sidebar panel for inputs"),
    mainPanel("main panel for outputs")
  ),
  
  ## add interactive leaflet map
  leafletOutput("map"),
  
  ## add control widget for user to interact with
  # add map layers as select box
  selectInput("selectLayer", "Layer", choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                     "Choice 3" = 3), selected = 1),
  
  # add risk clusters as checkbox group
  checkboxGroupInput("riskCluster",
                     h3("Checkbox group"), 
                     choices = list("Choice 1" = 1, 
                                    "Choice 2" = 2, 
                                    "Choice 3" = 3),
                     selected = 1)
)

# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    map
  })
}

# Run the app
shinyApp(ui, server)




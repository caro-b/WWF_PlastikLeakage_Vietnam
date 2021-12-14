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
packagelist <- c("dplyr","ggplot2","leafem","leaflet","raster","RColorBrewer","readr","rgdal","sf","shiny","sp","stars","tidyverse")
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
  setView(lng = 105.48, lat = 15.54, zoom = 6) %>%
  addMiniMap %>%
  addPolygons(data = vietnam, fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  addPolygons(data = landfills, fill = F, weight = 2, color = "black") %>%
  addCircleMarkers(color = ~cof(km_cluster_unstand), radius = sqrt(landfills_sf$area_ha)*2, fillOpacity = 0.5) %>%
  addLegend("bottomleft", colors= c("red","blue","green"), labels=c("high", "medium", "low"), title="Leakage Risk") 
map



#### add raster images as interactive layers
# may take some time to load due to big raster data sets
# for large rasters use stars package

## DEM
dem <- read_stars(paste(dir, "/dem/dem_slope.tif", sep = ""))

st_is_longlat(dem)

fl <- tempfile(fileext = ".tif")
write_stars(dem, dsn = fl)

# color palette
pal_dem <- RColorBrewer::brewer.pal(9, "Greys")

# define own color palette
pal_dem2 <- colorNumeric(palette = c('#ffffff', '#808080', '#000000'), domain = c(0,80), na.color = "transparent")


## JRC Water
jrc <- read_stars("C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/data/JRC_GlobalSurfaceWater_Vietnam_30.tif")

st_is_longlat(jrc)

## needs to be down-sampled else raster to big for R to handle
jrc_ds <- stars:::st_downsample(jrc, n = 10)
dim(jrc_ds)

file <- tempfile(fileext = ".tif")
write_stars(jrc_ds, dsn = file)

# remove 0 values for plotting
jrc_ds[jrc_ds == 0] <- NA
write_stars(jrc_ds, dsn = file)

# define own color palette
pal <- RColorBrewer::brewer.pal(9, "Blues")

# define own color palette
pal_jrc <- colorNumeric(palette= c("#FFFFCC", "#41B6C4", "#0C2C84"), domain = c(0,100), na.color = "transparent")


map_raster <- leaflet(options = leafletOptions(noWrap = T)) %>%
  setView(lng = 105.48, lat = 15.54, zoom = 5) %>%
  # Base groups
  #addTiles(group = "OSM") %>% # Add OpenStreetMap map tiles
  #addProviderTiles("OpenStreetMap", group = "OSM") %>% 
  #addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addMiniMap %>% 
  # Add raster images (as overlay groups)
  # addGeoRaster for stars & star_proxy objects
  leafem:::addGeotiff(file = fl, group = "DEM", colorOptions = leafem:::colorOptions( 
    palette = pal_dem, na.color = "transparent")) %>%
  leafem:::addGeotiff(file = file, group = "Water", colorOptions = leafem:::colorOptions(
    palette = pal, na.color = "transparent")) %>%
  # Overlay groups
  addPolygons(data = vietnam, fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  addPolygons(data = landfills, fill = F, weight = 2, color = "black", group = "Landfills") %>%
  addCircleMarkers(data = landfills_sf, color = ~cof(km_cluster_unstand), radius = sqrt(landfills_sf$area_ha)*2, 
                   fillOpacity = 0.5, label = ~name, group = "Risk") %>%
  addLegend("bottomleft", colors = c("red","blue","green"), labels = c("high", "medium", "low"), 
            title ="Leakage Risk", group = "Risk") %>%
  addLegend(pal = pal_dem2, values = c(0,80), bins = c(0,80), labFormat = labelFormat(suffix = "°"),
            title = "Slope", group = "DEM") %>%
  addLegend(pal = pal_jrc, values = c(0,100), bins = c(0,100), labFormat = labelFormat(suffix = "%"),
            title = "Water Occurrence", group = "Water") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("DEM", "Water"),
    options = layersControlOptions(collapsed = F)
  )
map_raster



#### Interactive Map ####

ui_inter <- navbarPage("Superzip", id="nav",
           
  tabPanel("Interactive Map",
    div(class="outer",
    
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "1700px", height = "800px"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
        draggable = T, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",
        
        h2("Plastic Leakage Risk"),
        
        selectInput(inputId = "data_type",
                    label = "Choose data layer:",
                    choices = c("","DEM", "Water")
        ),
        
        plotOutput("histRain", height = 200),
        
        plotOutput("histWind", height = 200),
    ),
  )
),

  tabPanel("Data Explorer",
    hr(),
    
    # display the data in an interactive table
    DT::dataTableOutput("landfills")
  )
)


server_inter <- function(input, output, session) {
  
  ## create interactive map with leaflet
  output$map <- renderLeaflet({
    map
  })
  
  # add new layer to map if "DEM" selected in dropdown
  observeEvent(input$data_type, {
    if(input$data_type == "DEM")
    {
      leafletProxy("map", session) %>%
        leafem:::addGeotiff(file = fl, group = "DEM", layerId = "layer1",
                            colorOptions = leafem:::colorOptions(palette = pal_dem, na.color = "transparent")) %>%
        addLegend(pal = pal_dem2, values = c(0,80), bins = c(0,80), labFormat = labelFormat(suffix = "°"),
                  title = "Slope", group = "DEM", layerId = "layer2", position = "bottomleft")
    }
  })
  
  observeEvent(input$data_type, {
    if(input$data_type == "Water")
    {
      leafletProxy("map", session) %>%
        leafem:::addGeotiff(file = file, group = "Water", layerId = "layer1",
                            colorOptions = leafem:::colorOptions(palette = pal, na.color = "transparent")) %>%
        addLegend(pal = pal_jrc, values = c(0,100), bins = c(0,100), labFormat = labelFormat(suffix = "%"),
                  title = "Water Occurrence", group = "Water", layerId = "layer2", position = "bottomleft")
    }
  })
  
  # create object for clicked marker (=landfill)
  observeEvent(input$map_marker_click,{
    ## click returns clickid, long & lat
    click <- input$map_marker_click
    # if(is.null(click))
    #   return()
    leafletProxy("map", session) %>% setView(lng = click$lng, lat = click$lat, zoom = 16)
  })
  
  # A reactive expression that returns the set of zips that are in map bounds
  landfillsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(landfills_sf[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(landfills_sf,
      st_coordinates(landfills_sf)[,2] >= latRng[1] & st_coordinates(landfills_sf)[,2] <= latRng[2] &
      st_coordinates(landfills_sf)[,1] >= lngRng[1] & st_coordinates(landfills_sf)[,1] <= lngRng[2])
  })

  output$histRain <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(landfillsInBounds()) == 0)
      return(NULL)
    hist(landfillsInBounds()$rain,
       main = "Weather Data",
       xlab = "Heavy Rain Days",
       xlim = range(landfills_sf$rain),
       col = '#00ffff',
       border = 'white')
  })
  
  output$histWind <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(landfillsInBounds()) == 0)
      return(NULL)
    hist(landfillsInBounds()$windspeed,
       main = "",
       xlab = "Heavy Wind Days",
       xlim = range(landfills_sf$windspeed),
       col = '#00DD00',
       border = 'white')
  })

  ## Data Explorer ##
  output$landfills <- DT::renderDataTable({
    df <- landfills_sf
  })
}

# Run the app
shinyApp(ui_inter, server_inter)



#### TODO: not zoom out again when layer added ####

#### TODO: publish app on shinyapps.io ####
# https://shiny.rstudio.com/articles/shinyapps.html#:~:text=Shinyapps.io%20is%20an%20online,focus%20on%20writing%20great%20apps!&text=Use%20the%20tokens%20generated%20by,to%20configure%20your%20rsconnect%20package.
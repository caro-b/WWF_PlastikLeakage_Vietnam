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
packagelist <- c("class", "dplyr","ggplot2","leafem","leaflet","leaflet.extras","raster","RColorBrewer","readr","rgdal","sf","shiny","sp","stars","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/data'


#### 0. Import Data ####

## import landfills clusters spatial data (point data)
landfills_clusters <- readOGR(paste(dir,"/landfill_clusters.gpkg", sep= ""))
landfills_clusters_sf <- st_read(paste(dir,"/landfill_clusters.gpkg", sep= ""))

### import landfill polygons
landfills_polygons <- readOGR(paste(dir, "/landfills/OpenLandfills_Vietnam.shp", sep = ""), use_iconv = T, encoding = "UTF-8")

## import shapefile of vietnam
vietnam <- readOGR(paste(dir, "/vietnam/vietnam.shp", sep = ""))



#### 1. Interactive Map (Leaflet) ####

## plot map with landfills colored by cluster
# e.g. plot water distance < 500m in red

# create color palette
cof <- colorFactor(c("green","blue","red"), domain=c("1","2","3"))

map <- leaflet(landfills_clusters_sf) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  setView(lng = 105.48, lat = 15.54, zoom = 6) %>%
  addMiniMap %>%
  addPolygons(data = vietnam, fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  addPolygons(data = landfills_polygons, fill = F, weight = 2, color = "#FFFFCC") %>%
  addCircleMarkers(data = landfills_clusters_sf, color = ~cof(km_cluster_unstand), radius = sqrt(landfills_clusters_sf$area_ha)*2, 
                   fillOpacity = 0.5, label = ~name, group = "Risk") %>%
  addLegend("bottomleft", colors= c("red","blue","green"), labels=c("high", "medium", "low"), title="Leakage Risk")
map



#### Interactive Map ####
ui_inter <- fluidPage("Classification the Plastic Leakage Risk of Landfills in Vietnam", id="nav",
           
  tabPanel("Interactive Map",
    div(class="outer",
    
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "1700px", height = "800px"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
        draggable = T, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",
        
        h2("Plastic Leakage Risk"),
        
        plotOutput("histRain", height = 200),
        plotOutput("histWind", height = 200),
    ),
  )
),

  tabPanel("Data Explorer",
    hr(),
    
    # display the data in an interactive table
    DT::dataTableOutput("landfills"),
    textInput('Long', 'Enter new landfill longitude'),
    textInput('Lat', 'Enter new landfill latitude'),
    actionButton("update", "Update Table")
  )
)

df <- landfills_clusters_sf[-c(2,9:10,12,16:17)]# select relevant columns
## add long & lat coordinates
df$long <- st_coordinates(landfills_clusters_sf)[,1]
df$lat <- st_coordinates(landfills_clusters_sf)[,2]


server_inter <- function(input, output, session) {
  
  ## create interactive map with leaflet
  output$map <- renderLeaflet({
    map %>%
      # add toolbox to draw polygons
      addDrawToolbar(
        targetGroup = "drawnPoly", 
        rectangleOptions = F, 
        polylineOptions = F, 
        markerOptions = F, 
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
        circleOptions = F,
        circleMarkerOptions = F,
        polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(fillColor="orange",clickable = TRUE))) %>%
        addStyleEditor()
  })
  
  latlongs <- reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  
  ## create empty reactive spdf to store drawn polygons
  value <- reactiveValues()
  value$drawnPoly <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data = data.frame(notes=character(0), stringsAsFactors = F))
  
  # fix the polygon to start another
  observeEvent(input$map_draw_new_feature, {
    
    coor <- unlist(input$map_draw_new_feature$geometry$coordinates)
    
    Longitude <- coor[seq(1,length(coor), 2)] 
    Latitude <- coor[seq(2,length(coor), 2)]
  
    isolate(latlongs$df2 <- rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly <- Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys <- Polygons(list(poly), ID=input$map_draw_new_feature$properties$`_leaflet_id`)
    spPolys <- SpatialPolygons(list(polys))
    print(spPolys)
    
    value$drawnPoly <- rbind(value$drawnPoly, SpatialPolygonsDataFrame(spPolys, data = data.frame(notes=NA, row.names = row.names(spPolys))))
    
    ## add polygons to landfills polygons df
    test <- SpatialPolygonsDataFrame(spPolys, data = data.frame(name=1:length(spPolys), row.names = row.names(spPolys)))
    test@data$area <- NA
    test@data$Notes <- NA
    test@data$location <- NA
    test@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

    #new_polygons <- rbind(landfills_polygons, test)
    ## export new & old landfills to shapefile
    #shapefile(x = new_polygons, filename = paste(dir, "/landfills/OpenLandfills_Vietnam.shp", sep = ""), overwrite = T)
    
    ## export only new landfills to shapefile, to re-run script only with new landfills (faster)
    shapefile(x = test, filename = paste(dir, "/landfills/OpenLandfills_Vietnam_new.shp", sep = ""), overwrite = T)
    
    ## run DataPreparation script to calculate data of new landfill
    source("C:/Users/carob/Documents/WWF_PlastikLeakage_Vietnam/PlasticLeakage_DataPreparation.R")
    
    
    ## import outcome of script
    variables <- readOGR(paste(dir,"/landfill_variables.gpkg", sep= ""))
    
    ## predict risk class/cluster of new landfill (without re-running clustering algorithm) ####
    # basic landfills as training data
    train <- landfills_clusters@data[,c(7:8,11,13:15)]
    # newly created landfills (from webapp) as testing data
    test <- variables@data[,c(7:8,11,13:14)]#[(length(landfills_clusters)+1):length(variables),]
    knnClust <- class::knn(train = train[,-6], test = test, k = 1, cl = train$km_cluster_unstand)
    knnClust
    
    ## add cluster as row
    #variables$km_cluster_unstand <- NA
    variables$km_cluster_unstand <- knnClust #[(length(landfills_clusters)+1):length(variables)] <- knnClust
    
    # # drop not needed columns
    # landfills$risk <- NULL
    # landfills$risk_label <- NULL
    # 
    # # combine all landfills into one spdf
    # new_variables <- rbind(landfills_clusters[,-c(16:17)], variables[(length(landfills_clusters)+1):length(variables),])
    # 
    # ## save results as shapefile
    # writeOGR(new_variables, paste(dir,"/landfill_clusters.gpkg", sep= ""), driver = "GPKG", overwrite=T)
    
    
    ## update plot upon ending draw
    observeEvent(input$map_draw_stop, {
      
      #replot it - take off the DrawToolbar to clear the features and add it back and use the values from the SPDF to plot the polygons
      leafletProxy('map') %>% 
        removeDrawToolbar(clearFeatures=TRUE) %>% removeShape('temp') %>% clearGroup('drawnPoly') %>% 
        addPolygons(data=value$drawnPoly, popup="poly", group='drawnPoly', color="blue", layerId=row.names(value$drawnPoly)) %>% 
        addDrawToolbar(
          targetGroup = "drawnPoly", 
          rectangleOptions = F, 
          polylineOptions = F, 
          markerOptions = F, 
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()), 
          circleOptions=F,
          polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F, shapeOptions=drawShapeOptions(fillColor="orange",clickable = TRUE)))
    })
    
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0)) # clear df
  
  #### TODO: plot newly added landfills & risk cluster ####
  leafletProxy("map", session) %>%
    #addPolygons(data = landfills_polygons, fill = F, weight = 2, color = "#FFFFCC") %>%
    addCircleMarkers(data = variables, color = ~cof(km_cluster_unstand), radius = sqrt(variables$area_ha)*2,
                  fillOpacity = 0.5, label = ~name, group = "Risk")
  })
  
  # create object for clicked marker (=landfill)
  observeEvent(input$map_marker_click,{
    ## click returns clickid, long & lat
    click <- input$map_marker_click
    # if(is.null(click))
    #   return()
    leafletProxy("map", session) %>% setView(lng = click$lng, lat = click$lat, zoom = 16)
  })
  
  # A reactive expression that returns the set of landfills that are in map bounds (to plot reactive graphs)
  landfillsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(landfills_clusters_sf[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(landfills_clusters_sf,
      st_coordinates(landfills_clusters_sf)[,2] >= latRng[1] & st_coordinates(landfills_clusters_sf)[,2] <= latRng[2] &
      st_coordinates(landfills_clusters_sf)[,1] >= lngRng[1] & st_coordinates(landfills_clusters_sf)[,1] <= lngRng[2])
  })

  output$histRain <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(landfillsInBounds()) == 0)
      return(NULL)
    hist(landfillsInBounds()$rain,
       main = "Weather Data",
       xlab = "Average Precipitation (mm)",
       xlim = range(landfills_clusters_sf$rain),
       col = '#00ffff',
       border = 'white')
  })
  
  output$histWind <- renderPlot({
    # If no landfills are in view, don't plot
    if (nrow(landfillsInBounds()) == 0)
      return(NULL)
    hist(landfillsInBounds()$windspeed,
       main = "",
       xlab = "Average Wind Speed (km/h)",
       xlim = range(landfills_clusters_sf$windspeed),
       col = '#00DD00',
       border = 'white')
  })
  
  output$landfills <- DT::renderDT({
    df
  })
}  

# Run the app
shinyApp(ui_inter, server_inter)


#### TODO: check if long lat lies in extent of vietnam, before running script ####


#### TODO: publish app on shinyapps.io ####
# https://shiny.rstudio.com/articles/shinyapps.html#:~:text=Shinyapps.io%20is%20an%20online,focus%20on%20writing%20great%20apps!&text=Use%20the%20tokens%20generated%20by,to%20configure%20your%20rsconnect%20package.
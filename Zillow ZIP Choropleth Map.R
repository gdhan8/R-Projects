####
# use the devtools package from CRAN to install choroplethrZip from github
install.packages("leaflet")
install.packages("dplyr")
install.packages('ZillowR')
install.packages("devtools")
library(devtools)

install_github('arilamstein/choroplethrZip@v1.5.0')

library(ZillowR)
library(choroplethrZip)
library(dplyr)
library(rgdal)
library(leaflet)

###
#Manipulate Zillow data here and join, zip, 5 year growth, 10 year growth
#https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1633024145

zhvi<-read.csv("~/Desktop/R Projects/Home Data Project/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
#RegionName == zip
#need to create 5 year growth rate, 10 year growth rate
attach(zhvi)
zhvisml<-data.frame(RegionName, State, City, Metro ,CountyName, X2011.08.31 ,X2016.08.31 , X2021.08.31)
zhvisml$fiveyrgrowth <- round((X2021.08.31 - X2016.08.31) / X2016.08.31 *100, 2)
zhvisml$tenyrgrowth <- round((X2021.08.31 - X2011.08.31) / X2011.08.31 *100, 2)
zillow_data <- zhvisml
colnames(zillow_data)[1] <-"region"
zillow_data$region <- as.character(zillow_data$region)
zillow_data <- zillow_data %>% filter(State=="IL")
###


setwd("~/Desktop/R Projects/Home Data Project")
zcta_shapefile <-  rgdal::readOGR(
  dsn = "cb_2019_us_zcta510_500k", # this is the unzipped folder
  layer = "cb_2019_us_zcta510_500k", # this is the file inside of the unzipped folder
  verbose = FALSE
)

# copy data
zcta_data <- data.table::copy(zcta_shapefile@data)

#  join zillow data - it's very important that we do NOT  duplicate or drop rows or it's going to screw up the shapefile
#zcta_data$ZCTA5CE10 <- as.numeric(zcta_data$ZCTA5CE10)
zcta_data2 <- zcta_data %>% left_join(zillow_data, by = c("ZCTA5CE10" = "region")) 

# Now reattach this data file back to the SpatialPolygonsDataFrame data slot
zcta_shapefile@data <- zcta_data2

# (optional) There are 33k ZCTAs in the US; consider reducing these to a particular region of interest.
# I'm using the `state` value that i just joined for demo purposes. You can skip this if you want to do the whole country
zcta_shapefile<-zcta_shapefile[!is.na(zcta_shapefile@data$State),]


#nrow(zcta_shapefile@data) == length(zcta_shapefile@polygons)
## [1] TRUE
#nrow(zcta_shapefile@data)

#length(zcta_shapefile@polygons)

labels <- sprintf(
  "<strong>Zip Code: %s</strong><br/> ZHVI: $%s <br/> 5 Year Pct Growth: %s <br/> 10 Year Pct Growth:  %s",
  zcta_shapefile@data$ZCTA5CE10,
  prettyNum(zcta_shapefile@data$X2021.08.31, big.mark=","),
  prettyNum(zcta_shapefile@data$fiveyrgrowth, big.mark=","),
  prettyNum(zcta_shapefile@data$tenyrgrowth, big.mark=",")
) %>% lapply(htmltools::HTML)

map_pal <-  leaflet::colorNumeric(palette="viridis", domain = zcta_shapefile@data$fiveyrgrowth, na.color="transparent")

zcta_map <- zcta_shapefile %>% 
  
  leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  
  # Check out map providers here: https://leaflet-extras.github.io/leaflet-providers/preview/
  addProviderTiles(providers$Esri.WorldStreetMap, options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = TRUE           # map won't load new tiles when panning
  )) %>%
  addTiles()%>%#"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
 
  #setView(lat = 39.286502, lng = -104.766577, zoom = 8) %>% # Colorado
  setView(lat = 39.796359, lng = -89.567207, zoom = 9) %>% # Springfield Illinois
  
  # Now add those polygons!
  addPolygons(
    fillColor = ~map_pal(fiveyrgrowth), # the map palette we made
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    stroke = TRUE,
    fillOpacity = 0.6,
    highlight = highlightOptions(
      weight = 5,
      color = "#667",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels 
  ) %>%
  
  addLegend(
    pal = map_pal,
    values =  ~fiveyrgrowth,
    opacity = 0.7,
    title = "5 Year Growth (%)"
  )

zcta_map

library(htmlwidgets)
setwd("~/Desktop/R Projects/Home Data Project")
saveWidget(widget = zcta_map,
           file = "Zillow_Home_Value_Index_Interactive_Map_IL.html",
           selfcontained = TRUE)


install.packages("leaflet")
install.packages("dplyr")
install.packages('ZillowR')
install.packages("devtools")
install.packages("sf")
install.packages("htmlwidgets")
install.packages("leaflegend")
install.packages("leafem")

library(devtools)
library(dplyr)
library(rgdal)
library(leaflet)
library(sf)
library(htmlwidgets)
library(leaflegend)
library(leafem)

###
# Connect to Zillow ECON Data API
# Completely migrate to GITHUB/Cloud for data storage
# Create an R Markdown file explaining code line by line'
# Integrate time series plots for each zipcode, displayed in html 
# Integrate crosstalk with geo-spatial and timeseries

###
#Manipulate Zillow data here and join, zip, 5 year growth, 10 year growth
#https://www.zillow.com/research/zhvi-user-guide/
#img src="https://s.zillowstatic.com/pfs/static/z-logo-default.svg" 

#Extract
url = "https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1633024145"
dest = paste0("/Users/gdhan/Documents/Data Science/R Projects/Home Data Project/", "Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month_",Sys.Date(),".csv")
download.file(url, dest)

zhvi<-read.csv(dest)
#zori<-read.csv("/Users/gdhan/Documents/Data Science/R Projects/Home Data Project/Zip_ZORI_AllHomesPlusMultifamily_SSA-1.csv")


#need to create 5 year growth rate, 10 year growth rate
#Transform
attach(zhvi)
zhvisml<-data.frame(RegionName, State, City, Metro ,CountyName, X2012.11.30 ,X2017.11.30 , X2022.11.30)
zhvisml$fiveyrgrowth <- round((X2022.11.30 - X2017.11.30) / X2017.11.30 *100, 2)
zhvisml$tenyrgrowth <- round((X2022.11.30 - X2012.11.30) / X2012.11.30 *100, 2)
zillow_data <- zhvisml
colnames(zillow_data)[1] <-"region"
zillow_data$region <- as.character(zillow_data$region)
zillow_data <- zillow_data %>% filter(State %in% c("Va", "VA"))

######
#attach(zori)
#zorisml<-data.frame(RegionName, State, City, X2016.08 , X2021.08)
#zorisml$fiveyrgrowth <- round((X2021.08 - X2016.08) / X2016.08 *100, 2)
#zori_data <- zorisml
#colnames(zori_data)[1] <-"region"
#zori_data$region <- as.character(zori_data$region)


#ptr_ratio_data <- zillow_data %>% 
                  #left_join(zori_data, by = c("region" = "region"))
#ptr_ratio_data <- ptr_ratio_data %>% filter(is.na(ptr_ratio_data$fiveyrgrowth.y)==F)
#ptr_ratio_data$ptr.ratio.16 <- ptr_ratio_data$X2016.08.31 / ptr_ratio_data$X2016.08
#ptr_ratio_data$ptr.ratio.21 <- ptr_ratio_data$X2021.08.31 / ptr_ratio_data$X2021.08
#ptr_ratio_data$ptr.fiveyr.chg <- ptr_ratio_data$ptr.ratio.21 - ptr_ratio_data$ptr.ratio.16

###


setwd("/Users/gdhan/Documents/Data Science/R Projects/Home Data Project/")

#Prepare shape file, The function reads an OGR data source and layer into a suitable Spatial vector object.
zipcode_shapefile <-  rgdal::readOGR(
  dsn = "tl_2020_us_zcta520", # this is the unzipped folder
  layer = "tl_2020_us_zcta520", # this is the file inside of the unzipped folder
  verbose = FALSE
)

# copy data
zc_data <- data.table::copy(zipcode_shapefile@data)

#  join zillow data - it's very important that we do NOT  duplicate or drop rows or it's going to mess up the shapefile
#zcta_data$ZCTA5CE10 <- as.numeric(zcta_data$ZCTA5CE10)
zillow_zc_data <- zc_data %>% left_join(zillow_data, by = c("ZCTA5CE20" = "region")) 

# Now reattach this data file back to the SpatialPolygonsDataFrame data slot
zipcode_shapefile@data <- zillow_zc_data

# (optional) There are 33k ZCTAs in the US; consider reducing these to a particular region of interest.
#handle all of the na in the shapefile
zipcode_shapefile<-zipcode_shapefile[!is.na(zipcode_shapefile@data$State),]

#create labels for the interactive graphic
labels <- sprintf(
  "<strong>Zip Code: %s</strong><br/> ZHVI: $%s <br/> 5 Year Pct Growth: %s <br/> 10 Year Pct Growth:  %s",
  zipcode_shapefile@data$ZCTA5CE20,
  prettyNum(zipcode_shapefile@data$X2022.11.30, big.mark=","),
  prettyNum(zipcode_shapefile@data$fiveyrgrowth, big.mark=","),
  prettyNum(zipcode_shapefile@data$tenyrgrowth, big.mark=",")
) %>% lapply(htmltools::HTML)

#create color pallete for the choropleth map and legend
my_map_pal <-  leaflet::colorNumeric(palette="viridis", domain = zipcode_shapefile@data$fiveyrgrowth, na.color="transparent")

#write filtered data and shape files to enable upload under GITHUB size constraints. 
#write.csv(zillow_data, file = "zillow_data_lite.csv")
#st_write(zcta_shapefile, "zcta_shapefile.shp")


zhvi_zc_map <- zipcode_shapefile %>% 
  
  leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  
  addProviderTiles(#####
    providers$Esri, 
    options = providerTileOptions(
    updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
    updateWhenIdle = TRUE ) # map won't load new tiles when panning)) 
    )%>%
  
  addTiles(#####
           )%>%#"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
 
  setView(#####
          #lat = 39.742160, lng = -104.967298, zoom = 9) %>% # Denver, Colorado
          #lat = 39.796359, lng = -89.567207, zoom = 9) %>% # Springfield Illinois
          lat = 38.7509488, lng = -77.4752667, zoom = 9) %>% # Charlottesville, Va
  
  addPolygons(#####
    fillColor = ~my_map_pal(fiveyrgrowth), # the map palette we made
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    stroke = TRUE,
    fillOpacity = 0.5,
    highlight = highlightOptions(
    weight = 5,
    color = "#667",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
    label = labels, 
    group = "5 Year Growth"
    
  ) %>%
  #addLayersControl(#####
    #overlayGroups = c("5 Year Growth"),
    #options = layersControlOptions(collapsed = FALSE)
  #) %>%
  addMiniMap(#####
  position = "bottomleft",
  width = 200,
  height = 200,
  collapsedWidth = 19,
  collapsedHeight = 19,
  zoomLevelOffset = -5,
  zoomLevelFixed = FALSE,
  centerFixed = FALSE,
  zoomAnimation = FALSE,
  toggleDisplay = FALSE,
  autoToggleDisplay = FALSE,
  minimized = FALSE,
  aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
  shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
                             0, fillOpacity = 0),
  strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
  tiles = NULL,
  mapOptions = list()
) %>%
  
  addLegendNumeric(#####
    pal = my_map_pal,
    values = zipcode_shapefile@data$fiveyrgrowth,
    position = 'bottomright',
    title = '5 Year Growth (%)',
    fillOpacity = .7,
    orientation = 'horizontal',
    bins = 5,
    width = 300,
    height = 15,
    numberFormat = function(x) {prettyNum(x, big.mark = ',',
                                          scientific = FALSE, digits = 2)})
  #addLogo("https://s.zillowstatic.com/pfs/static/z-logo-default.svg" , 
          #url = "https://s.zillowstatic.com/pfs/static/",  
          #width = 152,
          #height = 32)

  

#####
zhvi_zc_map

setwd("/Users/gdhan/Documents/Data Science/R Projects/Home Data Project/")
saveWidget(widget = zhvi_zc_map,
           file = "Zillow_Home_Value_Index_Interactive_Map_VA.html",
           selfcontained = TRUE)


library(rgdal)
library(raster)
library(rgeos)

#####polygonen bwk####
HT2190 <- readOGR(".",'habitattype2190')
CRS.lam72 <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs")
proj4string(HT2190) <- CRS.lam72 # definiëren dat coordinaat lam72 is
HT2190 <- spTransform(HT2190,CRS("+init=epsg:4326")) #converteren naar WGS84

center<- data.frame(gCentroid(HT2190, byid = TRUE))

##Pink####
PINK <- readOGR(".",'PQ_PINK_WH')
CRS.lam72 <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs")
proj4string(PINK) <- CRS.lam72 # definiëren dat coordinaat lam72 is
PINK <- spTransform(PINK,CRS("+init=epsg:4326")) #converteren naar WGS84

centerp<- data.frame(gCentroid(PINK, byid = TRUE))


#Watina####
watina<- readOGR(".",'watina')
CRS.lam72 <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs")
proj4string(watina) <- CRS.lam72 # definiëren dat coordinaat lam72 is
watina<- spTransform(watina,CRS("+init=epsg:4326")) #converteren naar WGS84

centerw<- data.frame(gCentroid(watina, byid = TRUE))
centerw$Meetpunt<- watina$MeetpuntCo
punt1<- subset(centerw, Meetpunt == "WESP141")
punt2<- subset(centerw, Meetpunt == "WESP009")
punt3<- subset(centerw, Meetpunt == "WESP034")
kliveg_punten<-rbind(punt1,punt2,punt3)
#####







leafIcon <- makeIcon(iconUrl="http://globetrotterlife.org/blog/wp-content/uploads/leaflet-maps-marker-icons/letter_p.png",
  iconWidth = 20, iconHeight = 20)

wat <- makeIcon(iconUrl="http://globetrotterlife.org/blog/wp-content/uploads/leaflet-maps-marker-icons/circle-stroked-12.png",
                     iconWidth = 10, iconHeight = 10)

  
kliveg <- makeIcon(iconUrl="http://globetrotterlife.org/blog/wp-content/uploads/leaflet-maps-marker-icons/letter_k.png",
                iconWidth = 40, iconHeight = 40)

####leaflet####
library("leaflet")
leaflet()  %>% addProviderTiles("Esri.WorldImagery", group="Openstreetmap") %>% 
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  addPolygons(data=HT2190,weight=1,col="yellow",popup = HT2190$Habitat,group="HT2190")%>%
  addMarkers(lng = centerp$x, lat = centerp$y,icon=leafIcon,clusterOptions = markerClusterOptions(),popup=PINK$Code,group="Meetnet PINK")%>% 
  addCircleMarkers(data=centerw,lng = centerw$x, lat = centerw$y,color="blue",stroke=TRUE, fillOpacity=0,group = "Meetpunten Watina")%>%
  addMarkers(lng = kliveg_punten$x, lat = kliveg_punten$y,icon=kliveg,popup=kliveg_punten$Meetpunt,group= "kliveg-sites")%>% 
  addLayersControl(overlayGroups = c("HT2190","Meetnet PINK","Meetpunten Watina","kliveg-sites") , baseGroups = c("Openstreetmap","Esri.WorldImagery"), options = layersControlOptions(collapsed = FALSE))
  
             
             
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = PINK$Vegetatie,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = F))


addLegend(pal = pal, values = centers$OBJECTnr, opacity = 0.7, title = NULL,
          position = "bottomright")

addLabelOnlyMarkers(data = center_all,
                    lng = ~x, lat = ~y, label = ~OBJECT,
                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))

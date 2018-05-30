library(leaflet)
####voorbeeld####
x <- punten$X__Lam72_
y <- punten$Y__Lam72_

# Define the coordinate systems.
#
library(rgdal)
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")

#CRS lam72 aanmaken bron (http://epsg.io/31370)
CRS.lam72 <- CRS("+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.869,52.2978,-103.724,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs")
proj4string(d) <- CRS.new # definiëren dat coordinaat lam72 is
convWGS84 <- spTransform(d,CRS("+init=epsg:4326")) #converteren naar WGS84

# Plot the results.
#
par(mfrow=c(1,3))
plot.default(x,y, main="Raw data", cex.axis=.95)
plot(d, axes=TRUE, main="Original lat-lon", cex.axis=.95)
plot(convWGS84, axes=TRUE, main="Projected", cex.axis=.95)
unclass(convWGS84)

## Plot in Leaflet 

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=convWGS84$lon, lat=convWGS84$lat, popup="The birthplace of R")
m  # Print the map

leaflet() %>% addTiles() %>%setView(4, 51, zoom = 10) %>%
  addWMSTiles("https://geoservices.informatievlaanderen.be/raadpleegdiensten/DHMV/wms",
    layers = "DHMV",
    options = WMSTileOptions(format = "image/bmp", transparent = TRUE),
    attribution = "")




####Kliveg sites####

x <- KLIVEG_profielputten$WGS84Longitude
y <- KLIVEG_profielputten$WGS84Latitude

# Define the coordinate systems.
#
library(rgdal)
d <- data.frame(lon=x, lat=y)
coordinates(d) <- c("lon", "lat")


## Plot in Leaflet 
names(providers)

#
m <- leaflet() %>%
  addTiles() %>% 
  # Add default OpenStreetMap map tiles
  addMarkers(lng=KLIVEG_profielputten$WGS84Longitude, lat=KLIVEG_profielputten$WGS84Latitude, label = KLIVEG_profielputten$ProfileId)
m  # Print the map

m %>%addProviderTiles(providers$Esri.WorldImagery)
m


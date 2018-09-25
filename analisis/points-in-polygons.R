# This scripts read a listing file and writes a new file with the location of the point (barrio)
# count points in polygons: https://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r#110246
# read GeoJSON in R https://stackoverflow.com/questions/24183007/is-it-possible-to-read-geojson-or-topojson-file-in-r-to-draw-a-choropleth-map

# Loads libraries ----
library("raster")
library("sp")
# restart R .rs.restartR() if rgdal does not work
library(rgdal)

# check ogr drivers to see if GeoJSON is loaded:
# ogrDrivers()

# read files ----

# shapes
barrios <- readOGR("data/original/contornos/barrios_geo.json")
distritos <- readOGR("data/original/contornos/distritos_geo.json")
# class(distritos) # checks that is patialPolygonsDataFrame
# plot(distritos) # plots the shape map
# proj4string(distritos) # check CRS

# Use this for municipios
# shape of municipalities from IGN
municipios <- readOGR("data/original/contornos/recintos_municipales_inspire_peninbal_wgs84_export_provincia-barcelona_geo.json")
barrios <- municipios

# points
# airbnb <- read.delim("data/original/airbnb/180619/barcelona_airbnb_datahippo.csv",sep = ",")
# airbnb <- read.delim("data/original/airbnb/180619/barcelona_homeaw_datahippo.csv",sep = ",")
airbnb <- read.delim("data/original/airbnb/180619/barcelona_provincia_airbnb_datahippo.csv",sep = ",")
# airbnb <- read.delim("data/original/airbnb/180818/listings_summary_barcelona_insideairbnb.csv",sep = ",")

# Prepares and counts ----

## Get long and lat from your data.frame. Make sure that the order is in lon/lat.
# source: https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates#29736844
xy <- airbnb[,c("longitude","latitude")]
airbnbSp <- SpatialPointsDataFrame(coords = xy, data = airbnb, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# class(airbnbSp)
# proj4string(airbnbSp)

# serves to count points (people in Padrón) in polygon (barrios)
# retrieve  the  geometry  indices  of Barrios at  the  locations  of A (people points).  
# More  in particular, an integer vector of length length(A) is returned, with NA values for locations in 
# A not matching with locations in B (e.g.  those points outside a set of polygons).
# (https://cran.r-project.org/web/packages/sp/vignettes/over.pdf) 
countBarrios <- over(airbnbSp, barrios)
# table(countBarrios$BAR_DS_O)

# same calc for distritos
countdistritos <- over(airbnbSp, distritos)
# table(countdistritos$Unidad_men)

# Adds calculated barrios to spatial poligon
airbnbSp$barrio <- countBarrios$N_Barri
airbnbSp$barrio <- countBarrios$NAMEUNIT # para municipios
airbnbSp$distrito <- countdistritos$N_Distri

# Maps: Where are those points without barrio ----
library(ggmap)
# Quedan fuera todos los anuncios que corrresponden a barcos del puerto olímpico!!!
qmplot(longitude, latitude, data = airbnb[is.na(airbnbSp$barrio =="no location"),], maptype = "toner-lite", 
       color = I("red"),alpha = I(.2)) + labs(title= "Points without barrio" )
# cuántos sin barrio?
nrow(airbnb[is.na(airbnbSp$barrio =="no location"),])
# plot todos los anuncios en mapa
qmplot(longitude, latitude, data = airbnb, maptype = "toner-lite", 
       color = I("red"),alpha = I(.2)) + labs(title= "Points with barrio" )

# Continues transformation ----

airbnb <- as.data.frame(airbnbSp) # convert spatial data to regular data frame

# removes duplicated columns with lat and long
drops <- c("latitude.1","longitude.1") 
airbnb <- airbnb[ , !(names(airbnb) %in% drops)]

# Some points will be outside polygons and have Barrio variable fixed
airbnb[is.na(airbnbSp$barrio),]$name

# There are n points that have no Barrio assigned
length(airbnb[is.na(airbnbSp$barrio),]$name)

# library(dplyr)
# airbnb$barrio <- lapply(airbnb$barrio, as.character)
# airbnb$distrito <- lapply(airbnb$distrito, as.character)

# Assign "no location" to points
# airbnb[is.na(airbnbSp$barrio),]$barrio <- "no location"
# airbnb[is.na(airbnbSp$menores),]$distrito <- "no location"
# airbnb[airbnb$distrito == "Sag�es" ,]$distrito <- "Sagües"


# Para municipios ----
# Cambia nombre de variable "Barrio" por "Municipio"
names(airbnb)[14] <- "municipio"

# saves file -----
# save(airbnb,file="data/output/180423_listings-airbnb-donostia_datahippo_barrio-umenor.Rda")
write.csv(airbnb, file = "data/output/airbnb/180619_listings-airbnb-provincia-barcelona_datahippo_municipio.csv", row.names = FALSE)
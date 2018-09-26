# Análisis de Airbnb en Donostia
# -Habitaciones vs Viviendas completas
# -numero anuncios por barrio
# -ratio anuncios por barrio por vivienda
# -numero plazas
# -ratio plazas por habitante

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(rgdal)


# ------ Load files ----------
# Load shapes
barrios <- readOGR("data/original/contornos/barrios_geo.json")
distritos <- readOGR("data/original/contornos/distritos_geo.json")
# mar <- readOGR("data/original/shapes/mar-donostia.geojson")

viviendas_barrios <- read.delim("data/original/demografia-vivienda/habitantes-viviendas-por-barrios_padron2018_ayto-barcelona.csv",sep = ",")

# Airbnb listings 2018-09-11
airbnb201809 <- read.delim("data/original/airbnb/180911/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# Translate room type
levels(airbnb201809$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")


barrios_n_listings <- as.data.frame(table(airbnb201809$neighbourhood))
names(barrios_n_listings) <- c("barrio","n_listings")
write.csv(as.data.frame(barrios_n_listings), file = "data/output/airbnb/180911/barrios-n-listings-airbnb-barcelona-insideairbnb.csv", row.names = FALSE)
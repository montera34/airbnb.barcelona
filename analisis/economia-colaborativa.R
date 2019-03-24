# ¿es colaborativa?

# Load libraries ----
library(tidyverse)

listingssept  <- read.delim("data/original/airbnb/150904/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# listingssept  <- read.delim("data/original/airbnb/180911/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# Translate room type values
levels(listingssept$room_type) <- c("Piso completo","Habitación","Habitación compartida")

table(listingssept$room_type,listingssept$calculated_host_listings_count)

listingssept$host.type <- ""
listingssept[listingssept$calculated_host_listings_count == 1,]$host.type <- "1 anuncio"
listingssept[listingssept$calculated_host_listings_count == 2,]$host.type <- "2 anuncios"
listingssept[listingssept$calculated_host_listings_count > 2 & listingssept$calculated_host_listings_count < 6,]$host.type <- "3-5 anuncios"
listingssept[listingssept$calculated_host_listings_count > 5 & listingssept$calculated_host_listings_count < 15,]$host.type <- "6-14 anuncios"
listingssept[listingssept$calculated_host_listings_count > 14,]$host.type <- "15 o más anuncios"

table(listingssept$room_type,listingssept$host.type)

listingssept$host.type.m <- ""
listingssept[listingssept$calculated_host_listings_count == 1,]$host.type.m <- "1 anuncio"
listingssept[listingssept$calculated_host_listings_count > 1,]$host.type.m <- "varios anuncios"

test1 <- as.data.frame.matrix(table(listingssept$room_type,listingssept$host.type.m))
test1$un.anuncio <- round(test1$`1 anuncio`/nrow(listingssept)*100, digits = 1)
test1$varios.anuncioa <- round(test1$`varios anuncios`/nrow(listingssept)*100, digits = 1)
test1

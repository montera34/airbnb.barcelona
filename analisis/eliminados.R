# Este script analiza las diferencias entre los diferentes listings de una localización de InsideAirbnb
# Usa los archivos listings-summary de Insideairbnb 

# Load libraries ----
library(gsubfn)
library(tidyverse)
# extends color paletter
library(RColorBrewer)
library("reshape2")
library(ggthemes) #install ggthemes
library(ggrepel) # for geom_text_repel to prevent overlapping

# ------ Get dates when data were scraped --------
# Loads dates with listings data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180818","181010","181107","181210","190114","190206","190308")

# reverse dates to get the most recent data in the loop
dates <- rev(dates)

# Load data ------------------------------------------------------------------------

# loop starts loading the first set of listings
listings  <- select(as.data.frame(read.delim("data/original/airbnb/190308/listings_summary_barcelona_insideairbnb.csv",sep = ",")),
                    id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)

listings.total <- listings

# Loop para ir insertando todos los listings en un dataframe
# El objetivo es tener una lista con todos los id de los anuncios y algunas características
for (i in 1:length(dates)) {
  print("listings totales: ")
  print(nrow(listings.total))
  print("------")
  print(paste("id:",i))
  print(paste("fecha: ",dates[i]))
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),
                id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  # names(neo) <- "id"
  print(paste("neo a leer",nrow(neo)))
  # TODO. Solamente inserta los que no estaban ya. Si el calculated_host_listings_count ha cambiado (el usuario tuvo menos o más anuncios) no lo actualizaría
  # Se podría guardar el host_id y luego mirar si ha tenido más de una anuncio en algún momento
  neo2 <- select(as.data.frame(neo[!neo$id %in% listings.total$id,]),
                 id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  neo_reviews <- select(as.data.frame(neo[neo$id %in% listings.total$id,]),
                        id,number_of_reviews)
  # names(neo2) <- "id"
  print(paste("neo2 a insertar",nrow(neo2)))
  # añade los anuncios al listado original
  listings.total <- rbind(listings.total,neo2)
  }

# counts whether a listing exists in a scraping
# each row is one listings. each column is one date when scraping was made by insideairbnb
# Note: it will insert the last update data from each listing.
for (i in 1:length(dates)) {
  print(i)
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),
                id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  # names(neo) <- "id"
  print(paste("n listings:",nrow(neo)))
  # sets to 0 for all
  listings.total[,paste("d",dates[i],sep="")] <- 0
  # sets to 1 if listing is in that date
  listings.total[ listings.total$id %in% neo$id, paste("d",dates[i],sep="") ] <- 1
}
remove("neo","neo2")

# Translate room type values
levels(listings.total$room_type) <- c("Piso completo","Habitación","Habitación compartida")

# createss long format directly ------------------------------------------------------------------------------
# creates dataframe
todos <- ""
# loop to insert every listing in every scraping in a row and add its scraping date
for (i in 1:length(dates)) {
  print(i)
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),
                id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  neo$date <- paste(dates[i],sep="")
  todos <- rbind(todos,neo)
}

# removes first empty line
todos <- todos[-1,]
head(todos)

# Translate room type values
levels(todos$room_type) <- c("Piso completo","Habitación","Habitación compartida")
# converts to integer
todos$calculated_host_listings_count <- as.integer(todos$calculated_host_listings_count)
todos$number_of_reviews <- as.integer(todos$number_of_reviews)
todos$availability_365 <- as.integer(todos$availability_365)

# saves the file in data_long dataframe, which was the previoues way of doing this
data_long <- todos
# data_long$fechab <- strapplyc( as.character(data_long$date), "([0-9]*)", simplify = TRUE)
data_long$fechab <- as.Date( paste(20,as.character(data_long$date),sep=""), "%Y%m%d")

# table(data_long$calculated_host_listings_count)

# converts to long format deprecated method, used later TODO document) ------------------------------------------------------------------------------
data_long2 <- listings.total %>% gather(fecha, exists, 8:39) #starts in 8th column after other variables
# parse date: extract date from variable and parse it. Adds "20" to have full year.
data_long2$fechab <- strapplyc( as.character(data_long2$fecha), "d([0-9]*)", simplify = TRUE)
data_long2$fechab <- as.Date( paste(20,as.character(data_long2$fechab),sep=""), "%Y%m%d")

# classify by type of host ----------------------------------------------------------------------------
data_long$host.type <- ""
data_long[data_long$calculated_host_listings_count == 1,]$host.type <- "1 anuncio"
data_long[data_long$calculated_host_listings_count > 1,]$host.type <- "varios anuncios"

# data_long$host.type.m <- ""
# data_long[data_long$calculated_host_listings_count == 1,]$host.type.m <- "1 anuncio"
# data_long[data_long$calculated_host_listings_count == 2,]$host.type.m <- "2 anuncios"
# data_long[data_long$calculated_host_listings_count == 3,]$host.type.m <- "3 anuncios"
# data_long[data_long$calculated_host_listings_count == 4,]$host.type.m <- "4 anuncios"
# data_long[data_long$calculated_host_listings_count > 4,]$host.type.m <- "5 o más anuncios"

data_long$host.type.m <- ""
data_long[data_long$calculated_host_listings_count == 1,]$host.type.m <- "1 anuncio"
data_long[data_long$calculated_host_listings_count == 2,]$host.type.m <- "2 anuncios"
data_long[data_long$calculated_host_listings_count > 2 & data_long$calculated_host_listings_count < 6,]$host.type.m <- "3-5 anuncios"
data_long[data_long$calculated_host_listings_count > 5 & data_long$calculated_host_listings_count < 15,]$host.type.m <- "6-14 anuncios"
data_long[data_long$calculated_host_listings_count > 14,]$host.type.m <- "15 o más anuncios"

# review type -------------------------
data_long$reviews.type <- ""
data_long[data_long$number_of_reviews == 0 & !is.na(data_long$availability_365) ,]$reviews.type <- "ninguna review"
data_long[data_long$number_of_reviews > 0 & !is.na(data_long$availability_365),]$reviews.type <- "1 o más reviews"
# data_long[data_long$number_of_reviews ==1,]$reviews.type <- "1"
# data_long[data_long$number_of_reviews > 1,]$reviews.type <- "2 o más"

# availability type -------------------------
data_long$availability.type <- ""
data_long[data_long$availability_365 == 0 & !is.na(data_long$availability_365),]$availability.type <- "ningún día"
data_long[data_long$availability_365 > 0 & data_long$availability_365 <31 & !is.na(data_long$availability_365),]$availability.type <- "0-30 días"
data_long[data_long$availability_365 > 30 & data_long$availability_365 <91 & !is.na(data_long$availability_365),]$availability.type <- "31-90 días"
data_long[data_long$availability_365 > 90 & data_long$availability_365 <181 & !is.na(data_long$availability_365),]$availability.type <- "91-180 días"
data_long[data_long$availability_365 > 180 & !is.na(data_long$availability_365),]$availability.type <- "180 o más días"

data_long$availability.type.s <- ""
data_long[data_long$availability_365 == 0 & !is.na(data_long$availability_365),]$availability.type.s <- "ningún día"
data_long[data_long$availability_365 > 0 & !is.na(data_long$availability_365),]$availability.type.s <- "con alguna disponibilidad"

# roomtype simplified
data_long$room_type.s <- data_long$room_type
data_long[data_long$room_type == "Habitación compartida", ]$room_type.s <- "Habitación"


# counts listings per scraping date ----------------------------------------------------------------------------
dates.count <- data_long %>% group_by(fechab) %>% summarise(anuncios=n())
# creates fake df
df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
# plot
png(filename=paste("images/airbnb/eliminados/anuncios-barcelona-por-mes-linea.png", sep = ""),width = 1000,height = 400)
dates.count  %>%
  ggplot(aes(fechab,anuncios)) + 
  geom_line(size=1.5) +
  # geom_line(aes(fechab,anuncios))
  geom_point(size=2.5,color="#BB3300") +
  geom_text(data=filter(dates.count,fechab > max(fechab-1)),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE),limits = c(0, max(dates.count$anuncios))) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada descarga de datos de InsideAirbnb",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # nota
  annotate(geom = "text", x = as.Date("2017-01-1"), y = 11500, label = "Cada punto es un scraping de InsideAirbnb", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  # annotate(geom = "segment", x = as.Date("2017-01-1"), xend = as.Date("2017-04-1"), y = 12000, yend = 17200,
  # color="#999999") +
  geom_curve(aes(x = as.Date("2017-01-01"), y = 12000, xend = as.Date("2017-04-08"), yend = 17200), 
             color="#999999", data =df,  curvature = 0.2)
dev.off()

# filter by number of reviews ---------------------------------
dates.count.active <- data_long %>% group_by(fechab,reviews.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-barcelona-reviews-por-mes-linea-compara.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  geom_line(data=dates.count.active %>% filter(reviews.type=="1 o más reviews"),
            aes(fechab,anuncios,color=reviews.type),size=1.5) +
  geom_point(data=dates.count.active %>% filter(reviews.type=="1 o más reviews"),
             aes(fechab,anuncios,color=reviews.type),size=2.5) +
  geom_line(data=dates.count,
            aes(fechab,anuncios),size=1.5) +
  annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(limits=c(0, max(dates.count$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(data=filter(dates.count,fechab > as.Date("2019-03-01")),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  geom_text(data=filter(dates.count.active,fechab > as.Date("2019-03-01") & reviews.type=="1 o más reviews"),
            aes(fechab+10,anuncios,label=format(anuncios, nsmall=1, big.mark=".")), size=4,hjust=0) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios publicados vs con alguna review",
       subtitle = "Airbnb. Barcelona 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color = "Según nº reviews" ) 
dev.off()

# filter by number of reviews and availability ---------------------------------
dates.count.reviews.availablity <- data_long %>% group_by(fechab,reviews.type,availability.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-barcelona-reviews-por-mes-linea-multi.png", sep = ""),width = 1000,height = 400)
ggplot(NULL) + 
  geom_line(data=filter(dates.count.reviews.availablity, !reviews.type ==""),
            aes(fechab,anuncios,color=availability.type),size=1.5) +
  scale_color_brewer(palette="PuBu") +
  scale_y_continuous(limits=c(0, max(dates.count.reviews.availablity$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb según número de reviews y disponibilidad",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color = "Según días disponbles" ) +
  facet_wrap(~reviews.type)
dev.off()

# counts listings por barrio --------------------------------------------------------------------
# creates extended color palette https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
colourCount <- length(unique(data_long$neighbourhood))
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))

dates.count.barrio <- data_long %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())
# dates.count.barrio2 <- data_long2 %>% filter (exists ==1) %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio.png", sep = ""),width = 1000,height = 700)
# plot_a <- dates.count.barrio %>% 
dates.count.barrio %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=0.5) +
  scale_color_manual(values = getPalette(colourCount)) +
  annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio,fechab==as.Date("2019-03-08"),anuncios>200), 
            aes(
              fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
              nudge_x = 35, # adjust the starting y position of the text label
              size=4,
              hjust=0,
              family = "Roboto Condensed",
              direction="y",
              segment.size = 0.2,
              segment.color="#333333"
            ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 18) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio en Barcelona",
       subtitle = "2015 - marzo 2019 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# export data in the line chart to use in external visualization
# eeee <- plot_a$data
# spread <- eeee %>% select(-.group) %>% spread(neighbourhood,anuncios)
# write.csv(spread, file = "tmp/anucios-barrio-evol.csv", row.names = FALSE)
# write.csv(eeee, file = "tmp/anucios-barrio-evol-long.csv", row.names = FALSE)


# counts listings por barrio y room type--------------------------------------------------------------
dates.count.barrio.room <- data_long %>% group_by(fechab,neighbourhood,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-room.png", sep = ""),width = 1300,height = 700)
dates.count.barrio.room %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=1300,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
                  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date(
    date_breaks = "1 year",
    limits = c(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-01-4")),
    date_labels = "%Y"
    ) +
  # xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio y tipo de alojamiento en Barcelona",
       subtitle = "2015- marzo 2019 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~room_type.s)
dev.off()

# counts listings por barrio y room type y reviews--------------------------------------------------------------
dates.count.barrio.room.reviews <- data_long %>% group_by(fechab,neighbourhood,room_type.s,reviews.type) %>% 
  summarise(anuncios=n()) %>% filter(!reviews.type=="")

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-room-reviews.png", sep = ""),width = 1100,height = 800)
dates.count.barrio.room.reviews %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=850,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room.reviews,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
                  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.room.reviews$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.room.reviews$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Tipo alojamiento vs nº reviews por barrio en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(reviews.type~room_type.s)
dev.off()

# counts listings por barrio y host type--------------------------------------------------------------
dates.count.barrio.host <- data_long %>% group_by(fechab,neighbourhood,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-host.png", sep = ""),width = 1000,height = 600)
dates.count.barrio.host %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=0.5) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.host,fechab==as.Date("2019-03-08"),anuncios>300), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  # xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
                  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.host$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio y tipo de host en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()

# counts listings por barrio y host type y reviews--------------------------------------------------------------
dates.count.barrio.host.reviews <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood,host.type,reviews.type) %>% 
  summarise(anuncios=n()) %>% filter(!reviews.type=="")

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-host-reviews.png", sep = ""),width = 1000,height = 600)
dates.count.barrio.host.reviews %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=0.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.host.reviews,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood)),
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.colour="grey") +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.host.reviews$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio, tipo de host y nº reviews en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(reviews.type~host.type)
dev.off()

# counts listings por distrito --------------------------------------------------------------------
dates.count.distrito <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-distrito.png", sep = ""),width = 1000,height = 600)
dates.count.distrito %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito,fechab==as.Date("2018-09-11")), 
            aes(fechab+5,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito$anuncios)) +
  xlim(as.Date(min(dates.count.distrito$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# counts listings por distrito y room type--------------------------------------------------------------
dates.count.distrito.room <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group,room_type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-distrito-room.png", sep = ""),width = 1000,height = 600)
dates.count.distrito.room %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito.room,fechab==as.Date("2018-09-11"),!room_type=="Habitación compartida"), 
            aes(fechab+7,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito.room$anuncios)) +
  xlim(as.Date(min(dates.count.distrito.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de alojamiento en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~room_type)
dev.off()

# counts listings por distrito y host type--------------------------------------------------------------
dates.count.distrito.host <- data_long %>% filter (exists ==1) %>% group_by(fechab,neighbourhood_group,host.type) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-distrito-host.png", sep = ""),width = 1000,height = 600)
dates.count.distrito.host %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  geom_point(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group,color=neighbourhood_group),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # distritos labels
  geom_text(data=filter(dates.count.distrito.host,fechab==as.Date("2018-09-11")), 
            aes(fechab+7,anuncios,label=neighbourhood_group),
            size=4,
            hjust=0,
            family = "Roboto Condensed") +
  ylim(0, max(dates.count.distrito.host$anuncios)) +
  xlim(as.Date(min(dates.count.distrito.host$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito y tipo de host en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()

# counts listings per scraping date and room type --------------------------------------------------------------------
dates.count.room_type <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())

# todos los anuncios
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes.png", sep = ""),width = 1000,height = 200)
dates.count.room_type %>%
  ggplot(aes(fechab,anuncios)) + 
  geom_col() +
  annotate("text",x=as.Date("2018-05-15"),y=21000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de alojamiento ----
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.room_type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-04-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_point(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  geom_line(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.room_type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de alojamiento en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# anuncios de pisos completos
dates.count.room_type %>% filter(room_type=="Vivienda completa") %>%
  ggplot () + 
  geom_col(aes(fechab,anuncios))

# separado por tipo de host ----
dates.count.host.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.type %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de host com más clasificaciones ----
dates.count.host.type.m <- data_long %>% filter (exists ==1) %>% group_by(fechab,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-type-1-2-3-more.png", sep = ""),width = 1000,height = 300)
dates.count.host.type.m %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  geom_point(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.type.m$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios de Airbnb por tipo de host en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona")
dev.off()

# separado por tipo de host y alojamiento ----
dates.count.host.room.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4,base_family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.room.type$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~room_type)
dev.off()

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-room-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~host.type)
dev.off()


# separado por tipo de host multiple y alojamiento -----------------------------------
dates.count.host.room.type.m <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,host.type.m) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-m-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type.m %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  geom_line(aes(fechab,anuncios,group=host.type.m,color=host.type.m),size=1.5) +
  annotate("text",x=as.Date("2018-05-15"),y=100,label="acuerdo",color="#000000",size=4, hjust = 1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count.host.room.type.m$anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~room_type)
dev.off()

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-room-host-m-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type.m %>% filter(!room_type=="Habitación compartida") %>%
  ggplot () +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_line(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  annotate("text",x=as.Date("2018-05-15"),y=100,label="acuerdo",color="#000000",size=4, hjust = 1 ) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de host",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="El host gestiona") +
  facet_wrap(~host.type.m)
dev.off()

# data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",] %>%
# ggplot(aes(x = as.factor(fecha), y = as.factor(id))) +
#   # geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",alpha=0.3,size=0.1)
#   geom_point(size=0.1,alpha=0.8)


# cada anuncio es una línea. eje y nº reviews-----------------
png(filename=paste("images/airbnb/numero-review-anuncio-201903.png", sep = ""),width = 1300,height = 700)
data_long %>% filter(neighbourhood == "la Dreta de l'Eixample" ) %>%
  ggplot () +
  # annotate("text",x=as.Date("2018-05-25"),y=1300,label="acuerdo",color="#000000",
  #          size=5,family = "Roboto Condensed",hjust=1) +
  geom_line(aes(fechab,number_of_reviews, group = id),size=0.06,alpha=0.5) +
  geom_point(aes(fechab,number_of_reviews),size=0.03,alpha=0.2) +
  # scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_date(
    date_breaks = "1 year",
    # limits = c(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-01-4")),
    date_labels = "%Y"
  ) +
  # ylim(0,5) +
  # xlim(as.Date(min(dates.count.barrio.room$fechab)),as.Date("2021-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(color = "#000000"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de reviews por anuncio en Barcelona",
       subtitle = "la Dreta de l'Eixample. 2015- marzo 2019",
       y = "número de reviews",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(room_type.s~host.type)
dev.off()

table(data_long$neighbourhood)
# detects which listings were erased/dissapeared -------------------------------------------------------------------------------
listings.desaparecidos <- listings.total
# sets all values to 0
listings.desaparecidos[,8:39] <- 0

# column.select <- paste("d",dates[1],sep = "")
# column.select2 <- paste("d",dates[1+1],sep = "")
# tesx <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
# 
# listings.desaparecidos[listings.desaparecidos$id %in% tesx$id,column.select2] <-33

for (i in 1:length(dates)) {
  print(i)
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  column.select2 <- paste("d",dates[i+1],sep = "")
  print(column.select2)
  # debe cumplir condición: está en columna 1 pero no está en 2
  id.removed <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
  # sets to 1 if erased and stores it in column.select2 date
  listings.desaparecidos[listings.desaparecidos$id %in% id.removed$id,column.select2] <- 1
}

# cuántas veces "desapareció" cada anuncios en el periodo estudiado -------
listings.desaparecidos$sum <- rowSums(listings.desaparecidos[,8:39])
table(listings.desaparecidos$sum)
# De los 63.704 anuncios que han pasado por la plataforma 15.147 (24%) siempre han estado presentes desde que se publicaron alguna vez. 
# 39.389 (62%) desaparecieron una vez. 
# 6.843 (10%) desaparecieron 2 veces.
# 2.325 (4%) desparecieron entre 3 y 7 veces.

# ¿cuántos anuncios desparecieron en junio y luego volvieron a desaperecer? 198
filter(listings.desaparecidos, d180609 == 1 & (d180710 == 1 | d180818 == 1 | d180911  == 1) ) %>% select(room_type,calculated_host_listings_count,d180710,d180818,d180911 )

# converts to long format -----
listings.desaparecidos_long <-  listings.desaparecidos %>% gather(fecha, eliminated, 8:39) #starts in 4th column after other variables
# parse date
listings.desaparecidos_long$fechab <- strapplyc( as.character(listings.desaparecidos_long$fecha), "d([0-9]*)", simplify = TRUE)
listings.desaparecidos_long$fechab <- as.Date( paste(20,as.character(listings.desaparecidos_long$fechab),sep=""), "%Y%m%d")
# classify type of host
listings.desaparecidos_long$host.type <- ""
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count == 1,]$host.type <- "1 anuncio"
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count > 1,]$host.type <- "varios anuncios"

desaparecidos.count <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab) %>% summarise(anuncios=n())
desaparecidos.count.room_type <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())
desaparecidos.count.host_type <- listings.desaparecidos_long %>% filter (eliminated ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_col(aes(fechab,anuncios),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
# dev.off()

# 1 Miramos cuántos son según habitación entre los eliminados
png(filename=paste("images/airbnb/eliminados/anuncios-eliminados-room-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos según tipo de habitación",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 2 Miramos cuál es la proporción de tipo de anuncios según habitación entre los eliminados
png(filename=paste("images/airbnb/eliminados/anuncios-eliminados-porcentaje-room-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios desaparecidos según tipo de habitación",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 3 Miramos cuál es el número de anuncios según host type entre los eliminados
png(filename=paste("images/airbnb/eliminados/anuncios-eliminados-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios desaparecidos según tipo de host",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# 4 Miramos cuál es la proporción según habitación entre los eliminados
png(filename=paste("images/airbnb/eliminados/anuncios-eliminados-porcentaje-host-type.png", sep = ""),width = 1000,height = 300)
desaparecidos.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios desaparecidos según tipo de host",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# aparecidos o nuevos
# detects which listings are NEW in each scraping date (compare with previous) -------------------------------------------------------------------------------
listings.new <- listings.total
# sets all values to 0
listings.new[,8:39] <- 0

for (i in 1:length(dates)) {
  print(i)
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  column.select2 <- paste("d",dates[i+1],sep = "")
  print(column.select2)
  # debe cumplir condición: no está en columna 1 pero sí está en 2
  id.new <- listings.total %>% filter(  (!!sym(column.select)) == 0 & (!!sym(column.select2)) == 1 ) %>% select(id)
  # sets to 1 if newand stores it in column.select2 date
  listings.new[listings.new$id %in% id.new$id,column.select2] <- 1
}

# converts to long format 
listings.new_long <-  listings.new %>% gather(fecha, new, 8:39) #starts in 5th column after other variables
# parse date
listings.new_long$fechab <- strapplyc( as.character(listings.new_long$fecha), "d([0-9]*)", simplify = TRUE)
listings.new_long$fechab <- as.Date( paste(20,as.character(listings.new_long$fechab),sep=""), "%Y%m%d")
# classify type of host
listings.new_long$host.type <- ""
listings.new_long[listings.new_long$calculated_host_listings_count == 1,]$host.type <- "single listing"
listings.new_long[listings.new_long$calculated_host_listings_count > 1,]$host.type <- "multiple listings"

new.count <- listings.new_long %>% filter (new ==1) %>% group_by(fechab) %>% summarise(anuncios=n())
new.count.room_type <- listings.new_long %>% filter (new ==1) %>% group_by(fechab,room_type) %>% summarise(anuncios=n())
new.count.host_type <- listings.new_long %>% filter (new ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

new.count %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_col(aes(fechab,anuncios),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios new",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")

# 1 Miramos cuántos son según habitación entre los new
png(filename=paste("images/airbnb/eliminados/anuncios-new-room-type.png", sep = ""),width = 1000,height = 300)
new.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios nuevos según tipo de habitación",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 2 Miramos cuál es la proporción de tipo de anuncios según habitación entre los new
png(filename=paste("images/airbnb/eliminados/anuncios-new-porcentaje-room-type.png", sep = ""),width = 1000,height = 300)
new.count.room_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=room_type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios nuevos según tipo de habitación",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento")
dev.off()

# 3 Miramos cuál es el número de anuncios según host type entre los new
png(filename=paste("images/airbnb/eliminados/anuncios-new-host-type.png", sep = ""),width = 1000,height = 300)
new.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios nuevos según tipo de host",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()

# 4 Miramos cuál es la proporción según habitación entre los new
png(filename=paste("images/airbnb/eliminados/anuncios-new-porcentaje-host-type.png", sep = ""),width = 1000,height = 300)
new.count.host_type  %>%
  ggplot () +
  geom_col(aes(fechab,anuncios,fill=host.type),position = "fill" ) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Porcentaje de anuncios nuevos según tipo de host",
       subtitle = "Barcelona 2015-2018 (entre fechas consecutivas de scraping)",
       y = "%",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       fill="El host gestiona")
dev.off()


# Adds when listing was first found ------------------------------------------------------
listings.total.found <- data_long %>% filter(exists == 1) %>% group_by(id) %>% summarise(found = min(fechab)) %>% ungroup()
# listings.total <- listings.total %>% select(-"min(fechab)")
listings.total <- inner_join(listings.total,listings.total.found)

# calculates in how many scrapings has been every listing ----
listings.total$sum <- rowSums(listings.total[,3:29])
listings.total$sum2018 <- rowSums(listings.total[,22:29])

# hist(listings.total$sum)

# extends color paletter
colourCount <- length(unique(listings.total$found))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# Plots: histograms -----

# histograma básico
png(filename=paste("images/airbnb/eliminados/eliminados-01.png", sep = ""),width = 1000,height = 750)
listings.total %>%
  ggplot(aes(sum)) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = "2015-2018. Número de listings analizados",
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado
png(filename=paste("images/airbnb/eliminados/eliminados-02.png", sep = ""),width = 1000,height = 750)
listings.total %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado y filtrando por mayor que una fecha
png(filename=paste("images/airbnb/eliminados/eliminados-03.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(found > "2018-04-01") %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# histograma coloreado según cuándo fue encontrado y filtrando por fecha concreta
png(filename=paste("images/airbnb/eliminados/eliminados-04.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(d180911 == 1) %>%
  ggplot(aes(sum,fill=as.factor(found))) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuántas veces está un anuncio en los 27 scraping de InsideAirbnb",
       subtitle = paste("2015-2018. Número de listings analizados",nrow(listings.total) ),
       y = "número de anuncios",
       x = "número de veces que aparece",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Plots: when they were first found. Bars -----

# barra  coloreado según cuándo fue encontrado y filtrando por fecha concreta
png(filename=paste("images/airbnb/eliminados/eliminados-06.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(d180818 == 1) %>%
  ggplot(aes(x=1,fill=as.factor(found))) + 
  geom_bar() +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Cuándo se publicaron por 1ª vez los anuncios que estaban en agosto 2018",
       subtitle = "",
       y = "nº de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
  )+
  coord_flip() 
dev.off()

# loop all the dates
for (i in 1:length(dates)) {
  # for (i in 1:2) {
  # barra  coloreado según cuándo fue encontrado y filtrando por fecha concreta
  column.select <- paste("d",dates[i],sep = "")
  print(column.select)
  png(filename=paste("images/airbnb/eliminados/matrix/",column.select,".png", sep = ""),width = 400,height = 1000)
  filename <- paste("images/airbnb/eliminados/matrix/",column.select,".png", sep = "")
  print(filename)
  # seen in Pass a string as variable name in dplyr::filter https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  p <- listings.total %>% filter((!!sym(column.select)) == 1) %>%
    ggplot(aes(x=1,fill=as.factor(found))) + # reverse orden of factors
    geom_bar() +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_minimal(base_size =10) +
    theme(
      panel.grid.minor.y = element_blank(),
      # panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      # axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank()
      # legend.position = "none"
    ) +
    labs(title = dates[i],
         subtitle = "",
         y = "",
         x = ""
         # caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
    )+
    scale_y_continuous(limits = c(0,20000)) 
  # coord_flip()
  print(p) 
  dev.off()
}

# Then build matrix with "montage" the imagemagick function with bash:
# montage d1* -geometry 200x800+0+0 vertical.png

# for network graph ------
# subset data for speed the process. Only after 2018-04 
test <- data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",]
ggplot(test[sample(nrow(test),100),],aes(x = as.factor(fecha), y = as.factor(id))) +
  geom_point(size=3,alpha=0.8)

# png(filename=paste("temp/eliminados-03.png", sep = ""),width = 2000,height = 5000)
# ggplot(test,aes(x = as.factor(fecha), y = as.factor(id))) +
#   geom_bin2d()
# dev.off()

# Exports data to use them in gephi
rownames(test) <- 1:111557

links <- test[,1:2]
names(links) <- c("source","target")
nodes <- as.data.frame(c(unique(links$target),unique(links$source)))

# Sve data if needed
write.csv(links, file="temp/links-listings-post-2017.csv")
write.csv(nodes, file="temp/nodes-listings-post-2017.csv")

# build matrix for heat map -----
# # Tests for calculating with 2 databases
# # Find listings that are in first two scrapings
# listings.total[listings.total$d150717 == 1 & listings.total$d150430 == 1,1:3]
# listings.total %>% filter(d150717 == 1 & d150430 == 1)
# 
# # count listings
# nrow(listings.total[listings.total$d150717 == 1 & listings.total$d150430 == 1,])
# nrow(listings.total %>% filter(d150717 == 1 & d150430 == 1))

# Analyses ony Entire home/apartment ads --------
listings.total.all <- listings.total # saves original
listings.total <- filter(listings.total.all,room_type == "Piso completo") #only entire homes


# Creates matrix
heat.matrix <- data.frame(matrix(ncol = length(dates),nrow = length(dates)  ))
names(heat.matrix) <- dates
row.names(heat.matrix) <- dates

for (j in 1:length(dates)) {
  print(paste("j:",j))
  for (i in 1:length(dates)) {
    print(paste("i:",i))
    column.select.i <- paste("d",dates[j],sep = "")
    column.select.j <- paste("d",dates[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
    coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
    print(coinciden)
    print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
    heat.matrix[j,i] <- coinciden 
    # heat.matrix[i,j] <- coinciden 
  }
}

write.csv(heat.matrix, file="temp/heat.matrix.csv")

# basic heatmap
image(as.matrix(heat.matrix), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix$id <- colnames(heat.matrix)
# melt, from wide to long format
heat.matrix.m <- melt(heat.matrix)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.m , aes(x = id, y = variable, fill = -value)) + geom_tile()

# help https://blog.aicry.com/r-heat-maps-with-ggplot2/index.html
# extends color palette
hm.palette <- colorRampPalette(rev(brewer.pal(9, 'YlOrBr')), space='Lab')

# png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.m , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 12) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de anuncios coincidentes en bases de datos de InsideAirbnb",
       # subtitle = "2015-2018",
       subtitle = "Pisos completos. 2015-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# heat matrix desde 161107-----------
dates.p <- dates
dates.p <- dates.p[-c(1:5)]

heat.matrix.p <- data.frame(matrix(ncol = length(dates.p),nrow = length(dates.p)  ))
names(heat.matrix.p) <- dates.p
row.names(heat.matrix.p) <- dates.p

for (j in 1:length(dates.p)) {
  print(paste("j:",j))
  for (i in 1:length(dates.p)) {
    print(paste("i:",i))
    column.select.i <- paste("d",dates.p[j],sep = "")
    column.select.j <- paste("d",dates.p[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
    coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
    print(coinciden)
    print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
    heat.matrix.p[j,i] <- coinciden 
    # heat.matrix[i,j] <- coinciden 
  }
}

write.csv(heat.matrix.p, file="temp/heat.matrix.p.csv")

# basic heatmap
image(as.matrix(heat.matrix.p), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix.p$id <- colnames(heat.matrix.p)
# melt, from wide to long format
heat.matrix.m.p <- melt(heat.matrix.p)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.m.p , aes(x = id, y = variable, fill = -value)) + geom_tile()

# png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02p.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02p-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.m.p , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  # geom_text(aes(label=value)) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 22) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. 2016-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Anuncios coincidentes en gráficos de línea
png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-03.png", sep = ""),width = 1400,height = 600)
ggplot(heat.matrix.m.p , aes(x = id, y = value, group = variable,color=variable)) +
  geom_line() +
  geom_point(size=0.5) +
  geom_text(aes(label=value),size=3,color="#000000")+
  scale_fill_manual(values = getPalette(colourCount)) +
  theme_minimal(base_size = 25) +
  theme(
    # panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Anuncios de pisos completos coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Cada línea es una base de datos. 2016-2018",
       y = "nº coincidencias",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()


# heat matrix desde 161107 normalizada-----------
heat.matrix.n <- data.frame(matrix(ncol = length(dates.p),nrow = length(dates.p)  ))
names(heat.matrix.n) <- dates.p
row.names(heat.matrix.n) <- dates.p

for (j in 1:length(dates.p)) {
  print(paste("j:",j))
  for (i in 1:length(dates.p)) {
    print("---------")
    print(paste("i:",i))
    column.select.i <- paste("d",dates.p[j],sep = "")
    column.select.j <- paste("d",dates.p[i],sep = "")
    print(paste(column.select.i,"interseccion con",column.select.j,":"))
    coinciden <- nrow(listings.total %>% filter((!!sym(column.select.i))== 1 & (!!sym(column.select.j)) == 1) )
    total <- nrow( listings.total %>% filter((!!sym(column.select.i))== 1) )
    percent <- round(coinciden/total*100, digits = 0)
    print(paste("elementos que coinciden ",coinciden," divididos por ",total," (",column.select.i,")",sep=""))
    print(paste("sale:",percent,"%"))
    # print(coinciden,total,percent)
    print(paste("mete dato en ","[j:",j,", i:",i,"]",sep = ""))
    heat.matrix.n[j,i] <- percent
  }
}

write.csv(heat.matrix.n, file="temp/heat.matrix.n.csv")

# basic heatmap
image(as.matrix(heat.matrix.n), xlab = 'Matrix rows', ylab = 'Matrix columns', axes = F)

# add id to column
heat.matrix.n$id <- colnames(heat.matrix.n)
# melt, from wide to long format
heat.matrix.n.m.p <- melt(heat.matrix.n)
# heat.matrix.m <- melt(heat.matrix,variable.name = "sample",value.name = "other",id="id")
ggplot(heat.matrix.n.m.p , aes(x = id, y = variable, fill = value)) + geom_tile()

# png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02-normalizada-text.png", sep = ""),width = 1200,height = 1200)
png(filename=paste("images/airbnb/eliminados/heat-map-coincidencias-barcelona-insideairbnb-02-normalizada-text-pisos-completos.png", sep = ""),width = 1200,height = 1200)
ggplot(heat.matrix.n.m.p , aes(x = id, y = variable, fill = value)) +
  geom_tile() +
  geom_text(aes(label=value),size=6) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(hm.palette(100))) +
  theme_minimal(base_size = 28) +
  theme(
    # panel.grid.minor.x = element_blank(), 
    # panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. 2016-2018",
       y = "fechas",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Plots coincidencias normalizadas

# fechas sin parsear -----
# png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-01-normalizada.png", sep = ""),width = 1400,height = 600)
# ggplot(heat.matrix.n.m.p , aes(x = id, y = value, group = variable,color=variable)) +
#   geom_line() +
#   geom_point(size=0.5) +
#   scale_fill_manual(values = getPalette(colourCount)) +
#   theme_minimal(base_size = 25) +
#   theme(
#     # panel.grid.minor.x = element_blank(), 
#     # panel.grid.major.x = element_blank(),
#     legend.position = "right",
#     axis.text.x = element_text(angle = 90, vjust = 0.4)
#   ) +
#   labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
#        subtitle = "Cada línea es una base de datos. 2016-2018",
#        y = "% coincidencia",
#        x = "fechas",
#        caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
# dev.off()

# converts to date -----
heat.matrix.n.m.p$date <- as.Date( paste(20,as.character(heat.matrix.n.m.p$variable),sep=""), "%Y%m%d")
# heat.matrix.n.m.p$id <- as.factor(heat.matrix.n.m.p$id)

# png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-02-normalizada.png", sep = ""),width = 1400,height = 600)
png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-02-normalizad-pisos-completos.png", sep = ""),width = 1400,height = 600)
# heat.matrix.n.m.p %>% filter(id=="180514") %>%
# ggplot(aes(x = date, y = value)) +
ggplot(heat.matrix.n.m.p, aes(x = date, y = value, group = id,color=variable)) +
  geom_line() +
  geom_point(size=1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%Y") +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-31"),y=40,label="acuerdo",color="#000000",size=4) +
  theme_minimal(base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de anuncios coincidentes en bases de datos de InsideAirbnb",
       subtitle = "Pisos completos. Cada línea es una base de datos. 2016-2018",
       y = "% coincidencia",
       x = "fechas",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Líneas: coincidencia en % de anuncios de pisos completos entre bases de datos de Airbnb --------------------
png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-03-normalizad-pisos-completos.png", sep = ""),width = 1000,height = 500)
ggplot() + 
  # lines
  geom_line(data=heat.matrix.n.m.p, aes(x = date, y = value, group = id),color="#bbbbbb") +
  # line destacada
  geom_line(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  geom_point(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete % en cada punto de mayo
  geom_text(data=filter(heat.matrix.n.m.p,id=="180514" & date > as.Date("2018-01-01") & date < as.Date("2018-09-01")),
            aes(x = date+10, y = value+3,label=paste(value,"%",sep="")),size=5,family = "Roboto Condensed")+
  # fechas de scrapings
  geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
            aes(x = date, y = 41,label=date),
            size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # nota fecha de scraping
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # theme
  theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de pisos completos de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Cada línea es un scraping. 2016-2018. Barcelona",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Explicación 01 -------
heat.matrix.m.p$date <- as.Date( paste(20,as.character(heat.matrix.n.m.p$variable),sep=""), "%Y%m%d")

png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-03-normalizad-pisos-completos_01.png", sep = ""),width = 1000,height = 500)
ggplot(heat.matrix.m.p , aes(x = date, y = value, group = id),fill="#bbbbbb") +
  # geom_line() +
  # geom_point(size=0.5) +
  # geom_text(aes(label=value),size=3,color="#000000") +
  # geom_line(heat.matrix.m.p , aes(x = id, y = value, group = variable)) +
  # line destacada
  geom_line(data=filter(heat.matrix.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  # geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  geom_point(data=filter(heat.matrix.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete n en cada punto de mayo
  geom_text( data=filter(heat.matrix.m.p, id=="180514"),
             aes(x = date, 
                 y = value+100,
                 label=value,
                 family = "Roboto Condensed"),size=4 )+
  # fechas de scrapings
  # geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
  # aes(x = date, y = 41,label=date),
  # size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  # scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  # geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  # annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  # annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
  #          family = "Roboto Condensed", hjust = 1,size=6) +
  # geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
  #            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
# ) +
# nota fecha de scraping
# annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
#          family = "Roboto Condensed", hjust = 1,size=6) +
# geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
#            color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
# ) +
# theme
theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Número de pisos completos coincidentes de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Mayo 2018. Barcelona",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# Explicación 02: Líneas: coincidencia en % de anuncios de pisos completos entre bases de datos de Airbnb explicación--------------------
png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-03-normalizad-pisos-completos_02.png", sep = ""),width = 1000,height = 500)
ggplot() + 
  # lines
  # geom_line(data=heat.matrix.n.m.p, aes(x = date, y = value, group = id),color="#bbbbbb") +
  # line destacada
  geom_line(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value, group =id),color="#ddbbbb",size=2) +
  # line destacada
  # geom_line(data=filter(heat.matrix.n.m.p,id=="180818"), aes(x = date, y = value, group = id),color="#bbddbb",size=2) +
  # points
  # geom_point(data=heat.matrix.n.m.p, aes(x = date, y = value), size=1,color="#bbbbbb") +
  # destaca puntos mayo
  geom_point(data=filter(heat.matrix.n.m.p,id=="180514"), aes(x = date, y = value), size=2,color="#bb9999") +
  # mete % en cada punto de mayo
  geom_text(data=filter(heat.matrix.n.m.p,id=="180514" & date > as.Date("2018-01-01") & date < as.Date("2018-09-01")),
            aes(x = date+10, y = value+3,label=paste(value,"%",sep="")),size=5,family = "Roboto Condensed")+
  # fechas de scrapings
  geom_text(data=filter(heat.matrix.n.m.p, value ==100 ),
            aes(x = date, y = 41,label=date),
            size=5,family = "Roboto Condensed",angle = 90,color ="#333333" )+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  scale_x_date(date_breaks = "2 month",date_labels = "%m-%Y") +
  # anotations
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-26"),y=57,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  annotate("text",x=as.Date("2018-06-1"),y=87,label="El 29% desapareció",color="#000000",size=5,hjust=0,family = "Roboto Condensed") +
  # nota 71%
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 55, label = "El 71% de los anuncios de mayo seguía en junio", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 55, xend = as.Date("2018-06-6"), yend = 70.5), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # nota fecha de scraping
  annotate(geom = "text", x = as.Date("2018-01-1"), y = 103, label = "Fecha del scraping de mayo 2018", 
           family = "Roboto Condensed", hjust = 1,size=6) +
  geom_curve(aes(x = as.Date("2018-01-1"), y = 100, xend = as.Date("2018-05-10"), yend = 100), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.03, "npc"))
  ) +
  # theme
  theme_minimal(base_family = "Roboto Condensed",base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.major.x = element_blank(),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_blank()
    # axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de pisos completos de Airbnb entre scrapings de InsideAirbnb",
       subtitle = "Mayo 2018. Barcelona",
       y = "% coincidencia entre scrapings",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()


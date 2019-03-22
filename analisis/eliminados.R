# Este script analiza las diferencias entre los diferentes listings de una localización de InsideAirbnb
# Usa los archivos listings-summary de Insideairbnb 

# Load libraries ----
library(gsubfn)
library(tidyverse)
# extends color paletter
library(RColorBrewer)
library("reshape2")
library(ggthemes) #install ggthemes

# ------ Get dates when data are --------
# Loads dates with listings data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180818","180911")

# loop starts
listings  <- select(as.data.frame(read.delim("data/original/airbnb/150430/listings_summary_barcelona_insideairbnb.csv",sep = ",")),id,room_type,calculated_host_listings_count)
# names(listings) <- "id"
listings.total <- listings

# Set up style theme for ggplot
# theme_our <- function{ theme(theme_minimal(base_family = "Roboto Condensed")) }

# Loop para ir insertando todos los listings en un dataframe
# El objetivo es tener una lista con todos los id de los anuncios
for (i in 1:length(dates)) {
  print("listings totales: ")
  print(nrow(listings.total))
  print("------")
  print(paste("id:",i))
  print(paste("fecha: ",dates[i]))
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),id,room_type,calculated_host_listings_count)
  # names(neo) <- "id"
  print(paste("neo a leer",nrow(neo)))
  # TODO. Solamente inserta los que no estaban ya. Si el calculated_host_listings_count ha camiado (el usuario ahora tiene más anuncios) no lo actualizaría
  # Se podría guardar el host_id y luego mirar si ha tenido más de una anuncio en algún momento
  neo2 <- select(as.data.frame(neo[!neo$id %in% listings.total$id,]),id,room_type,calculated_host_listings_count)
  # names(neo2) <- "id"
  print(paste("neo2 a insertar",nrow(neo2)))
  # añade los anuncios al listado original
  listings.total <- rbind(listings.total,neo2)
}

# counts whether a listing exists in a scraping
# each row is one listings. each column is one date when scraping was made by insideairbnb
for (i in 1:length(dates)) {
  print(i)
  # Adds rows to the original
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),id,room_type)
  # names(neo) <- "id"
  print(paste("n listings:",nrow(neo)))
  # sets to 0 for all
  listings.total[,paste("d",dates[i],sep="")] <- 0
  # sets to 1 if listing is in that date
  listings.total[listings.total$id %in% neo$id,paste("d",dates[i],sep="")] <- 1
}

# converts to long format -----
data_long <- listings.total %>% gather(fecha, exists, 4:30) #starts in 4th column after other variables
# parse date
data_long$fechab <- strapplyc( as.character(data_long$fecha), "d([0-9]*)", simplify = TRUE)
data_long$fechab <- as.Date( paste(20,as.character(data_long$fechab),sep=""), "%Y%m%d")
# classify type of host
data_long$host.type <- ""
data_long[data_long$calculated_host_listings_count == 1,]$host.type <- "single listing"
data_long[data_long$calculated_host_listings_count > 1,]$host.type <- "multiple listings"

# counts listings per scraping date ----
dates.count <- data_long %>% filter (exists ==1) %>% group_by(fechab) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-barcelona-por-mes-linea.png", sep = ""),width = 1000,height = 400)
dates.count  %>%
  ggplot(aes(fechab,anuncios)) + 
  geom_line(size=1.5) +
  # geom_line(aes(fechab,anuncios))
  geom_point(size=2.5,color="#BB3300") +
  annotate("text",x=as.Date("2018-04-20"),y=4000,label="acuerdo",color="#000000",size=5) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  ylim(0, max(dates.count$anuncios)) +
  # geom_text(aes(label=anuncios)) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
    # legend.position = "bottom"
  ) +
  labs(title = "Número de anuncios de Airbnb en cada scraping de InsideAirbnb",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # nota
  annotate(geom = "text", x = as.Date("2016-01-20"), y = 11500, label = "Cada punto es un scraping de InsideAirbnb", 
           family = "Roboto Condensed", hjust = 0,size=6) +
  annotate(geom = "segment", x = as.Date("2017-01-1"), xend = as.Date("2017-04-1"), y = 12000, yend = 17200,
           color="#999999")
dev.off()

# counts listings per scraping date and room type ----
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
dates.count.room_type %>% filter(!room_type=="Shared room") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-04-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=room_type,color=room_type),size=1.5) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 14) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  ) +
  labs(title = "Número de anuncios en cada scraping de InsideAirbnb por tipo de alojamiento",
       subtitle = "Barcelona 2015-2018",
       y = "número de anuncios",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# anuncios de pisos completos
dates.count.room_type %>% filter(room_type=="Entire home/apt") %>%
  ggplot () + 
  geom_col(aes(fechab,anuncios))

# separado por tipo de host ----
dates.count.host.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.type %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-1"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# separado por tipo de host y alojamiento ----
dates.count.host.room.type <- data_long %>% filter (exists ==1) %>% group_by(fechab,room_type,host.type) %>% summarise(anuncios=n())

# timeline
png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-host-room-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Shared room") %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-15"),y=1000,label="acuerdo",color="#000000",size=4) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  geom_step(aes(fechab,anuncios,group=host.type,color=host.type),size=1.5) +
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
  facet_wrap(~room_type)
dev.off()

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-room-host-type.png", sep = ""),width = 1000,height = 300)
dates.count.host.room.type %>% filter(!room_type=="Shared room") %>%
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

# data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",] %>%
# ggplot(aes(x = as.factor(fecha), y = as.factor(id))) +
#   # geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",alpha=0.3,size=0.1)
#   geom_point(size=0.1,alpha=0.8)


# detects which listings were erased/dissapeared -------------------------------------------------------------------------------
listings.desaparecidos <- listings.total
# sets all values to 0
listings.desaparecidos[,4:30] <- 0

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
  id.removed <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
  # sets to 1 if erased in column.select2
  listings.desaparecidos[listings.desaparecidos$id %in% id.removed$id,column.select2] <- 1
}

# converts to long format 
listings.desaparecidos_long <-  listings.desaparecidos %>% gather(fecha, eliminated, 4:30) #starts in 4th column after other variables
# parse date
listings.desaparecidos_long$fechab <- strapplyc( as.character(listings.desaparecidos_long$fecha), "d([0-9]*)", simplify = TRUE)
listings.desaparecidos_long$fechab <- as.Date( paste(20,as.character(listings.desaparecidos_long$fechab),sep=""), "%Y%m%d")
# classify type of host
listings.desaparecidos_long$host.type <- ""
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count == 1,]$host.type <- "single listing"
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count > 1,]$host.type <- "multiple listings"

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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

# aparecidos o nuevos
# detects which listings were erased/dissapeared -------------------------------------------------------------------------------
listings.desaparecidos <- listings.total
# sets all values to 0
listings.desaparecidos[,4:30] <- 0

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
  id.removed <- listings.total %>% filter(  (!!sym(column.select)) == 1 & (!!sym(column.select2)) == 0 ) %>% select(id)
  # sets to 1 if erased in column.select2
  listings.desaparecidos[listings.desaparecidos$id %in% id.removed$id,column.select2] <- 1
}

# converts to long format 
listings.desaparecidos_long <-  listings.desaparecidos %>% gather(fecha, eliminated, 4:30) #starts in 4th column after other variables
# parse date
listings.desaparecidos_long$fechab <- strapplyc( as.character(listings.desaparecidos_long$fecha), "d([0-9]*)", simplify = TRUE)
listings.desaparecidos_long$fechab <- as.Date( paste(20,as.character(listings.desaparecidos_long$fechab),sep=""), "%Y%m%d")
# classify type of host
listings.desaparecidos_long$host.type <- ""
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count == 1,]$host.type <- "single listing"
listings.desaparecidos_long[listings.desaparecidos_long$calculated_host_listings_count > 1,]$host.type <- "multiple listings"

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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
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

# Plots: when they were frist found. Bars -----

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
listings.total <- filter(listings.total.all,room_type == "Entire home/apt") #only entire homes


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

# fechas sin parsear
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

# converts to date
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

png(filename=paste("images/airbnb/eliminados/lineas-coincidencias-barcelona-insideairbnb-03-normalizad-pisos-completos.png", sep = ""),width = 1400,height = 600)
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
  geom_text(data=filter(heat.matrix.n.m.p,id=="180514" & date > as.Date("2018-05-01") & date < as.Date("2018-07-01")),
            aes(x = date, y = value+3,label=paste(value,"%",sep="")),size=4)+
  # colors
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale
  scale_x_date(date_breaks = "1 month",date_labels = "%m-%Y") +
  # anotation
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  annotate("text",x=as.Date("2018-05-15"),y=40,label="acuerdo",color="#000000",size=4) +
  annotate("text",x=as.Date("2018-06-30"),y=87,label="El 29% desapareció",color="#000000",size=4) +
  # theme
  theme_minimal(base_size = 25) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major = element_line(size=0.6),
    panel.grid.minor = element_line(size=0.3),
    # panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 90, vjust = 0.4)
  ) +
  labs(title = "Porcentaje de anuncios de pisos completos coincidentes entre bases de datos de InsideAirbnb",
       subtitle = "Cada línea es una base de datos. 2016-2018",
       y = "% coincidencia",
       x = "fecha",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb")
dev.off()

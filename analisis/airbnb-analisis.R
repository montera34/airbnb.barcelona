# Análisis de Airbnb
# -Habitaciones vs Viviendas completas
# -numero anuncios por barrio
# -ratio anuncios por barrio por vivienda
# -numero plazas
# -ratio plazas por habitante

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(reshape)
library(gridExtra)
library(gsubfn) # select text in the parenthesis with regex

# ------ Load files ----------
# Load shapes
barrios <- readOGR("data/original/contornos/barrios_geo.json")
distritos <- readOGR("data/original/contornos/distritos_geo.json")
# mar <- readOGR("data/original/shapes/mar-donostia.geojson") TODO

viviendas_barrios <- read.delim("data/original/demografia-vivienda/habitantes-viviendas-por-barrios_padron2018_ayto-barcelona.csv",sep = ",")

# Airbnb listings 2017-09-12
airbnb201709 <- read.delim("data/original/airbnb/170912/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# Translate room type
levels(airbnb201709$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# Airbnb listings 2018-09-11
airbnb201809 <- read.delim("data/original/airbnb/180911/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# Translate room type
levels(airbnb201809$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")

# counts listings per barrio
barrios_n_listings <- as.data.frame(table(airbnb201709$neighbourhood))
names(barrios_n_listings) <- c("barrio","n_listings")
write.csv(as.data.frame(barrios_n_listings), file = "data/output/airbnb/170912/barrios-n-listings-airbnb-barcelona-insideairbnb.csv", row.names = FALSE)

barrios_n_listings <- as.data.frame(table(airbnb201809$neighbourhood))
names(barrios_n_listings) <- c("barrio","n_listings")
write.csv(as.data.frame(barrios_n_listings), file = "data/output/airbnb/180911/barrios-n-listings-airbnb-barcelona-insideairbnb.csv", row.names = FALSE)

# ----- Comparative room types -----------

# Compare two data sets by number of room types----
compare_room <- merge(data.frame(table(airbnb201709$room_type)),data.frame(table(airbnb201809$room_type)),by="Var1")
colnames(compare_room) <- c("tipo_habitacion","2017","2018")

# reshape data to long format to prepare to plot bar chart year comparison
m <- reshape(compare_room, direction = "long", varying = list(names(compare_room)[2:3]), v.names = "Value", 
             idvar = c("tipo_habitacion"), timevar = "Year", times = 2017:2018)

ggplot(m,aes(x = tipo_habitacion, y = Value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~Year)

m$Year <- as.factor(m$Year)
png(filename="images/airbnb/hab-viv-barras-airbnb-darcelona-2017-2018.png",width = 600,height = 400)
ggplot(m,aes(x = tipo_habitacion, y = Value)) +
  geom_bar(aes(fill = Year), position = "dodge", stat="identity")+
  scale_y_continuous(expand = c(0, 800)) + #limits = c(0,1500)
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb en Barcelona.",
       subtitle = "Septiembre Años 2017 y 2018",
       y = "nº de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = Value, group=Year),
            position = position_dodge(width = 1), hjust = -0.1,
            size=3,color="#777777") +
  coord_flip()
dev.off()


# Clasidicación por barrios ---------------------------------------------------------
airbnb.barrio.room_type.1 <- airbnb201709 %>% 
  group_by(neighbourhood,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count) 

airbnb.barrio.room_type.2 <- airbnb201809 %>% 
  group_by(neighbourhood,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count)

# Clasidicación por distritos ---------------------------------------------------------
airbnb.distrito.room_type.1 <- airbnb201709 %>% 
  group_by(neighbourhood_group,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count) 

airbnb.distrito.room_type.2 <- airbnb201809 %>% 
  group_by(neighbourhood_group,room_type) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count)

png(filename="images/airbnb/hab-viv-barras-airbnb-distritos-barcelona-201809.png",width = 900,height = 1400)
ggplot(data = airbnb.barrio.room_type.2,aes(x = reorder(neighbourhood,suma), y = count, fill=room_type)) +
  # "reverse" es la clave para reordenar las barras y que coincida con leyenda https://github.com/tidyverse/ggplot2/issues/1837
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(expand = c(0, 30)) + #limits = c(0,950),
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top"
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Barcelona",
       subtitle = "Septiembre 2018",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb",
       fill="tipo de alojamiento") +
  # partial text
  geom_text(aes(label = count,group=room_type), 
            data=airbnb.barrio.room_type.2[airbnb.barrio.room_type.2$count > 50,], 
            position = position_stack(reverse = TRUE,vjust = 0.5),size=3,color="#FFFFFF") +
  # total text
  geom_text(aes(label = suma, y = suma+50),
            position = "dodge",
            size=3,color="#888888") +
  coord_flip()
dev.off()


# slope graph 2017-2018. Por barrios y tipo alojamiento ----------------------------------------------------------------------------------
#  Compare evolution
airbnb.barrio.room_type.1$name_complete <- paste(airbnb.barrio.room_type.1$neighbourhood,airbnb.barrio.room_type.1$room_type,sep = "_")
airbnb.barrio.room_type.2$name_complete <- paste(airbnb.barrio.room_type.2$neighbourhood,airbnb.barrio.room_type.2$room_type,sep = "_")

airbnb_20172018_barr <- merge(airbnb.barrio.room_type.1,
                         select(airbnb.barrio.room_type.2,count,suma,name_complete,-neighbourhood), 
                         by="name_complete" )

names(airbnb_20172018_barr) <- c("name_complete","neighbourhood","room_type" ,"anuncios2017","total2017","anuncios2018","total2018")

airbnb_20172018_barr$evolution <- round( (airbnb_20172018_barr$anuncios2018 - airbnb_20172018_barr$anuncios2017) / airbnb_20172018_barr$anuncios2017 * 100,
                                    digits=2)
airbnb_20172018_barr$base100 <- round( (airbnb_20172018_barr$anuncios2018 - airbnb_20172018_barr$anuncios2017) / airbnb_20172018_barr$anuncios2017 * 100 + 100,
                                    digits=2)
airbnb_20172018_barr$dif <- airbnb_20172018_barr$anuncios2018 - airbnb_20172018_barr$anuncios2017 

# slope graph evolution 2017-2018 per barrio
png(filename="images/airbnb/pendiente-numero-room.type-2017-2018-barrios-barcelona.png",width = 900,height = 1400)
airbnb_20172018_barr %>% filter(!room_type == "Habitación compartida") %>%
ggplot() +
  geom_segment(aes(x=0,xend=12,y=anuncios2017,yend=anuncios2018,colour=room_type),size=.5) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 5))  +
  labs(title = "Evolución por barrios de número de habitaciones y viviendas de Airbnb en Donostia.",
       subtitle = "Evolución 2017-2018",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos:InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # geom_text(label=l2017, y=airbnb_20172018_barr$count.x, x=rep.int(0,nrow(airbnb_20172018_barr)), hjust=1.2,size=3.5) +
  # geom_text(label=l2018, y=airbnb_20172018_barr$count.y, x=rep.int(12,nrow(airbnb_20172018_barr)), hjust=-0.2,size=3.5) +
  # diferencia
  # geom_text(data=filter(airbnb_20172018_barr,anuncios2018 > 400),
  #           aes(label=dif), 
  #           y=airbnb_20172018_barr$anuncios2018-((airbnb_20172018_barr$anuncios2018-airbnb_20172018_barr$anuncios2017)/2),
  #           x=rep.int(6,nrow(airbnb_20172018_barr)), hjust=1,vjust=-1,size=3.5) +
  # nombre barrio
  geom_text(data=filter(airbnb_20172018_barr,anuncios2018 > 200),
            aes(label=neighbourhood,y= anuncios2018),
            x=12,
            hjust=0,size=3.5) +
  # annotate("text",label="2017", x=0, y=(1.02*(max(airbnb_20172018_barr$anuncios2017,airbnb_20172018_barr$anuncios2018))),hjust= 0,size=3) +
  # annotate("text",label="2018", x=12, y=(1.02*(max(airbnb_20172018_barr$anuncios2017,airbnb_20172018_barr$anuncios2018))),hjust= 1,size=3)
  facet_wrap(~room_type)
dev.off()

# base 100 slope graph
png(filename="images/airbnb/pendiente-base100-room.type-2017-2018-barrios-barcelona.png",width = 900,height = 1400)
airbnb_20172018_barr %>% filter(!room_type == "Habitación compartida") %>%
  ggplot() +
  geom_segment(aes(x=0,xend=12,y=100,yend=base100,colour=room_type),size=.5) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 5))  +
  labs(title = "Evolución por barrios de habitaciones y viviendas de Airbnb en Donostia",
       subtitle = "Evolución 2017-2018",
       y = "base 100 = sept. 2017",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # nombre barrio
  geom_text(data=filter(airbnb_20172018_barr,!room_type == "Habitación compartida",base100 > 100),
            aes(label=neighbourhood,y= base100),
            x=12,
            hjust=0,size=3.5) +
  facet_wrap(~room_type)
dev.off()

# base 100 text
airbnb_20172018_barr$random <- runif(174,4,12)

png(filename="images/airbnb/texto-base100-numero-room.type-2017-2018-barrios-barcelona.png",width = 900,height = 600)
airbnb_20172018_barr %>% 
  ggplot() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,12))  +
  labs(title = "Evolución por barrios de habitaciones y viviendas de Airbnb en Donostia",
       subtitle = "Evolución septiembre 2017- septiembre 2018",
       y = "base 100 = sept. 2017",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  # nombre barrio
  geom_text(data=filter(airbnb_20172018_barr, anuncios2018>0 ),
            aes(label=neighbourhood,y= base100,x=random),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_text(data=filter(airbnb_20172018_barr, base100>150 | base100<150 ),
            aes(label=paste(anuncios2018,"d:",dif),
                y= base100-11,x=random+0.4),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_point(data=filter(airbnb_20172018_barr,anuncios2018>0 ),
            aes(y= base100,x=random+0.2,
                color=room_type,
                size=anuncios2018)
            ) +
  facet_wrap(~room_type)
dev.off()

png(filename="images/airbnb/texto-base100-numero-room.type-2017-2018-barrios-barcelona_masde100.png",width = 900,height = 600)
airbnb_20172018_barr %>% filter( anuncios2018>100 ) %>%
  ggplot() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,12))  +
  labs(title = "Evolución por barrios de habitaciones y viviendas de Airbnb en Donostia",
       subtitle = "Barrios con >100 anuncios. Evolución septiembre 2017-septiembre 2018",
       y = "base 100 = sept. 2017",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  # nombre barrio
  geom_text(data=filter(airbnb_20172018_barr, anuncios2018>100 ),
            aes(label=neighbourhood,y= base100,x=random),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_text(data=filter(airbnb_20172018_barr, anuncios2018>100 ),
            aes(label=paste(anuncios2018,"d:",dif),
                y= base100-2,x=random+0.4),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_point(data=filter(airbnb_20172018_barr,anuncios2018>0  & anuncios2018>100),
             aes(y= base100,x=random+0.2,
                 color=room_type,
                 size=anuncios2018)
  ) +
  facet_wrap(~room_type)
dev.off()

# slope graph 2017-2018. Por distrito y tipo alojamiento ----------------------------------------------------------------------------------
#  Compare evolution
airbnb.distrito.room_type.1$name_complete <- paste(airbnb.distrito.room_type.1$neighbourhood_group,airbnb.distrito.room_type.1$room_type,sep = "_")
airbnb.distrito.room_type.2$name_complete <- paste(airbnb.distrito.room_type.2$neighbourhood_group,airbnb.distrito.room_type.2$room_type,sep = "_")

airbnb_20172018_dist <- merge(airbnb.distrito.room_type.1,
                         select(airbnb.distrito.room_type.2,count,suma,name_complete,-neighbourhood_group), 
                         by="name_complete" )

names(airbnb_20172018_dist) <- c("name_complete","neighbourhood_group","room_type" ,"anuncios2017","total2017","anuncios2018","total2018")

airbnb_20172018_dist$evolution <- round( (airbnb_20172018_dist$anuncios2018 - airbnb_20172018_dist$anuncios2017) / airbnb_20172018_dist$anuncios2017 * 100,
                                    digits=2)
airbnb_20172018_dist$base100 <- round( (airbnb_20172018_dist$anuncios2018 - airbnb_20172018_dist$anuncios2017) / airbnb_20172018_dist$anuncios2017 * 100 + 100,
                                  digits=2)
airbnb_20172018_dist$dif <- airbnb_20172018_dist$anuncios2018 - airbnb_20172018_dist$anuncios2017 

# slope graph evolution 2017-2018 per distrito
png(filename="images/airbnb/pendiente-numero-room.type-2017-2018-distritos-barcelona.png",width = 900,height = 1400)
airbnb_20172018_dist %>% filter(!room_type == "Habitación compartida") %>%
  ggplot() +
  geom_segment(aes(x=0,xend=12,y=anuncios2017,yend=anuncios2018,colour=room_type),size=.5) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 5))  +
  labs(title = "Evolución por distritos de número de habitaciones y viviendas de Airbnb en Donostia.",
       subtitle = "Evolución 2017-2018",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos:InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # geom_text(label=l2017, y=airbnb_20172018_dist$count.x, x=rep.int(0,nrow(airbnb_20172018_dist)), hjust=1.2,size=3.5) +
  # geom_text(label=l2018, y=airbnb_20172018_dist$count.y, x=rep.int(12,nrow(airbnb_20172018_dist)), hjust=-0.2,size=3.5) +
  # diferencia
  # geom_text(data=filter(airbnb_20172018_dist,anuncios2018 > 400),
  #           aes(label=dif), 
  #           y=airbnb_20172018_dist$anuncios2018-((airbnb_20172018_dist$anuncios2018-airbnb_20172018_dist$anuncios2017)/2),
  #           x=rep.int(6,nrow(airbnb_20172018_dist)), hjust=1,vjust=-1,size=3.5) +
  # nombre distrito
  geom_text(data=filter(airbnb_20172018_dist,anuncios2018 > 200),
            aes(label=neighbourhood_group,y= anuncios2018),
            x=12,
            hjust=0,size=3.5) +
  # annotate("text",label="2017", x=0, y=(1.02*(max(airbnb_20172018_dist$anuncios2017,airbnb_20172018_dist$anuncios2018))),hjust= 0,size=3) +
  # annotate("text",label="2018", x=12, y=(1.02*(max(airbnb_20172018_dist$anuncios2017,airbnb_20172018_dist$anuncios2018))),hjust= 1,size=3)
  facet_wrap(~room_type)
dev.off()

# base 100 slope graph
png(filename="images/airbnb/pendiente-base100-room.type-2017-2018-distritos-barcelona.png",width = 600,height = 800)
airbnb_20172018_dist %>% filter(!room_type == "Habitación compartida") %>%
  ggplot() +
  geom_segment(aes(x=0,xend=12,y=100,yend=base100,colour=room_type),size=.5) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 10))  +
  labs(title = "Evolución por distritos de habitaciones y viviendas de Airbnb en Donostia",
       subtitle = "Evolución 2017-2018",
       y = "base 100 = sept. 2017",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # nombre distrito
  geom_text(data=filter(airbnb_20172018_dist,!room_type == "Habitación compartida"),
            aes(label=paste(neighbourhood_group,base100),y= base100),
            x=12,
            hjust=0,size=3.5) +
  facet_wrap(~room_type)
dev.off()

# base 100 text
airbnb_20172018_dist$random <- runif(nrow(airbnb_20172018_dist),4,12)

png(filename="images/airbnb/texto-base100-numero-room.type-2017-2018-distritos-barcelona.png",width = 900,height = 600)
airbnb_20172018_dist %>% filter(  !room_type == "Habitación compartida" ) %>%
ggplot() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    # axis.ticks=element_blank(),
    axis.text.x=element_blank(),
    legend.position="top"
  ) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,12))  +
  labs(title = "Evolución por distritos de habitaciones y viviendas de Airbnb en Donostia",
       subtitle = "Evolución septiembre 2017- septiembre 2018",
       y = "base 100 = sept. 2017",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb",
       color="Tipo de alojamiento") +
  # nombre distrito
  geom_text(data=filter(airbnb_20172018_dist, anuncios2018>0 & !room_type == "Habitación compartida" ),
            aes(label=neighbourhood_group,y= base100,x=random),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_text(data=filter(airbnb_20172018_dist, !room_type == "Habitación compartida" ),
            aes(label=paste(anuncios2018,"d:",dif),
                y= base100-2,x=random+0.4),
            hjust=1,
            size=3.5,
            color= "#666666") +
  geom_point(data=filter(airbnb_20172018_dist,anuncios2018>0 & !room_type == "Habitación compartida"),
             aes(y= base100,x=random+0.2,
                 color=room_type,
                 size=anuncios2018)
  ) +
  facet_wrap(~room_type)
dev.off()

# -----------Mapas---------------
png(filename="images/airbnb/hab-viv-mapa-airbnb-barcelona-sept2018.png",width = 900,height = 400)
ggplot() + 
  # scale_fill_manual(values=cbPalette) +
  geom_point(data=airbnb201809,aes(x=longitude, y=latitude), #,colour=room_type
             alpha=0.1,size = 0.5)+
  # geom_path(data=menores,aes(x=long, y=lat,group=group), colour="black",size = 0.1)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="top",
        axis.text = element_blank()) +
  coord_fixed(1.3) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Anuncios de Airbnb según tipo (septiembre 2018)",
       subtitle = paste("Un total de",nrow(airbnb201809),
                        "anuncios:",
                        nrow(filter(airbnb201809,room_type=="Vivienda completa")),"viviendas,",
                        nrow(filter(airbnb201809,room_type=="Habitación privada")),"habitaciones y",
                        nrow(filter(airbnb201809,room_type=="Habitación compartida")),"habitaciones compartidas."),
       x = NULL,
       y = NULL,
       caption = "Datos: datahippo.org. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~room_type)
dev.off()  


# ----- Prepara datos por distritos ------------------------------------------------------------------------------------
# Añade datos de vivienda
por_barrios <- merge(airbnb_20172018_barr,viviendas_barrios, by.x="neighbourhood",by.y="Nom_Barri")

#  Ratio Airbnb por barrios: barras ------------------------------------------------------------------------------------
por_barrios$ratio2017_room_type <- round(por_barrios$anuncios2017 / por_barrios$Domicilis *100,digits = 2)
por_barrios$ratio2018_room_type <- round(por_barrios$anuncios2018 / por_barrios$Domicilis *100,digits = 2)
por_barrios$ratio2017_listings <- round(por_barrios$total2017 / por_barrios$Domicilis *100,digits = 2)
por_barrios$ratio2018_listings <- round(por_barrios$total2018 / por_barrios$Domicilis *100,digits = 2)

# provides position for reordering
por_barrios$pos_airbnb_2018 <- 1
por_barrios[ order(-por_barrios[,6]), ]$pos_airbnb_2018 <- 1:17
por_barrios$pos_ratio2018 <- 1
por_barrios[ order(-por_barrios[,"ratio2018"]), ]$pos_ratio2018 <- 1:17

# Ratio anuncios por viviendas en barrios. Barras
png(filename="images/airbnb/ratio-airbnb-barrios-barcelona-201809.png",width = 900,height = 1400)
ggplot(por_barrios,aes(x = reorder(neighbourhood, ratio2018_listings), y = ratio2018_listings)) + #order by Value or by -pos_ratio2018
  geom_col(position = "dodge") +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Septiembre 2018. Barcelona",
       subtitle = "Ratio de anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb (septiembre 2018). Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2018_listings),
            position = position_dodge(width = 1), hjust = -0.1,
            size=3,color="#777777") +
  coord_flip()
dev.off()

# Ratio anuncios pisos completos por viviendas en barrios. Barras
png(filename="images/airbnb/ratio-airbnb-pisos-completos-barrios-barcelona-201809.png",width = 900,height = 1400)
por_barrios %>% filter( room_type == "Vivienda completa") %>%
ggplot(aes(x = reorder(neighbourhood, ratio2018_room_type), y = ratio2018_room_type)) + #order by Value or by -pos_ratio2018
  geom_col(position = "dodge") +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Septiembre 2018. Barcelona",
       subtitle = "Ratio de anuncios de viviendas completas en Airbnb por cada 100 viviendas",
       y = "ratio viviendas compeltas de Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb (septiembre 2018). Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2018_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=3,color="#777777") +
  coord_flip()
dev.off()


# -------- Gráfico mariposa ratio anuncios airbnb 1 year--------
# inspired/copied from https://github.com/meneos/R_Dataviz/blob/master/RENTABILIDAD%20INMUEBLES%20MADRID/rentabilidad_distritos.R
# library(gridExtra)

# provides position for reordering
# create dataframe only with viviendas
por_barrios.viviendas <- por_barrios %>% filter( room_type == "Vivienda completa")
# ordena por numero de anuncios
por_barrios.viviendas$pos_airbnb_2018 <- 1
por_barrios.viviendas[ order(-por_barrios.viviendas[,"ratio2018_listings"]), ]$pos_airbnb_2018 <- 1:nrow(por_barrios.viviendas)
# ordena por ratio 2018
por_barrios.viviendas$pos_ratio2018 <- 1
por_barrios.viviendas[ order(-por_barrios.viviendas[,"ratio2018_listings"]), ]$pos_ratio2018 <- 1:nrow(por_barrios.viviendas)

# por_barrios <- merge(por_barrios, select( por_barrios.viviendas, neighbourhood,pos_airbnb_2018,pos_ratio2018), by.x="neighbourhood",by.y="neighbourhood" )
por_barrios <- full_join(por_barrios, select( por_barrios.viviendas, neighbourhood,pos_airbnb_2018,pos_ratio2018), by ="neighbourhood"  )

plot1 <-
  por_barrios %>% filter ( !(neighbourhood == "Can Peguera" | neighbourhood == "Ciutat Meridiana" | neighbourhood == "Montbau" | neighbourhood == "Torre Baró" )) %>%
ggplot(aes(x = reorder(neighbourhood, -pos_ratio2018), y = ratio2018_listings)) + #order by Value or by -pos_ratio2018
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,21), expand = c(0,1)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "",
       subtitle = "Anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAIrbnb (septiembre 2018). Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2018_listings),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777") +
  coord_flip()

# Barras numero de anuncios 
plot2 <- 
por_barrios %>% filter ( !(neighbourhood == "Can Peguera" | neighbourhood == "Ciutat Meridiana" | neighbourhood == "Montbau" | neighbourhood == "Torre Baró" )) %>%
  ggplot(aes(x = reorder(neighbourhood, -pos_ratio2018), y = anuncios2018, fill=room_type)) + #order by Value or by -pos_ratio2018
    geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_reverse(limits = c(2200,0), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0.2),
    plot.caption = element_text(margin = margin(20,0,0,0)), 
    # axis.text.y = element_text(hjust = 0.5),
    axis.text.y = element_blank(), #uncomment to plot the names of barrios
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",
    plot.margin = unit(c(0.3,0,0.4,4), "cm")
  ) +
  labs(title = "Presencia de Airbnb en barrios de Barcelona",
       subtitle = "Número de anuncios de Airbnb",
       y = "nº anuncios Airbnb",
       x = NULL,
       caption = "") +
  geom_text(aes(label = total2018,y = total2018+2), hjust = 1,
            size=4,color="#777777") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-airbnb-barrios-barcelona-201819.png",width = 900,height = 1000)
grid.arrange(plot2,plot1,ncol=2,widths=c(2,3))
dev.off()

#  Mariposa para solo viviendas -----------------------------------------------------------------------------
plot1v <- ggplot(por_barrios.viviendas,aes(x = reorder(neighbourhood, -pos_ratio2018), y = ratio2018_room_type)) + #order by Value or by -pos_ratio2018
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,21), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="bottom") +
  labs(title = "",
       subtitle = "Anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAIrbnb (septiembre 2018). Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2018_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777") +
  coord_flip()

# Barras numero de anuncios 
plot2v <- ggplot(por_barrios.viviendas,aes(x = reorder(neighbourhood, -pos_ratio2018), y = anuncios2018)) + #order by Value or by -pos_ratio2018
  geom_col(position = "dodge")+
  scale_y_reverse(limits = c(2200,0), expand = c(0,0)) + #invert axis
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0.2),
    plot.caption = element_text(margin = margin(20,0,0,0)), 
    # axis.text.y = element_text(hjust = 0.5),
    axis.text.y = element_blank(), #uncomment to plot the names of barrios
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="bottom",
    plot.margin = unit(c(0.3,0,0.4,4), "cm")
  ) +
  
  labs(title = "Presencia de Airbnb en barrios de Barcelona por nº de anuncios",
       subtitle = "Número de anuncios de Airbnb",
       y = "nº anuncios Airbnb",
       x = NULL,
       caption = "") +
  geom_text(aes(label = anuncios2018,y = anuncios2018+5),
            position = position_dodge(width = 1), hjust = 1, 
            size=4,color="#777777") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-viviendas-completas-airbnb-barrios-barcelona-201819.png",width = 900,height = 1000)
grid.arrange(plot2v,plot1v,ncol=2,widths=c(2,3))
dev.off()

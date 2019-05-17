# Análisis de Airbnb de un data set
# -Habitaciones vs Viviendas completas
# -numero anuncios por barrio y distrito
# -ratio anuncios por barrio por vivienda
# -mapas de coropletas de ratio 
# -exporta a geojson

# ---- Load libraries -----
library(tidyverse)
# read shapes
library(rgdal)
library(ggmap) #for theme nothing
library(reshape)
library(gridExtra)
library(gsubfn) # select text in the parenthesis with regex
library(spdplyr) #easier to dpplyr with spatialpolygons

# settings ---------
# emulating ggplot color palette https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n <- 3
cols <- gg_color_hue(n)
# sets color discrete scale
colores <- c("#F8766D","#00BA38","#ffc02a")

# ------ Load files ----------
# load shapes
barrios <- readOGR("data/original/contornos/barrios_geo.json")
distritos <- readOGR("data/original/contornos/distritos_geo.json")
# mar <- readOGR("data/original/shapes/mar-donostia.geojson") TODO
municipios <- readOGR("data/original/contornos/recintos_municipales_inspire_peninbal_wgs84_export_provincia-barcelona_geo.json")

viviendas_barrios <- read.delim("data/original/demografia-vivienda/habitantes-viviendas-por-barrios_padron2018_ayto-barcelona.csv",sep = ",")
# viviendas_distritos <- read.delim("data/original/numero-viviendas-por-distrito-valencia-censo2011-ine.csv",sep = ",")
# habitantes_distritos <- read.delim("data/original/poblacion-por-distrito-valencia-censo2011-ine.csv",sep = ",")

# Airbnb listings 2019-03-08
# Original InsideAirbnb file
# airbnbsource <- read.delim("data/original/airbnb/190308/listings_summary_barcelona_insideairbnb.csv",sep = ",")
# You need a properly processed file with points-in-polygons.R script. Inside AIrbnb counts point by vecinity, it seems
airbnbsource <- read.delim("data/output/airbnb/190308/listings-airbnb-provincia-barcelona_insideairbnb_barrio-distrito.csv",sep = ",")

# Translate room type
levels(airbnbsource$room_type) <- c("Vivienda completa","Habitación privada","Habitación compartida")
# Creates simplified version of room type:
airbnbsource <- mutate(airbnbsource, room_type_s = ifelse( room_type == "Vivienda completa", "Vivienda", "Habitación"))

# output number of listings per barrio 
barrios_n_listings <- as.data.frame(table(airbnbsource$barrio)) # You can also use $neighbourhood original insideairbnb vale
names(barrios_n_listings) <- c("barrio","n_listings")
write.csv(as.data.frame(barrios_n_listings), file = "data/output/airbnb/190308/barrios-n-listings-airbnb-valencia-insideairbnb.csv", row.names = FALSE)

# output number of listings per distrito 
distritos_n_listings <- as.data.frame(table(airbnbsource$distrito)) # You can also use $neighbourhood_group original insideairbnb vale
names(distritos_n_listings) <- c("distrito","n_listings")
write.csv(as.data.frame(barrios_n_listings), file = "data/output/airbnb/190308/distritos-n-listings-airbnb-valencia-insideairbnb.csv", row.names = FALSE)


# Clasificación por distritos ---------------------------------------------------------
airbnb.distrito.room_type.2 <- airbnbsource %>% 
  group_by(distrito,room_type_s) %>% 
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count)

# Clasificación por barrios ---------------------------------------------------------
airbnb.barrio.room_type.2 <- airbnbsource %>% 
  group_by(barrio,room_type_s) %>% #uses simplified version or roomtype
  summarise(count=n()) %>% 
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count)

# Plots barrios y distritos -------------

png(filename="images/airbnb/hab-viv-barras-airbnb-distritos-barcelona-201903a.png",width = 700,height = 500)
filter(airbnb.distrito.room_type.2, !is.na(distrito)) %>%
  ggplot(aes(x = reorder(distrito,suma), y = count, fill=room_type_s)) +
  # "reverse" es la clave para reordenar las barras y que coincida con leyenda https://github.com/tidyverse/ggplot2/issues/1837
  geom_col(position = position_stack(reverse = TRUE)) +
  # scale_y_continuous(expand = c(0, 30)) + #limits = c(0,950),
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=colores) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(),
    legend.position="top",
    axis.text.y = element_text(margin = margin(0,-20,0,0))
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por distritos en Barcelona",
       subtitle = "Marzo 2019",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento") +
  # partial text
  geom_text(aes(label = count,group=room_type_s), 
            data= filter(airbnb.distrito.room_type.2, !is.na(distrito) & count > 50) , 
            position = position_stack(reverse = TRUE,vjust = 0.5),size=3,color="#FFFFFF",family = "Roboto Condensed") +
  # total text
  geom_text(aes(label = suma, y = suma+150),
            position = "dodge",
            size=4,color="#888888",
            family = "Roboto Condensed") +
  coord_flip()
dev.off()

png(filename="images/airbnb/hab-viv-barras-airbnb-barrio-barcelona-201903.png",width = 800,height = 1600)
filter(airbnb.barrio.room_type.2, !is.na(barrio)) %>%
  ggplot(aes(x = reorder(barrio,suma), y = count, fill=room_type_s)) +
  # "reverse" es la clave para reordenar las barras y que coincida con leyenda https://github.com/tidyverse/ggplot2/issues/1837
  geom_col(position = position_stack(reverse = TRUE)) +
  # scale_y_continuous(expand = c(0, 30)) + #limits = c(0,950),
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=colores) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 17) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top",
    axis.text.y = element_text(margin = margin(0,-20,0,0))
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Barcelona",
       subtitle = "Marzo 2019",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento") +
  # partial text
  geom_text(aes(label = count,group=room_type_s), 
            data=filter(airbnb.barrio.room_type.2, !is.na(barrio) & count > 100), 
            position = position_stack(reverse = TRUE,vjust = 0.5),size=3.5,color="#FFFFFF",family = "Roboto Condensed") +
  # total text
  geom_text(aes(label = suma, y = suma+50),
            position = "dodge",
            size=4,color="#888888",
            family = "Roboto Condensed") +
  coord_flip()
dev.off()

# top 15
png(filename="images/airbnb/hab-viv-barras-airbnb-barrio-top10-barcelona-201903.png",width = 800,height = 600)
filter(airbnb.barrio.room_type.2, !is.na(barrio) & suma > 700) %>%
  ggplot(aes(x = reorder(barrio,suma), y = count, fill=room_type_s)) +
  # "reverse" es la clave para reordenar las barras y que coincida con leyenda https://github.com/tidyverse/ggplot2/issues/1837
  geom_col(position = position_stack(reverse = TRUE)) +
  # scale_y_continuous(expand = c(0, 30)) + #limits = c(0,950),
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_fill_manual(values=colores) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 17) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top",
    axis.text.y = element_text(margin = margin(0,-20,0,0))
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por barrios en Barcelona. Top 10",
       subtitle = "Marzo 2019",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento") +
  # partial text
  geom_text(aes(label = count,group=room_type_s),
            position = position_stack(reverse = TRUE,vjust = 0.5),size=4,color="#FFFFFF",family = "Roboto Condensed") +
  # total text
  geom_text(aes(label = suma, y = suma+50),
            position = "dodge",
            size=5,color="#888888",
            family = "Roboto Condensed") +
  coord_flip()
dev.off()

png(filename="images/airbnb/hab-viv-barras-airbnb-distritos-barcelona-201903-facet.png",width = 800,height = 500)
filter(airbnb.distrito.room_type.2, !is.na(distrito)) %>%
  ggplot(aes(x = reorder(distrito,suma), y = count, fill=room_type_s)) +
  # "reverse" es la clave para reordenar las barras y que coincida con leyenda https://github.com/tidyverse/ggplot2/issues/1837
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE),expand = c(0, 30),limits = c(0,3700)) +
  scale_fill_manual(values=colores) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position="top",
    axis.text.y = element_text(margin = margin(0,-5,0,0))
  ) +
  labs(title = "Habitaciones y viviendas de Airbnb por distritos en Barcelona",
       subtitle = "Marzo 2019",
       y = "número de anuncios",
       x = NULL,
       caption = "Datos: Insideairbnb. Gráfico: lab.montera34.com/airbnb",
       fill="Tipo de alojamiento") +
  # total text
  geom_text(aes(label = count, y = count+10),
            # position = "dodge",
            size=5,color="#888888",hjust=0,
            family = "Roboto Condensed") +
  coord_flip() +
  facet_wrap(~room_type_s)
dev.off()

# -----------Mapas---------------
png(filename="images/airbnb/hab-viv-mapa-airbnb-barcelona-feb2019.png",height = 600,width = 900)
ggplot() + 
  # geom_path(data=municipios,aes(x=long, y=lat,group=group), colour="grey",size = 0.3)+
  geom_path(data=distritos,aes(x=long, y=lat,group=group), colour="grey",size = 0.3)+
  # scale_fill_manual(values=cbPalette) +
  geom_point(data=airbnbsource,aes(x=longitude, y=latitude), #,colour=room_type
             alpha=0.09,size = 0.5)+
  theme_minimal(base_family = "Roboto Condensed", base_size = 19) +
  # theme_nothing(legend = TRUE) +
  # theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position="top",
        axis.text = element_blank()) +
  coord_fixed(1.3) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.9))) +
  labs(title = "Anuncios de Airbnb según tipo en Barcelona. Marzo 2019",
       subtitle = paste("", format(nrow(airbnbsource), big.mark = ".", scientific = FALSE),
                        "anuncios:",
                        format(nrow(filter(airbnbsource,room_type=="Vivienda completa")), big.mark = ".", scientific = FALSE),"viviendas,",
                        format(nrow(filter(airbnbsource,room_type=="Habitación privada")), big.mark = ".", scientific = FALSE),"habitaciones y",
                        format(nrow(filter(airbnbsource,room_type=="Habitación compartida")), big.mark = ".", scientific = FALSE),"habitaciones compartidas."),
       x = NULL,
       y = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  # coord_flip( ) +
  facet_wrap(~room_type_s, ncol = 2)
dev.off()  


# ----- Prepara datos por barrios y distritos ------------------------------------------------------------------------------------
# add district and barrio code to listings
barrios_simple <- unique(barrios@data[,c("C_Distri","N_Distri","C_Barri","N_Barri")])
airbnbsource <- left_join(airbnbsource, barrios_simple, by = c( "barrio" = "N_Barri" ))

# agrupa datos añadiendo código de barrio
airbnb.barrios <- airbnbsource %>%
  group_by(barrio,C_Barri) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  arrange(-count)

# adds room_type
airbnb.barrios.room_type <- airbnbsource %>%
  group_by(barrio,C_Barri,room_type) %>% 
  summarise(count=n()) %>%  #suma el número de items por barrio
  mutate(suma=sum(count)) %>%
  ungroup() %>%
  arrange(-count)

# agrupa datos añadiendo código de distrito
airbnb.distritos <- airbnbsource %>% 
  group_by(distrito,C_Distri) %>% 
  summarise(count=n()) %>% 
  ungroup() %>%
  arrange(-count)

# nota para Valencia: aquí se pierden 235 de ser contados porque caen fuera de los contornos de los barrios (ver airbnb.distritos). Insideairbnb si los detecta como que están
# barrios. Si se le quita el coddistrit del gropup_by pilla todo os anuncios
# sum(por_barrios[is.na(por_barrios$coddistbar),]$count)

# Añade datos de vivienda a los datos de airbnb
por_barrios <- merge(airbnb.barrios,viviendas_barrios, by.x="barrio",by.y="Nom_Barri")

# igual para room_type
por_barrios.room_type <-merge(airbnb.barrios.room_type, viviendas_barrios, by.x="barrio",by.y="Nom_Barri")

# # une datos de airbnb con datos de distritos
# por_distritos <- merge(airbnb.distritos,viviendas_distritos, by="coddistrit")
# # removes rows without código, it means that points are outside area
# por_distritos <- por_distritos %>% filter( !is.na(coddistrit) )

#  Ratio Airbnb por barrios: barras ------------------------------------------------------------------------------------
por_barrios$ratio2019_listings <- round(por_barrios$count / por_barrios$Domicilis *100,digits = 1)
por_barrios.room_type$ratio2019_room_type <- round(por_barrios.room_type$count / por_barrios.room_type$Domicilis *100,digits = 1)

# provides position for reordering in mariposa bars graph
por_barrios$pos_airbnb_2019 <- 1
# ordena por la columna del count airbnb
por_barrios[ order(-por_barrios[,"count"]), ]$pos_airbnb_2019 <- 1:nrow(por_barrios)
por_barrios$pos_ratio2019 <- 1
# ordena por la columna del ratio
por_barrios[ order(-por_barrios[,"ratio2019_listings"]), ]$pos_ratio2019 <- 1:nrow(por_barrios)

# Ratio anuncios por viviendas en barrios. Barras ------
png(filename="images/airbnb/ratio-listings-airbnb-anuncios-barrios-barcelona-201903.png",width = 900,height = 1400)
por_barrios %>% filter(!is.na(ratio2019_listings)) %>%
  ggplot(aes(x = reorder(barrio, ratio2019_listings), y = ratio2019_listings)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge") +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Marzo 2019. Barcelona",
       subtitle = "Ratio de anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_listings),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",
            family = "Roboto Condensed") +
  coord_flip()
dev.off()

# Ratio anuncios por viviendas en barrios. Barras. Top 15-------
png(filename="images/airbnb/ratio-listings-airbnb-anuncios-barrios-barcelona-201903_top15-blues.png",width = 750,height = 600)
por_barrios %>% arrange(-ratio2019_listings) %>% head(15) %>%
  ggplot(aes(x = reorder(barrio, ratio2019_listings), y = ratio2019_listings)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge", aes(fill=ratio2019_listings)) +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  scale_fill_gradient( low = "#ededed", high= "#0cb2ff") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 18) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 1),
    plot.subtitle = element_text(hjust = 1)
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Top15. Marzo 2019. Barcelona",
       subtitle = "Ratio de anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_listings, y= ratio2019_listings/2 ),
            hjust = 0.5,
            size=5,color="#777777",family = "Roboto Condensed") +
  coord_flip()
dev.off()

# Ratio anuncios pisos completos por viviendas completas en barrios. Barras -------
png(filename="images/airbnb/ratio-airbnb-viviendas-completas-barrios-barcelona-201903.png",width = 900,height = 1400)
por_barrios.room_type %>% filter( room_type == "Vivienda completa") %>%
  ggplot(aes(x = reorder(barrio, ratio2019_room_type), y = ratio2019_room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Marzo 2019. Barcelona",
       subtitle = "Ratio de anuncios de viviendas completas en Airbnb por cada 100 viviendas",
       y = "ratio viviendas completas de Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()
dev.off()

# Ratio anuncios habitaciones privadas por viviendas completas en barrios. Barras -------
png(filename="images/airbnb/ratio-airbnb-habitacion-privada-barrios-barcelona-201903.png",width = 900,height = 1400)
por_barrios.room_type %>% filter( room_type == "Habitación privada" ) %>%
  ggplot(aes(x = reorder(barrio, ratio2019_room_type), y = ratio2019_room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Marzo 2019. Barcelona",
       subtitle = "Ratio de anuncios de habitaciones en Airbnb por cada 100 viviendas",
       y = "ratio habitaciones privadas de Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()
dev.off()

# Ratio anuncios pisos completos por viviendas completas en barrios. Barras. Top15 -----
png(filename="images/airbnb/ratio-airbnb-viviendas-completas-barrios-barcelona-201903_top15-blues.png",width = 600,height = 600)
por_barrios.room_type %>% filter( room_type == "Vivienda completa") %>% arrange(-ratio2019_room_type) %>% head(15) %>%
  ggplot(aes(x = reorder(barrio, ratio2019_room_type), y = ratio2019_room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge",aes(fill=ratio2019_room_type)) +
  scale_fill_gradient( low = "#ededed", high= "#44c0fa") +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) + 
  labs(title = "Presencia de Airbnb en barrios. Top15. Marzo 2019. Barcelona",
       subtitle = "Ratio de anuncios de viviendas completas en Airbnb por cada 100 viviendas",
       y = "ratio viviendas compeltas de Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()
dev.off()

# -------- Gráfico mariposa ratio anuncios airbnb 1 year--------
# inspired/copied from https://github.com/meneos/R_Dataviz/blob/master/RENTABILIDAD%20INMUEBLES%20MADRID/rentabilidad_distritos.R
# library(gridExtra)

# provides position for reordering
# create dataframe only with viviendas
por_barrios.viviendas <- por_barrios.room_type %>% filter( room_type == "Vivienda completa")
# ordena por numero de anuncios
por_barrios.viviendas$pos_airbnb_viv_2019 <- 1
por_barrios.viviendas[ order(-por_barrios.viviendas[,"count"]), ]$pos_airbnb_viv_2019 <- 1:nrow(por_barrios.viviendas)
# ordena por ratio 2019
por_barrios.viviendas$pos_ratio_viv_2019 <- 1
por_barrios.viviendas[ order(-por_barrios.viviendas[,"ratio2019_room_type"]), ]$pos_ratio_viv_2019 <- 1:nrow(por_barrios.viviendas)

# por_barrios <- merge(por_barrios, select( por_barrios.viviendas, neighbourhood,pos_airbnb_2019,pos_ratio2019), by.x="neighbourhood",by.y="neighbourhood" )
por_barrios <- full_join(por_barrios, select( por_barrios.viviendas,barrio,ratio2019_room_type, pos_airbnb_viv_2019,pos_ratio_viv_2019), by ="barrio"  )

por_barrios.room_type <- full_join(por_barrios.room_type, select( por_barrios.viviendas, barrio,pos_airbnb_viv_2019,pos_ratio_viv_2019), by ="barrio"  )
por_barrios.room_type <- full_join(por_barrios.room_type, select( por_barrios, barrio,pos_ratio2019,pos_airbnb_2019), by ="barrio"  )

# set color for distritos
colourCountdistrict <- length(unique(airbnbsource$distrito))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

plot1 <-
  por_barrios %>%
  ggplot(aes(x = reorder(barrio, -pos_ratio2019), y = ratio2019_listings, fill=Nom_Districte)) + #order by Value or by -pos_ratio2019
  scale_fill_manual(values = getPalette(colourCountdistrict)) +
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,20), expand = c(0,1)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(axis.title.x = element_text(margin = margin(20,0,0,0)),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.2),
        plot.caption = element_text(margin = margin(20,0,0,0)), 
        axis.text.y = element_text(hjust = 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        # legend.text = element_text(size = 10),
        # legend.title = element_text(size = 11),
        legend.position=c(.6,.2)) +
  labs(title = "",
       subtitle = "Anuncios de Airbnb por cada 100 viviendas",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       fill = "Distrito",
       caption = "Datos: InsideAIrbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_listings),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()

# Barras numero de anuncios 
plot2 <-
  por_barrios.room_type %>%
  ggplot(aes(x = reorder(barrio, -pos_ratio2019), y = count, fill=room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_reverse(limits = c(2100,0), expand = c(0,50)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    axis.title.x = element_text(margin = margin(20,0,0,0)),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0.2),
    plot.caption = element_text(margin = margin(20,0,0,0)), 
    # axis.text.y = element_text(hjust = 0.5),#uncomment to plot the names of barrios
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position=c(.4,.2),
    plot.margin = unit(c(0.31,0,0.4,4), "cm")
  ) +
  labs(title = "Presencia de Airbnb en barrios de Barcelona. Marzo 2019",
       subtitle = "Número de anuncios de Airbnb",
       y = "nº anuncios Airbnb",
       x = NULL,
       fill= "Tipo de alojamiento",
       caption = "") +
  geom_text(aes(label = suma,y = suma+2), hjust = 1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-airbnb-barrios-barcelona-201903-color-distrito.png",width = 900,height = 1000)
grid.arrange(plot2,plot1,ncol=2,widths=c(2,3))
dev.off()

# Mariposa para solo viviendas -----------------------------------------------------------------------------
plot1v <-
  ggplot(por_barrios.viviendas,aes(x = reorder(neighbourhood_cleansed, -pos_ratio_viv_2019), y = ratio2019_room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,15), expand = c(0,1)) +
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
       caption = "Datos: InsideAIrbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label = ratio2019_room_type),
            position = position_dodge(width = 1), hjust = -0.1,
            size=4,color="#777777",family = "Roboto Condensed") +
  coord_flip()

# Barras numero de anuncios 
plot2v <-
  ggplot(por_barrios.viviendas,aes(x = reorder(neighbourhood_cleansed, -pos_ratio_viv_2019), y = count, fill=room_type)) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge")+
  scale_y_reverse(limits = c(600,0), expand = c(0,50)) + #invert axis
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
  
  labs(title = "Anuncios de viviendas completas en barrios de Barcelona",
       subtitle = "Número de anuncios de viviendas completas de Airbnb",
       y = "nº anuncios viviendas Airbnb",
       x = NULL,
       caption = "") +
  geom_text(aes(label = count,y = count+5),
            position = position_dodge(width = 1), hjust = 1, 
            size=4,color="#777777") +
  coord_flip()

png(filename="images/airbnb/barras-mariposa-n-y-ratio-viviendas-completas-airbnb-barrios-barcelona-201903.png",width = 900,height = 1000)
grid.arrange(plot2v,plot1v,ncol=2,widths=c(2,3))
dev.off()


# Mapas de coropletas ---------------------------------------------------------------------
library(tmap)
library(gpclib)
library(maptools)

# load fresh data
# barrios <-   readOGR("data/original/shapes/barrios.valencia.wgs84.geojson") #TODO

# prepare and prepare data -----------------
# adds 0 to coddistbat to allow join
# por_barrios$coddistbar <- as.character(por_barrios$coddistbar)
# add leading 0 to barrios with only two cifras in coddistbar
# por_barrios <- por_barrios %>% mutate(
#   coddistbar = ifelse( nchar(coddistbar) == 2, 
#                        paste0("0",coddistbar),
#                        coddistbar
#   )
# )

# por_barrios.viviendas$bario <- as.character(por_barrios.viviendas$coddistbar)
# add leading 0 to barrios with only two cifras in coddistbar
# por_barrios.viviendas <- por_barrios.viviendas %>% mutate(
#   coddistbar = ifelse( nchar(coddistbar) == 2, 
#                        paste0("0",coddistbar),
#                        coddistbar
#   )
# )

# adds airbnb data to shapes
barrios@data <- left_join(barrios@data, por_barrios.viviendas, by="barrio")
barrios@data <- left_join(barrios@data, select(por_barrios,coddistbar,ratio2019_listings),by="barrio")

# tmap numer of listings MAP ----------------
colores <- c("#ededed", "#0cb2ff")
breaks.n <- c(seq(0,2000,by = 250))

# png(filename="images/airbnb/mapa-coropletas-numero-anuncios-barcelona-201909.png",width = 500,height = 600)
tm_shape(barrios) +
  tm_polygons(col="count",
              palette = colores, 
              # breaks = breaks.n,
              title = "anuncios", 
              border.alpha = 1, lwd = 0.2, legend.show = T, legend.outside = T,
              textNA="sin anuncios") +
  tm_layout(between.margin = 5, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            title = "Anuncios airbnb" ,
            title.fontface = "bold",
            legend.format = list(text.separator = "-" )
  ) +
  tm_legend(legend.text.size = 0.5, 
            legend.title.size = 2) 
# dev.off()

# tmap Ratio anuncios / 100 viviendas ----------------
breaks.ratio <- c(0,1,2,4,6,8,10,12,14,16)

# to select which labels are displayed
barrios_select <- barrios %>% filter(ratio2019_listings >3)

png(filename="images/airbnb/mapa-coropletas-ratio-anuncios-100viv-barcelona-201903.png",width = 500,height = 700)
tm_shape(barrios) +
  tm_polygons(col="ratio2019_listings",
              palette = colores,
              breaks = breaks.ratio,
              title = "",
              border.alpha = 1, lwd = 0.7,
              textNA="sin anuncios") +
  # to display labels on top
  # tm_shape(barrios_select) +
  #   tm_text("nombre", size = 0.8, col = "black" ) +
  # tm_shape(distritos) +
  #   tm_borders(lwd=0.9, col = "black") +
  tm_shape(municipios) +
  tm_borders(lwd=0.1, col = "black") +
  tm_layout(between.margin = 5, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            main.title = "Anuncios airbnb por 100 domicilios",
            main.title.size = 1.1,
            main.title.fontface = "bold",
            title = "" ,
            title.size = 2,
            title.position = c("center","top"),
            title.fontface = "bold",
            legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.format = list(text.separator = "-")
  ) 
dev.off()

# Ratio anuncios vivienda completa / 100 viviendas mapa estático------------------

# selects only few variables
airbnb <- airbnbsource %>% select(longitude,latitude,room_type)

# creates spatialpointsdataframe of airbnb listings
coordinates(airbnb) <- ~longitude+latitude

# to select which labels are displayed
barrios_select <- barrios %>% filter(ratio2018_room_type > 1)
distritos_select <- distritos %>% filter( nombre == "CIUTAT VELLA" 	| nombre == "ALGIROS" ) #| nombre == "POBLATS MARITIMS"

png(filename="images/airbnb/mapa-coropletas-ratio-anuncios-vivcompletas-100viv-barcelona-201903.png",width = 2500,height = 2700)
tm_polygons( col="ratio2018_room_type",
             palette = colores,
             breaks = breaks.ratio,
             title = "",
             border.alpha = 1, lwd = 0.7,
             textNA="sin anuncios"
) +
  # tm_shape(barrios_select) +
  tm_shape(barrios) +
  tm_text("nombre", size = 1.2, col = "black", remove.overlap = TRUE) +
  # airbnb listings
  tm_shape(airbnb) +
  tm_dots(alpha=0.3,size=0.06) + #col = "room_type",
  tm_shape(distritos) +
  tm_borders(lwd=0.9, col = "black") +
  # nombre de distritos
  # tm_shape(distritos) +
  #   tm_text("nombre", size = 1.5, col = "#000099", just="bottom") + #, just="left"
  tm_shape(municipios) +
  tm_borders(lwd=0.1, col = "black") +
  tm_layout(between.margin = 5, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            main.title = "Anuncios viviendas completas por 100 domicilios",
            main.title.size = 1.1,
            main.title.fontface = "bold",
            title = "" ,
            title.size = 2,
            title.position = c("center","top"),
            title.fontface = "bold",
            legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.format = list(text.separator = "-")
  ) 
dev.off()

# Ratio anuncios vivienda completa / 100 viviendas mapa interactivo------------------

# selects only few variables
airbnb <- airbnbsource %>% select(longitude,latitude,room_type)

# creates spatialpointsdataframe of airbnb listings
coordinates(airbnb) <- ~longitude+latitude

# to select which labels are displayed
barrios_select <- barrios %>% filter(ratio2018_room_type > 2)
distritos_select <- distritos %>% filter( nombre == "CIUTAT VELLA" 	| nombre == "ALGIROS" ) #| nombre == "POBLATS MARITIMS"

interactive_map <- tm_shape(barrios) +
  tm_polygons( col="ratio2018_room_type",
               palette = colores,
               breaks = breaks.ratio,
               title = "anuncios viviendas Airbnb / 100 viviendas",
               border.alpha = 1, lwd = 0.7,
               textNA="sin anuncios",
               id = "nombre",
               # popup.vars=TRUE
               # popup.vars=c("count", "ratio2018_room_type","seabstienen"), #selects data in popup
               popup.vars=c( # "barrio" = "nombre",
                 "ratio anuncios Airbnb / 100 viviendas"="ratio2019_listings",
                 "anuncios Airbnb"="suma",
                 "anuncios viviendas Airbnb"="count",
                 "ratio anuncios vivienda / 100 viviendas"="ratio2018_room_type",
                 "viviendas existentes" = "Total",
                 "viviendas principales" = "Principales")
  ) +
  # nombre barrios
  tm_shape(barrios_select) +
  # tm_shape(barrios) +
  tm_text("nombre", size = 1.3, col = "black", remove.overlap = TRUE,shadow = TRUE) +
  # airbnb listings
  tm_shape(airbnb) +
  # tm_dots(alpha=0.3,size=0.06) + #col = "room_type",
  tm_dots(alpha=0.3,size=0.0006,popup.vars=FALSE) + #col = "room_type",
  tm_shape(distritos) +
  tm_borders(lwd=0.9, col = "black") +
  # nombre de distritos
  # tm_shape(distritos) +
  #   tm_text("nombre", size = 1.5, col = "#000099", just="bottom") + #, just="left"
  tm_shape(municipios) +
  tm_borders(lwd=0.1, col = "black") +
  tm_layout(between.margin = 5, frame = FALSE,
            fontfamily = "Roboto Condensed", 
            main.title = "Anuncios viviendas completas por 100 domicilios",
            main.title.size = 1.1,
            main.title.fontface = "bold",
            fontface = "bold",
            title = "" ,
            title.size = 2,
            title.position = c("center","top"),
            title.fontface = "bold",
            legend.title.size = 1,
            legend.text.size = 1,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 0.8,
            legend.format = list(text.separator = "-")
  ) +
  # zoom view to district selected
  tm_view(bbox = barrios %>% filter(nombre == "LA PETXINA" 	| nombre == "LA MALVA-ROSA" | nombre == "NA ROVELLA"))

tmap_save(interactive_map,"/home/numeroteca/sites/airbnb/barcelona/output/html/ratio-vivcompletaairbnb-x-viv-coropletas-barcelona-201903.html")



# Plazas de Airbnb por habitante -------------------------------
# agrupa datos añadiendo código de distrito
airbnb.distritos.plazas <- airbnbsource %>% 
  group_by(distrito,coddistrit) %>% 
  summarise(plazas=sum(accommodates),anuncios=n()) %>% 
  ungroup() %>%
  arrange(-plazas)

airbnb.distritos.plazas.room_type <- airbnbsource %>% filter(room_type=="Vivienda completa") %>%
  group_by(distrito,coddistrit,room_type) %>% 
  summarise(viviendas_completas=n()) %>%  #suma el número de items por barrio
  filter( !is.na(coddistrit) ) %>% 
  ungroup() %>% 
  arrange(-viviendas_completas) 

# une datos de airbnb con datos de distritos
por_distritos.plazas <- full_join(airbnb.distritos.plazas,select(habitantes_distritos,-distrito), by="coddistrit")
por_distritos.plazas <- full_join(por_distritos.plazas,select(airbnb.distritos.plazas.room_type,-distrito,-room_type), by="coddistrit")

# removes rows without código, it means that points are outside area
por_distritos.plazas <- por_distritos.plazas %>% filter( !is.na(coddistrit) )

# añade datos de vivienda
por_distritos.plazas <- full_join(por_distritos.plazas,select(por_distritos,-distrito,-count), by="coddistrit")

por_distritos.plazas$ratio2019_plazasxhab <- round(por_distritos.plazas$plazas/ por_distritos.plazas$poblacion *100,digits = 2)
por_distritos.plazas$ratio2019_airbnbxviv <- round(por_distritos.plazas$anuncios/ por_distritos.plazas$Total *100,digits = 2)
por_distritos.plazas$ratio2019_airbnbvivxviv <- round(por_distritos.plazas$viviendas_completas/ por_distritos.plazas$Total *100,digits = 2)

# convierte a formato largo
dist.plazas_long <- por_distritos.plazas %>% select(distrito,ratio2019_plazasxhab,ratio2019_airbnbxviv,ratio2019_airbnbvivxviv) %>%
  gather("tipo","ratio",-distrito)

write.csv(por_distritos.plazas %>% select(distrito,ratio2019_plazasxhab,ratio2019_airbnbxviv,ratio2019_airbnbvivxviv,anuncios), file = "data/output/airbnb/190227/ratios_airbnb-barcelona-distritos-201903.csv", row.names = FALSE)

# simples gráficos de barras de ratios ---------------------
ggplot(por_distritos.plazas) + 
  geom_col(
    aes(y=ratio2019_plazasxhab,x=reorder(distrito,ratio2019_plazasxhab))) +
  coord_flip()

ggplot(por_distritos.plazas) +
  geom_col(
    aes(y=ratio2019_airbnbxviv,x=reorder(distrito,ratio2019_airbnbxviv))) +
  coord_flip()

ggplot(por_distritos.plazas) + 
  geom_col(
    aes(y=ratio2019_airbnbvivxviv,x=reorder(distrito,ratio2019_airbnbvivxviv))) +
  coord_flip()

# barras ratios comparativas ----------
png(filename="images/airbnb/ratio-listings-airbnb-anuncios-distritos-barcelona-201903.png",width = 600,height = 600)
por_distritos.plazas %>% 
  ggplot(aes(y=ratio2019_hab,x=reorder(distrito,ratio2019_hab))) + #order by Value or by -pos_ratio2019
  geom_col(position = "dodge") +
  # scale_y_continuous(limits = c(0,8.5), expand = c(0,0)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) + 
  labs(title = "Presencia de Airbnb en distritos. Marzo 2019. Barcelona",
       subtitle = "Ratio de plazas de Airbnb por cada 100 habitantes",
       y = "ratio anuncios Airbnb / 100 viviendas",
       x = NULL,
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  geom_text(aes(label =ratio2019_hab),
            position = position_dodge(width = 1), hjust = -0.1,
            size=3,color="#777777") +
  coord_flip()
dev.off()

# barras de los tres ratios ------------------
png(filename="images/airbnb/ratios-airbnb-distritos-barcelona-201903_b.png",width = 900,height = 450)
ggplot(dist.plazas_long) + 
  geom_col(
    aes(x=reorder(distrito,ratio),y=ratio,fill=tipo,position="fill")
  ) +
  geom_text(aes(x=reorder(distrito,ratio), y = ratio+0.1,label = ratio),
            position = "dodge",
            size=3,color="#666666",hjust=0) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Ratios de presencia de Airbnb por distritos. Marzo 2019. Barcelona",
       # subtitle = "",
       y = "ratio",
       x = "",
       fill="ddd",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb"
  ) +
  coord_flip() +
  facet_wrap(~tipo, scales = "free")
dev.off()

# Prepare data for cartograma -----------------------------------------------------
library(geojsonio)

barrios_json <- geojson_json(barrios)
# salva como geojson
geojson_write(barrios_json, file = "tmp/test_geojson")





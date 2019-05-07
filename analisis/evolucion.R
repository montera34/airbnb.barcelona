# evolución
# Este script analiza las diferencias entre los diferentes listings de una localización de InsideAirbnb
# Usa los archivos listings-summary de Insideairbnb 

# 1. Load libraries ----
library(gsubfn)
library(tidyverse)
# extends color paletter
library(RColorBrewer)
library("reshape2")
library(ggthemes) #install ggthemes
library(ggrepel) # for geom_text_repel to prevent overlapping


# 2. Load data: createss long format directly ------------------------------------------------------------------------------
# Get dates when data were scraped ----------------------------------------------------------------------------
# Loads dates with listings data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180814","180911","181010","181107","181210","190114","190206","190308")


# creates dataframe
todos <- ""
# loop to insert every listing in every scraping in a row and add its scraping date
for (i in 1:length(dates)) {
  print(i)
  print(dates[i])
  # basic data
  # neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")),
  #               id,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_group,number_of_reviews,availability_365)
  # full data
  neo <- select(as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/data/listings.csv.gz",sep=""),sep = ",")),
  id,license,room_type,calculated_host_listings_count,neighbourhood,neighbourhood_cleansed,neighbourhood_group_cleansed,number_of_reviews,availability_365)
neo$date <- paste(dates[i],sep="")
todos <- rbind(todos,neo)
}

# removes first empty line
todos <- todos[-1,]
head(todos)

# 3. Prepares data --------------------------------------------------------------------------------

# Translate room type values
levels(todos$room_type) <- c("Piso completo","Habitación","Habitación compartida")
# converts to integer
todos$calculated_host_listings_count <- as.integer(todos$calculated_host_listings_count)
todos$number_of_reviews <- as.integer(todos$number_of_reviews)
todos$availability_365 <- as.integer(todos$availability_365)

# saves the file in data_long dataframe, which was the previous way of doing this
data_long <- todos
# data_long$fechab <- strapplyc( as.character(data_long$date), "([0-9]*)", simplify = TRUE)
data_long$fechab <- as.Date( paste(20,as.character(data_long$date),sep=""), "%Y%m%d")

# table(data_long$calculated_host_listings_count)
# has license
data_long$has.license <- "sin licencia"
data_long[data_long$license != "",]$has.license <- "con licencia"

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


# 4. Plots
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
colourCount <- length(unique(data_long$neighbourhood_cleansed))
getPalette <- colorRampPalette(brewer.pal(9, "Set2"))

dates.count.barrio <- data_long %>% group_by(fechab,neighbourhood_cleansed) %>% summarise(anuncios=n())
# dates.count.barrio2 <- data_long2 %>% filter (exists ==1) %>% group_by(fechab,neighbourhood) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio.png", sep = ""),width = 1000,height = 700)
# plot_a <- dates.count.barrio %>% 
dates.count.barrio %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=5000,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.5) +
  scale_color_manual(values = getPalette(colourCount)) +
  # annotate("text",x=as.Date("2018-05-28"),y=4000,label="acuerdo",color="#000000",size=5,hjust=1) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(
                    fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
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

# counts listings por barrio y license--------------------------------------------------------------
dates.count.barrio.license <- data_long %>% group_by(fechab,neighbourhood_cleansed,has.license) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-license.png", sep = ""),width = 1300,height = 700)
dates.count.barrio.license %>%
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=1300,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
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
  labs(title = "Número de anuncios de Airbnb por barrio y si tiene licencia en Barcelona",
       subtitle = "2015 - marzo 2019 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(~has.license)
dev.off()


# counts listings por barrio y license y room type--------------------------------------------------------------
dates.count.barrio.license.room <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s,has.license) %>% 
  summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-license-room.png", sep = ""),width = 1100,height = 800)
dates.count.barrio.license.room %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=950,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.license.room,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.barrio.license.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.license.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por barrio, si tiene licencia y tipo de alojamiento en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(room_type.s~has.license)
dev.off()

# counts listings por distrito y license y room type--------------------------------------------------------------
dates.count.distrito.license.room <- data_long %>% group_by(fechab,neighbourhood_group_cleansed,room_type.s,has.license) %>% 
  summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-distrito-license-room.png", sep = ""),width = 1100,height = 800)
dates.count.distrito.license.room %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=950,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_group_cleansed,color=neighbourhood_group_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.distrito.license.room,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_group_cleansed)),
                  nudge_x = 35, # adjust the starting y position of the text label
                  size=4,
                  hjust=0,
                  family = "Roboto Condensed",
                  direction="y",
                  segment.size = 0.2,
                  segment.color="#333333",
                  xlim  = c(as.Date(max(dates.count.barrio.room$fechab)),as.Date("2021-06-4"))
  ) +
  scale_y_continuous(limits=c(0, max(dates.count.distrito.license.room$anuncios)),labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlim(as.Date(min(dates.count.barrio.license.room$fechab)),as.Date("2020-06-4")) +
  theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "#555555"),
    # panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "Número de anuncios de Airbnb por distrito, si tiene licencia y tipo de alojamiento en Barcelona",
       subtitle = "2015-2018 (publicados en cada scraping de InsideAirbnb)",
       y = "número de anuncios",
       x = "",
       caption = "Datos: InsideAirbnb. Gráfico: lab.montera34.com/airbnb") +
  facet_wrap(room_type.s~has.license)
dev.off()

# counts listings por barrio y room type--------------------------------------------------------------
dates.count.barrio.room <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s) %>% summarise(anuncios=n())

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-room.png", sep = ""),width = 1300,height = 700)
dates.count.barrio.room %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=1300,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
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
dates.count.barrio.room.reviews <- data_long %>% group_by(fechab,neighbourhood_cleansed,room_type.s,reviews.type) %>% 
  summarise(anuncios=n()) %>% filter(!reviews.type=="")

png(filename=paste("images/airbnb/eliminados/anuncios-por-mes-barrio-room-reviews.png", sep = ""),width = 1100,height = 800)
dates.count.barrio.room.reviews %>% 
  ggplot () +
  annotate("text",x=as.Date("2018-05-25"),y=850,label="acuerdo",color="#000000",
           size=5,family = "Roboto Condensed",hjust=1) +
  # geom_point(aes(fechab,anuncios,group=neighbourhood,color=neighbourhood),size=1) +
  geom_line(aes(fechab,anuncios,group=neighbourhood_cleansed,color=neighbourhood_cleansed),size=0.6) +
  scale_color_manual(values = getPalette(colourCount)) +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # barrios labels
  geom_text_repel(data=filter(dates.count.barrio.room.reviews,fechab==as.Date("2019-03-08"),anuncios>200), 
                  aes(fechab+5,anuncios,label=paste(anuncios,neighbourhood_cleansed)),
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
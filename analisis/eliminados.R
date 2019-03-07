# Este script analiza las diferencias entre los diferentes listings de una localización de InsideAirbnb
# Usa los archivos listings-summary de Insideairbnb 

# Load libraries ----
library(gsubfn)
library(tidyverse)

# Loadlistings -----
listings  <- read.delim("data/original/airbnb/150430/listings_summary_barcelona_insideairbnb.csv",sep = ",")$id

# ------ Get dates when data are --------
# Loads dates with listings data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180818","180911")

# loop starts
listings  <- as.data.frame(read.delim("data/original/airbnb/150430/listings_summary_barcelona_insideairbnb.csv",sep = ",")$id)
names(listings) <- "id"
listings.total <- listings

# Loop para ir comparando las reviews e insertando las de los listings que no están.
for (i in 1:length(dates)) {
  print("listings totales: ")
  print(nrow(listings.total))
  print("------")
  print(paste("id:",i))
  print(paste("fecha: ",dates[i]))
  # Adds rows to the original
  neo <- as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")$id)
  names(neo) <- "id"
  print(paste("neo a leer",nrow(neo)))
  
  neo2 <- as.data.frame(neo[!neo$id %in% listings.total$id,])
  names(neo2) <- "id"
  print(paste("neo2 a insertar",nrow(neo2)))
  
  listings.total <- rbind(listings.total,neo2)
}

# counts whether a listing exists in a scraping
for (i in 1:length(dates)) {
  # Adds rows to the original
  neo <- as.data.frame(read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")$id)
  names(neo) <- "id"
  print(paste("n listings:",nrow(neo)))
  
  listings.total[,paste("d",dates[i],sep="")] <- 0
  listings.total[listings.total$id %in% neo$id,paste("d",dates[i],sep="")] <- 1
}

# converts to long format -----
data_long <- listings.total %>% gather(fecha, exists, 2:28)

# parses date
data_long$fechal <- strapplyc( as.character(data_long$fecha), "d([0-9]*)", simplify = TRUE)
data_long$fechab <- as.Date( paste(20,as.character(data_long$fechal),sep=""), "%Y%m%d")

# data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",] %>%
# ggplot(aes(x = as.factor(fecha), y = as.factor(id))) +
#   # geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",alpha=0.3,size=0.1)
#   geom_point(size=0.1,alpha=0.8)

# subset data for speed
test <- data_long[data_long$exists == 1 & data_long$fechab > "2018-04-01",]
ggplot(test[sample(nrow(test),100),],aes(x = as.factor(fecha), y = as.factor(id))) +
  geom_point(size=3,alpha=0.8)

# png(filename=paste("temp/eliminados-03.png", sep = ""),width = 2000,height = 5000)
# ggplot(test,aes(x = as.factor(fecha), y = as.factor(id))) +
#   geom_bin2d()
# dev.off()

# Adds when listing was found -----
listings.total.found <- data_long %>% filter(exists == 1) %>% group_by(id) %>% summarise(found = min(fechab)) %>% ungroup()
# listings.total <- listings.total %>% select(-"min(fechab)")
listings.total <- inner_join(listings.total,listings.total.found)

# calculates in how many scrapings has been every listing ----
listings.total$sum <- rowSums(listings.total[,2:28])
listings.total$sum2018 <- rowSums(listings.total[,21:28])

# hist(listings.total$sum)

# extends color paletter
library(RColorBrewer)
colourCount <- length(unique(listings.total$found))
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

# histograma básico
png(filename=paste("images/airbnb/eliminados/eliminados-01.png", sep = ""),width = 1000,height = 750)
listings.total %>%
  ggplot(aes(sum)) + 
  geom_histogram(binwidth = 1) +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
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
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
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
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
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
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
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

# barra  coloreado según cuándo fue encontrado y filtrando por fecha concreta
png(filename=paste("images/airbnb/eliminados/eliminados-06.png", sep = ""),width = 1000,height = 750)
listings.total %>% filter(d180818 == 1) %>%
  ggplot(aes(x=1,fill=as.factor(found))) + 
  geom_bar() +
  scale_fill_manual(values = getPalette(colourCount)) +
  # scale_fill_continuous() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
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
      theme_minimal(base_family = "Roboto Condensed", base_size =10) +
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

# Then build matrix with "montage" the imagemagick function:
# montage d1* -geometry 200x800+0+0 vertical.png

# for network graph ------
# Exports data to use them in gephi
rownames(test) <- 1:111557

links <- test[,1:2]
names(links) <- c("source","target")
nodes <- as.data.frame(c(unique(links$target),unique(links$source)))

# Sve data if needed
write.csv(links, file="temp/links-listings-post-2017.csv")
write.csv(nodes, file="temp/nodes-listings-post-2017.csv")
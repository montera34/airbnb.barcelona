# Este script analiza los reviews de una localización para estudiar estacionalidad y evolución de uso
# Usa el archivo reviews de Insideairbnb, que incluye todas las reviews de una localización

# Load libraries ----
library(gsubfn)
library(tidyverse)

location <- "Barcelona"
date_abr <- "20180911"
date <- "11 sept 2018"


# Load reviews and listings -----
# reviews <- read.delim("data/original/airbnb/180818/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
reviews <- read.delim("data/original/airbnb/180911/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# listings  <- read.delim("data/original/airbnb/180818/listings_summary_barcelona_insideairbnb.csv",sep = ",")
listings  <- read.delim("data/original/airbnb/180911/listings_summary_barcelona_insideairbnb.csv",sep = ",")

# ids <- c(listings$id,listings2018$id)

# ------ Merge reviews and remove duplicates --------
# Loads dates with review data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180818")
# Las fechas tienen que estar de la más reciente a la más antigua, si no no funcionar el loop siguiente
dates <- (dates)

# Loop para ir comparando las reviews e insertando las de los listings que no están.
for (i in 1:length(dates)) {
  # name <- as.data.frame(assign(paste("reviews",i,sep = ""),4))
  print (i)
  print (dates[i])
  # Create dataframe with reviews of that day
  # assign(paste("reviews",i,sep = ""), read.delim(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")) 
  # Adds rows to the original review file
  reviews <- rbind(reviews,
                   read.delim(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")[!(read.delim(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")$listing_id %in% reviews$listing_id),]
                   )
  print(nrow(reviews))
}

# reviews2 <- read.delim("data/original/airbnb/150904/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews3 <- read.delim("data/original/airbnb/161107/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews4 <- read.delim("data/original/airbnb/170912/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews5 <- read.delim("data/original/airbnb/171113/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews6 <- read.delim("data/original/airbnb/171209/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews7 <- read.delim("data/original/airbnb/180117/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews8 <- read.delim("data/original/airbnb/180412/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews9 <- read.delim("data/original/airbnb/180609/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews10<- read.delim("data/original/airbnb/180710/reviews_summary_barcelona_insideairbnb.csv",sep = ",")
# reviews11 <- read.delim("data/original/airbnb/180818/reviews_summary_barcelona_insideairbnb.csv",sep = ",")

# reviews <- rbind(reviews,reviews2[!(reviews2$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews3[!(reviews3$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews4[!(reviews4$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews5[!(reviews5$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews6[!(reviews6$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews7[!(reviews7$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews8[!(reviews8$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews9[!(reviews9$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews10[!(reviews10$listing_id %in% reviews$listing_id),])
# reviews <- rbind(reviews,reviews11[!(reviews11$listing_id %in% reviews$listing_id),])

# To merge few review files -----
# de 1 que no están en 2
# db1_not_in_2 <- reviews[!(reviews$id %in% reviews2$id),]
# db1_in_2 <- reviews[reviews$id %in% reviews2$id,]
# de 2 que no están en 1
db2_not_in_1 <- reviews2[!(reviews2$listing_id %in% reviews$listing_id),]
# db2_in_1 <- reviews2[reviews2$id %in% reviews$id,]
reviews <- rbind(reviews,db2_not_in_1)

# from 3 not in merged
db3_not_in_1 <- reviews3[!(reviews3$listing_id %in% reviews$listing_id),]
reviews <- rbind(reviews,db3_not_in_1)
# from 4 not in merged
db4_not_in_1 <- reviews4[!(reviews4$listing_id %in% reviews$listing_id),]
reviews <- rbind(reviews,db4_not_in_1)
# from 5 not in merged
db5_not_in_1 <- reviews5[!(reviews5$id %in% reviews$id),]
reviews <- rbind(reviews,db5_not_in_1)
# from 6 not in merged
db6_not_in_1 <- reviews6[!(reviews6$id %in% reviews$id),]
reviews <- rbind(reviews,db6_not_in_1)


# ------ Process review to insert year, month, day, hour ------------------
reviews$year <- as.numeric(strapplyc( as.character(reviews$date), "([0-9]*).*", simplify = TRUE))
reviews$month <- as.numeric(strapplyc( as.character(reviews$date), "[0-9]*-([0-9]*)-[0-9]*", simplify = TRUE))
reviews$day <- as.numeric(strapplyc( as.character(reviews$date), ".*[0-9]*-[0-9]*-([0-9]*)", simplify = TRUE))
# Create date field
reviews$datex <- as.Date(reviews$date)

# Sve data if needed
save(reviews ,file="data/output/airbnb/180911/temp/reviews-summary-barcelona-insideairbnb-merged-2010-20180911.Rda")
write.csv(reviews, file="data/output/airbnb/180911/reviews-summary-barcelona-insideairbnb-2010-20180911.csv")

# ------ Load data in case data are already produced------------------
# load("data/original/airbnb/180818/temp/reviews_summary_barcelona_insideairbnb.Rda")
reviews$fix <- 1

# Change this list of listings to select only a particular kind of listing
# listings <- data.frame(unique(reviews$listing_id))
# colnames(listings) <- "id"

# ------ Analyze reviews----------
plot(reviews$datex,reviews$fix)
# extract of the data: 10.000  reviews
reviews_p <- reviews[sample(nrow(reviews), 10000), ]

# ------ Single strip: each point is a review. full period analyzed ------
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-2010-2018.png",width = 800,height = 400)
ggplot(reviews, aes(datex,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.012) +
  labs(title = paste("Reviews por fecha. ",location,".",sep=""),
       subtitle = "",
       x = "Fecha review",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

# ------ Single strip: each point is a review. One year ------
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-pormes-2016.png",width = 800,height = 400)
ggplot(reviews[reviews$year==2016,], aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.05) +
  # geom_point(aes(),position = "jitter", alpha=0.2, size=0.01) +
  labs(title = paste("Reviews por mes, 2016. ",location,".",sep=""),
       subtitle = "",
       x = "Mes",
       y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

# ------ Single strip each point is a review. One year analyzed ------
# Only display listings included in a predefined listings data.frame (for example, 
#  listings in a location or belonging to a certain list of hosts)
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-pormes-2016-b.png",width = 800,height = 400)
ggplot(reviews[reviews$year==2016 & reviews$listing_id %in% unique(listings$id),], aes(month,year)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.08) +
  labs(title = "Reviews per date. 2016. Barcelona",
       subtitle = "",
       x = "Review date",
       y = "2016",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 
dev.off()

# ------ Single strip each point is a review. Full period analyzed ------
# Only display listings included in a predefined listings data.frame
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-2010-2018.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id),], aes(datex,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.02) +
  labs(title = "Reviews por fecha. Barcelona",
       subtitle = "2010-2018",
       x = "Fecha",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")  +
  scale_x_date(date_minor_breaks = "1 year", date_labels = "%Y")
dev.off()

# ------ Single strip each point is a review. Full period analyzed, faceted in years ------
# Only display listings included in a predefined listings data.frame
png(filename="images/airbnb/reviews/airbnb-reviews-donostia-2012-2017-b.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2011,], aes(month,fix)) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.03) +
  labs(title = "Airbnb reviews por mes y año. 2016. Donostia - San Sebastián",
       subtitle = "",
       x = "Review date",
       y = "",
       caption = "Efecto Airbnb. Data: InsideAirbnb")+
  scale_x_continuous(breaks=seq(0,12,1))+
  facet_wrap(~ year)
dev.off()
# ------ reviews by month as points. not very useful------
ggplot(reviews, aes(month,factor(listing_id))) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter",alpha=0.009) +
  labs(title = "Reviews per date. Euskadi",
       # subtitle = "",
       # x = "Review date",
       # y = "year",
       caption = "Efecto Airbnb. Data: InsideAirbnb") 

# ------ ????? ------
# qplot(reviews$month, geom="histogram") 

# ------ Reviews per trimester histogram ------
png(filename="images/airbnb/reviews/airbnb-reviews-trimestral-barelona-2011-2017.png",width = 800,height = 400)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010 & !reviews$year==2018,],
       aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010 & !reviews$year==2018,]$month)) + 
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_histogram(breaks=seq(0, 12, by = 3), 
                 # col="red", 
                 # fill="green", 
                 alpha = .9) + 
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = "Reviews por trimestre. Barcelona. 2011-2017",
       subtitle = "",
       x = "Mes",
       y = "Número reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
  # stat_bin(binwidth= 4, geom="text", colour="white", size=3.5, aes(label=..count..) , 
           # position=position_stack(vjust=0.5)) 
dev.off()

# ------ Reviews per month histogram ------
png(filename="images/airbnb/reviews/airbnb-reviews-mensual-barcelona-2011-2017.png",width = 800,height = 600)
ggplot(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010 & !reviews$year==2018,],
       aes(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010 & !reviews$year==2018,]$month)) + 
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  geom_histogram(breaks=seq(0, 12, by = 1), 
                 # col="red", 
                 # fill="green", 
                 alpha = .9) + 
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(title = "Reviews por mes. Barcelona. 2012-2016",
       subtitle = "",
       x = "Mes",
       y = "Número reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")
dev.off()

table(reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2017 & !reviews$year==2011& !reviews$year==2012,]$month)

# ------ Reviews per month in a year, colored bar by year------
# 1. ----
res2 <- reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010 & !reviews$year==2018,] %>% 
  group_by(month,year) %>% 
  summarise(count=n())

cbPalette <- c("#dddddd","#cccccc","#bbbbbb","#aaaaaa","#999999","#666666","#333333")

png(filename="images/airbnb/reviews/airbnb-reviews-por-mes-stacked-barcelona-2011-2017.png",width = 800,height = 600)
ggplot(res2,aes(x = month, y = count,fill = fct_rev(factor(year)))) + 
  scale_fill_manual(values = cbPalette) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title=element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews por mes en Airbnb en Barcelona 2011-2017",
       subtitle = "",
       x = "Mes",
       y = "Número de reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")
dev.off()

# 2. ----
res2b <- reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010,] %>% 
  group_by(month,year) %>% 
  summarise(count=n())

cbPalette2 <- c("#dddddd","#cccccc","#bbbbbb","#aaaaaa","#999999","#666666","#333333","#111111")

png(filename="images/airbnb/reviews/airbnb-reviews-por-mes-stacked-barcelona-2011-2018.png",width = 800,height = 600)
ggplot(res2b,aes(x = month, y = count,fill = fct_rev(factor(year)))) + 
  scale_fill_manual(values = cbPalette2) +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title=element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_bar(stat="identity") +
  labs(title = "Reviews por mes en Airbnb en Barcelona 2011-2018",
       subtitle = "",
       x = "Mes",
       y = "Número de reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")
dev.off()


# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
#  See gaps in the listings, if listings is not factorized you'll see gaps related to difference in number of listing id
png(filename="images/airbnb/reviews/airbnb-reviews-puntos-barcelona-2010-201809_all-data_g2.png",width = 7000,height = 5000)
ggplot(reviews, aes(datex,factor(listing_id))) +
  theme_minimal(base_family = "Roboto", base_size = 80) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_point(aes(),position = "jitter", alpha=0.8,size=0.4) + # for big plot geom_point(aes(),position = "jitter", alpha=0.5,size=0.1) 
labs(title = "Reviews por fecha y número de id de alojamiento Airbnb. Barcelona 2010-2018 (11 septiembre 2018)",
     subtitle = "Cada línea es un alojamiento.",
     x = "Fecha de review",
     y = "id de alojamiento",
     caption = "Efecto Airbnb. lab.montera34.com. Datos: InsideAirbnb desde 2015 (27 series de datos)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  #minor_breaks = "3 month", 
dev.off()

# ------ Acumulation of reviews: date vs listing_id. Smaller darker points. Full period analyzed ------
#  Seen gaps in the listings, if listings is not factorized you'll see gaps related to difference in number of listing id
png(filename="images/airbnb/reviews/airbnb-reviews-puntos-barcelona-2010-2018_2.png",width = 1200,height = 900)
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 13) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=0.5,size=0.000001) +
  labs(title = "Reviews por fecha y número de id de alojamiento Airbnb. Barcelona 2010-2018 (agosto)",
       subtitle = "Cada línea es un alojamiento.",
       x = "Fecha de review",
       y = "id de alojamiento",
       caption = "Efecto Airbnb. lab.montera34.com Datos: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%%Y")  #date_labels = "%m.%Y" minor_breaks = "3 month", 
dev.off()
# ------ Acumulation of reviews: date vs listing_id. Smaller darker points. Full period analyzed ------
# Factorized listings avoids the gaps due to difference in listings id number
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# ------ Number of reviews per month. Histogram. Full period --------
# ggplot(reviews[ reviews$year==2015,], aes(datex, ..count..)) +
png(filename="images/airbnb/reviews/airbnb-barcelona-reviews-mes-2010-2018_all-data.png",width = 900,height = 600)
ggplot(reviews, aes(datex, ..count..)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_histogram(binwidth = 30.41, colour="white") + #  bins = 72
  labs(title = "Reviews de Airbnb por mes en Barcelona",
       subtitle = "2010-2018",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Datos: InsideAirbnb desde 2015 (27 series de datos)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

# ted
png(filename="images/airbnb/reviews/airbnb-barcelona-reviews-mes-2010-20180911_faceted_all-data.png",width = 900,height = 600)
ggplot(reviews, aes(month)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  labs(title = "'Reviews' de Airbnb por mes en Barcelona",
       subtitle = "2010-2018",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Datos: 26 archivos de InsideAirbnb desde 2015") +
  facet_wrap(~ year)
dev.off()

# Madrid
ggplot(reviews[!reviews$year==2010 & !reviews$year==2011 & !reviews$year==2018,], aes(datex, ..count..)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_histogram(binwidth = 30.41, colour="white") + #  bins = 72
  # labs(title = "Reviews per month. Madrid 2012-2017",
  #      subtitle = "Using 6 different review files from InsideAirbnb (years 2015, 2017 and 2018)",
  #      x = "Date",
  #      y = "Number of reviews by month",
  #      caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  labs(title = "'Reviews' de Airbnb por mes en Madrid 2012-2017",
       subtitle = "Usando 6 diferentes archivos de reviews InsideAirbnb (3 de 2015, 2 de 2017 y 1 de 2018)",
       x = "Año",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  geom_hline(yintercept = seq(0,30000,by=5000),colour = "#999999",size=0.5,alpha=0.1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_text(aes(x = as.Date("2012-01-01"), y = 6000, label = "5.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 11000, label = "10.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 16000, label = "15.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 21000, label = "20.000"), color = "#999999", size=3) +
  geom_text(aes(x = as.Date("2012-01-01"), y = 26000, label = "25.000"), color = "#999999", size=3) 



# ------ Acumulation of reviews: date vs listing_id. Full period analyzed ------
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), alpha=1,size=0.0001) +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_minor_breaks = "1 month")

# ------ Acumulation of reviews: date vs listing_id. Plot points by host id selected in other color. Full period analyzed ------
# Which listings are owned by hosts that have multiple listings
# group hosts id and order them by number of listings
# listings <- as.data.frame(unique(reviews$listing_id))
list2 <- listings %>% 
  group_by(host_id) %>% 
  summarise(count=n()) %>% arrange(-count)

# Select the top 30 host id
listings$host_id %in% list2$host_id[1:30]

listings_top <- listings[listings$host_id==4459553 |listings$host_id==102947901 | listings$host_id== 1391607 |
                           listings$host_id== 170716140 |listings$host_id== 396363,]$id

# Use this if do not know which listings to select
# listings_top <- listings[1:100,]
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-mes-top5-marcado-2010-2018_all-data_g3.png",width = 2700,height = 1800)
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 40) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id),],aes(datex,factor(listing_id)), 
             alpha=0.7,size=0.15) + #alpha=0.07,size=0.005)
  geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(datex,factor(listing_id)),
             alpha=1,size=1,color="#FF0000") +
  labs(title = "Reviews de Airbnb por mes en Barcelona 2010-2018",
     subtitle = "Cada línea es un listing. En rojo los top 5 anfitriones.",
     x = "Año",
     y = "id de alojamiento",
     caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb, 27 bases de datos desde 2015") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  # geom_hline(yintercept = seq(0,40000,by=10000),colour = "#999999",size=1,alpha=1) +
  # geom_text(aes(x = as.Date("2012-01-01"), y = 10110, label = "10.000"), color = "#999999", size=40) 
  # geom_text(aes(x = as.Date("2012-01-01"), y = 1010, label = "1.000"), color = "#999999", size=3)
  # geom_text(aes(x = as.Date("2012-01-01"), y = 16000, label = "15.000"), color = "#999999", size=3) +
  # geom_text(aes(x = as.Date("2012-01-01"), y = 21000, label = "20.000"), color = "#999999", size=3) +
  # geom_text(aes(x = as.Date("2012-01-01"), y = 26000, label = "25.000"), color = "#999999", size=3) 
dev.off()

# ------ Acumulation of reviews: date vs listing_id.Select per date -------------
ggplot() +
  theme_minimal(base_family = "Roboto", base_size = 10) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y=element_blank()
  ) +
  geom_point(data=reviews[reviews$listing_id %in% unique(listings$id) & reviews$datex > "2016-04-01",],aes(datex,factor(listing_id)), alpha=0.3,size=0.0001) +
  # geom_point(data=reviews[reviews$listing_id %in% listings_top,],aes(datex,factor(listing_id)), alpha=1,size=0.005,color="#FF0000") +
  labs(title = "Reviews per date and listing. Donostia 2011-2017 (abril)",
       subtitle = "Every line is one listing. In red: the top 4 hosts (users with more listings) manage 155 ads.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") 

# ------ Acumulation of reviews: date vs listing_id.Select per date -------------
ggplot(data = reviews, aes(x=datex, y=listing_id, fill=month)) + 
  geom_tile()

# ------ Others. TODO -------------
ggplot(data = reviews, aes(x=datex, y=factor(listing_id))) + 
  stat_density(aes(fill = ..density..), geom = "raster", position = "identity")

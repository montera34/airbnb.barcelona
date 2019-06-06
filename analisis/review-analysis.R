# Este script analiza los reviews de una localización para estudiar estacionalidad y evolución de uso
# Usa el archivo reviews de Insideairbnb, que incluye todas las reviews de una localización

# Load libraries ----
library(gsubfn)
library(tidyverse)
library(R.utils)
library(reshape2) #to melt
library(RColorBrewer)
library(lubridate) #to extract month in plot

location <- "Barcelona"
date_abr <- "20180911"
date <- "11 sept 2018"

# create dataframe with dates and number of listings from different sources
sources <-data.frame(matrix(ncol = 6  ))
names(sources)  <- c("date","source","listings","entire_home","private_room","shared_room")
sources[1,] <- c("2019-03-29","AirDNA",18760,11368,7241,151)
sources[2,] <- c("2019-05-14","AirDNA",18966,11326,7459,181)
# sources[4,] <- c("2018-06-4","Datahippo (active)",1431,1066,358,7)
# sources[6,] <- c("2018-09-15","Ayuntamiento", 1289,1163,126,0)
# número de VUT registradas en Barcelona en abril 2019 por distrito
# https://twitter.com/VictriaVic/status/1121779034998956032
# Ciutat Vella 605
# Eixample 4459
# Sants-Montjuic 1171
# Les Corts 266
# Sarria-Sant Gervasi 496
# Gracia 1077
# Horta-Guinardó 253
# Nou Barris 21
# Sant Andreu 78
# Sant Martí 1133
# TOTAL 9559

sources$date <- as.Date(sources$date)
# converts to numeric columns
sources[,3:6] <- lapply(sources[,3:6], function(x) as.numeric(as.character(x)))
# Convert data to long-form with 'melt' from the reshape2 package.
sources_melt <- melt(sources, id.vars=c("date","source"),
                     measure.vars=c("entire_home", "private_room","shared_room"))


# ------ Merge reviews and remove duplicates --------
# Loads dates with review data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180814","181010","181107","181210","190114","190206","190308")
# Las fechas tienen que estar de la más reciente a la más antigua, si no no funciona el loop siguiente
dates <- rev(dates)

# Loop revisado
reviews <-data.frame(matrix(ncol = 3  ))
names(reviews)  <- c("listing_id","date","scrapdate")

listings <-data.frame(matrix(ncol = 1  ))
names(listings)  <- c("id")

# loads newest listings and reviews files
listings_total <- read.delim("data/original/airbnb/190308/listings_summary_barcelona_insideairbnb.csv", sep = ",")
# listings_total <- read.delim("data/original/airbnb/180911/listings_summary_barcelona_insideairbnb.csv", sep = ",")
# reviews_total<- read.delim("data/original/airbnb/180911/reviews_summary_barcelona_insideairbnb.csv", sep = ",")

# Loop para ir comparando las reviews y listings e insertando las que no están en Barcelona ----------------------------
# Este loop crea varios dataframes:
# -reviews no repetidas (versión reducida y otra con todas las variables)
# -listings no repetidos (versión reducida y otra con todas las variables)
for (i in 1:length(dates)) {
# for (i in 1:2) {
  print(dates[i])
  # Loads reviews of that day in temp dataframe
  reviews_temp <- read.delim(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")
  # Stores date of scraping in new variable
  reviews_temp$scrapdate <- dates[i]
  # Adds rows to the original review file
  # only stores reviews from listings that were not in previous dataframe.
  # It asumes that it is not possible to remove reviews
  reviews <- rbind(reviews,
                   reviews_temp[!(reviews_temp$listing_id %in% reviews$listing_id),]
                  )
  # Loads reviews (all variables included) of that day in temp dataframe
  # reviews_total_temp <- read.delim(gzfile(paste("data/original/airbnb/",dates[i],"/reviews_summary_barcelona_insideairbnb.csv",sep="")),
  #                                  sep = ",")
  # reviews_total<- rbind(reviews_total,
  #                       reviews_total_temp[!(reviews_total_temp$id %in% reviews_total$id),]
  #                 )
  
  # Loads listings  of that day in temp dataframe
  listings_temp <- read.delim(paste("data/original/airbnb/",dates[i],"/listings_summary_barcelona_insideairbnb.csv",sep=""),sep = ",")
  # print(table(listings_temp$room_type))
  # only stores listings that were not in the dataframe
  # stores listigns that _____ with only two variables
  listings <- rbind(listings,
                    select(listings_temp[!(listings_temp$id %in% listings$id),], id)
  )
  # stores listings with all the variables (well, the .csv.gz has even more variables) that were not before
  listings_total <- rbind(listings_total, listings_temp[!(listings_temp$id %in% listings_total$id),])
  
  print(paste("reviews: ", nrow(reviews), sep=""))
  # print(paste("reviews_total_temp: ", nrow(reviews_total_temp), sep=""))
  # print(paste("reviews_total: ", nrow(reviews_total), sep=""))
  print(paste("listings: ",nrow(listings), sep=""))
  print(paste("listings_temp: ", nrow(listings_temp), sep=""))
  print(paste("listings_total: ", nrow(listings_total), sep=""))
  print("-------------")
}

# removes not used dataframes
rm(reviews_temp)
rm(listings_temp)
rm(reviews_total_temp)

# ------ Process review to insert year, month, day, hour ------------------
reviews$year <- as.numeric(strapplyc( as.character(reviews$date), "([0-9]*).*", simplify = TRUE))
reviews$month <- as.numeric(strapplyc( as.character(reviews$date), "[0-9]*-([0-9]*)-[0-9]*", simplify = TRUE))
reviews$day <- as.numeric(strapplyc( as.character(reviews$date), ".*[0-9]*-[0-9]*-([0-9]*)", simplify = TRUE))
# Parse date and create date field
reviews$datex <- as.Date(reviews$date)

# Save data if needed
save(reviews, file="tmp/reviews-summary-barcelona-insideairbnb-merged-2010-20190308.Rda")
write.csv(reviews, file="data/output/airbnb/180911/reviews-summary-barcelona-insideairbnb-2010-20190308.csv")

# Insert room_type in every review
reviews <- reviews %>% left_join(select(listings_total, id, room_type), by= c("listing_id" = "id"))

# translate room type
levels(reviews$room_type) <- c("Piso completo","Habitación","Habitación compartida")

# Save data in R format -----
# save(reviews ,file="data/output/reviews-donostia-airbnb-insideairbnb-2017.Rda")
save(reviews, file="tmp/reviews-summary-barcelona-insideairbnb-merged-2010-20190308.Rda")
save(listings, file="tmp/listings-barcelona-airbnb-insideairbnb-2010-201903.Rda")


# ------ Load reviews data in case data are already produced------------------
# load("data/output/airbnb/180911/tmp/reviews-barcelona-airbnb-insideairbnb-2010-2018.Rda")
# load("data/output/airbnb/180911/tmp/listings-barcelona-airbnb-insideairbnb-2010-2018.Rda")

load("tmp/reviews-summary-barcelona-insideairbnb-merged-2010-20190308.Rda")
load("tmp/listings-barcelona-airbnb-insideairbnb-2010-201903.Rda")

reviews$fix <- 1

# Change this list of listings to select only a particular kind of listing ------------------
# listings <- data.frame(unique(reviews$listing_id))
# colnames(listings) <- "id"

# reviews per year
table(reviews$year)
# 2009   2010   2011   2012   2013   2014   2015   2016   2017   2018   2019 
#    2    197   2276  10216  34058  94404 197404 261922 333254 311280  36223 

levels(reviews$room_type) <- c("Piso completo","Habitación","Habitación compartida")

# Creates results data frame:
results <- data.frame(matrix(ncol = 5,nrow = 108  ))
# names(results)  <- c("date","listings","entire_home","entire_home2","private_room","private_room2","shared_room","shared_room2","not_listed")
names(results)  <- c("date","listings","entire_home","private_room","shared_room")

k <- 1
# for ( i in as.list(levels(reviews$year))) {
for ( i in 2011:2019 ) {
  for ( j in 1:12) {
    paste(print(i),print(j),sep = "-")
    print(length(unique(reviews[reviews$year==i & reviews$month==j,]$listing_id)))
    print( as.Date( paste(i,j,1,sep = "-") ) )
    results$date[k] <- as.Date(paste(i,j,1,sep = "-"),origin="1970-01-01")
    results$listings[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & !is.na(reviews$date),]$listing_id))
    results$entire_home[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Piso completo" & !is.na(reviews$date),]$listing_id))
    results$private_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Habitación" & !is.na(reviews$date),]$listing_id))
    results$shared_room[k] <- length(unique(reviews[reviews$year==i & reviews$month==j & reviews$room_type == "Habitación compartida" & !is.na(reviews$date),]$listing_id))
    k <- k+1
  }
}

results$date <- as.Date(as.numeric(results$date),origin="1970-01-01")

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
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-pormes-2016-b.png",width = 800,height = 400)
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
png(filename="images/airbnb/reviews/airbnb-reviews-barcelona-2012-2017-b.png",width = 800,height = 400)
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
  labs(title = "Airbnb reviews por mes y año. 2016. Barcelona",
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
# ggplot(reviews, aes(reviews$month)) +
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
cbPalette.inv <- colorRampPalette( c( "#333333", "#dddddd" ) )( 7 )

# windowsFonts(Times=windowsFont("Roboto Condensed"))

png(filename="images/airbnb/reviews/airbnb-reviews-por-mes-stacked-barcelona-2011-2017.png",width = 800,height = 600)
ggplot(res2,aes(x = month, y = count,fill = fct_rev(factor(year)))) + 
  # scale_fill_manual(values = cbPalette.inv) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 10) +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title=element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,12,1))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_bar(stat="identity") +
  labs(title = "Reviews por mes en Airbnb en Barcelona 2011-2017",
       subtitle = "",
       x = "Mes",
       y = "Número de reviews",
       caption = "Efecto Airbnb. lab.montera34.com. Data: InsideAirbnb")  +
  geom_text(
            aes(label = count),
            position = position_stack(vjust = 0.5),size=3,color="#FFFFFF")
dev.off()

# 2. ----
res2b <- reviews[reviews$listing_id %in% unique(listings$id) & !reviews$year==2010,] %>% 
  group_by(month,year) %>% 
  summarise(count=n())

cbPalette2 <- cbPalette.inv <- colorRampPalette( c( "#dddddd","#111111" ) )( 8 )

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
  labs(title = "Reviews per date and listing. Barcelona 2011-2017 (abril)",
       subtitle = "Every line is one listing. Look at the line gaps: listings without reviews.",
       x = "Review date",
       y = "listing id",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# ------ Number of reviews per month. Histogram. Full period --------
# ggplot(reviews[ reviews$year==2015,], aes(datex, ..count..)) +
png(filename="images/airbnb/reviews/airbnb-barcelona-reviews-mes-2012-2018_all-data.png",width = 900,height = 350)
ggplot(filter(reviews,date > as.Date("2012-01-01") & date < as.Date("2019-01-01") ), aes(datex, ..count..)) +
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
  # notes
  annotate("text",x=as.Date("2018-05-26"),y=36000,label="acuerdo",color="#000000",size=5,hjust = 1,family = "Roboto Condensed") +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  # summer
  # se marcan meses de verano
  annotate("rect", alpha = .07,
           xmin = c(as.Date("2012-06-21"),as.Date("2013-06-21"),as.Date("2014-06-21"),as.Date("2015-06-21"),as.Date("2016-06-21"),as.Date("2017-06-21"),as.Date("2018-06-21")),
           xmax = c(as.Date("2012-09-21"),as.Date("2013-09-21"),as.Date("2014-09-21"),as.Date("2015-09-21"),as.Date("2016-09-21"),as.Date("2017-09-21"),as.Date("2018-09-21")),
           ymin = 0,
           ymax = Inf) +
  labs(title = "Reviews de Airbnb por mes en Barcelona",
       subtitle = "2010-2018",
       x = "",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Datos: InsideAirbnb") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) 
dev.off()

# ted
png(filename="images/airbnb/reviews/airbnb-barcelona-reviews-mes-2010-201812_faceted_2015-2018.png",width = 700,height = 550)
ggplot(filter(reviews,date > as.Date("2014-12-31") & date < as.Date("2019-01-01") ), aes(month)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.0)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  geom_bar() + #  bins = 72
  # summer
  # se marcan meses de verano TODO
  # annotate("rect", alpha = .07,
  #          xmin = c(as.Date("2015-06-21"),as.Date("2016-06-21"),as.Date("2017-06-21"),as.Date("2018-06-21")),
  #          xmax = c(as.Date("2015-09-21"),as.Date("2016-09-21"),as.Date("2017-09-21"),as.Date("2018-09-21")),
  #          ymin = 0,
  #          ymax = Inf) +
  labs(title = "'Reviews' de Airbnb por mes en Barcelona",
       subtitle = "2015-2018",
       x = "meses",
       y = "Número de reviews por mes",
       caption = "Efecto Airbnb. lab.montera34.com. Datos: InsideAirbnb") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
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




# ------ Number of listings reviewd per month/year. Histogram. Full period --------
png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-month-2011-2018-line.png",width = 900,height = 350)
ggplot(results, aes(date, listings)) +
  # geom_col() +
  geom_line() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Listings con reviews de Airbnb por mes en Barcelona",
       subtitle = "2011-2018",
       x = "Fecha",
       y = "Número de listing con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2017 y 2018") +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y")
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2011-01-01"),as.Date("2018-12-31")))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) 

dev.off()


png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-mes-2011-2018_julio-agosto-marcado.png",width = 900,height = 350)

ggplot(filter(results,date < as.Date("2019-01-1")), aes(date, listings, fill = month(date) == c(7,8) )) +
  geom_col() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    legend.position = "none"
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes en Barcelona",
       subtitle = "2011-2018 (destacados julio y agosto)",
       x = "Fecha",                 
       y = "Número de anuncios con review por mes",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2014-2018") +
  guides(fill=guide_legend(title="Agosto")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) 
dev.off()

# -------------- Room type classification
# Convert data to long-form with 'melt' from the reshape2 package.
results_to_melt <- results

results_to_melt$room <- results_to_melt$shared_room + results_to_melt$private_room

results_melt <- melt(results_to_melt, id.vars=c("date"),
                     measure.vars=c("entire_home", "room"))

levels(results_melt$variable) <- c("Vivienda","Habitación")

# Remove dates without data
results_melt <- results_melt[results_melt$date < "2019-04-01" , ]



# Plots -----------------
# Bars
png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-mes-2015-2018_room-type_bar.png",width = 800,height = 350)
ggplot(filter(results_melt, date > as.Date("2014-12-31") & date < as.Date("2019-01-1")), aes(x=date, y=value, fill=variable)) +
  geom_bar(position="stack", stat="identity")  + #use position "fill" to create stacker 100% height 
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1)
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes en Barcelona",
       subtitle = "2015-2018",
       x = "",
       y = "Número de anuncios",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2015-2018",
       fill="Tipo alojamiento") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) 
# scale_y_continuous(labels = percent_format())
dev.off()

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-mes-2015-2018_rooom-type_bar_facet.png",width = 900,height = 300)
ggplot(filter(results_melt, date > as.Date("2014-12-31") & date < as.Date("2019-01-1")), aes(x=date, y=value, fill=variable)) +
  geom_bar(position="stack", stat="identity")  + #use position "fill" to create stacker 100% height 
  annotate("text",x=as.Date("2018-05-26"),y=6400,label="acuerdo",color="#000000",size=3,hjust = 1,family = "Roboto Condensed") +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    legend.position = "none"
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes en Barcelona",
       subtitle = "2015-2018",
       x = "",
       y = "Número de anuncios",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2015-2018") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  facet_wrap(~variable)
dev.off()

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-mes-2015-2018_rooom-type_line_con-acuerdo.png",width = 800,height = 350)
ggplot(filter(results_melt, date > as.Date("2014-12-31") & date < as.Date("2019-01-1")), aes(x=date, y=value, color=variable)) +
  geom_line(size=2)  +
  annotate("text",x=as.Date("2018-05-26"),y=6400,label="acuerdo",color="#000000",size=4,hjust = 1,family = "Roboto Condensed") +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=2) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    legend.position = "top"
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes en Barcelona",
       subtitle = "2015-2018",
       x = "",
       y = "Número de anuncios",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2015-2018",
       color="Tipo alojamiento") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2015-01-01"),as.Date("2018-12-31"))) +
  scale_y_continuous( labels=function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0,max(results_melt$value)) )
#  scale_x_continuous(limits = c(as.Date("2011-01-01"),as.Date("2018-08-31"))) 
dev.off()

# Active listings. Calculate active listings if 30% of users leave review ----------
results_melt$value_calculated_max <- round(results_melt$value / 0.5, 0)
results_melt$value_calculated_min <- round(results_melt$value / 0.7, 0)

png(filename="images/airbnb/reviews/airbnb-listings-insideairbnb-barcelona-with-review-mes-2015-2018_rooom-type_line_calculated.png",width = 900,height = 400)
ggplot(filter(results_melt,date > as.Date("2014-12-31") & date < "2019-01-01"),aes(x=date, color=variable)) +
  annotate("text",x=as.Date("2018-05-26"),y=1200,label="acuerdo",color="#000000",size=4,hjust = 1,family = "Roboto Condensed") +
  geom_vline(xintercept=as.Date("2018-05-31"),size=0.5,linetype=1,color="#bbbbbb") +
  geom_line(aes(y=value),size=2)  +
  geom_line(aes(y=value_calculated_max),linetype = 5,size=2 )  +
  theme_minimal(base_family = "Roboto Condensed", base_size = 19) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    legend.position = "top"
    # panel.grid.minor.y = element_blank()
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes en Barcelona",
       subtitle = "2015-2018. Línea de puntos: cálculo de anuncios si el 50% deja reviews.",
       x = "",
       y = "Número de anuncios",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb 2015-2018",
       color="Tipo alojamiento") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2014-12-31"),as.Date("2018-12-31")))+
  scale_y_continuous( labels=function(x) format(x, big.mark = ".", scientific = FALSE), limits = c(0,max(results_melt$value_calculated_max)) )
dev.off()

# Compare with AIRDNA active listings data for Barcelona-----
# airdna$date <- as.Date(airdna$date)
# names(airdna) <- c("date","entire_home_airdna","private_room_airdna","shared_room_airdna","total_airdna")
# results_airdna <- merge(results,airdna, by.x="date",by.y = "date",all = TRUE)

# Convert data to long-form with 'melt' from the reshape2 package.
# results_airdna_melt <- melt(results_airdna, id.vars=c("date"),
#                      measure.vars=c("entire_home", "private_room","shared_room",
#                                     "entire_home_airdna", "private_room_airdna","shared_room_airdna"))

# airdna_melt <- melt(airdna, id.vars=c("date"),
                    # measure.vars=c("entire_home_airdna", "private_room_airdna","shared_room_airdna"))


# names(airdna_melt) <- c("date","variable","value_airdna")
# resuls_melt <- merge(results_melt,airdna_melt, by.x="date",by.y = "date",all = TRUE)
# reviews <- merge(reviews,listings[,c("id","room_type","city")], by.x = "listing_id", by.y = "id", all.x=TRUE)

# results_airdna_melt$value_calculated <- round(results_airdna_melt$value / 0.3, 0)
# Remove NA
# results_melt <- results_melt[complete.cases(results_melt),]

# Gr'afico resumen --------------------
value_max <- 0.5
value_min <- 0.7

# dates.count.room_type viene de eliminados.R
listings.found <- dates.count.room_type
names(listings.found) <- c("date","variable","value")

listings.found$variable.s <- as.character(listings.found$variable)
listings.found$variable <- listings.found$variable.s  

# simplify roomtype
listings.found[listings.found$variable.s =="Piso completo",]$variable.s <- "Vivienda"
listings.found[listings.found$variable.s =="Habitación compartida",]$variable.s <- "Habitación"

# fake dataframe to use in annotate
df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)

# levels(results_melt$variable) <- c("Piso completo","Habitación","Habitación compartida")
levels(sources_melt$variable) <- c("Piso completo","Habitación","Habitación compartida")

# # annotate ony one panel https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
# ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
#                        cyl = factor(8,levels = c("4","6","8")))
# p + geom_text(data = ann_text,label = "Text")


png(filename="images/airbnb/reviews/airbnb-listings-barcelona-with-review-by-room-type_found-in-inside-airbnb.png",
    width = 1200,height = 600)
ggplot(NULL,aes(x=date)) +
  # scales
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2014-12-31"),as.Date("2019-03-31"))) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  # InsideAirbnb reviews based counting of active listigns
  # geom_line(data = filter(results_melt,variable == "entire_home"),aes(y=value))  + 
  # Viviendas
  geom_ribbon(data = filter(results_melt,date < "2019-03-01",variable == "Vivienda"),
              aes( ymin = value/value_min, ymax = value/value_max), fill="lightblue",alpha=0.5)  + 
  geom_line(data=filter(results_melt,date < "2019-03-01", variable == "Vivienda"),
            aes(x=date, y=value, color=variable),alpha=0.8,size=1)  + 
  # Habitaciones
  geom_ribbon(data = filter(results_melt,date < "2019-03-01",variable == "Habitación"),
              aes(ymin=value/value_min,ymax=value/value_max), fill="pink",alpha=0.5)  + 
  geom_line(data=filter(results_melt,date < "2019-03-01",variable == "Habitación"),
            aes(x=date, y=value, color=variable),alpha=0.8,size=1)  + 
  
  # listings found
  geom_line(data=filter(listings.found,date < "2019-04-01",!variable == "Habitación compartida"), 
            aes(x=date, y=value, color=variable))  + 
  # number of listings found in each scraping 
  geom_point(data=filter(listings.found,date < "2019-04-01",!variable == "Habitación compartida"),
  aes(x=date, y=value, color=variable))  + 
  annotate("text",x = as.Date("2017-01-1"),y = 12000, label="área: cáculo de anuncios según reviews",color="#000000",size=4,hjust = 1) + 
  geom_curve(aes(x = as.Date("2017-01-19"), y = 12000, xend = as.Date("2017-06-6"), yend = 11000), 
             color="#333333", data =df,  curvature = -0.2, arrow = arrow(length = unit(0.02, "npc"))   ) + 
  
  annotate("text",x = as.Date("2016-12-1"),y = 10000, label="anuncios encontrados online (en cada scraping)",color="#000000",size=4,hjust = 1) +
  geom_curve(aes(x = as.Date("2016-12-19"), y = 10000, xend = as.Date("2017-05-6"), yend = 9300), 
             color="#333333", data =df,  curvature = -0.2, arrow = arrow(length = unit(0.02, "npc"))   ) + 
  
  annotate("text",x = as.Date("2018-07-1"),y = 2000, label="anuncios que tuvieron review ese mes",color="#000000",size=4,hjust = 1) +
  geom_curve(aes(x = as.Date("2018-07-19"), y = 2000, xend = as.Date("2018-10-6"), yend = 4000), 
             color="#333333", data =df,  curvature = 0.2, arrow = arrow(length = unit(0.02, "npc"))   ) + 
  
  
  # annotate("text",x = as.Date("2014-07-1"),y = 7500, label="30%",color="#000000",size=2,hjust = 1) +
  # annotate("text",x = as.Date("2014-07-1"),y = 4000, label="70%",color="#000000",size=42,hjust = 1) +
  
  # geom_line(data = filter(results_melt,date < "2018-12-01"),aes(y=value_calculatede_min),linetype=2)  +
  # AirDNA active listings
  # geom_line(data = airdna_melt,aes(y=value, color=variable), size=1.5  )  +
  # Number of listings in existing sources
  # geom_point(data =  filter(sources_melt, !variable == "Habitación compartida"),aes(y=value,color=variable), size=2 ) +
  # geom_text(data = filter(sources_melt, !variable == "Habitación compartida"),
  #           aes(y=value,label=source,x=date-150),
  #           size=3.5,hjust = -0.1, nudge_x = 0.05, check_overlap = TRUE, hjust = 1) + 
  # scale_linetype_manual("",values=c(1, 2, 3,2, 2, 2),guide=FALSE)+
  # scale_color_manual("",values = c("#F8766D","#F8766D","#44BB55","#44BB55","#AAAADD","#AAAADD","#AAAA55")) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 20) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x= element_line(size = 0.1),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
    # panel.grid.major.y = element_blank(),
    # axis.text.y=element_blank()
  ) +
  labs(title = "Anuncios con reviews de Airbnb por mes vs Anuncios publicados. Barcelona",
       subtitle = "2014-2019 (marzo). Área: cálculo entre 50%-70% evaluaciones basado en reviews",
       x = "",
       y = "Número de anuncios",
       caption = "Efecto Airbnb. lab.montera34.com Data: InsideAirbnb.",
       color = "Anuncios con reviews") +
  facet_wrap(~variable)
dev.off()


ggplot(NULL,aes(x=date)) +
  geom_line(data=filter(results_melt,date < "2019-01-01",!variable == "Habitación compartida"),
            aes(x=date, y=value, color=variable))  +
  geom_line(data=filter(listings.found,date < "2019-01-01",!variable == "Habitación compartida"), 
            aes(x=date, y=value, color=variable))  +
  facet_wrap(~variable)



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
  labs(title = "Reviews per date and listing. Barcelona 2011-2017 (abril)",
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
  labs(title = "Reviews per date and listing. Barcelona 2011-2017 (abril)",
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

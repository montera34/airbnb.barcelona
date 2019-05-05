# scraping of insideairbnb data
dates <- c("150430","150717","150904","151002","160103","161107","161208","170104","170209","170306","170408","170507",
           "170605","170706","170806","170912","171007","171113","171209","180117","180207","180412","180514","180609",
           "180710","180814","180911","181010","181107","181210","190114","190206","190308")

dates2 <- c("2015-04-30","2015-07-17","2015-09-04","2015-10-02","2016-01-03","2016-11-07","2016-12-08","2017-01-04","2017-02-09","2017-03-06",
            "2017-04-08","2017-05-07",
            "2017-06-05","2017-07-06","2017-08-06","2017-09-12","2017-10-07","2017-11-13","2017-12-09","2018-01-17","2018-02-07","2018-04-12",
            "2018-05-14","2018-06-09",
            "2018-07-10","2018-08-14","2018-09-11","2018-10-10","2018-11-07","2018-12-10","2019-01-14","2019-02-06","2019-03-08")

# files_data <- c("listings.csv.gz", "calendar.csv.gz", "reviews.csv.gz")
files_data <- c("listings.csv.gz")
# files_vis <- c("listings.csv","reviews.csv")


for (i in 28:length(dates)) {
  print(i)
# for (i in 1:2) {
  # creates directories
  # dir.create(paste("data/original/airbnb/",dates[i],sep=""))
  # dir.create(paste("data/original/airbnb/",dates[i],"/data",sep=""))
  # dir.create(paste("data/original/airbnb/",dates[i],"/visualizations/",sep=""))
  # for (j in 1:length(files_vis)) {
  # download.file(paste("http://data.insideairbnb.com/spain/comunidad-de-madrid/madrid/",dates[i],"/visualisations/",files_vis[j],sep=""),
  #             destfile=paste("data/original/airbnb/",dates[i],"/visualizations/",files_vis[j],sep=""),
  #                            method = "wget")
  # }
  for (k in 1:length(files_data)) {
    download.file(paste("http://data.insideairbnb.com/spain/catalonia/barcelona/",dates2[i],"/data/",files_data[k],sep=""),
                  destfile=paste("data/original/airbnb/",dates[i],"/data/",files_data[k],sep=""),
                  method = "wget")
  }
}


# http://data.insideairbnb.com/spain/catalonia/barcelona/2019-03-08/data/listings.csv.gz
# http://data.insideairbnb.com/spain/catalonia/barcelona/15-04-30/data/listings.csv.gz
# i<-1


import csv
import urllib2
from bs4 import BeautifulSoup
import time
from datetime import datetime
import re
 
inputfilename = "listings_barcelona_dh_ia.csv" # nombre del archivo con los ids de los listings, uno por linea
inputpath = "../data/output/airbnb/180818/"+inputfilename # nombre del archivo con los ids de los listings, uno por linea
today = datetime.now().date().strftime("%Y%m%d")
outputfilename = inputfilename.replace(".csv","")+"_with-last-review-"+today+".csv"
outputpath = "../data/output/airbnb/180818/"+outputfilename
outputpathrev = "../data/output/"+outputfilename.replace(".csv","")+"-reviewed.csv" # nombre del archivo para guardar las URLs que existen, tienen reviews pero hayan dado algun problema y no se hayan descargado
hdr = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11',
       'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
       'Accept-Charset': 'ISO-8859-1,utf-8;q=0.7,*;q=0.3',
       'Accept-Encoding': 'none',
       'Accept-Language': 'en-US,en;q=0.8', #cambia idioma 'Accept-Language': 'es-ES,es;q=0.8' 	
       'Connection': 'keep-alive'}

## scraping
count=0
with open(outputpath, "w") as outfile:
    
    writer = csv.writer(outfile)
    writer.writerow(['id','name','host_id','host_name','neighbourhood_group','neighbourhood','latitude','longitude','room_type','price','minimum_nights','number_of_reviews','last_review','reviews_per_month','calculated_host_listings_count','availability_365','url','reviews_'+today,'lastreview_'+today,'exists_'+today])
    with open(inputpath, "r") as f:
        csvf = csv.DictReader(f, delimiter=',')
        for row in csvf:

            # uncomment the following two lines
            # to debug with first 5 rows of input file
            #if count == 5:
            #    break

            count +=1
            time.sleep(3)
            print ""
            print count
            url = "https://www.airbnb.com/rooms/"+row['id']
            print url
          
            try:
                # Get URL
                response = urllib2.Request(url, headers=hdr)
                pagedata = urllib2.urlopen(response)
                if pagedata.geturl() == url:
                    exists = 1
                    print "URL found! :)"
                    html = pagedata.read()
                    # Get page code
                    soup = BeautifulSoup(html, "html.parser")

                    try:
                        nreviewsRaw = soup.select("#reviews ._1xu9tpch")[0].get_text().encode('utf-8').strip()
                        nreviewsArray = re.match('\d{1,}',nreviewsRaw)
                        nreviews = nreviewsArray.group()
                        #nreviews = nreviews.translate(None, ' Reviews').encode('utf-8').strip()

                        lastreviews = soup.select("#reviews ._17oldnte")
                        lrdates = []
                        for lr in lastreviews:
                            lrstr = lr.get_text().encode('utf-8').strip()
                            lrdate = datetime.strptime(lrstr,'%B %Y')
                            lrdates.append(lrdate)

                        lastreview = max(lrdates)
                        print "Reviews data found"

                    except:
                        nreviews = 0
                        lastreview = 0
                        print "No reviews for this listing"

                else:
                    exists = 0
                    nreviews = 0
                    lastreview = 0
                    print "URL not found :("

            except:
                exists = 0
                nreviews = 0
                lastreview = 0
                print "URL not found :("

            writer.writerow([row['id'],row['name'],row['host_id'],row['host_name'],row['neighbourhood_group'],row['neighbourhood'],row['latitude'],row['longitude'],row['room_type'],row['price'],row['minimum_nights'],row['number_of_reviews'],row['last_review'],row['reviews_per_month'],row['calculated_host_listings_count'],row['availability_365'],nreviews,lastreview,exists])
            print "Data saved."

            pagedata.close()
# FIN scraping

# comprobando scraping: buscando registros fallidos. Tienen que cumplir tres condiciones:
#+ que exista url
#+ que las reviews de datahippo sean distintas de 0
#+ que las reviews del scraping sean 0
count=0
with open(outputpathrev, "w") as outfile:
    
    writer = csv.writer(outfile)
    writer.writerow(['id','name','host_id','host_name','neighbourhood_group','neighbourhood','latitude','longitude','room_type','price','minimum_nights','number_of_reviews','last_review','reviews_per_month','calculated_host_listings_count','availability_365','url','reviews_'+today,'lastreview_'+today,'exists_'+today])
    with open(outputpath, "r") as f:
        csvf = csv.DictReader(f, delimiter=',')
        for row in csvf:

            # uncomment the following two lines
            # to debug with first 5 rows of input file
            #if count == 5:
            #    break

            count +=1

            # comprobacion de registro fallido y rescrapeo si dio fallo
            if row['exists_'+today] == '1' and row['reviews'] != '0' and row['reviews_'+today] == '0':
                time.sleep(3)
                print ""
                print count
                url = row['url']
                print url

                try:
                    # Get URL
                    response = urllib2.Request(url, headers=hdr)
                    pagedata = urllib2.urlopen(response)
                    if pagedata.geturl() == url:
                        exists = 1
                        print "URL found! :)"
                        html = pagedata.read()
                        # Get page code
                        soup = BeautifulSoup(html, "html.parser")
    
                        try:
                            nreviewsRaw = soup.select("#reviews ._1xu9tpch")[0].get_text().encode('utf-8').strip()
                            nreviewsArray = re.match('\d{1,}',nreviewsRaw)
                            nreviews = nreviewsArray.group()
                            #nreviews = nreviews.translate(None, ' Reviews').encode('utf-8').strip()
    
                            lastreviews = soup.select("#reviews ._17oldnte")
                            lrdates = []
                            for lr in lastreviews:
                                lrstr = lr.get_text().encode('utf-8').strip()
                                lrdate = datetime.strptime(lrstr,'%B %Y')
                                lrdates.append(lrdate)
    
                            lastreview = max(lrdates)
                            print "Reviews data found"
    
                        except:
                            nreviews = row['reviews_'+today]
                            lastreview = row['lastreview_'+today]
                            print "No reviews for this listing"
    
                    else:
                        exists = 0
                        nreviews = row['reviews_'+today]
                        lastreview = row['lastreview_'+today]
                        print "URL not found :("
    
                except:
                    exists = 0
                    nreviews = row['reviews_'+today]
                    lastreview = row['lastreview_'+today]
                    print "URL not found :("

            else:
                url = row['url']
                exists = row['exists_'+today]
                nreviews = row['reviews_'+today]
                lastreview = row['lastreview_'+today]

    
            writer.writerow([row['id'],row['name'],row['host_id'],row['host_name'],row['neighbourhood_group'],row['neighbourhood'],row['latitude'],row['longitude'],row['room_type'],row['price'],row['minimum_nights'],row['number_of_reviews'],row['last_review'],row['reviews_per_month'],row['calculated_host_listings_count'],row['availability_365'],nreviews,lastreview,exists])
            print "Data saved."
    

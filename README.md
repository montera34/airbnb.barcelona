Ciencia Participativa: Taller de datos sobre alojamientos turísticos
====================================================================

28 y 29 de octubre 2018 en Barcelona

Documentación sobre los análisis y datos de anáĺisis de alojamientos turísticos de Barcelona.
Más información https://meta.decidim.org/assemblies/eix-lab/f/130/meetings/1135?locale=es 
Informe Efecto Airbnb https://lab.montera34.com/airbnb/barcelona/

# Structure of files and directories

This is our simplified tree of directories and files

├── analisis																	To store scripts for analysys
│   ├── airbnb-analisis.R												analysis: room type, listings per neighbourhood, ratio listings / dwelings, Choropleth maps, export to geojson
│   ├── cartograma-barcelona										D3 cartogram fo airbnb ratio
│   ├── compare-datasets-insideairbnb.Rmd				Compares two data sets of Insideairbnb
│   ├── compare-datasets.Rmd										Compares two data sets, datahippo?
│   ├── economia-colaborativa.R
│   ├── eliminados														
│   │   ├── links-listings-posts-20180401.csv
│   │   └── scraped-listings-post2018-03_barcelona-insideairbnb.gexf
│   ├── eliminados.R														Analyses removed listings
│   ├── evolucion.R															Analysis evolution in neighbourhoods and districts
│   ├── hosts-analysis-barcelona.html						
│   ├── hosts-analysis.html
│   ├── hosts-analysis.Rmd											Host and listings concentration
│   ├── informes																Reports generated with .Rmd files				
│   ├── points-in-polygons.R										calculates location of point (output location of listings in barrios)
│   ├── preanalisis-airbnb.Rmd									host concentration analysis
│   └── review-analysis.R												review analysis (estacionalidad)
├── data																		where to store data
│   ├── original															original data
│   │   ├── airbnb															airbnb data
│   │   │   ├── 150430
│   │   │   │   ├── data
│   │   │   │   │   └── listings.csv.gz [not included in repo). Use 
│   │   │   │   ├── listings_summary_barcelona_insideairbnb.csv
│   │   │   │   └── reviews_summary_barcelona_insideairbnb.csv
│   │   │   ├── (...)
│   │   │   └── 190308
│   │   │       ├── data
│   │   │       ├── listings_summary_barcelona_insideairbnb.csv
│   │   │       └── reviews_summary_barcelona_insideairbnb.csv
│   │   ├── contornos														shapes
│   │   └── demografia-vivienda	
│   └── output																processed data
│       └── airbnb
├── images																	output images
│   └── airbnb
│       ├── eliminados
│       ├── hosts
│       │   ├── mapas
│       ├── reviews
├── README.md
├── scraping
│   ├── airbnb.lastreview.py
│   └── get-insideairbnb-data.R
├── taller
│   └── mango 																files of one of the workshop groups

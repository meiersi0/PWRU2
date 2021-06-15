library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(plyr)
library(viridis)
library(tidyr)

tag9035 <- read_delim("tag9035_gps.txt",",")
tag9221 <- read_delim("tag9221_gps.txt",",")
tag9229 <- read_delim("tag9229_gps.txt",",")


tag9035%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

tag9221%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

tag9229%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

tag_wald <- tag9035%>%filter(
  longitude > 8.8, longitude <8.8265,
  latitude >47.245, latitude <47.2925)

tag_wiese <- tag9221%>%filter(
  longitude > 8.8, longitude <8.8265,
  latitude >47.245, latitude <47.2925)

tag_waldrand <- tag9229%>%filter(
  longitude > 8.8, longitude <8.8265,
  latitude >47.245, latitude <47.2925)

tag_wald%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

tag_wiese%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

tag_waldrand%>%
  ggplot(aes(longitude, latitude))  +
  geom_point(aes()) +
  coord_fixed()

ggplot()  +
  geom_point(data=tag_wald, aes(longitude, latitude, color = "green")) +
  geom_point(data=tag_wiese, aes(longitude, latitude, color = "red")) +
  geom_point(data=tag_waldrand, aes(longitude, latitude, colour = "blue")) +
  coord_fixed()

#________________________________________________________________________________
#Vergleich der Sender

tag_9035_Verg <- tag9035%>%filter(
  longitude > 8.7, longitude <8.757,
  latitude >47.24, latitude <47.248)

tag_9221_Verg <- tag9221%>%filter(
  longitude > 8.7, longitude <8.757,
  latitude >47.24, latitude <47.248)

tag_9229_Verg <- tag9229%>%filter(
  longitude > 8.7, longitude <8.757,
  latitude >47.24, latitude <47.248)

ggplot()  +
  geom_point(data=tag_9035_Verg, aes(longitude, latitude, color = "green")) +
  geom_point(data=tag_9221_Verg, aes(longitude, latitude, color = "red")) +
  geom_point(data=tag_9229_Verg, aes(longitude, latitude, colour = "blue")) +
  coord_fixed()





#Mittelpunkt klakulieren
tag_wald$long<- as.numeric(summarise(group_by(tag_wald,longitude),center = mean(longitude, na.rm = TRUE)))
tag_wald$lat<- as.numeric(summarise(group_by(tag_wald,latitude), center= mean(latitude, na.rm = TRUE)))


#Euclidean dist zu Mittelpunkt
tag_wald<-tag_wald%>%mutate(eucl_dist = sqrt((longitude-long.center)^2+(latitude-lat.center)^2)) #]wieso geht das nicht????
         




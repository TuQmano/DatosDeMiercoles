# install.packages(readr)

library(tidyverse)
library(sf)
library(leaflet)


terremotos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-29/terremotos.csv")

mes <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-29/mes.csv")

terremotos
mes


top100 <- terremotos %>% 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = 4326) %>% 
  arrange(desc(magnitud)) %>% 
  top_n(100)  
  
leaflet(top100) %>% 
  addCircles(color ="red", weight = 15) %>% 
  addProviderTiles(providers$Stamen.Watercolor)
  

#AIRBNB

library(tidyverse)
library(sf)
library(ggforce)

buenos_aires <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/buenos_aires.csv")

bsas_geo <- read_sf("http://data.insideairbnb.com/argentina/ciudad-aut%C3%B3noma-de-buenos-aires/buenos-aires/2019-04-17/visualisations/neighbourhoods.geojson") %>% 
  rename('barrio' = 'neighbourhood') %>% 
  select(barrio) %>% 
  print()



airbnb_bsas <- buenos_aires  %>% 
  group_by(barrio) %>% 
  summarise(n = n()) %>% 
  left_join(bsas_geo)%>% 
  st_as_sf() %>% 
  print()
  
  ####
  airbnb_bsasXTipo <-  buenos_aires %>% 
  group_by(barrio, tipo_alojamiento)  %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  left_join(bsas_geo)%>% 
  st_as_sf() %>% 
  mutate(area_mk2 = as.numeric(st_area(geometry)/1000000), 
         airbnbXkm2 = n/area_mk2, 
         pct_barrio = n/sum(n)*100) %>% 
  ungroup() %>% 
  mutate(pct_total = n/sum(n)*100, 
         acumulado_total = cumsum(pct_total))  %>% 
  print(n = Inf)


#### Airbnb´s por barrio

ggplot(airbnb_bsas) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  #theme(legend.position = "bottom") +
  labs(fill = "Cantidad", 
       title = "Publicaciones de Airbnb", 
       subtitle = "48 barrios de Buenos Aires", 
       caption = "@TuQmano con datos de @R4DS_es")


## TIPO DE ALOJAMIENTO
  airbnb_bsasXTipo %>% 
    mutate(top10 = ifelse(acumulado_total <65.5, "top10", "low90")) %>% 
    ggplot() +
    geom_sf(aes(fill = top10))+
    theme_minimal() +
    theme(axis.text = element_blank(), 
          legend.position = "none") +
    #theme(legend.position = "bottom") +
    labs(fill = "Cantidad", 
         title = "Publicaciones de Airbnb (Top10)", 
         subtitle = "8 barrios acumulan el 65.5% de las publicaciones", 
         caption = "@TuQmano con datos de @R4DS_es") +
    scale_fill_brewer(type = "div", palette = 5, direction = -1)
  
  
  # TOP 10 TODOS LOS BARRIOS
  airbnb_bsasXTipo %>% 
    mutate(top10 = ifelse(acumulado_total <65.5, "top10", "low90")) %>% 
    ggplot() +
    geom_sf(aes(fill = airbnbXkm2))+
    facet_grid(~tipo_alojamiento) +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          legend.position = "none") +
    #theme(legend.position = "bottom") +
    labs(fill = "Cantidad", 
         title = "Publicaciones de Airbnb", 
         subtitle = "8 barrios acumulan el 65.5% de las publicaciones", 
         caption = "@TuQmano con datos de @R4DS_es") +
    scale_fill_viridis_c("volcano")
  
  
  # 8 BARRIOS
  airbnb_bsasXTipo %>% 
    mutate(top10 = ifelse(acumulado_total <65.5, "top10", "low90")) %>% 
    filter(top10 == "top10") %>% 
    ggplot() +
    geom_sf(aes(fill = pct_total))+
    theme_minimal() + 
    scale_fill_gradient2() +
    facet_grid(~tipo_alojamiento) +
    theme(axis.text = element_blank(), 
          legend.position = "bottom") +
    labs(fill = "% Total \n (top 10)", 
         title = "Publicaciones de Airbnb", 
         subtitle = "8 barrios por tipo de alojamiento ofrecido", 
         caption = "@TuQmano con datos de @R4DS_es")

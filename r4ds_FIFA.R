
#LIBRERIAS 

library(tidyverse)
library(readr)
library(gganimate)
library(ggthemes)
library(treemapify)
#  devtools::install_github("rensa/ggflags") # Instalar por primera vez
library(ggflags) # Para geom_flags
library(gifski) # Para hacer render del GIF
library(countrycode)  # Para obtener codigos de paises


### CARGAMOS DATOS
fifa <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t") %>% 
  print()

### TRABAJAMOS LOS DATOS PARA QUE SIRVAN PARA LOS GRAFICOS
 data <- fifa %>% 
  filter(str_detect(equipo_1, "Argen") |  str_detect(equipo_2, "Argen") ) %>% 
  select(anio, contains("equip"), anfitrion) %>% 
   mutate(campeonato =  paste0(anfitrion , " " , as.character(anio))) %>% 
  group_by(campeonato) %>% 
  mutate(jugados_anio = n()) %>% 
  ungroup() %>% 
  mutate(resultado = ifelse((equipo_1 == "Argentina" & equipo_1_final - equipo_2_final > -1 ), 
                            "G",ifelse(equipo_1 == "Argentina" & equipo_1_final - equipo_2_final == 0,
                                       "E", "P")), 
         argentina = ifelse(equipo_1 == "Argentina", equipo_1, "Argentina"), 
         rival = ifelse(equipo_1 != "Argentina", equipo_1, equipo_2), 
         goles = equipo_1_final + equipo_2_final, 
         marcador = paste0(equipo_1, " ", equipo_1_final, " - ", equipo_2, " ", equipo_2_final)) %>% 
  group_by(argentina, rival) %>% 
  mutate(n = n()) %>% 
  arrange(desc(jugados_anio)) %>% 
  ungroup()  %>% 
  print(n = Inf) 

 
 
 ### GRAFICO DE ARBOL

ggplot2::ggplot(data, ggplot2::aes(area = goles , fill = campeonato, 
                                        subgroup = campeonato,
                                   subgroup2 = marcador)) +
  geom_treemap()+
  geom_treemap_subgroup_border(color = "black") +
  geom_treemap_subgroup_text(size = 30, alpha =.5) +
  geom_treemap_subgroup2_text(size = 14, place = "center") +
  guides(fill = F) +
  labs(title = "Resultados de Argentina en los Mundiales FIFA (1930 - 2014)", 
       subtitle = "Por @TuQmano con datos de @r4ds_es", 
       caption = "(Tamaños de los recuadros en función de cantidad de goles convertidos)") +
  theme(text = element_text(size = 25, family = "Arial", face = "bold"))
  

library(tidyverse)
library(ggparliament)
library(patchwork)


#CARGAMOS DATOS
datos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv") %>% 
  print()

#FILTRAMOS PAIS - ARGENTINA
datos_AR <- datos %>% 
  filter(str_detect(pais, "Arg")) %>% 
  print()



# TRANSFORMAMOS DATOS PARA CAMARA BAJA (DIPUTADES)
diputadosAR <- datos_AR %>% 
  filter(camara == "baja") %>% 
  mutate(ellas = round(porcentaje_mujeres/100 * numero_integrantes), 
         ellos = numero_integrantes - ellas) %>% 
  gather(genero, n, ellas:ellos) %>% 
  select(genero, n) %>% 
  print()


# TRANSFORMAMOS DATOS PARA CAMARA ALTA (SENADORES)
senadoresAR <- datos_AR %>% 
  filter(camara == "alta") %>% 
  mutate(ellas = round(porcentaje_mujeres/100 * numero_integrantes), 
         ellos = numero_integrantes - ellas) %>% 
  gather(genero, n, ellas:ellos) %>% 
  select(genero, n) %>% 
  print()


# FORMATO DE DATOS PARA ggparliament ####


#DIPUTADES
AR_data_dip <- parliament_data(election_data = diputadosAR, 
                           type = "semicircle", 
                           party_seats = diputadosAR$n, 
                           parl_rows = 6) %>% 
  as_tibble() %>% 
  print()

#SENADORES 

AR_data_sen <- parliament_data(election_data = senadoresAR, 
                               type = "semicircle", 
                               party_seats = senadoresAR$n, 
                               parl_rows = 3) %>% 
  as_tibble() %>% 
  print()



# PLOTS POR SEPARADO GUARDADOS COMO OBJETO

# DIPUTADES

plot_dipu <- ggplot2::ggplot(AR_data_dip, ggplot2::aes(x, y, colour = genero)) +
  geom_parliament_seats(size = 5) +
  draw_majoritythreshold(n = 128, label = F, type = 'semicircle')+
  geom_highlight_government(genero == "ellas", colour = "black", size = 5) +
  theme_ggparliament() +
  scale_color_manual(values = c("purple", "blue")) +
  theme(text = element_text(family = "AvenirNext LT Pro Bold")) +
  labs(title = "¿Cuánto falta para la paridad de género en Argentina?", 
       subtitle = "Cámara de Diputados (n = 257)") +
  theme(legend.position = "none")


#SEANDORES
plot_sen <- ggplot2::ggplot(AR_data_sen, ggplot2::aes(x, y, colour = genero)) +
  geom_parliament_seats(size = 5) +
  draw_majoritythreshold(n = 37, label = F, type = 'semicircle')+
  geom_highlight_government(genero == "ellas", colour = "black", size = 5) +
  theme_ggparliament() +
  scale_color_manual(values = c("purple", "blue")) +
  theme(text = element_text(family = "AvenirNext LT Pro Bold")) +
  labs(subtitle = "Cámara de Senadores (n = 72)", 
       caption = "Por @TuQmano con datos de @r4ds_es", 
       color = "Género") +
  theme(legend.position = "bottom")


# ARAMAMOS UN GRAFICO COMBINANDO LOS 2 PLOTS (con libreria patchwork)

plot_dipu + plot_sen + plot_layout(ncol = 1) # el layout en una columna nos permite generar una imagen vertical (default lado a lado)

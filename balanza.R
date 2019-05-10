library(tidyverse)
library(readxl)
library(ggthemes)
library(readr)

#CARGAMOS DATOS
comercio<- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")

# TRANSFORAMMOS DATOS 
data_ar <- comercio %>%
  as_tibble() %>% 
  filter(codigo_iso_origen == "arg" | codigo_iso_destino == "arg", #FILTARMOS PAIS
         anio == 2017) %>%  # FILTRAMOS Año
  group_by(nombre_comunidad_producto) %>% 
  summarise(expo = sum(valor_exportado_dolares/1000000), # TOTALES DE EXPOS EN MILLONES
            impo = sum(valor_importado_dolares/1000000)) %>%  # TOTALES DE IMPOS EN MILLONES
  mutate(balanza = expo - impo) %>%  # BALANZA COMERCIAL
  ungroup()  %>% 
  print(n = Inf) 

# GENERAMOS GRAFICO
  ggplot(data = data_ar,
         aes(balanza, fct_reorder(nombre_comunidad_producto, balanza))) +
  geom_point(colour = ifelse(data_ar$balanza > 0, "blue", "red"), size = log10(data_ar$expo)) +
  theme_tufte() +
  geom_vline(aes(xintercept = 0), colour = "black", linetype = "dotted") +
  labs(title = "Balanza Comercial Argentina (2017)", 
       subtitle = "@TuQmano con datos de @r4ds_es - #MiércolesDeDatos ", 
       x = "Exportaciones - Importanciones (Millones)", 
       y = "", 
       caption = "(El tamaño de los puntos refleja el peso relativo del sector)") 
  
             


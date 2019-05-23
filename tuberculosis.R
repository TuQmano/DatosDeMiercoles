# R4ds  23 de mayo


library(tidyverse)


tuberculosis_oms <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-22/tuberculosis_oms.csv") %>% 
  print()


tuberculosis_oms %>% 
  filter(iso2 == "AR") %>% 
  gather(tipo, n, 5:60) %>% 
  mutate(casos = ifelse(str_detect(tipo, "recaida"), "recaida", "nuevo"))%>%
  filter(casos != "recaida") %>% 
  select(- casos) %>% 
  filter(!is.na(n)) %>% 
  mutate(tipo = str_remove(tipo, "nuevos_")) %>% 
  separate(tipo, into = c("tipo", "paciente"), sep = "_") %>% 
  mutate(genero = str_sub(paciente, 1, 1), 
         edad = str_sub(paciente, 2), 
         edad = ifelse(str_length(edad) == 3 ,  paste0(str_sub(edad, 1, 1), "-", str_sub(edad, 2, 3)), 
                       ifelse(str_length(edad) == 4, paste0(str_sub(edad, 1,2), "-", str_sub(edad, 3,4)), edad))) %>% 
  select(pais:anio, tipo, n , genero, edad) %>% 
  filter(anio == max(anio))%>% 
  group_by(edad, genero) %>% 
  mutate(total_edad = sum(n)) %>% 
  print() -> ARG


etiquetas <- c("Hombre", "Mujer")
names(etiquetas) <- c("h", "m")

ggplot(ARG) +
  geom_col(aes(edad, n, fill = tipo)) +
  facet_grid(~ genero, labeller = labeller(genero = etiquetas)) +
  geom_text(aes(edad, total_edad, label = total_edad), vjust = -.2, size = 4) +
  scale_fill_viridis_d(labels =  c("Extra Pulmonar", 
                                   "Frotosis Pulmonar Negativo",
                                   "Frotosis Pulmonar Positivo"), option = "cividis") +
  theme_void() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(face = "bold")) +
  labs(fill = "Tipo de Diagnóstico", 
       title = "Tuberculosis en Argentina (2012)", 
       x = "EDAD")





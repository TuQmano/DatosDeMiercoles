#PAQEUTES NECESARIOS 
library(Rspotify)
library(tidyverse)
library(treemapify)


#########################
app_id <- "nombre de la app"
client_id <- "clave_que_se_genera_en_la_app"
client_secret <- "clave_secreta_de_la_app"

# INSTRUCCIONES PARA CREAR LA APP PARA PODER DESCARGAR DATA ACA ----> https://github.com/cienciadedatos/datos-de-miercoles/tree/master/datos/2019/2019-05-15


#### DESCARGAR DATA 

########### Extraer Top 50 ############



keys <- spotifyOAuth("app_id", "client_id", "client_secret") # AUTORIZA SPOTIFY 

# DESCARGA DATA DE LATINOAMERICA

paises_es <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica",
               "Cuba","la Republica Dominicana", "Dominican Republic",
               "Ecuador", "El Salvador", "Equatorial Guinea", "España",
               "Guatemala", "Honduras", "México", "Nicaragua", "Panamá",
               "Paraguay", "Perú", "Puerto Rico", "Uruguay", "Venezuela")
user_playlists_1 <- getPlaylists("qn9el801z6l32l2whymqqs18p", token = keys)
user_playlists_2 <- getPlaylists("qn9el801z6l32l2whymqqs18p", 50, token = keys)
tops_50 <- rbind(user_playlists_1, user_playlists_2)
# encontré aparte el de venezuela que no estaba incluido
tops_50 <- rbind(tops_50, c("624oAiyjMdmpdJWIylharU", "El Top 50 de Venezuela", "suo2sbl91eeth3elwrfuq7qwn", 50))

paises <- purrr::map_chr(tops_50$name, ~ str_remove(.x, "El Top 50 de "))
bool_es <- purrr::map_lgl(paises, ~ .x %in% paises_es)
tops_50_es <- tops_50[bool_es, ]

viralcharts_user = "qn9el801z6l32l2whymqqs18p"

canciones_tops50_es <- purrr::map(tops_50_es$id[-length(tops_50_es$id)],
                                  ~ getPlaylistSongs(user_id = viralcharts_user,
                                                     .x,
                                                     token = keys))
canciones_tops50_es[[18]] <- getPlaylistSongs(user_id = "suo2sbl91eeth3elwrfuq7qwn",
                                              "624oAiyjMdmpdJWIylharU",
                                              token = keys)

dataset_canciones = tibble()
for (i in 1:length(canciones_tops50_es)) {
  dataset_canciones = rbind(dataset_canciones, cbind(canciones_tops50_es[[i]],
                                                     top = as.character(tops_50_es$name)[i],
                                                     numero = 1:nrow(canciones_tops50_es[[i]])))
}
features_canciones = tibble()
for (j in 1:nrow(dataset_canciones)) {
  features_canciones = rbind(features_canciones,
                             getFeatures(dataset_canciones$id[j], keys))
}
dataset_spotify = cbind(dataset_canciones, features_canciones)

fechas = purrr::map(unique(dataset_spotify$album_id), ~getAlbumInfo(.x, keys)[1, 6])
album_fechas =  tibble(album_id = unique(dataset_spotify$album_id),
                       fecha = as.character(unlist(fechas)))
dataset_spotify = dataset_spotify[, -2] %>%
  left_join(album_fechas, by = "album_id")

dataset_spotify = dataset_spotify %>%
  select(-id, -artist_id, - album_id, -uri, -analysis_url)

nombres_columnas = c("cancion", "popularidad", "artista", "artista_completo",
                     "album", "top_pais", "puesto", "bailabilidad", "energia",
                     "nota_musical", "volumen", "modo", "hablado", "acustico",
                     "instrumental","en_vivo", "positividad", "tempo",
                     "duracion", "tiempo_compas", "fecha")
colnames(dataset_spotify) <- nombres_columnas

#########

# Analisis de datos de Argentina 

datos_arg <- dataset_spotify %>% as_tibble() %>% 
  filter(str_detect(top_pais , "Argentina"))  %>% 
  group_by(artista) %>% 
  mutate(n = n()) %>% 
  arrange(puesto)%>%
  select(n , everything()) %>% 
  print(n = Inf)


### GRAFICO DE ARBOL


ggplot2::ggplot(datos_arg, ggplot2::aes(area = max(puesto)-puesto , fill = artista, 
                                   subgroup = artista,
                                   subgroup2 = cancion, subgroup3 = puesto)) +
  geom_treemap()+
  geom_treemap_subgroup_border(color = "black") +
  geom_treemap_subgroup_text(size = 13, alpha =.65)  +
  geom_treemap_subgroup2_text(size = 11, alpha =.9,  place = "center")+
  geom_treemap_subgroup3_text(size = 16,  place = "topright")+
  guides(fill = F) +
  labs(title = "Qué música escuchan les argentines?", 
       subtitle = "TOP 50 canciones/ artistas de Soptify en Argentina",
       caption = "(@TuQmano - #DatosDeMiércoles)")


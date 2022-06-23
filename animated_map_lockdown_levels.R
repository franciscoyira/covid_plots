library(tidyverse)
library(gganimate)
library(sinimr) # https://github.com/robsalasco/sinimr
library(sf)

# References:
# https://gganimate.com/articles/gganimate.html
# https://stackoverflow.com/questions/52332967/problem-with-many-50-states-in-gganimate

# Import polygons of Santiago municipalities with urban borders
varcode <- 882

var <- get_sinim(varcode, 2018, 
                 region = 13, 
                 truevalue = T, 
                 geometry = T, 
                 auc = T, 
                 unit = "limites")

# Importar lockddown level data
pasoapaso_raw <- 
  read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto74/paso_a_paso.csv")

# Filter to use only data of Santiago
pasopaso_slice <- 
  pasoapaso_raw %>% 
  filter(codigo_region == 13) %>% 
  select(codigo_region:`2022-04-13`) %>%
  pivot_longer(
    cols = -(1:5),
    names_to = "fecha",
    values_to = "paso"
  ) %>%
  filter(zona != "Rural") %>% 
  mutate(fecha = lubridate::ymd(fecha))

mapa_comunas_pasoapaso <- 
  var %>% 
  left_join(pasopaso_slice, by = c("code" = "codigo_comuna")) %>% 
  mutate(paso = factor(paso,
                       levels = c(1,2,3,4),
                       labels = c("Cuarentena (full lockdown)", 
                                  "Transición (weekend lockdown)", 
                                  "Preparación (indoor dinning allowed)", 
                                  "Apertura Inicial (less restrictions)")))
# Create animated map
mapa_animado <- 
  mapa_comunas_pasoapaso %>% 
  ggplot() +
  geom_sf(aes(fill = paso)) +
  scale_fill_manual(
    values = c("Cuarentena (full lockdown)" = "#F75C5C",
               "Transición (weekend lockdown)" = "#CC9800",
               "Preparación (indoor dinning allowed)" = "#FFF200",
               "Apertura Inicial (less restrictions)" = "#338AD1")
  ) +
  theme_void() +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))+
  labs(fill = "Lockdown level") +
  transition_time(fecha) +
  ggtitle('Lockdown levels in municipalities of Santiago (Urban area), by date',
          subtitle = "{gsub('^0', '', format(frame_time, ('%d %B, %Y')))}") 

# Export to GIF
animate(mapa_animado, nframes = n_distinct(pasopaso_slice$fecha),
        height = 335, width = 600)

anim_save("lockdown_levels_santiago.gif")
library(tidyverse)

bigrona_activos_comuna_raw <- 
  read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto19/CasosActivosPorComuna.csv") 

# Qué son los NA en Codigo Comuna?
bigrona_activos_comuna_raw %>% 
  filter(is.na(`Codigo comuna`)) %>%
  View()

# Son totales de regionales o casos sin atribución de comuna

# Eliminando filas que no corresponden a comuna, y cambiando a formato long
bigrona_activos_comuna_clean <- 
  bigrona_activos_comuna_raw %>% 
  filter(!is.na(`Codigo comuna`)) %>% 
  pivot_longer(cols = -(Region:Poblacion),
               names_to = "fecha",
               values_to = "casos_activos") %>% 
  rename(cod_region = `Codigo region`,
         cod_comuna = `Codigo comuna`) %>% 
  mutate(fecha = lubridate::ymd(fecha)) %>% 
  # Filtro de fechas para reproducibilidad
  filter(fecha <= lubridate::ymd(20210104))

# Filtrando para dejar solo la RM y agregando categoría de comunas
comunas_fase_3_ene2020 <- 
  c("Alhue", "Calera de Tango", "Isla de Maipo", "Tiltil", "Penaflor",
  "Quilicura", "El Monte", "La Pintana", "Conchali", "Colina", "Melipilla",
  "Talagante", "Lampa", "Recoleta", "Huechuraba", "San Bernardo", "Maria Pinto",
  "Buin", "Paine", "San Pedro", "Pirque")

activos_comuna_rm <- 
  bigrona_activos_comuna_clean %>% 
  filter(Region == "Metropolitana") %>% 
  mutate(fase_ene2020 =
           ifelse(Comuna %in% comunas_fase_3_ene2020,
                  "Fase 3",
                  "Fase 2"))

poblacion_por_tipo_comuna <- 
  activos_comuna_rm %>% 
  distinct(Comuna, fase_ene2020, Poblacion) %>% 
  group_by(fase_ene2020) %>% 
  summarise(Poblacion = sum(Poblacion))

activos_por_fase <-  
  activos_comuna_rm %>% 
  group_by(fase_ene2020, fecha) %>% 
  summarise(casos_activos = sum(casos_activos), .groups = "drop") %>% 
  left_join(poblacion_por_tipo_comuna) %>% 
  mutate(casos_x_100mh = (casos_activos/Poblacion)*100000)

# Grafico de linea
min_fecha <- lubridate::ymd(20200915)

grafico_casos_por_fase <- 
  activos_por_fase %>% 
  filter(fecha >= min_fecha) %>% 
  ggplot() +
  geom_line(aes(fecha, casos_x_100mh, color = fase_ene2020),
            size = 1.2) +
  
  scale_color_manual(values = c("Fase 2" = "#BF7200",
                                "Fase 3" = "#A69B00")) +
  ggthemes::theme_fivethirtyeight() +
  labs(y = "Casos activos por 100 mil habitantes",
       color = "Grupo de comunas según fase al 4 de enero 2020") +
  theme(legend.position = "bottom",
        axis.title.y = element_text()) +
  guides(color =guide_legend(nrow=1,byrow=TRUE,
                             label.hjust = -1, label.position = "right",
                             title.hjust = 1))

grafico_casos_por_fase

# Agregarle comunas emblemáticas
activos_comunas_top_rm <- 
  activos_comuna_rm %>% 
  filter(Comuna %in% c("Maipu", "Santiago", "Las Condes", "Puente Alto",
                       "Vitacura", "Pudahuel")) %>% 
  mutate(casos_x_100mh = (casos_activos/Poblacion)*100000) %>% 
  filter(fecha >= min_fecha)

grafico_casos_por_fase +
  geom_line(data = activos_comunas_top_rm,
            aes(fecha, casos_x_100mh,
                group = Comuna), linetype = "dashed", alpha = 0.4,
            color = "#BF7200") +
  geom_point(data = activos_comunas_top_rm %>% 
               filter(fecha == max(fecha)),
             aes(fecha, casos_x_100mh),
             color = "#BF7200") +
  geom_text(data = activos_comunas_top_rm %>%
              filter(fecha == max(fecha)),
            aes(fecha, casos_x_100mh,
                label = Comuna),
            color = "#BF7200", hjust = 0, nudge_x = 1) +
  expand_limits(x = max(activos_por_fase$fecha) + 15) +
  lemon::coord_capped_cart() +
  ggsave(here("outputs", "casos_activos_por_fase.png"),
         dpi = 400,
         # IG stories aspect ratio
         width = 9, height = 9,
         units = "cm", type = "cairo-png", scale = 1.8)


# TODO: ocultar breaks verticales posteriores a última fecha




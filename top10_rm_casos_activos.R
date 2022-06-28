library(tidyverse)
library(gt)

bigrona_activos_comuna_raw <- 
  read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto19/CasosActivosPorComuna.csv") 

# TODO: set a fixed week to compare

bigrona_activos_comuna_clean <- 
  bigrona_activos_comuna_raw %>% 
  filter(!is.na(`Codigo comuna`)) %>% 
  pivot_longer(cols = -(Region:Poblacion),
               names_to = "fecha",
               values_to = "casos_activos") %>% 
  rename(cod_region = `Codigo region`,
         cod_comuna = `Codigo comuna`) %>% 
  mutate(fecha = lubridate::ymd(fecha))

activos_comuna_rm <- 
  bigrona_activos_comuna_clean %>% 
  filter(Region == "Metropolitana")

## Tabla de comunas top 10 
## Ranking por casos activos
df_ranking_casos_act <- 
  activos_comuna_rm %>% 
  mutate(casos_x_100mh = (casos_activos/Poblacion)*100000) %>% 
  select(Comuna, fecha, casos_x_100mh) %>% 
  filter(fecha %in% c(lubridate::ymd("2021-07-30"), lubridate::ymd("2020-07-31"))) %>% 
  mutate(fecha = format(fecha, "%d %b %Y")) %>% 
  pivot_wider(
    id_cols = Comuna,
    values_from = casos_x_100mh,
    names_from = fecha
  ) %>% 
  mutate(variacion = .[[3]] - .[[2]]) %>% 
  arrange(desc(.[[3]])) %>% 
  slice_head(n = 10)

table_top10_active_cases <- 
  df_ranking_casos_act %>% 
  rename(`Variation` = variacion,
         Municipality = Comuna) %>% 
  gt() %>% 
  fmt_number(
    columns = 2:4,
    decimals = 1
  ) %>% 
  tab_spanner(
    label = "Active cases per 100 thousand people",
    columns = 2:4
  ) %>% 
  tab_header(md("**The 10 municipalities with the highest rate of active cases in Chile's Metropolitan Region**")) %>% 
  tab_options(data_row.padding = px(15)) %>% 
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("red", 0.7)),
      cell_text(color = "white", weight = "bold")
    ),
    locations = list(
      cells_body(
        columns = 4,
        rows = `Variation` > 0)
    )) %>% 
  data_color(
    columns = 3, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>% 
  tab_source_note(
    md("**Data:** github.com/MinCiencia/Datos-COVID19")) %>% 
  tab_source_note(
    md("**Code:** github.com/franciscoyira/covid_plots")) %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left")

gtsave(table_top10_active_cases,
       filename = "gtable_top10_active_cases.png",
       path = here::here("outputs"),
       expand = 10,
       vwidth  = 600,
       vheight = 1200)

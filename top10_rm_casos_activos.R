library(tidyverse)
library(gt)

bigrona_activos_comuna_raw <- 
  read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto19/CasosActivosPorComuna.csv") 

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

activos_comuna_rm <- 
  bigrona_activos_comuna_clean %>% 
  filter(Region == "Metropolitana") %>% 
  mutate(fase_ene2020 =
           ifelse(Comuna %in% comunas_fase_3_ene2020,
                  "Fase 3",
                  "Fase 2"))

## Tabla de comunas top 10 
## Ranking por casos activos
df_ranking_casos_act <- 
  activos_comuna_rm %>% 
  mutate(casos_x_100mh = (casos_activos/Poblacion)*100000) %>% 
  select(Comuna, fecha, casos_x_100mh) %>% 
  filter(fecha %in% c(max(fecha), lubridate::ymd("2020-07-31"))) %>% 
  mutate(fecha = format(fecha, "%d %b %Y")) %>% 
  pivot_wider(
    id_cols = Comuna,
    values_from = casos_x_100mh,
    names_from = fecha
  ) %>% 
  mutate(variacion = .[[3]] - .[[2]]) %>% 
  arrange(desc(.[[3]])) %>% 
  slice_head(n = 10)

df_ranking_casos_act %>% 
  rename(`Variación` = variacion) %>% 
  gt() %>% 
  fmt_number(
    columns = 2:4,
    decimals = 1
  ) %>% 
  tab_spanner(
    label = "Casos activos por 100 mil habitantes",
    columns = 2:4
  ) %>% 
  tab_header(md("**Las 10 comunas de la RM con mayor tasa de casos activos**")) %>% 
  tab_options(data_row.padding = px(15)) %>% 
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("red", 0.7)),
      cell_text(color = "white", weight = "bold")
    ),
    locations = list(
      cells_body(
        columns = 4,
        rows = `Variación` > 0)
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
    md("**Datos:** github.com/MinCiencia/Datos-COVID19")) %>% 
  tab_source_note(
    md(" **Código:** github.com/franciscoyira/covid_plots")) %>% 
  opt_all_caps() %>% 
  opt_align_table_header("left")

# TODO: Aprender como modificar rango de heatmap 

# TODO: indicar que segunda columna es valor actual y primera columna es cuando
# estábamos en cuarentena

# TODO: destacar títulos con negrita

# TODO: Agregar temas https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset1=theme-code&panelset3=theme-code2&panelset4=theme-code3 

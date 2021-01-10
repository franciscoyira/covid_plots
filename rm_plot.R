library(tidyverse)
library(here)
library(modelr)
library(lubridate)

source(here("functions", "retrieve_tests_cases_region.R"))

examenes_casos_rm <- 
  retrieve_tests_cases_region() %>% 
  filter(Region == "Metropolitana")

max_fecha <- 
  examenes_casos_rm %>% 
  pull(Fecha) %>% max()

## Fitting model
model2 <- 
  lm(N_Casos ~ factor(semana) + N_Tests, data = examenes_casos_rm)

df_adj_cases_model2 <- 
  examenes_casos_rm %>% 
  mutate(N_Tests = 10000) %>% 
  add_predictions(model2, var = "adjusted_cases") %>%
  group_by(semana) %>% 
  summarise(adjusted_cases = mean(adjusted_cases))

max_semana <- df_adj_cases_model2 %>% pull(semana) %>% max()

# Third plot
color_hitos <- "firebrick3"

df_adj_cases_model2 %>% 
  filter(semana >= ymd('2020-07-15')) %>% 
  ggplot(aes(semana, adjusted_cases)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd('2020-08-10'), color = color_hitos) +
  geom_vline(xintercept = ymd('2020-09-14'), color = color_hitos) +
  geom_vline(xintercept = ymd('2020-12-07'), color = color_hitos) +
  annotate("label",
           x = ymd('2020-08-10'),
           y = 350,
           label = "Stgo. Centro avanzó\na fase 2", color = color_hitos) +
  annotate("label",
           x = ymd('2020-09-14'),
           y = 700,
           label = "Puente Alto avanzó\na fase 2", color = color_hitos) +
  annotate("label",
           x = ymd('2020-12-07'),
           y = 700,
           label = "RM retrocedió\na fase 2", color = color_hitos) +
  ggrepel::geom_text_repel(aes(label = scales::comma(adjusted_cases, 1)),
                           size = 3) +
  labs(title = "Casos diarios en Región Metropolitana\najustando por cantidad de tests PCR",
       subtitle = str_c("Asumiendo 10.000 tests diarios. Actualizado a ", 
                        format(max_fecha, "%d de %B")),
       y = "Casos diarios",
       x = "Semana",
       caption = "Datos: github.com/MinCiencia/Datos-COVID19
       Código: github.com/franciscoyira/covid_plots") +
  scale_x_date(breaks = seq(max_semana, ymd('2020-07-15'), -14),
                     date_labels = "%b %d") +
  expand_limits(y = 0) +
  #coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title.position = "plot",
        # Whitespace in top and bottom to not overlap IG stories controls
        #plot.margin = unit(c(0.7,0.1,0.7,0.1), "inches"),
        axis.title = element_text()) +
  ggsave(here("outputs", "casos_rm_stories.png"),
         dpi = 400,
         # IG stories aspect ratio
         width = 16, height = 9,
         units = "cm", type = "cairo-png", scale = 1.4)

# TODO: cambiar alineamiento de texto en hitos (probar con justificado a
# izquierda o derecha)
# TODO: make function to adapt to IG stories

  
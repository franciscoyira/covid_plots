library(tidyverse)
library(here)
library(modelr)

source(here("functions", "retrieve_tests_cases_region.R"))

examenes_casos_rm <- 
  retrieve_tests_cases_region() %>% 
  filter(Region == "Metropolitana")

max_fecha <- 
  examenes_casos_rm %>% 
  pull(Fecha) %>% max()

# Exploratory plot: Cases vs Tests
examenes_casos_rm %>% 
  mutate(Mes = lubridate::month(Fecha, label = TRUE)) %>% 
  ggplot(aes(N_Tests, N_Casos, color = Mes)) +
  geom_point() +
  geom_smooth(method = "lm")

# Regressions by week --------
# Get median of daily tests
median_tests <- 
  examenes_casos_rm %>% 
  pull(N_Tests) %>% 
  median()

# Nest by week and run regressions
casos_pred_model1 <- 
  examenes_casos_rm %>% 
  nest(data = c(-Semana)) %>% 
  mutate(reg_casos = map(data, ~lm(N_Casos ~ N_Tests, data = .)),
         pred_with_median_tests = map_dbl(reg_casos, ~predict(., newdata = tibble(N_Tests = 10000))))

# First plot
examenes_casos_rm_filt <- 
  examenes_casos_rm %>% 
  filter(Semana %in% c(31, 35, 40, 49, 51)) %>% 
  left_join(casos_pred_model1,
            by = "Semana")

examenes_casos_rm_last2days <- 
  examenes_casos_rm_filt %>% 
  filter(Fecha %in% c(lubridate::ymd(20201217, 20201219, 20201218, 20201220)))

examenes_casos_rm_filt %>% 
  ggplot(aes(N_Tests, N_Casos, color = factor(Semana))) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE) +
  ggrepel::geom_label_repel(data = examenes_casos_rm_last2days,
                          aes(label = as.character(Fecha))) +
  labs(
    subtitle = "Data for Metropolitan Region, Chile",
    x = "Number of Tests",
    y = "Number of Cases",
    color = "Week")

# Second plot
ggplot(examenes_casos_rm_filt,
       aes(N_Tests, N_Casos, color = factor(Semana))) +
  geom_point() +
  geom_vline(xintercept = 10000, color = "blue4") +
  geom_point(aes(x = 10000, y = pred_with_median_tests), size = 5,
             show.legend = FALSE) +
  geom_smooth(method = "lm",
              fullrange = TRUE,
              se = FALSE) +
  labs(
    subtitle = "Data for Metropolitan Region, Chile",
    x = "Number of Tests",
    y = "Number of Cases",
    color = "Week") +
  annotate("label",
           x = 10000*0.95,
           y = 1000,
           label = "10.000 daily tests",
           color = "blue4")

## Pred with model 2
model2 <- 
  lm(N_Casos ~ factor(Semana) + N_Tests, data = examenes_casos_rm)

df_adj_cases_model2 <- 
  examenes_casos_rm %>% 
  mutate(N_Tests = 10000) %>% 
  add_predictions(model2, var = "adjusted_cases") %>%
  group_by(Semana) %>% 
  summarise(adjusted_cases = mean(adjusted_cases))

# Function to create date from week number
week_to_date <- function(week_n) {
  week_start <- 
    as.Date(str_c("2020", week_n, "1"), "%Y%U%u") %>% 
    magrittr::subtract(7) %>% 
    format("%b %d",
           locale = locale(date_names = "en"))
  
}


# Third plot
color_hitos <- "firebrick3"

df_adj_cases_model2 %>% 
  filter(Semana >= 31) %>% 
  ggplot(aes(Semana, adjusted_cases)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 33, color = color_hitos) +
  geom_vline(xintercept = 38, color = color_hitos) +
  geom_vline(xintercept = 50.5, color = color_hitos) +
  annotate("label",
           x = 33,
           y = 350,
           label = "Stgo. Centro avanzó\na fase 2", color = color_hitos) +
  annotate("label",
           x = 38,
           y = 700,
           label = "Puente Alto avanzó\na fase 2", color = color_hitos) +
  annotate("label",
           x = 50.5,
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
  scale_x_continuous(breaks = seq(31, 51, 2),
                     labels = week_to_date) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title.position = "plot",
        # Whitespace in top and bottom to not overlap IG stories controls
        plot.margin = unit(c(0.7,0.1,0.7,0.1), "inches"),
        axis.title = element_text()) +
  ggsave(here("outputs", "casos_rm_stories.png"),
         dpi = 400,
         # IG stories aspect ratio
         width = 9, height = 16,
         units = "cm", type = "cairo-png", scale = 1.4)

# TODO: cambiar alineamiento de texto en hitos (probar con justificado a
# izquierda o derecha)
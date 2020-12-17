library(tidyverse)
library(here)

examenes_raw <- read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto7/PCR.csv")

examenes_region_dia <- 
  examenes_raw %>% 
  pivot_longer(
    cols = -c(Region, `Codigo region`, Poblacion),
    names_to = "Fecha",
    values_to = "N_Tests"
  ) %>% 
  mutate(Fecha = lubridate::ymd(Fecha),
         N_Tests = replace_na(N_Tests, 0))

casos_raw <- read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto13/CasosNuevosCumulativo.csv")

casos_region_dia <- 
  casos_raw %>% 
  pivot_longer(
    cols = -Region,
    names_to = "Fecha",
    values_to = "N_Casos"
  ) %>% 
  mutate(Fecha = lubridate::ymd(Fecha))

examenes_casos <- 
  examenes_region_dia %>% 
  left_join(casos_region_dia,
            by = c("Region", "Fecha")) %>% 
  mutate(Semana = lubridate::isoweek(Fecha))


examenes_casos_rm <- 
  examenes_casos %>% 
  filter(Region == "Metropolitana")


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
casos_pred_10000_tests <- 
  examenes_casos_rm %>% 
  nest(data = c(-Semana)) %>% 
  mutate(reg_casos = map(data, ~lm(N_Casos ~ N_Tests, data = .)),
         pred_with_median_tests = map_dbl(reg_casos, ~predict(., newdata = tibble(N_Tests = 10000))))

# First plot
examenes_casos_rm %>% 
  filter(Semana %in% c(31, 35, 40, 49)) %>% 
  ggplot(aes(N_Tests, N_Casos, color = factor(Semana))) +
  geom_point() +
  geom_smooth(method = "lm", fullrange = TRUE) +
  labs(
    subtitle = "Data for Metropolitan Region, Chile",
    x = "Number of Tests",
    y = "Number of Cases",
    color = "Week")

# Second plot
examenes_casos_rm %>% 
  filter(Semana %in% c(31, 35, 40, 49)) %>% 
  left_join(casos_pred_10000_tests,
            by = "Semana") %>% 
  ggplot(aes(N_Tests, N_Casos, color = factor(Semana))) +
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

casos_pred_10000_tests %>% 
  filter(Semana >= 31) %>% 
  ggplot(aes(Semana, pred_with_median_tests)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 33, color = color_hitos) +
  geom_vline(xintercept = 38, color = color_hitos) +
  geom_vline(xintercept = 47.5, color = color_hitos) +
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
           x = 47.5,
           y = 700,
           label = "Providencia y Est. Central\navanzó a fase 4", color = color_hitos) +
  annotate("label",
           x = 50.5,
           y = 700,
           label = "RM retrocedió\na fase 2", color = color_hitos) +
  ggrepel::geom_text_repel(aes(label = scales::comma(pred_with_median_tests, 1)),
                           size = 3) +
  labs(title = "Casos diarios en Región Metropolitana\najustando por cantidad de tests PCR",
       subtitle = "Asumiendo 10.000 tests diarios",
       y = "Casos diarios",
       x = "Semana",
       caption = "Datos: github.com/MinCiencia/Datos-COVID19
       Código: github.com/franciscoyira/covid_plots") +
  scale_x_continuous(breaks = seq(30, 50, 2),
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

# TODO: cambiar alineamiento de texto en histos (probar con justificado a
# izquierda o derecha)

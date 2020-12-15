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
  mutate(Semana = lubridate::week(Fecha))


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
casos_pred_median_testing <- 
  examenes_casos_rm %>% 
  nest(data = c(-Semana)) %>% 
  mutate(reg_casos = map(data, ~lm(N_Casos ~ N_Tests, data = .)),
         pred_with_median_tests = map_dbl(reg_casos, ~predict(., newdata = tibble(N_Tests = median_tests))))

# First plot
examenes_casos_rm %>% 
  filter(Semana %in% c(31, 35, 40, 49)) %>% 
  ggplot(aes(N_Tests, N_Casos, color = factor(Semana))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    subtitle = "Data for Metropolitan Region, Chile",
    x = "Number of Tests",
    y = "Number of Cases",
    color = "Week")

# Second plot
examenes_casos_rm %>% 
  filter(Semana %in% c(31, 35, 40, 49)) %>% 
  left_join(casos_pred_median_testing,
            by = "Semana") %>% 
  ggplot(aes(N_Tests, N_Casos, color = factor(Semana))) +
  geom_point() +
  geom_vline(xintercept = median_tests, color = "blue4") +
  geom_point(aes(x = median_tests, y = pred_with_median_tests), size = 5,
             show.legend = FALSE) +
  geom_smooth(method = "lm") +
  labs(
    subtitle = "Data for Metropolitan Region, Chile",
    x = "Number of Tests",
    y = "Number of Cases",
    color = "Week") +
  annotate("label",
           x = median_tests*0.91,
           y = 900,
           label = "Median of daily tests",
           color = "blue4")

# Function to create date from week number
week_to_date <- function(week_n) {
  as.Date(str_c("2020", week_n, "1"), "%Y%U%u") %>% 
    magrittr::subtract(1) %>% 
    format("%B %d",
           locale = locale(date_names = "en"))
  
}


# Third plot
color_hitos <- "firebrick3"

casos_pred_median_testing %>% 
  filter(Semana >= 30) %>% 
  ggplot(aes(Semana, pred_with_median_tests)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 33, color = color_hitos) +
  geom_vline(xintercept = 38, color = color_hitos) +
  annotate("label",
           x = 33,
           y = 800,
           label = "Stgo. Centro avanzó\na fase 2", color = color_hitos) +
  annotate("label",
           x = 38,
           y = 800,
           label = "Puente Alto avanzó\na fase 2", color = color_hitos) +
  ggrepel::geom_text_repel(aes(label = scales::comma(pred_with_median_tests, 1)),
                           size = 3) +
  labs(title = "Casos diarios en Región Metropolitana\najustando por cantidad de tests PCR",
       subtitle = "Asumiendo 10.000 tests diarios",
       y = "Casos diarios",
       x = "Semana",
       caption = "Datos: github.com/MinCiencia/Datos-COVID19.
       Código: github.com/franciscoyira/covid_plots") +
  scale_x_continuous(breaks = seq(30, 50, 2),
                     labels = week_to_date) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title.position = "plot",
        plot.margin = unit(c(0.7,0.1,0.7,0.1), "inches")) +
  ggsave(here("outputs", "casos_rm_stories.png"),
         dpi = 400, width = 9, height = 16,
         units = "cm", type = "cairo-png", scale = 1.4)

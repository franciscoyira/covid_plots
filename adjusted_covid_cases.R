library(tidyverse)
library(here)
library(modelr)
library(lubridate)

source(here("functions", "retrieve_tests_cases_region.R"))

examenes_casos_rm <- 
  retrieve_tests_cases_region() %>% 
  filter(Region == "Metropolitana") %>% 
  filter(Fecha >= ymd('2020-07-15'),
         Fecha <= ymd('2021-07-21'))

max_fecha <- ymd('2021-07-21')
  # examenes_casos_rm %>% 
  # pull(Fecha) %>% max()

## Fitting model
model2 <- 
  lm(N_Casos ~ factor(semana) + N_Tests, data = examenes_casos_rm)

df_adj_cases_model2 <- 
  examenes_casos_rm %>% 
  mutate(N_Tests = 10000) %>% 
  add_predictions(model2, var = "adjusted_cases") %>%
  group_by(semana) %>% 
  summarise(adjusted_cases = mean(adjusted_cases)) %>% 
  filter(semana >= ymd('2020-07-15'),
         semana <= ymd('2021-07-21'))

max_semana <- df_adj_cases_model2 %>% pull(semana) %>% max()

# Third plot
color_hitos <- "firebrick3"

plot_adjusted_cases <- 
  df_adj_cases_model2 %>% 
  ggplot(aes(semana, adjusted_cases)) + 
  geom_line() +
  geom_point() +
  geom_vline(xintercept = ymd('2020-08-10'), color = color_hitos) +
  geom_vline(xintercept = ymd('2020-09-14'), color = color_hitos) +
  geom_vline(xintercept = ymd('2020-12-07'), color = color_hitos) +
  geom_vline(xintercept = ymd('2021-03-15'), color = color_hitos) +
  annotate("label",
           x = ymd('2020-08-10'),
           y = 1800,
           label = "Stgo. Centro comes\nout of full lockdown", color = color_hitos) +
  annotate("label",
           x = ymd('2020-09-14'),
           y = 1100,
           label = "Puente Alto comes\nout of full lockdown", color = color_hitos) +
  annotate("label",
           x = ymd('2020-12-07'),
           y = 1100,
           label = "Metropolitan Region goes\ninto weekend lockdown", color = color_hitos) +
  annotate("label",
           x = ymd('2021-03-15'),
           y = 2500,
           label = "Stgo. Centro goes into full lockdown", color = color_hitos) +
  ggrepel::geom_text_repel(aes(label = scales::comma(adjusted_cases, 1)),
                           size = 3) +
  labs(title = "Daily COVID-19 cases in Chile's Metropolitan Region
adjusting by number of PCR tests",
       subtitle = str_c("Assuming 10,000 daily PCR tests. Last update: ", 
                        format(max_fecha, "%d %B %Y")),
       y = "Adjusted daily cases",
       x = "Week",
       caption = "Data: github.com/MinCiencia/Datos-COVID19
       Code: github.com/franciscoyira/covid_plots") +
  scale_x_date(breaks = seq(max_semana, ymd('2020-07-15'), -28),
                     date_labels = "%d %b %y") +
  expand_limits(y = 0) +
  ggthemes::theme_fivethirtyeight() +
  theme(plot.title.position = "plot",
        axis.title = element_text(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

ggsave(here("outputs", "adjusted_covid_cases.png"),
       plot = plot_adjusted_cases,
         dpi = 400,
         # IG stories aspect ratio
         width = 16, height = 9,
         units = "cm", type = "cairo-png", scale = 1.6)



  
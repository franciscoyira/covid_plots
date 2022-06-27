# Script goal: generate plots showing relationship between daily current cases
# and deaths + UCI ocupation 14 days later

# Load packages
library(tidyverse)
library(here)

# Import deaths and cases
covid_totales_nacionales <-
  read_csv(
    "https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto5/TotalesNacionales_T.csv",
    col_types = cols_only(
      Fecha = col_guess(),
      Fallecidos = col_guess(),
      `Casos nuevos totales` = col_guess()
    )
  ) %>% 
  janitor::clean_names()


# Offset variable for daily deaths (+14 days) and create rollmean variable
df_cases_deaths <- covid_totales_nacionales %>% 
  rename(
    date = fecha,
    deaths = fallecidos,
    cases = casos_nuevos_totales
  ) %>% 
  arrange(date) %>% 
  mutate(deaths = c(0, diff(deaths)),
         deaths_lead = lead(deaths, 14),
         deaths_lead_rollmean = 
           zoo::rollmean(deaths_lead,7,
                         na.pad = TRUE, align = "right"),
         cases_rollmean =
           zoo::rollmean(cases, 7,
                         na.pad = TRUE, align = "right")) %>% 
  filter(date >= '2020-08-01')

# Create plot
plot_covid_deaths <- 
  df_cases_deaths %>% 
  filter(date <= '2021-12-31') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cases, color = "cases",
                alpha = "daily value",
                size = "daily value")) +
  geom_line(aes(y = deaths_lead*34, color = "deaths (14 days later)",
                alpha = "daily value",
                size = "daily value")) +
  geom_line(aes(y = deaths_lead_rollmean*34, color = "deaths (14 days later)",
                alpha = "rollmean",
                size = "rollmean")) +
  geom_line(aes(y = cases_rollmean, color = "cases",
                alpha = "rollmean",
                size = "rollmean")) +
  geom_vline(xintercept = lubridate::ymd(20210731),
             color = "dodgerblue4") +
  annotate(geom = "label",
           label = "80% of the target population\nis fully vaccinated",
           x = lubridate::ymd(20210731),
           y = 10000,
           color = "dodgerblue4") +
  scale_y_continuous(name = "Confirmed cases",
                     sec.axis = sec_axis(~./34,
                                         name = "Daily deaths due to COVID (14 days later)")) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b %Y",
               name = "date") +
  scale_color_manual(values = c(
                       'cases' = 'red',
                       'deaths (14 days later)' = 'brown4')) +
  scale_alpha_manual(values = c(
    'daily value' = 0.5,
    'rollmean' = 1
  ), guide = "none") +
  scale_size_manual(values = c(
    'daily value' = 0.75,
    'rollmean' = 1.5
  )) +
  labs(title = "COVID-19 in Chile: daily cases and deaths 14 days later") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(here("outputs", "covid_deaths_offset.png"),
       bg = "white")

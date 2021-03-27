# Script goal: generate plots showing relationship between daily current cases
# and deaths + UCI ocupation 14 days later

# Load packages
library(tidyverse)
library(here)

# Import hospitalizations data
hospitalizados <-
  read_csv(
    "https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto9/HospitalizadosUCIEtario_T.csv",
    col_types = cols(
      `Grupo de edad` = col_date(format = ""),
      `<=39` = col_double(),
      `40-49` = col_double(),
      `50-59` = col_double(),
      `60-69` = col_double(),
      `>=70` = col_double()
    )
  ) %>% 
  rename(date = 1) %>% 
  pivot_longer(
    cols = 2:6,
    names_to = "age_range",
    values_to = "hospitalized"
  )

hospitalizados %>%
ggplot(aes(date, hospitalized, color = age_range)) +
geom_line()

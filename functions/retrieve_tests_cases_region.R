retrieve_tests_cases_region <- function() {
  examenes_raw <- 
    readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto7/PCR.csv")
  
  examenes_region_dia <- 
    examenes_raw %>% 
    tidyr::pivot_longer(
      cols = -c(Region, `Codigo region`, Poblacion),
      names_to = "Fecha",
      values_to = "N_Tests"
    ) %>% 
    dplyr::mutate(Fecha = lubridate::ymd(Fecha),
           N_Tests = replace_na(N_Tests, 0))
  
  casos_raw <- 
    readr::read_csv("https://github.com/MinCiencia/Datos-COVID19/raw/master/output/producto13/CasosNuevosCumulativo.csv")
  
  casos_region_dia <- 
    casos_raw %>% 
    tidyr::pivot_longer(
      cols = -Region,
      names_to = "Fecha",
      values_to = "N_Casos"
    ) %>% 
    dplyr::mutate(Fecha = lubridate::ymd(Fecha))
  
  examenes_casos <- 
    examenes_region_dia %>% 
    dplyr::left_join(casos_region_dia,
              by = c("Region", "Fecha")) %>% 
    dplyr::mutate(semana = lubridate::floor_date(Fecha, unit = "week",
                                                 week_start = 1))

  
  examenes_casos
}
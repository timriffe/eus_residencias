source("R/00_install_load.R")

im <- read_csv("Data/intermediate/imserso_harmonized.csv",
               show_col_types = FALSE) %>% 
  mutate(resident = ifelse(tipo %in% c("Atención Residencial","PE Vinculada al Servicio"), "resident","not resident")) %>% 
  group_by(geo, sexo, edad, year, month,resident) %>% 
  summarize(deaths = sum(deaths, na.rm = TRUE),
            pop = sum(pop),
            .groups = "drop") %>% 
  mutate(frac = case_when(
  month == "enero" ~ 1 / 24,
  month == "febrero" ~ 3 / 24,
  month == "marzo" ~ 5 / 24,
  month == "abril" ~ 7 / 24,
  month == "mayo" ~ 9 / 24,
  month == "junio" ~ 11 / 24,
  month == "julio" ~ 13 / 24,
  month == "agosto" ~ 15 / 24,
  month == "septiembre" ~ 17 / 24,
  month == "octubre" ~ 19 / 24,
  month == "noviembre" ~ 21 / 24,
  month == "diciembre" ~ 23 / 24,
  TRUE ~ NA_real_),
  tfrac = year + frac)

ages <- c(45,55,80,65)
years <- 2017:2022
months <- c("abril", "agosto", "diciembre", "enero", "febrero", "julio", 
            "junio", "marzo", "mayo", "noviembre", "octubre", "septiembre")
sexes <- c("Hombre","Mujer")
tipos <- c("Atención Residencial", "Ayuda a Domicilio", "PE Cuidados Familiares", 
           "Teleasistencia", "PE Vinculada al Servicio", "Centros Día/Noche", 
           "PE Asistencia Personal", "PAPD")
geo   <- c("Araba","Bizkaia","CAE","Gipuzkoa")
# im %>% 
#   dplyr::filter(edad >= 45) %>% 
#   arrange(geo, Sexo, year, month, tipo, edad) %>% 
#   tidyr::complete(edad = ages, year = years, month = months, Sexo = sexes, tipo = tipos, geo, fill)

# im %>% 
#   dplyr::filter(edad >= 45) %>% 
#   group_by(geo, year, month, tipo, Sexo) %>% 
#   summarize(n=n()) %>% 
#   pivot_wider(names_from = month, values_from = n) %>% 
#   View()
# im %>% 
#   filter(edad >= 45,
#          Sexo == "Hombre",
#          geo == "Bizkaia") %>% 
#   mutate(deaths = ifelse(is.na(deaths),0,deaths),
#          expos = pop / 12,
#          mx = deaths / expos) %>% 
# 
#   ggplot(aes(x = tfrac, y = mx)) +
#   geom_line() +
#   facet_grid(row=vars(edad),col=vars(resident), scales = "free_y")
im %>% 
  filter(edad >= 65) %>% 
  group_by(edad,tfrac,resident) %>% 
  summarize(deaths = sum(deaths, na.rm = TRUE),
            pop = sum(pop, na.rm=TRUE),.groups= "drop") %>% 
  mutate(deaths = ifelse(is.na(deaths),0,deaths),
         expos = pop / 12,
         mx = deaths / expos) %>% 
  # filter(edad == 65,
  #        resident == "resident") %>% pull(mx) %>% mean()
  select(-pop, -expos, -deaths) %>% 
  pivot_wider(names_from = resident, values_from = mx) %>% 
  
  mutate(ratio = resident / `not resident`) %>% 
  ggplot(aes(x = tfrac, y = ratio, color = edad, group = edad)) +
  geom_line() +
  geom_smooth()
  # facet_grid(row=vars(edad),scales = "free_y")
im %>% 
  dplyr::filter(edad >= 65) %>% 
  group_by(resident) %>% 
  summarize(pop = sum(pop))
im %>% 
  filter(is.na(pop))

im %>% 
  filter(geo == "Araba",
         year == 2020,
         month == "abril",
         edad == 45)

# Araba


library(HMDHFDplus)

mlt <- readHMDweb("ESP",
                  "fltper_1x1",
                  username = "tim.riffe@gmail.com",
                  password = "asus")

  mlt %>% 
    filter(Year >= 2000) %>% 
  group_by(Year) %>% 
  summarize(m65 = sum(dx[between(Age, 65, 79)])/sum(Lx[between(Age, 65, 79)]),
            m80 = sum(dx[Age >= 80]) / sum(Lx[Age >= 80])) %>% 
  ggplot(aes(x = Year, y = m65)) + 
  geom_line() 

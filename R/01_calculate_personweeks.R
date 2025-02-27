source("R/00_install_load.R")
# Pin <- read_delim(here::here("Data/inputs/Pobl_Eusk_EdadSexo_2002-2021.csv"),
#                   delim = ";", 
#                   locale = locale(encoding = "latin1"))
# Pin$Age %>% unique()
# Pin$Sex %>% unique()
# Pin$Period %>% unique()
# Pin %>% filter(Age == "TOT") %>% pull(Sex)
# Pin$Area %>% unique()
download.file("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/31304.csv?nocab=1", destfile = "Data/31304.csv")

P <- read_delim("Data/31304.csv", 
                delim = ";", 
                locale = locale(encoding = "UTF8")) 
Eout <-
  P %>% 
  mutate(Total = gsub(Total, pattern = "\\.", replacement = ""),
         pob = as.integer(Total)) %>% 
  dplyr::filter(Edad != "85 y más años" ,
                Provincias %in% c("01 Araba/Álava", "48 Bizkaia", "20 Gipuzkoa"),
                Sexo != "Ambos sexos") %>% 
  mutate(age = suppressWarnings(parse_number(Edad)),
         prov = case_when(Provincias == "01 Araba/Álava" ~ "Araba",
                          Provincias == "48 Bizkaia" ~ "Bizkaia",
                          Provincias == "20 Gipuzkoa" ~ "Gipuzkoa"),
         sex = case_when(Sexo == "Hombres" ~ "H",
                          Sexo == "Mujeres" ~ "M")) %>% 
  separate(Periodo, 
           into = c("day","junk1","month","junk2","year"), 
           sep = " ", 
           convert = TRUE) %>% 
  mutate(time = year + (month == "julio")/2,
         age_interval = 1) %>% 
  filter(month != "julio") %>% 
  select(prov, time, sex, age, pob) %>% 
  group_by(prov, time, sex) %>% 
  mutate(tot = pob[is.na(age)]) %>% 
  ungroup() %>% 
  filter(!is.na(age)) %>% 
  mutate(age = as.integer(age)) %>% 
  group_by(prov,time,sex) %>% 
  mutate(pob = pob / sum(pob) * tot) %>% 
  select(-tot) %>% 
  group_by(prov, sex, age) %>% 
  do(linear_interp(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(personweeks = pob * (7 / 365.25),
         age = age - age %% 5) %>% 
  group_by(prov, sex, age, time, isoyear, isoweek) %>% 
  summarize(personweeks = sum(personweeks),.groups = "drop")

# Sanity check of levels
# Eout %>% 
#   group_by(time) %>% 
#   summarize(personweeks = sum(personweeks) * 365.25/7) %>% 
#   ggplot(aes(x=time, y = personweeks)) +
#   geom_line()
# Eout %>% 
#   group_by(time, age) %>% 
#   summarize(pob = sum(personweeks)) %>% 
#   ggplot(aes(x=time, y = pob, color = age, group=age)) +
#   geom_line()

write_csv(Eout, file = "Data/intermediate/Expos_Eusk_analisis.csv")

# report UNK ages
unk_age <-
  P %>% 
  mutate(Total = gsub(Total, pattern = "\\.", replacement = ""),
         pob = as.integer(Total)) %>% 
  dplyr::filter(Edad != "85 y más años" ,
                Provincias %in% c("01 Araba/Álava", "48 Bizkaia", "20 Gipuzkoa"),
                Sexo != "Ambos sexos",
                !is.na(Total)) %>% 
  mutate(age = suppressWarnings(parse_number(Edad)),
         prov = case_when(Provincias == "01 Araba/Álava" ~ "Araba",
                          Provincias == "48 Bizkaia" ~ "Bizkaia",
                          Provincias == "20 Gipuzkoa" ~ "Gipuzkoa"),
         sex = case_when(Sexo == "Hombres" ~ "H",
                         Sexo == "Mujeres" ~ "M")) %>% 
  separate(Periodo, 
           into = c("day","junk1","month","junk2","year"), 
           sep = " ", 
           convert = TRUE) %>% 
  mutate(age_interval = 1) %>% 
  filter(month != "julio",
         year >= 2002) %>% 
  select(prov, year, sex, age, pob) %>% 
  group_by(prov, year, sex) %>% 
  summarize(tot = pob[is.na(age)],
            known = sum(pob[!is.na(age)]),
            .groups = "drop") %>% 
  mutate(unk = tot - known) %>% 
  select(-c(tot, known)) %>% 
  pivot_wider(names_from = c(prov,sex),values_from = unk)

unk_age %>% 
  write_csv("Data/other_outputs/poblacion_edad_unk.csv")


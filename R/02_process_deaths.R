download.file("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/36166.csv?nocab=1", destfile = "Data/36166.csv")

since2019_in <- read_delim("Data/36166.csv", delim = ";", show_col_types = FALSE,
                        trim_ws = TRUE)

# since2019_in %>% 
#   filter(Provincias == "01 Araba/Álava",
#          `Edad (grupos quinquenales)` %in% c("De 80 a 84 años","De 85 a 89 años","90 y más años"),
#          Periodo %in% c("2020SM01","2020SM02","2020SM03","2020SM04"),
#          `Tipo de dato` == "Dato base",
#          Sexo == "Total") %>% pull(Total) %>% sum()
since2019 <-  
  since2019_in %>% 
  dplyr::filter(Provincias %in% c("01 Araba/Álava", "20 Gipuzkoa","48 Bizkaia"),
               `Tipo de dato` == "Dato base") %>% 
  separate(Periodo, into = c("year","week"), sep = "SM", convert = TRUE) %>% 
  mutate(prov = case_when(Provincias == "01 Araba/Álava" ~ "Araba" ,
                          Provincias == "48 Bizkaia" ~ "Bizkaia",
                          Provincias == "20 Gipuzkoa" ~ "Gipuzkoa" ),
         sex = case_when(Sexo == "Hombres" ~ "Males",
                         Sexo == "Mujeres" ~ "Females",
                         TRUE ~ "Total")) %>% 
  select(prov, year, week, sex, age = `Edad (grupos quinquenales)`, deaths = Total) %>% 
  mutate(age = case_when(age == "Todas las edades" ~ "TOT",
                         age == "No consta" ~ "UNK",
                         age == "De 0 a 4 años" ~ "0",
                         age == "De 5 a 9 años" ~ "5",
                         age == "De 10 a 14 años" ~ "10",
                         age == "De 15 a 19 años" ~ "15",
                         age == "De 20 a 24 años" ~ "20",
                         age == "De 25 a 29 años" ~ "25",
                         age == "De 30 a 34 años" ~ "30",
                         age == "De 35 a 39 años" ~ "35",
                         age == "De 40 a 44 años" ~ "40",
                         age == "De 45 a 49 años" ~ "45",
                         age == "De 50 a 54 años" ~ "50",
                         age == "De 55 a 59 años" ~ "55",
                         age == "De 60 a 64 años" ~ "60",
                         age == "De 65 a 69 años" ~ "65",
                         age == "De 70 a 74 años" ~ "70",
                         age == "De 75 a 79 años" ~ "75",
                         age == "De 80 a 84 años" ~ "80",
                         age == "De 85 a 89 años" ~ "85",
                         age == "90 y más años" ~ "90",
                         TRUE ~ age)) %>% 
  dplyr::filter(year < 2022)
# since2019 %>% 
#   dplyr::filter(age == "TOT",
#                 sex == "Total") %>% 
#   mutate(time = year + week / 53) %>% 
#   ggplot(aes(x = time, y = deaths)) +
#   geom_line() +
#   facet_wrap(~prov, scales = "free_y")


before2019_in <- read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?file=data/demo_r_mweek3.tsv.gz",
                         delim = "\t",
                         na = ":",
                         trim_ws = TRUE)
before2019 <- 
  before2019_in %>% 
  separate(1, into = c("unit","sex","age","geo"), sep = ",") %>% 
  mutate(iso2 = substr(geo,1,2), .before = "geo") %>% 
  dplyr::filter(geo %in% c("ES211","ES212","ES213")) %>% 
  pivot_longer(6:ncol(.), names_to = "yrwk", values_to = "deaths") %>% 
  dplyr::filter(!is.na(deaths)) %>% 
  mutate(deaths = parse_number(deaths),
         prov = case_when(geo == "ES211" ~ "Araba",
                       geo == "ES212" ~ "Gipuzkoa",
                       geo == "ES213" ~ "Bizkaia"),
         sex = case_when(sex == "F" ~ "Females",
                         sex == "M" ~ "Males",
                         sex == "T" ~ "Total")) %>% 
  separate(yrwk, into = c("year","week"), sep = "W", convert = TRUE) %>% 
  select(prov, year, week, sex, age, deaths) %>% 
  mutate(age = case_when(age == "TOTAL" ~ "TOT",
                         age == "UNK" ~ "UNK",
                         age == "Y_GE90" ~ "90",
                         age == "Y_LT5" ~ "0",
                         age == "Y5-9" ~ "5",
                         TRUE ~ substr(age,2,3))) %>% 
  dplyr::filter(year < 2019)

Dout <- 
  bind_rows(before2019,
            since2019) %>% 
  pivot_wider(names_from = sex, values_from = deaths) %>% 
  mutate(Males = Total * ifelse(Total == 0, 0, Males / (Males + Females)),
         Females = Total * ifelse(Total == 0, 0, Females / (Males + Females))) %>% 
  select(-Total) %>% 
  pivot_longer(cols = c(Males, Females), names_to = "sex", values_to = "deaths") %>% 
  group_by(prov, year, week, sex) %>%
  mutate(TOT = deaths[age == "TOT"]) %>% 
  dplyr::filter(age != "TOT", age != "UNK") %>% 
  mutate(deaths = if_else(TOT == 0, 0, TOT * (deaths / sum(deaths))),
         age = as.integer(age)) %>% 
  ungroup() %>% 
  select(-TOT) %>% 
  mutate(sex = if_else(sex == "Males", "H","M")) %>% 
  rename(isoyear = year, isoweek = week) %>% 
  arrange(prov,isoyear,isoweek,sex,age)


write_csv(Dout, "Data/intermediate/Def_Eusk_analisis.csv")



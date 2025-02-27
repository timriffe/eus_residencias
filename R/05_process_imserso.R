source("R/00_install_load.R")

years <- 2017:2022
geo   <- c("Araba","Bizkaia","CAE","Gipuzkoa")
months <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")

compose_path <- function(y,g,m){
  here::here(
    "Data",
    "inputs",
    "Tablas_beneficiarios",
    y,
    paste(g, y, sep = "_"),
    paste0(m,y,"_",tolower(g),".xlsx")
  )
}

# for (y in years){
#   for (g in geo){
#     this_folder <- here::here("Data",
#                               "Tablas_beneficiarios",
#                               y,paste0(g,"_",y))
#     names_in <- dir(this_folder)
#     for (i in names_in){
#       
#       name_out <- ifelse(substr(i, nchar(i), nchar(i)) == "x",i,paste0(i,"x"))
#       file.rename(from = file.path(this_folder, i),
#              to = file.path(this_folder, tolower(name_out)))
#     }
#   }
# }

read_imserso_sheet <- function(y,g,m){
  pathi <- compose_path(y, g, m)
  X <- read_excel(pathi, skip = 2) 
  X <- X[1:(which(X$`Rango Edad` == "TOTAL")-1),]
  X %>% 
    tidyr::fill(`Rango Edad`,Sexo, `Tipo Prestación`,.direction = "down")
}

beneficiarios <- list()
i<-0
for (y in years){
  for (g in geo){
    for (m in months){
      i <- i + 1

      if ((y == 2022 & m != "enero")){
        X <- tibble()
      } else{
        X <- read_imserso_sheet(y, g, m) %>% 
          mutate(geo = g,
                 year = y,
                 month = m)
      }
      beneficiarios[[i]] <- X
      
    }
  }
}

ben <- bind_rows(beneficiarios) %>% 
  mutate(edad = case_when(`Rango Edad` == "Menores de 3"~"0 a 2", 
                          `Rango Edad` == "80 y +"~"80 a 99",
                          TRUE ~`Rango Edad`),
         geo = case_when(geo == "Bizaia" ~ "Bizkaia",
                         geo == "Gipukzoa" ~ "Gipuzkoa",
                         geo == "Cae" ~ "CAE",
                         TRUE ~ geo)) %>% 
  select(-`Rango Edad`) %>% 
  separate(edad, into = c("edad",NA), sep = " a ", convert = TRUE) %>% 
  group_by(geo, year, month, edad, Sexo, `Tipo Prestación`) %>% 
  summarize(pop = sum(`Beneficiarios Vigentes`), .groups = "drop")%>% 
  mutate(edad = case_when(edad == 46 ~ 45L,
                          edad == 31 ~ 30L,
                          TRUE ~ edad)) %>% 
  pivot_wider(names_from = Sexo, values_from = pop,values_fill=0) %>% 
  mutate(Hombre = Hombre + Hombre / (Hombre + Mujer) * Nulo,
         Mujer = Mujer + Mujer / (Hombre + Mujer) * Nulo) %>% 
  select(-Nulo) %>% 
  pivot_longer(Hombre:Mujer, names_to = "Sexo", values_to = "pop") %>% 
  rename(sexo = Sexo, tipo = `Tipo Prestación`)

# --------------------------------------------------
# now for deaths

death_files <- dir("Data/inputs/Tabla fallecidos IMSERSO/Tabla fallecidos IMSERSO")
deaths_list <- list()
for (i in 1:length(death_files)){
  y <- parse_number(death_files[i])
  g <- str_split(death_files[i],pattern="_")[[1]][[2]] %>% gsub(pattern=".xlsx",replacement="") %>% tools::toTitleCase()
  m <- str_split(death_files[i],pattern="_")[[1]][[1]] %>% str_extract( "[a-z]+")
  pathi <- here::here("Data",
                      "inputs",
                      "Tabla fallecidos IMSERSO",
                      "Tabla fallecidos IMSERSO",
                      death_files[i])
  X <- read_excel(pathi, skip = 2) 
  X <- X[1:(which(X$`Rango Edad` == "TOTAL")-1),] %>% 
    tidyr::fill(`Rango Edad`,Sexo, .direction = "down") %>% 
    mutate(geo = g,
           year = y,
           month = m)
    deaths_list[[i]] <- X
}
deaths <-
  bind_rows(deaths_list) %>% 
  mutate(edad = case_when(`Rango Edad` == "Menores de 3"~"0 a 2", 
                          `Rango Edad` == "80 y +"~"80 a 99",
                          TRUE ~`Rango Edad`),
         geo = case_when(geo == "Bizaia" ~ "Bizkaia",
                         geo == "Gipukzoa" ~ "Gipuzkoa",
                         geo == "Cae" ~ "CAE",
                         TRUE ~ geo)) %>% 
  select(-`Rango Edad`) %>% 
  separate(edad, into = c("edad",NA), sep = " a ", convert = TRUE) %>% 
  group_by(geo, year, month, edad, Sexo, `Tipo Prestación`) %>% 
  summarize(deaths = sum(`Beneficiarios Fallecidos`), .groups = "drop") %>% 
  mutate(edad = case_when(edad == 46 ~ 45L,
                          edad == 31 ~ 30L,
                          TRUE ~ edad)) %>% 
  pivot_wider(names_from = Sexo, values_from = deaths, values_fill=0) %>% 
  mutate(Hombre = Hombre + Hombre / (Hombre + Mujer) * Nulo,
         Mujer = Mujer + Mujer / (Hombre + Mujer) * Nulo) %>% 
  select(-Nulo) %>% 
  pivot_longer(Hombre:Mujer, names_to = "Sexo", values_to = "deaths") %>% 
  rename(sexo = Sexo, tipo = `Tipo Prestación`) 

imserso <-
  full_join(deaths, ben,
            by = c("sexo", "tipo",
                   "geo", "year", "month","edad")) %>% 
  mutate(pop = if_else(is.na(pop), deaths, pop),
         deaths = if_else(is.na(deaths), 0, deaths))

write_csv(imserso, "Data/intermediate/imserso_harmonized.csv")


# imserso <- read_csv("Data/intermediate/imserso_harmonized.csv")
# imserso$tipo %>% unique()

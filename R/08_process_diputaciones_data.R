
source("R/00_install_load.R")
# version 2 of the input file has a new definition category added
Dip_In <- read_delim("Data/inputs/BaseDefuncionesDDFF_variablesEustat2.csv", delim =";",
                locale = locale(encoding = "latin1"))


Dip_Out <-
  Dip_In %>% 
  dplyr::filter(!Codigo_Essec%in%c(10020010,10090008,10170006,10310011,10550003,10590794,10590916)) %>% 
  mutate(USU_FECHA = as.integer(USU_FECHA),
         edad = case_when(Edad_grup == "< 65 años" ~ 60L,
                          Edad_grup == "65 - 74 años" ~ 65L,
                          Edad_grup == "75 - 79 años" ~ 75L,
                          Edad_grup == "80 - 84 años" ~ 80L,
                          Edad_grup == "85 89 años" ~ 85L,
                          Edad_grup == ">90 años" ~ 90L),
         sexo = case_when(Sexo_cod == "Hombre" ~ "Hombres",
                          Sexo_cod == "Mujer" ~ "Mujeres")) %>% 
  pivot_longer(c(COSTE_PLAZA_GRUP ,PRESTA_MEDICO ,ZONA_OZONO,
                 FACTOR_RURAL5b_GRUP ,TITULAR_EM ,FACTOR_TIT ,
                 TAMAÑO_50 ,USUG2Y3_TERCILES ,RATIO_TOTAL_GRUP ,
                 TASA_POSIT_GRUP, ALTO_IMPACTO), 
               names_to = "definition", values_to = "subgroup") %>% 
  select(-c(Edad_grup,Codigo_Essec,Sexo_cod,TH_cod,TH)) %>% 
  rename(def = DEFUN_AÑO, pop_end = USU_FECHA, year = Año) %>% 
  mutate(exposure = pop_end + def / 2,
         # 2021 data is cut at Sept 30, ergo 3/4 of a year.
         exposure = if_else(year == 2021, exposure * (3 / 4), exposure)) %>% 
  select(-pop_end) %>% 
  mutate(subgroup = case_when(subgroup %in% c("Zona litoral de Gipuzkoa", "Zona litoral de Bizkaia") ~ "Litoral",
                              subgroup %in% c("Valles cantábricos costeros de Bizkaia y Araba", "Valles cantábricos costeros de Gipuzkoa") ~ "Valles",
                              subgroup %in% c("Cuencas interiores de Álava", "Resto de Álava y Bizkaia") ~ "Interior",
                              TRUE ~ subgroup)) %>% 
  group_by(year, edad, sexo, definition, subgroup) %>% 
  summarize(def = sum(def),
            exposure = sum(exposure), .groups = "drop")

write_csv(Dip_Out, "Data/intermediate/diputaciones_input.csv")

# A <- read_excel("Data/inputs/BASE DFA 2015_21_TercilesPonder.xlsx", 
#                 na = c("Desconocido","")) %>% 
#   mutate(edad = case_when(Edad_grup == "< 65 años" ~ 60L,
#                           Edad_grup == "65 - 74 años" ~ 65L,
#                           Edad_grup == "75 - 79 años" ~ 75L,
#                           Edad_grup == "80 - 84 años" ~ 80L,
#                           Edad_grup == "85 89 años" ~ 85L,
#                           Edad_grup == ">90 años" ~ 90L)) %>% 
#   select(-Edad_grup) %>% 
#   mutate(sexo = case_when(Sexo_cod == "Hombre" ~ "Hombres",
#                           Sexo_cod == "Mujer" ~ "Mujeres")) %>% 
#   select(-Sexo_cod, -Codigo_Essec, -Centro, 
#          -INICIO_ACTIVIDAD, -ComarcaSS, -AreaSS) %>% 
#   rename(usuarios = Usuarios.Año.Fecha, def = Defun.Año, año = Año,
#          TipoECDC = `Tipo de centro (definición ECDC)`,
#          PerfilUsuario = `Perfil (ratio usuarios grados 2 y 3)`) %>% 
#   mutate(geo = "Araba") %>% 
#   filter(!is.na(año)) %>% # throw out 9 deaths of unknown year
#   mutate(TipoECDC = case_when(
#     TipoECDC == "Personal médico o de enfermería propio y mas del 50% de usuarios con alta depen." ~ "Nursing home",
#     TipoECDC == "Sin personal médico o de enfermería propio o menos del 50% de usuarios con alta depen." ~ "Residential home",
#     TRUE ~ NA_character_)) 
# 
# 
# G <- 
#   read_excel("Data/inputs/BASE_DFG_CENTROS_2015_21TercilesPonder.xlsx", 
#                   na = c("Desconocido","")) %>% 
#   filter(!is.na(Edad)) %>% 
#   mutate(edad = case_when(Edad == "<65" ~ 60L,
#                           Edad == "65-74" ~ 65L,
#                           Edad == "75-79" ~ 75L,
#                           Edad == "80-84" ~ 80L,
#                           Edad == "84-89" ~ 85L,
#                           Edad == ">=90" ~ 90L,
#                           TRUE ~ NA_integer_)) %>% 
#   select(-Edad_grup, -Edad) %>% 
#   mutate(sexo = case_when(Sexo == "H" ~ "Hombres",
#                           Sexo == "M" ~ "Mujeres",
#                           TRUE ~ NA_character_)) %>% 
#   select(-Sexo_cod, -Sexo, -Codigo_Essec, -Centro, -INICIO_ACTIVIDAD, -ComarcaSS,
#          -USU_SERV_AÑO, -AreaSS) %>% 
#   rename(usuarios = USU_SERV_FECHA, def = DEFUN_SERV, año = Año,
#          TipoECDC = `Tipo de centro (definición ECDC)`,
#          PerfilUsuario = `Perfil (ratio usuarios grados 2 y 3)`) %>% 
#   mutate(geo = "Gipuzkoa")
# # A %>% group_by(sexo) %>% dplyr::summarize(def = sum(def))
# # A %>% colnames()
# # G %>% colnames()
# 
# Dip <-
#   bind_rows(A, G) %>% 
#   pivot_longer(c(def,usuarios), names_to = "variable", values_to = "value") %>%
#   mutate(value = ifelse(is.na(value),0,value)) %>% 
#   pivot_longer(Titularidad:PerfilUsuario, 
#                names_to = "definicion", 
#                values_to = "subgrupo",
#                values_transform = list(subgrupo=as.character)) %>% 
#   # collapse redundancies, 0s, etc
#   group_by(edad, geo, definicion, año, subgrupo, sexo, variable) %>%
#   dplyr::summarize(value = sum(value, na.rm = TRUE),
#             .groups = "drop") %>% 
#     # group_by(geo,sexo,variable) %>% dplyr::summarize(value=sum(value))
#   # distribute UNK sexo: no longer needed???
#   # pivot_wider(names_from = sexo, 
#   #             values_from = value, 
#   #             values_fill = list(value = 0)) %>% 
#   # head()
#   # mutate(TOTHM = Hombres + Mujeres,
#   #        Hombres = Hombres + (Hombres / TOTHM) * `NA`,
#   #        Mujeres = Mujeres + (Mujeres / TOTHM) * `NA`,
#   #        Hombres = ifelse(TOTHM == 0, `NA`/2, Hombres),
#   #        Mujeres = ifelse(TOTHM == 0, `NA`/2, Mujeres)) %>% 
#   # select(-`NA`,-TOTHM) %>% 
#   # pivot_longer(Hombres:Mujeres, 
#   #              names_to = "sexo", 
#   #              values_to = "value") %>% 
#   # filter(variable == "def") %>% group_by(definicion) %>% dplyr::summarize(value = sum(value))
#   # distribute UNK subgrupo within sexo,geo,año,definicion,edad
#   group_by(sexo,geo,año,definicion,variable,edad) %>% 
#   mutate(na = ifelse(any(is.na(subgrupo)), value[is.na(subgrupo)],0)) %>% 
#   # filter(edad == 60, geo == "Araba", definicion == "Coste/plaza_terciles",
#          # año == 2015,sexo == "Mujeres",variable == "def") %>% 
#   filter(!is.na(subgrupo)) %>% 
#     group_by(sexo,geo,año,definicion,variable,edad) %>% 
#   mutate(
#     keep0s = all(value == 0),
#     value = value + value / sum(value) * na,
#     value = ifelse(keep0s,na/n(),value)) %>% 
#   ungroup() %>% 
#   select(-na,-keep0s) %>% 
#   # remove UNK year...
#   # send to fitting format:
#   pivot_wider(names_from = variable, values_from = value) #%>% 
  #group_by(definicion) %>% dplyr::summarize(def = sum(def))

# sums check: 12258
# Dip %>% 
#   group_by(definicion) %>% 
#   dplyr::summarize(def = sum(def,na.rm=TRUE))
# bind_rows(A,G) %>% pull(def) %>% sum()

# write_csv(Dip, "Data/intermediate/diputaciones_input.csv")


# do_this <- FALSE
# if (do_this){
# 
# all_sheets_G <- excel_sheets("Data/inputs/DatosMortalidadDFG_tipologías.xls")
# G <- list()
# for (i in all_sheets_G){
# IN <- read_excel("Data/inputs/DatosMortalidadDFG_tipologías.xls", 
#            sheet = i, 
#            skip = 4, 
#            col_names = c("junk1","año","junk2","sexo","junk3",
#                          "edad","junk4","subgrupo","pob","def"),
#            col_types = "text") %>% 
#   select(-starts_with("junk")) %>% 
#   fill(año,sexo, edad, subgrupo,.direction = "down") %>% 
#   mutate(edad = gsub(edad, pattern = "<", replacement = "60"),
#          edad = gsub(edad, pattern = ">", replacement = ""),
#          edad = gsub(edad, pattern = " ", replacement = ""),
#          edad = substr(edad, 1,2) %>% as.integer(),
#          pob  = suppressWarnings(as.integer(pob)),
#          def = suppressWarnings(as.integer(def)),
#          tipo_cat = i,
#          geo = "Gipuzkoa") %>% 
#   dplyr::filter(!is.na(pob))
# G[[i]] <- IN
# }
# 
# 
# all_sheets_A <- excel_sheets("Data/inputs/DatosMortalidadDFA_tipologías.xls")
# A <- list()
# for (i in all_sheets_A){
#   IN <- read_excel("Data/inputs/DatosMortalidadDFA_tipologías.xls", 
#                    sheet = i, 
#                    skip = 4, 
#                    col_names = c("junk1","año","junk2","sexo","junk3",
#                                  "edad","junk4","subgrupo","pob","def"),
#                    col_types = "text") %>% 
#     select(-starts_with("junk")) %>% 
#     fill(año,sexo, edad, subgrupo,.direction = "down") %>% 
#     mutate(edad = gsub(edad, pattern = "<", replacement = "60"),
#            edad = gsub(edad, pattern = ">", replacement = ""),
#            edad = gsub(edad, pattern = " ", replacement = ""),
#            edad = substr(edad, 1,2) %>% as.integer(),
#            pob  = suppressWarnings(as.integer(pob)),
#            def = suppressWarnings(as.integer(def)),
#            tipo_cat = i,
#            geo = "Araba") %>% 
#     dplyr::filter(!is.na(pob))
#   A[[i]] <- IN
# }
# A <- bind_rows(A)
# G <- bind_rows(G)
# 
# 
# 
# Dip <- bind_rows(G,A)
# 
# Dip <-
#   Dip %>% 
#   pivot_longer(pob:def, names_to = "variable",values_to = "value") %>% 
#   group_by(geo, año, sexo, edad, tipo_cat, variable) %>% 
#   mutate(
#     unk = ifelse(sum(subgrupo == "Desconocido") == 1, 
#                  value[subgrupo == "Desconocido"],
#                  0)) %>% 
#   filter(subgrupo != "Desconocido") %>% 
#   group_by(geo, año, sexo, edad, tipo_cat, variable) %>% 
#   mutate(value = value + (value / sum(value)) * unk) %>% 
#   ungroup() %>% 
#   select(-unk) %>% 
#   distinct() %>% 
#   pivot_wider(names_from = variable,
#               values_from = value) %>% 
#   mutate(sexo = case_when(sexo == "Mujer"~ "Mujeres", 
#                           sexo == "Hombre" ~ "Hombres",
#                           TRUE ~ sexo),
#          año = as.integer(año))
# 
# write_csv(Dip, "Data/intermediate/diputaciones_input_fixed.csv")
# }
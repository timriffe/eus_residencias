source("R/00_install_load.R")



im <- 
  read_csv("Data/intermediate/imserso_harmonized.csv",
               show_col_types = FALSE) %>% 
  dplyr::filter(edad >= 65) %>% 
  mutate(resident = if_else(tipo %in% c("Atención Residencial","PE Vinculada al Servicio"), 
                           "resident",
                           "not resident")) %>% 
  group_by(geo, sexo, edad, year, month, resident) %>% 
  dplyr::summarize(deaths = sum(deaths, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
            .groups = "drop") %>% 
  
  # This needs to be double checked: same stagger for both pops and deaths?
  mutate(month_int = case_when( # stagger months back 1
    month == "enero" ~ 12 ,
    month == "febrero" ~ 1,
    month == "marzo" ~ 2,
    month == "abril" ~ 3,
    month == "mayo" ~ 4,
    month == "junio" ~ 5,
    month == "julio" ~ 6,
    month == "agosto" ~ 7,
    month == "septiembre" ~ 8,
    month == "octubre" ~ 9,
    month == "noviembre" ~ 10,
    month == "diciembre" ~ 11,
    TRUE ~ NA_real_),
    year = if_else(month_int == 12, year - 1, year),
    tfrac = calc_month_midpoint_tfrac(month = sprintf("%02d",month_int), year),
    frac = tfrac - year,
    ws = case_when(month %in% c("enero","febrero","agosto") ~ 0,
                   # year == 2017 & month %in% c("agosto") ~0,
                   # year == 2018 & month %in% c("marzo") ~ 0,
                   # year == 2019 & month %in% c("mayo") ~ 0,
                  tfrac > 2020.1 ~ 0, # excludes march+
                  #year == 2021 #& month %in% c("junio","julio","agosto") ~ 0,
                  TRUE ~ 1),
    # TR This needs to be double checked: if pop is at end of interval then we need to 
    # add 1/2 deaths back to it. If it's at stat of interval, then we subtract
    exposure = adjust_month_exposure(pop - deaths / 2, month_int, year) %>% unname %>% c) 


# check exclusions:
 # im_check <-
 # im %>% 
 #   group_by(geo, tfrac, resident) %>% 
 #   dplyr::summarize(def = sum(deaths),
 #             expos = sum(exposure),
 #             ws = ws[1],
 #             .groups= "drop") %>% 
 #   ggplot(aes(x = tfrac, y = def/expos, color = ws)) +
 #   geom_line() +
 #   geom_point() +
 #   facet_grid(vars(geo), vars(resident), scales = "free")
 # im_check

im_baseline <- 
  im %>% 
  arrange(geo, edad, sexo, resident,tfrac) %>% 
  group_by(geo, edad, sexo, resident) %>% 
  do(fit_baseline_imserso(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(excess = deaths - baseline) %>% 
  select(geo, sexo, residente = resident, edad, 
         año = year, mes = month, tfrac, ws, edad, pob = pop,
         expos = exposure, def = deaths, baseline, 
         exceso = excess, se.fit) %>% 
  arrange(geo, tfrac, sexo, residente, edad) %>% 
  mutate(baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) %>% 
  # these are just labels: above calcs take 
  # into account the proper time shift
  mutate(mes2 = case_when(mes == "enero" ~ "diciembre",
                          mes == "febrero" ~ "enero",
                          mes == "marzo" ~ "febrero",
                          mes == "abril" ~ "marzo",
                          mes == "mayo" ~ "abril",
                          mes == "junio" ~ "mayo",
                          mes == "julio" ~ "junio",
                          mes == "agosto" ~ "julio",
                          mes == "septiembre" ~ "agosto",
                          mes == "octubre" ~ "septiembre",
                          mes == "noviembre" ~ "octubre",
                          mes == "diciembre" ~ "noviembre")) %>% 
  select(-mes) %>% 
  rename(mes = mes2) 

im_baseline %>% 
  write_csv("Data/outputs/imserso_only_exceso.csv")
# read in general pop data, aggregate CAE,
# aggregate to months, then,
# subtract the IMSERSO from it and see what comes out.
eus <- read_csv("Data/outputs/eus_baseline_2015-2019.csv",
         show_col_types = FALSE) %>%
  rename(geo = prov) %>% 
  dplyr::filter(edad >= 65,
         between(fecha, dmy("01.12.2016"), dmy("31.12.2021")))

# CAE results are the aggregate of the province results,
# they are not estimated separately, although this would be
# possible. This ensures a constrained result. IMSERSO, on the other
# hand gets CAE and the provinces separately estimated, and these
# are unconstrained, mostly due to small numbers. These comparisons
# are approximate, also for many other reasons.

eus_CAE <- 
  eus %>% 
  group_by(edad, fecha, sexo) %>% 
  summarize(def = sum(def), expos = sum(expos), 
            .groups = "drop") %>% 
  mutate(geo = "CAE") %>% 
  bind_rows(eus) %>% 
  select(edad, fecha, sexo, def, expos, geo) %>% 
  pivot_longer(def:expos, names_to = "measure", values_to = "value")



date_range <-
  eus_CAE %>% 
  select(fecha) %>% 
  distinct() %>% 
  # ref date given is Thursday (midweek)
  mutate(thu = fecha,
         fri = thu + 1,
         sat = thu + 2,
         sun = thu + 3,
         mon = thu - 3,
         tue = thu - 2,
         wed = thu - 1) %>% 
  pivot_longer(c(thu,fri,sat,sun,mon,tue,wed), names_to = "weekday", values_to = "date") %>% 
  select(-weekday) %>% 
  arrange(date)

eus_monthly <-
  date_range %>% 
  full_join(eus_CAE,by="fecha") %>% 
  dplyr::filter(edad >= 65) %>% # redundant
  mutate(edad = ifelse(edad >= 80, 80, 65)) %>% 
  group_by(geo, sexo, measure, edad, date) %>% 
  dplyr::summarize(value = sum(value, na.rm=TRUE),.groups = "drop") %>% 
  mutate(daily = value / 7,
         month_int = month(date) %>% as.integer(),
         año = year(date) %>% as.integer()) %>% 
  # dplyr::filter(año >= 2017) %>% 
  group_by(geo, sexo, measure, edad, month_int, año) %>% 
  dplyr::summarize(
            mean_daily = mean(daily, na.rm = TRUE), 
            value = mean_daily * lubridate::days_in_month(date[1]),
            .groups = "drop") %>% 
    select(-mean_daily) %>% 
   mutate(value = unname(value)) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(residente = "general",
         mes = case_when(month_int == 1 ~ "enero",
                         month_int == 2 ~ "febrero",
                         month_int == 3 ~ "marzo",
                         month_int == 4 ~ "abril",
                         month_int == 5 ~ "mayo",
                         month_int == 6 ~ "junio",
                         month_int == 7 ~ "julio",
                         month_int == 8 ~ "agosto",
                         month_int == 9 ~ "septiembre",
                         month_int == 10 ~ "octubre",
                         month_int == 11 ~ "noviembre",
                         month_int == 12 ~ "diciembre"),
         sexo = ifelse(sexo == "H","Hombre","Mujer"),
         ws = case_when(mes %in% c("enero","febrero") ~ 0,
                        año == 2020 & mes %in% c("marzo","abril","mayo") ~ 0,
                        año == 2021 ~ 0,
                        TRUE ~ 1)) %>% 
  select(geo, sexo, residente, edad, año, mes, expos, def) 

# eus_monthly %>% pull(geo) %>% unique()
# eus_monthly %>% filter(geo == "CAE") %>% 
#   group_by(año) %>% 
#   summarize(def = sum(def)) 

# im_baseline %>% 
#   select(geo, sexo, residente, edad,año, mes, expos, def) %>% 
#   bind_rows(eus_monthly) %>% 
#   pivot_longer(expos:def, names_to = "measure", values_to = "value") %>% 
#   group_by(geo,sexo,residente,edad,año, mes,measure) %>% 
#   summarize(n = n()) %>% 
#   dplyr::filter(n>1)

# This uses the SAME fitting window to each of the four groups,
# Which will give a more stagnant general pop (and by extension,
# non-dependent pop) trend. Not strictly comparable with our best 
# estimates. We should prefer to do separate calcs of IMSERSO
# excess and general pop excess, and then take differences post hoc
# Then excess approaches not strictly same, but at least estimates 
# will be the best we have.
all_results <-
  im_baseline %>% 
  select(geo, sexo, residente, edad,año, mes, expos, def) %>% 
  bind_rows(eus_monthly) %>% 
  pivot_longer(expos:def, names_to = "measure", values_to = "value") %>% 
  pivot_wider(names_from = residente, values_from = value) %>% 
  mutate(
    `no dependiente` = general - `not resident` - resident) %>% 
  pivot_longer(c(general, `no dependiente`,`not resident`,resident), names_to = "residente", values_to = "value") %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(
    month_int = case_when(mes == "enero" ~ 1,
                          mes == "febrero" ~ 2,
                          mes == "marzo" ~ 3,
                          mes == "abril" ~ 4,
                          mes == "mayo" ~ 5,
                          mes == "junio" ~ 6,
                          mes == "julio" ~ 7,
                          mes == "agosto" ~ 8,
                          mes == "septiembre" ~ 9,
                          mes == "octubre" ~ 10,
                          mes == "noviembre" ~ 11,
                          mes == "diciembre" ~ 12),
    tfrac = calc_month_midpoint_tfrac(month = sprintf("%02d",month_int), año),
    frac = tfrac - año,
    ws = case_when(mes %in% c("enero","febrero","agosto") ~ 0,
               # year == 2017 & month %in% c("agosto") ~0,
               # year == 2018 & month %in% c("marzo") ~ 0,
               # year == 2019 & month %in% c("mayo") ~ 0,
               tfrac > 2020.2 ~ 0, # excludes march+
               #year == 2021 #& month %in% c("junio","julio","agosto") ~ 0,
               TRUE ~ 1)) %>% 
  mutate(def = ifelse(def < 0, 0, def)) %>% 
  rename(deaths = def, exposure = expos) %>% 
  group_by(geo, edad, sexo, residente) %>% 
  do(fit_baseline_imserso(chunk = .data)) %>% 
  ungroup() %>% 
  select(geo, sexo, residente, edad, 
         año, mes, tfrac, ws, edad,
         expos = exposure, def = deaths, baseline, se.fit) %>% 
  arrange(geo, tfrac, sexo, residente, edad) %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) %>% 
  dplyr::filter(tfrac > 2016.9)

all_results %>% 
  write_csv("Data/outputs/imserso_exceso_comparar.csv")


# Redux to max estimate quality for each subgroup:

im_results <-
  all_results %>% 
  dplyr::filter(residente %in% c("resident","not resident")) %>% 
  select(geo,sexo,residente,año,mes,edad,expos,def,baseline,baseline_lower, baseline_upper) %>% 
  pivot_longer(c(expos,def,baseline,baseline_lower,baseline_upper), names_to = "measure", values_to = "value")

general_results_weekly <- read_csv("Data/outputs/eus_baseline_2015-2019.csv",
                                   show_col_types = FALSE)

  
# group ages;
general_monthly <-
general_results_weekly %>% 
  dplyr::filter(edad >= 65,
                t_frac >= 2016.8) %>% 
  select(prov,sexo,edad,fecha,def,expos,baseline,exceso,baseline_lower, baseline_upper) %>% 
  pivot_longer(def:baseline_upper, names_to = "measure", values_to = "value") %>% 
  mutate(edad = if_else(edad >= 80, 80, 65)) %>% 
  group_by(prov,sexo,edad,fecha,measure) %>% 
  summarize(value = sum(value), .groups = "drop") %>% 
  mutate(sexo = if_else(sexo == "H", "Hombre","Mujer")) %>% 
  rename(geo = prov) %>% 
  inner_join(date_range, by = "fecha") %>% 
  mutate(daily = value / 7,
         month_int = lubridate::month(date) %>% as.integer(),
         año = year(date) %>% as.integer()) %>% 
  # filter(month_int == 1, geo == "Araba", año == 2020, measure == "def", edad == 80) %>% pull(daily) %>% sum()
  # dplyr::filter(año >= 2017) %>% 
  group_by(geo, sexo, measure, edad, month_int, año) %>% 
  # filter(edad == 80, measure == "def", año == 2020, month_int == 1) %>% View()
  dplyr::summarize(
    mean_daily = mean(daily, na.rm = TRUE), 
    month_days = lubridate::days_in_month(date[1]),
    # value = sum(daily, na.rm = TRUE),
    value = mean_daily * month_days,
    .groups = "drop") %>% 
  select(-mean_daily, -month_days) %>% 
  mutate(value = unname(value)) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(residente = "general",
         mes = case_when(month_int == 1 ~ "enero",
                         month_int == 2 ~ "febrero",
                         month_int == 3 ~ "marzo",
                         month_int == 4 ~ "abril",
                         month_int == 5 ~ "mayo",
                         month_int == 6 ~ "junio",
                         month_int == 7 ~ "julio",
                         month_int == 8 ~ "agosto",
                         month_int == 9 ~ "septiembre",
                         month_int == 10 ~ "octubre",
                         month_int == 11 ~ "noviembre",
                         month_int == 12 ~ "diciembre")) %>% 
  select(geo,sexo,residente,año,mes,edad,expos,def,baseline,baseline_lower, baseline_upper) %>% 
  pivot_longer(expos:baseline_upper, names_to = "measure", values_to = "value") %>% 
  pivot_wider(names_from = geo, values_from = value) %>% 
  mutate(CAE = Araba + Gipuzkoa + Bizkaia) %>% 
  pivot_longer(c(Araba, Bizkaia, Gipuzkoa, CAE), names_to = "geo", values_to = "value") %>% 
  dplyr::filter(!is.na(value))


im_results_final <- 
  bind_rows(im_results,
            general_monthly) %>% 
  pivot_wider(names_from = residente, values_from = value) %>% 
  rename(`no residente` = `not resident`,
         residente = resident) %>% 
  mutate(`no dependiente` = general - residente - `no residente`) %>% 
  pivot_longer(c(`no residente`, residente, general, `no dependiente`), names_to = "residente", values_to = "value") %>% 
  mutate(value = if_else(value < 0, 0 ,value)) %>% 
  pivot_wider(names_from  = measure, values_from = value) %>% 
  mutate(exceso = def - baseline,
         exceso_upper = def - baseline_lower,
         exceso_lower = def - baseline_upper) %>% 
  dplyr::filter(año < 2022)

# Just redo baseline estimation for the non-dependent population, using the IMSERSO fitting 
no_dependientes <-
  im_results_final %>% 
  filter(residente == "no dependiente") %>% 
  select(geo, sexo, año, mes, edad, residente, expos, def) %>% 
  mutate(
    month_int = case_when(mes == "enero" ~ 1,
                          mes == "febrero" ~ 2,
                          mes == "marzo" ~ 3,
                          mes == "abril" ~ 4,
                          mes == "mayo" ~ 5,
                          mes == "junio" ~ 6,
                          mes == "julio" ~ 7,
                          mes == "agosto" ~ 8,
                          mes == "septiembre" ~ 9,
                          mes == "octubre" ~ 10,
                          mes == "noviembre" ~ 11,
                          mes == "diciembre" ~ 12),
    tfrac = calc_month_midpoint_tfrac(month = sprintf("%02d",month_int), año),
    frac = tfrac - año,
    ws = case_when(mes %in% c("enero","febrero","agosto") ~ 0,
                   # year == 2017 & month %in% c("agosto") ~0,
                   # year == 2018 & month %in% c("marzo") ~ 0,
                   # year == 2019 & month %in% c("mayo") ~ 0,
                   tfrac > 2020.2 ~ 0, # excludes march+
                   #year == 2021 #& month %in% c("junio","julio","agosto") ~ 0,
                   TRUE ~ 1)) %>% 
  mutate(def = ifelse(def < 0, 0, def)) %>% 
  rename(deaths = def, exposure = expos) %>% 
  group_by(geo, edad, sexo, residente) %>% 
  do(fit_baseline_imserso(chunk = .data)) %>% 
  ungroup() %>% 
  select(geo, sexo, residente, edad, 
         año, mes, tfrac, ws, edad,
         expos = exposure, def = deaths, baseline, se.fit) %>% 
  arrange(geo, tfrac, sexo, residente, edad) %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) %>% 
  dplyr::filter(tfrac > 2016.9)

im_results_final <-
  im_results_final %>% 
  dplyr::filter(residente != "no dependiente") %>% 
  bind_rows(no_dependientes) %>% 
  filter(año >= 2017)
write_csv(im_results_final,file = "Data/outputs/imserso_y_general_exceso.csv")

# dip on feb is because these are count scales
# and months are 'exact'
im_results_final %>% 
  pivot_longer(expos:exceso_lower, names_to = "measure", values_to = "value") %>% 
  group_by(geo, sexo, año, mes, residente, measure) %>% 
  summarize(value = sum(value), .groups = "drop") %>% 
  mutate(month_int = case_when(mes == "enero" ~ 1,
                               mes == "febrero" ~ 2,
                               mes == "marzo" ~ 3,
                               mes == "abril" ~ 4,
                               mes == "mayo" ~ 5,
                               mes == "junio" ~ 6, 
                               mes == "julio" ~ 7,
                               mes == "agosto" ~ 8,
                               mes == "septiembre" ~ 9,
                               mes == "octubre" ~ 10,
                               mes == "noviembre" ~ 11,
                               mes == "diciembre" ~ 12),
         fecha = ymd(paste(año,month_int,1,sep="-"))) %>% 
  filter(residente == "general",
         measure %in% c("def","baseline")) %>% 
  ggplot(aes(x = fecha, y = value, color = measure)) +
  geom_line() +
 facet_grid(rows = vars(sexo), cols = vars(geo), scale = "free_y")

all_results %>% 
  filter(edad == 80) %>% 
  ggplot(aes(x = tfrac, y = def / baseline, group = sexo, color = sexo)) +
  geom_line() +
  facet_wrap(residente ~ geo) +
  theme_minimal()
  

str(all_results)

# im2 %>% 
#   dplyr::filter(edad == 80) %>% 
#   ggplot(aes(x= tfrac, y = def, color = sexo)) +
#   geom_line() +
#   facet_wrap(~geo)
  
  # dplyr::filter(edad == 80) %>% 
  # ggplot(aes(x = tfrac, y = def, color = sexo)) + 
  # geom_line() +
  # facet_grid(vars(residente),vars(geo)) +
  # ylim(.2,5)
  
# eus_monthly %>% 
#   dplyr::filter(edad == 80) %>% 
#   ggplot(aes(x = tfrac, y = def/baseline, color = sexo)) + 
#   geom_line() +
#   facet_wrap(~geo, scales = "free_y")


all_results <- read_csv("Data/outputs/imserso_exceso_comparar.csv",
                        show_col_types = FALSE)
# all_results %>% 
#   filter(residente == "general",
#          geo == "CAE",
#          edad >= 65) %>%
#   group_by(año) %>% 
#   summarize(def = sum(def))
  
  
# colors
# araba green
# gipuzkoa blue
# bizkaia red
# cae yellow


all_results %>% 
  filter(año >= 2020,
         tfrac < 2021.78,
         residente != "general") %>% 
  group_by(geo, residente, año) %>% 
  dplyr::summarize(def=sum(def),
                   baseline = sum(baseline),
                   baseline_lower = sum(baseline_lower),
                   baseline_upper = sum(baseline_upper), 
                   .groups= "drop") %>% 
  mutate(geo2 = factor(geo, levels = c("CAE", "Araba", "Bizkaia","Gipuzkoa")),
         residente = case_when(residente == "no dependiente" ~ "no dependientes",
                               residente == "not resident" ~ "dependientes \nen domicilio",
                               residente == "resident" ~ "dependientes \nen residencias"),
         residente = factor(residente, levels = c("dependientes \nen residencias",
                                                  "dependientes \nen domicilio",
                                                  "no dependientes"))) %>% 
  ggplot(aes(x = as.factor(año), 
             y = (def / baseline - 1) * 100, 
             ymin = (def / baseline_upper - 1) * 100,
             ymax = (def / baseline_lower - 1) * 100,
             color = geo2)) +
  geom_point(position = position_dodge2(width =.4)) +
  geom_pointrange(position = position_dodge2(width =.4)) +
  facet_grid(vars(residente)) +
  geom_hline(yintercept = 0, color = "#FF000050") +
  labs(y= "porcentaje de defunciones en exceso",
       x = "",
       color = "") +
  scale_color_manual(values = c("#E7B800","#4bc947","#FC4E07","#2E9FDF")) +
  theme_minimal() +
  theme(strip.text.y.right = element_text(angle = 0),
        strip.placement = "outside",
        strip.text = element_text(size = 14))


all_results %>% 
  filter(año >= 2020,
         residente != "general") %>% 
  group_by(geo, sexo, residente, año) %>% 
  dplyr::summarize(def=sum(def),
                   baseline = sum(baseline),
                   baseline_lower = sum(baseline_lower),
                   baseline_upper = sum(baseline_upper), .groups= "drop") %>% 
  mutate(geo2 = factor(geo, levels = c("CAE", "Araba", "Bizkaia","Gipuzkoa")),
         residente = case_when(residente == "no dependiente" ~ "no dependientes",
                               residente == "not resident" ~ "dependientes \nen domicilio",
                               residente == "resident" ~ "dependientes \nen residencias"),
         residente = factor(residente, levels = c("dependientes \nen residencias",
                                                  "dependientes \nen domicilio",
                                                  "no dependientes"))) %>% 
  ggplot(aes(x = as.factor(año), 
             y = (def / baseline - 1) * 100, 
             ymin = (def / baseline_upper - 1) * 100,
             ymax = (def / baseline_lower - 1) * 100,
             color = geo2)) +
  geom_point(position = position_dodge2(width =.4)) +
  geom_pointrange(position = position_dodge2(width =.4)) +
  facet_grid(vars(residente),vars(sexo)) +
  geom_hline(yintercept = 0, color = "#FF000050") +
  labs(y= "porcentaje de defunciones en exceso",
       x = "",
       color = "") +
  scale_color_manual(values = c("#E7B800","#4bc947","#FC4E07","#2E9FDF")) +
  theme_minimal() +
  theme(strip.text.y.right = element_text(angle = 0),
        strip.placement = "outside",
        strip.text = element_text(size = 14))


all_results %>% 
  filter(año >= 2020,
         residente != "general") %>% 
  group_by(geo, edad, residente, año) %>% 
  dplyr::summarize(def=sum(def),
                   baseline = sum(baseline),
                   baseline_lower = sum(baseline_lower),
                   baseline_upper = sum(baseline_upper), .groups= "drop") %>% 
  mutate(geo2 = factor(geo, levels = c("CAE", "Araba", "Bizkaia","Gipuzkoa")),
         residente = case_when(residente == "no dependiente" ~ "no dependientes",
                               residente == "not resident" ~ "dependientes \nen domicilio",
                               residente == "resident" ~ "dependientes \nen residencias"),
         residente = factor(residente, levels = c("dependientes \nen residencias",
                                                  "dependientes \nen domicilio",
                                                  "no dependientes")),
         edad_label = case_when(edad == 65 ~ "65-79 años",
                                edad == 80 ~ "80 y más años"),
         baseline_lower = ifelse(edad == 65 & geo == "Araba" & residente == "dependientes \nen residencias",
                                 def / 2.49,
                                 baseline_lower)) %>% 
  ggplot(aes(x = as.factor(año), 
             y = (def / baseline - 1) * 100, 
             ymin = (def / baseline_upper - 1) * 100,
             ymax = (def / baseline_lower - 1) * 100,
             color = geo2)) +
  geom_point(position = position_dodge2(width =.4)) +
  geom_pointrange(position = position_dodge2(width =.4)) +
  facet_grid(vars(residente),vars(edad_label)) +
  geom_hline(yintercept = 0, color = "#FF000050") +
  labs(y= "porcentaje de defunciones en exceso",
       x = "",
       color = "") +
  scale_color_manual(values = c("#E7B800","#4bc947","#FC4E07","#2E9FDF")) +
  theme_minimal() +
  theme(strip.text.y.right = element_text(angle = 0),
        strip.placement = "outside",
        strip.text = element_text(size = 14)) +
  ylim(-50,150)


all_results %>% 
  filter(año == 2020,
         residente != "general") %>% 
  group_by(geo, mes, residente, año) %>% 
  dplyr::summarize(def=sum(def),
                   baseline = sum(baseline),
                   baseline_lower = sum(baseline_lower),
                   baseline_upper = sum(baseline_upper), 
                   .groups= "drop") %>% 
  mutate(geo2 = factor(geo, levels = c("CAE", "Araba", "Bizkaia","Gipuzkoa")),
         residente = case_when(residente == "no dependiente" ~ "no dependientes",
                               residente == "not resident" ~ "dependientes \nen domicilio",
                               residente == "resident" ~ "dependientes \nen residencias"),
         residente = factor(residente, levels = c("dependientes \nen residencias",
                                                  "dependientes \nen domicilio",
                                                  "no dependientes")),
         mes = factor(mes, levels = c("enero","febrero","marzo","abril",
                                      "mayo","junio","julio","agosto",
                                      "septiembre","octubre","noviembre",
                                      "diciembre"))) %>% 
  ggplot(aes(x = mes, 
             y = (def / baseline - 1) * 100, 
             ymin = (def / baseline_upper - 1) * 100,
             ymax = (def / baseline_lower - 1) * 100,
             color = geo2)) +
  geom_point(position = position_dodge2(width =.4)) +
  geom_pointrange(position = position_dodge2(width =.4)) +
  facet_grid(vars(geo),vars(residente)) +
  geom_hline(yintercept = 0, color = "#FF000050") +
  labs(y= "porcentaje de defunciones en exceso",
       x = "",
       color = "") +
  scale_color_manual(values = c("#E7B800","#4bc947","#FC4E07","#2E9FDF")) +
  theme_minimal() +
  theme(strip.text.y.right = element_text(angle = 0),
        strip.placement = "outside",
        strip.text = element_text(size = 14),
        axis.text.x = element_text(angle = -45, vjust = 0, hjust=0)) 

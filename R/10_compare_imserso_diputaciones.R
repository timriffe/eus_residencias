# This script is for one-off diagnostic checks for whether
# 1) IMSERSO residents and diputaciones residents have the same essential signal: yes
#    they show roughly constant (crude) rate ratios centered on 1. Rate ratios are sometimes
#    thrown off due to artifacts.
#    populations are closest together in Gipuzkoa, steadiest in Araba, and most different in Bizkaia 
# 2) We then check the diputaciones 2015+ trends, and they are flat, both visually and when
#    a line is fit to pre 2020 data
# 3) We then check diputaciones for years 2010+ for Gipuzkoa only, and crude rate trends are 
#    also clearly constant.
# For these reasons, we no longer have reason to suspect that the IMSERSO flat trend is 
# due to a calendar coincidence of general population mortality stagnation. It is likely a 
# long term characteristic of this highly selected population. Replacements are with similarly
# frail people, so it's a roughly constant risk profile.

source("R/00_install_load.R")

B <- read_excel("Data/inputs/Base_defunciones_mensual_2015_21(diputaciones).xlsx")

# treat the monthly diputaciones data as if it were IMSERSO, using same age bins,
# albeit with data starting in 2015 instead of 2017, so 5 instead of 3 fitting years.
Dip_monthly <-
  B %>% 
  mutate(DEFUN_TOTAL = suppressWarnings(as.integer(DEFUN_TOTAL))) %>% 
  dplyr::filter(!is.na(DEFUN_TOTAL),
                !(Año == 2021 & Mes >= 10),
                Edad_grup != "< 65 años") %>% 
  mutate(edad = if_else(Edad_grup %in% c("65-74 años","75-79 años","80-84 años"),65,85),
         sexo = if_else(Sexo == "H","Hombre","Mujer")) %>% 
  group_by(TH_cod, Año, Mes, sexo, edad) %>% 
  summarize(def = sum(DEFUN_TOTAL), 
            usuarios = sum(USU_TOTAL_FECHA),
            .groups = "drop") %>% 
  mutate(fecha = dmy(paste(1,Mes, Año, sep = ".")),
         tfrac = calc_month_midpoint_tfrac(month = sprintf("%02d",Mes), Año),
         frac = tfrac - Año,
         exposure = adjust_month_exposure(usuarios + def / 2, Mes, Año) %>% unname %>% c,
         ws = case_when(Mes %in% c(1,2,8) ~ 0,
                        tfrac > 2020.1 ~ 0, # excludes march+
                        TRUE ~ 1)) %>% 
  rename(deaths = def) %>% 
  arrange(TH_cod, edad, sexo,tfrac) %>% 
  group_by(TH_cod, edad, sexo) %>% 
  # fit same model as imserso
  do(fit_baseline_imserso(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(excess = deaths - baseline) %>% 
  select(geo = TH_cod, sexo, edad, 
         año = Año, mes = Mes, tfrac, ws, edad, pob = usuarios,
         expos = exposure, def = deaths, baseline, 
         exceso = excess, se.fit) %>% 
  arrange(geo, tfrac, sexo, edad) %>% 
  mutate(baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower,
         geo = if_else(geo == "Alava","Araba",geo)) %>% 
  select(-se.fit, -ws, -pob) %>% 
  pivot_longer(expos:exceso_upper, names_to = "measure", values_to = "diputaciones") %>% 
  pivot_wider(names_from = geo, values_from = diputaciones) %>% 
  mutate(CAE = Araba + Gipuzkoa + Bizkaia) %>% 
  pivot_longer(c(Araba, Gipuzkoa, Bizkaia, CAE), names_to = "geo", values_to = "diputaciones")

months <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
month_int <- 1:12
names(month_int) <- months
im_baseline <- read_csv("Data/outputs/imserso_y_general_exceso.csv",
                        show_col_types = FALSE) %>% 
  dplyr::filter(residente == "residente") %>% 
  pivot_longer(expos:exceso_lower, names_to = "measure", values_to = "imserso") %>% 
  mutate(mes = month_int[mes]) %>% 
  select(-c(residente, tfrac, ws,se.fit))

compare <-
  inner_join(im_baseline,
             Dip_monthly,
             by = c("geo","sexo","año","mes","edad","measure"))


compare %>% 
  dplyr::filter(año == 2020,
                geo == "CAE",
                measure %in% c("def","baseline")) %>% 
  pivot_longer(c(imserso, diputaciones), names_to = "source", values_to = "value") %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(rel_exceso = def / baseline) %>% 
  ggplot(aes(x = tfrac,y=rel_exceso, color = source)) +
  geom_line() +
  scale_y_log10()+
  facet_grid(vars(edad),vars(sexo))
  


im_baseline$edad %>% unique()
Dip_monthly$edad %>% unique()

compare_data <- 
  compare %>% 
  dplyr::filter(measure %in% c("expos","def")) %>% 
  pivot_wider(names_from = measure, values_from = c(imserso,diputaciones)) %>% 
  mutate(Md = diputaciones_def / diputaciones_expos,
         Mi = imserso_def / imserso_expos,
         tfrac = año + mes / 12) %>% 
  select(-contains("def")) %>% 
  select(-contains("expos")) 

compare_annual <-
  compare %>% 
  group_by()

compare_data %>% 
  ggplot(aes(x = tfrac, 
             y = Md / Mi, 
             color = sexo,
             group = sexo)) +
  geom_line() +
  facet_wrap(~geo) +
  scale_y_log10() +
    geom_smooth(data = dplyr::filter(compare_data,tfrac <2020.1),
                mapping = aes(x = tfrac, 
                              y = Md / Mi, 
                              color = sexo,
                              group = sexo),
                method = 'lm')
  

compare %>% 
  dplyr::filter(measure %in% c("def")) %>% 
  pivot_wider(names_from = measure, values_from = c(imserso,diputaciones)) %>% 

  ggplot(aes(x = tfrac, 
             y = def, 
             color = sexo,
             group = sexo)) +
  geom_line() +
  scale_y_log10()

dip_deaths_ann <-
  Dip_monthly %>% 
  filter(measure == "def") %>% 
  group_by(geo,año,mes,sexo) %>% 
  summarize(diputaciones = sum(diputaciones))

im_deaths_ann <-
  im_baseline %>% 
  filter(measure == "def",
         between(año,2017,2021)) %>% 
  group_by(geo,año,mes,sexo) %>% 
  summarize(imserso = sum(imserso, na.rm = TRUE))

compare_def_ann <-
  inner_join(dip_deaths_ann,
             im_deaths_ann,
             by = c("geo","año","mes","sexo"))

compare_def_ann %>% 
  mutate(tfrac = año + mes / 12) %>% 
  ggplot(aes(x=tfrac,y=diputaciones / imserso, color = sexo,group=sexo)) +
  geom_line() +
  facet_wrap(~geo) +
  scale_y_log10()

compare_def_ann %>% 
  pivot_longer(c(diputaciones, imserso), names_to = "version", values_to = "def") %>% 
  mutate(tfrac = año + mes / 12) %>% 
  ggplot(aes(x = tfrac, y = def, color = version)) +
  geom_line() +
  facet_grid(vars(sexo), vars(geo))


dip_expos_ann <-
  Dip_monthly %>% 
  filter(measure == "expos") %>% 
  group_by(geo,año,mes,sexo) %>% 
  summarize(diputaciones = sum(diputaciones))


im_expos_ann <-
  im_baseline %>% 
  filter(measure == "expos",
         between(año,2017,2021)) %>% 
  group_by(geo,año,mes,sexo) %>% 
  summarize(imserso = sum(imserso, na.rm = TRUE))

compare_expos_ann <-
  inner_join(dip_expos_ann,
             im_expos_ann,
             by = c("geo","año","mes","sexo"))

compare_expos_ann %>% 
  mutate(tfrac = año + mes / 12) %>% 
  ggplot(aes(x=tfrac,y=diputaciones / imserso, color = sexo,group=sexo)) +
  geom_line() +
  facet_wrap(~geo) +
  scale_y_log10()

compare_expos_ann %>% 
  pivot_longer(c(diputaciones, imserso), names_to = "version", values_to = "expos") %>% 
  mutate(tfrac = año + mes / 12) %>% 
  ggplot(aes(x = tfrac, y = expos, color = version)) +
  geom_line() +
  facet_grid(vars(sexo), vars(geo))

im_goods <-
im_baseline %>% 
  filter(measure %in% c("expos","def"),
         between(año,2017,2021)) %>% 
  group_by(geo,año,mes,sexo, measure) %>% 
  summarize(imserso = sum(imserso, na.rm = TRUE), .groups = "drop")

dip_goods<-
  Dip_monthly %>% 
  filter(measure%in% c("expos","def")) %>% 
  group_by(geo,año,mes,sexo,measure) %>% 
  summarize(diputaciones = sum(diputaciones), .groups = "drop")

crude_rate_ratio_plots <-
  inner_join(dip_goods,
             im_goods,
             by = c("geo","año","mes","sexo","measure")) %>% 
  pivot_longer(c(diputaciones, imserso), names_to = "version", values_to = "counts") %>% 
  pivot_wider(names_from = measure, values_from = counts) %>% 
  mutate(mx = def / expos, .keep = "unused") %>% 
  pivot_wider(names_from = version, values_from = mx) %>% 
  mutate(tfrac = año + mes / 12) 
crude_rate_ratio_plots %>% 
    ggplot(aes(x = tfrac, y = diputaciones / imserso, color = sexo)) +
    geom_line() +
    scale_y_log10() +
    facet_wrap(~geo) +
  geom_smooth(data =filter(crude_rate_ratio_plots, tfrac < 2020.1),
              aes(x = tfrac, y = diputaciones / imserso, color = sexo),
              method = "lm")+
    labs(
      title = "Las razones de tasas brutas entre IMSERSO y Diputaciones son constantes en el tiempo")
  
  inner_join(dip_goods,
             im_goods,
             by = c("geo","año","mes","sexo","measure")) %>% 
    pivot_longer(c(diputaciones, imserso), names_to = "version", values_to = "counts") %>% 
    pivot_wider(names_from = measure, values_from = counts) %>% 
    mutate(mx = def / expos, .keep = "unused") %>% 
    mutate(tfrac = año + mes / 12) %>% 
    ggplot(aes(x = tfrac, y = mx, color = version)) +
    geom_line() +
    # scale_y_log10() +
    facet_grid(vars(sexo),vars(geo))
  
  
  
  
# Time trend for diputaciones since 2015: looks flat
  
  Dip_monthly %>% 
    dplyr::filter(measure %in% c("def","expos"),
                  tfrac < 2020.1) %>% 
    pivot_wider(names_from = measure, values_from = diputaciones) %>% 
    mutate(M = def / expos) %>% 
    ggplot(aes(x = tfrac, y = M, color = sexo)) +
    geom_line() +
    facet_grid(vars(edad), vars(geo)) +
    geom_smooth(method = "lm")
  
# what about Gipuzkoa diputaciones since 2010? Also flat!
GD <- read_excel("Data/inputs/Gipuzkoa2010_2021.xlsx",sheet = 2) %>% 
  pivot_longer(`1`:`12`, names_to = "month", values_to = "def") %>% 
  dplyr::filter(!is.na(def))

GU <- read_excel("Data/inputs/Gipuzkoa2010_2021.xlsx",sheet = 3) %>% 
  select(-c(1,2)) %>% 
  pivot_longer(`1`:`12`, names_to = "month", values_to = "usuarios_end") %>% 
  dplyr::filter(!is.na(usuarios_end))

GM <- 
left_join(GD, GU, by = c("year", "month")) %>% 
  mutate(month = as.integer(month),
         exposure = adjust_month_exposure(usuarios_end + def/2, month, year) %>% unname %>% c,
         M = def / exposure,
         tfrac = year + month / 12) 
  GM_plot <-
GM %>% 
  ggplot(aes(x = tfrac, y = M)) +
  geom_line() +
  geom_smooth(data = filter(GM, tfrac < 2020.1),
              mapping = aes(x = tfrac, y = M)) +
  labs(x = "", y = "tasa bruta de mortalidad",
       title = "Tasas brutas de mortalidad en las residencias de Guipuzkoa",
       subtitle = "Una tendencia relativamente constante en el tiempo")
ggsave(GM_plot,
       filename = "Figs/Guipuzkoa_validation.png",
       height = 6, width = 6, dpi = 300)

# Conclusion: the fact that IMSERSO baseline shows no time trend is not a problem,
# and it's probably *not* due to a coincidence of the years 2017+ with a period of
# approximate general-population mortality stagnation. Before these checks, that
# was a pretty major conundrum.


read_csv("Data/outputs/imserso_y_general_exceso.csv") %>% 
  dplyr::filter(residente == "residente",
                año == 2020,
                geo == "CAE") %>% 
  summarize(exceso_lower = sum(exceso_lower),
            exceso = sum(exceso),
            exceso_upper = sum(exceso_upper),
            def = sum(def))

dip <- read_csv("Data/outputs/diputaciones_exceso_total.csv")

dip %>% 
  dplyr::filter(año == 2020) %>% 
  summarize(exceso_lower = sum(exceso_lower),
            exceso = sum(exceso),
            exceso_upper = sum(exceso_upper),
            def = sum(def))


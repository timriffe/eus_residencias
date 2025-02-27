source("R/00_install_load.R")
library(forcats)
Dip <- read_csv("Data/intermediate/diputaciones_input.csv",
                show_col_types = FALSE)
# Dip_fixed <- read_csv("Data/intermediate/diputaciones_input_fixed.csv",
#                 show_col_types = FALSE)

# Dip_ready %>% 
#   filter(year == 2020,
#          definition == "ALTO_IMPACTO") %>% 
#   pull(exposure) %>% sum() # 13889 / 9907.622
Dip_ready <-
  Dip %>% 
  dplyr::filter(edad > 60) %>% 
  mutate(tfrac = year + .5,
         tfrac = if_else(year == 2021, 2021 + 3/8, tfrac),
         tfrac = tfrac - 2015,
         ws = if_else(year < 2020, 1, 0),
         ws = if_else(exposure < def, 0, ws )) 


Dip_saturated <-
  Dip_ready %>% 
  group_by(definition,sexo,subgroup) %>% 
  do(fit_baseline_dip_saturated(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) %>% 
  select(año = year, sexo, edad, definition, subgrupo = subgroup, 
         def, expos = exposure, baseline, exceso, exceso_lower, exceso_upper)
 

write_csv(Dip_saturated, file = "Data/outputs/diputaciones_exceso.csv")

Dip_total <-
  Dip_ready %>% 
  dplyr::filter(definition == "ALTO_IMPACTO") %>% 
  group_by(year, edad, sexo) %>% 
  summarize(exposure = sum(exposure),
            def = sum(def),
            .groups = "drop") %>% 
  mutate(tfrac = year + .5 - min(year),
         ws = if_else(year < 2020,1,0)) %>% 
  group_by(sexo) %>% 
  do(fit_baseline_dip_saturated(chunk=.data)) %>% 
  ungroup() %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) %>% 
  select(año = year, sexo, edad, 
         def, expos = exposure, baseline, exceso, exceso_lower, exceso_upper)
write_csv(Dip_total, file = "Data/outputs/diputaciones_exceso_total.csv")
Dip_total %>% 
  group_by(año,sexo) %>% 
  summarize(exceso = sum(exceso),
            .groups = "drop") %>% 
  dplyr::filter(año>=2020)


Dip_saturated %>% 
  group_by(año,sexo,definition) %>% 
  summarize(exceso = sum(exceso),
            .groups = "drop") %>% 
  dplyr::filter(año>=2020) %>% 
  pivot_wider(names_from = definition, values_from = exceso)

Dip_plot_data <-
  Dip_saturated %>% 
  mutate(subgrupo = case_when(
    subgrupo == "Alto (>30.000\u0080)" ~ "Alto",
    subgrupo == "Medio (23.500\u0080 - 30.000\u0080)" ~ "Medio",
    subgrupo == "Bajo (< 23.500\u0080)" ~ "Bajo",
    subgrupo == "Entidades rurales y urbanas asimilables" ~ "Rural",
    subgrupo == "Entidades urbanas" ~ "Urbano",
    subgrupo == "Privadas con el 50% o más de financ. pública" ~ "Privada+",
    subgrupo == "Privadas con menos del 50% de financ. pública" ~ "Privada-",
    subgrupo == "Públicas" ~ "Pública",
    subgrupo == "No dispone de servicio médico en el centro" ~ "no",
    subgrupo == "Sí dispone de servicio médico en el centro" ~ "si",
    subgrupo == "Alto: >= 0,7565"  ~ "Alto",
    subgrupo == "Bajo: <= 0,6061"   ~ "Bajo",
    subgrupo == "Medio: 0,6062 - 0,7564"   ~ "Medio",
    subgrupo == "100 o más plazas"   ~ "100+",
    subgrupo == "51 - 99 plazas"   ~ "51 - 99",
    subgrupo == "Hasta 50 plazas"   ~ "50-",
    subgrupo =="Alta: >=0,75" ~ "Alta",
    subgrupo =="Baja:<= 0,44"  ~ "Baja",
    subgrupo =="Media: 0,45 - 0,74" ~ "Media",
    subgrupo == "<= 9679" ~ "Bajo",
    subgrupo == "9680 - 11940" ~ "Medio",
    subgrupo == "11941+" ~ "Alto",
    subgrupo == "Privada" ~ "Privada",
    TRUE ~ subgrupo
  ),
  subgrupo = fct_relevel(subgrupo,
                         c("Bajo","Medio","Alto","Baja","Media","Alta","Rural","Urbano",
                          "50-","51 - 99","100+","si","no","Privada","Privada-","Privada+","Pública","Resto","Alto impacto"))) 
Dip_Cx <-
Dip_saturated %>% 
  group_by(edad) %>% 
  summarize(Cx = sum(expos)) %>% 
  mutate(Cx = Cx/ sum(Cx))

Dip_St <-
  Dip_plot_data %>% 
  left_join(Dip_Cx, by = "edad") %>% 
  mutate(
    baseline_lower = def - exceso_upper,
    baseline_upper = def - exceso_lower,
    M = def / expos,
    Mb = baseline / expos,
    Mb_lower = baseline_lower / expos,
    Mb_upper = baseline_upper / expos) %>% 
  group_by(año,sexo,definition, subgrupo) %>% 
  summarize(Mst = sum(M * Cx),
            Mbst = sum(Mb * Cx),
            Mbst_lower = sum(Mb_lower * Cx),
            Mbst_upper = sum(Mb_upper * Cx),
            .groups = "drop")


Dip_crude <- 
  Dip_plot_data %>% 
  group_by(año,sexo,definition, subgrupo) %>% 
  summarize(def = sum(def),
            expos = sum(expos),
            baseline = sum(baseline),
            baseline_lower = sum(def) - sum(exceso_upper),
            baseline_upper = sum(def) - sum(exceso_lower),
            exceso = sum(exceso),
            exceso_lower = sum(exceso_lower),
            exceso_upper = sum(exceso_upper),
            .groups = "drop") 

Dip_St %>% 
  filter(año == 2021) %>% 
  ggplot(aes(x = subgrupo, y = Mst / Mbst, color = sexo, ymax = Mst / Mbst_lower, ymin = Mst / Mbst_upper)) +
  geom_point(position=position_dodge(width=.2)) +
  geom_pointrange(position=position_dodge(width=.2)) +
  facet_wrap(~definition, scales = "free_x") +
  scale_color_manual(values = c("Hombres" = "blue", "Mujeres" = "red")) +
  geom_hline(yintercept = 1, color = "#DD110050")


Dip_crude %>% 
  filter(año == 2021) %>% 
  ggplot(aes(x = subgrupo, y = exceso, color = sexo, ymax = exceso_upper, ymin = exceso_lower)) +
  geom_point() +
  geom_pointrange() +
  facet_wrap(~definition, scales = "free_x") +
  scale_color_manual(values = c("Hombres" = "blue", "Mujeres" = "red")) +
  scale_y_continuous(limits = c(-100,200))




Dip_crude %>% 
  filter(año == 2020) %>% 
  ggplot(aes(x = subgrupo, y = def / baseline, color = sexo, ymax = def / baseline_lower, ymin = def / baseline_upper)) +
  geom_point(position = position_dodge(width=.2)) +
  geom_pointrange(position = position_dodge(width=.2)) +
  facet_wrap(~definition, scales = "free_x")+
  scale_color_manual(values = c("Hombres" = "blue", "Mujeres" = "red")) + 
  scale_y_log10() +
  geom_hline(yintercept = 1, color = "#DD110050")


Dip_plot_data %>% 
  filter(año == 2021,
         definition == "COSTE_PLAZA_GRUP") %>% 
  pull(expos) %>% sum()
Dip_plot_data %>% 
  filter(año == 2020,
         definition == "COSTE_PLAZA_GRUP") %>% 
  pull(baseline) %>% sum()


# ------------------------------------------
# tested, but not used:
do_this <- FALSE
if (do_this){
fit_baseline_dip_sexo_subgroup <- function(chunk, wt){
  gam(def ~ tfrac + sexo + subgroup + 
        ti(edad,bs = 'ps', np = FALSE) +
        offset(log(exposure)),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>% 
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit) %>% 
    mutate(baseline = baseline %>% unname %>% c,
           se.fit = se.fit %>% unname %>% c)
}
fit_baseline_dip_subgroup <- function(chunk){
  gam(def ~ tfrac + subgroup + 
        ti(edad,bs = 'ps', np = FALSE) +
        offset(log(exposure)),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>% 
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit) %>% 
    mutate(baseline = baseline %>% unname %>% c,
           se.fit = se.fit %>% unname %>% c)
}
fit_baseline_dip_sexo <- function(chunk){
  gam(def ~ tfrac + sexo + #ti(t, bs = 'ps') +
        ti(edad,bs = 'ps', np = FALSE) +
        offset(log(exposure)),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>% 
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit) %>% 
    mutate(baseline = baseline %>% unname %>% c,
           se.fit = se.fit %>% unname %>% c)
}




Dip_sexo_subgroup <-
  Dip_ready %>% 
  group_by(definition) %>% 
  do(fit_baseline_dip_sexo_subgroup(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) 
Dip_sexo <-
  Dip_ready %>% 
  group_by(definition,subgroup) %>% 
  do(fit_baseline_dip_sexo(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) 
Dip_subgroup <-
  Dip_ready %>% 
  group_by(definition,sexo) %>% 
  do(fit_baseline_dip_subgroup(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = def - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = def - baseline_upper,
         exceso_upper = def - baseline_lower) 


# compare RMSE of rates for years 2015-2019

# well, saturated model of course performs consistently better...
# But trends can go any direction...

left_join(
Dip_sexo_subgroup %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure,
         weight = if_else(def == 0, 0, 1)) %>% 
  group_by(sexo, definition) %>% 
  summarize(lM_RMSE_sexo_subgroup = sqrt(mean(((log(M) - log(Mb)) * weight) ^ 2, na.rm = TRUE)),
            .groups = "drop"), 
Dip_sexo %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure,
         weight = if_else(def == 0, 0, 1)) %>% 
  group_by(sexo, definition) %>% 
  summarize(lM_RMSE_sexo = sqrt(mean(((log(M) - log(Mb)) * weight) ^ 2, na.rm = TRUE)),
            .groups = "drop"), by = c("sexo","definition")) %>% left_join(
Dip_subgroup %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure,
         weight = if_else(def == 0, 0, 1)) %>% 
  group_by(sexo, definition) %>% 
  summarize(lM_RMSE_subgroup = sqrt(mean(((log(M) - log(Mb)) * weight) ^ 2, na.rm = TRUE)),
            .groups = "drop"), by = c("sexo","definition")) %>% left_join(
Dip_saturated %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure,
         weight = if_else(def == 0, 0, 1)) %>% 
  group_by(sexo, definition) %>% 
  summarize(lM_RMSE_saturated = sqrt(mean(((log(M) - log(Mb)) * weight) ^ 2, na.rm = TRUE)),
            .groups = "drop") , by = c("sexo","definition")) %>% 
  pivot_longer(3:6, names_to = "trend spec", values_to = "lM_RMSE") %>% 
  # pivot_longer(M_RMSE:dw_M_RMSE, names_to = "error type", values_to = "RMSE") %>% 
  ggplot(aes(x= definition, y  = lM_RMSE, color = sexo, shape = `trend spec`)) +
  geom_point(size=2) +
  scale_y_log10(limits = c(.07,.25)) +
  coord_flip() 

Dip_sexo %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure) %>% 
  group_by(sexo, definition) %>% 
  summarize(M_RMSE = sqrt(mean((M - Mb) ^ 2)),
            dw_M_RMSE = sqrt(mean(def/sum(def) * ((M - Mb) ^ 2))),
            .groups = "drop") %>% 
  pivot_longer(M_RMSE:dw_M_RMSE, names_to = "error type", values_to = "RMSE") %>% 
  ggplot(aes(x= definition, y  = RMSE, color = sexo, shape = `error type`)) +
  geom_point() +
  scale_y_log10(limits = c(.001,.05)) +
  coord_flip() +
  labs(title = "trends shared across sex and not subgroups")

Dip_subgroup %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure) %>% 
  group_by(sexo, definition) %>% 
  summarize(M_RMSE = sqrt(mean((M - Mb) ^ 2)),
            dw_M_RMSE = sqrt(mean(def/sum(def) * ((M - Mb) ^ 2))),
            .groups = "drop") %>% 
  pivot_longer(M_RMSE:dw_M_RMSE, names_to = "error type", values_to = "RMSE") %>% 
  ggplot(aes(x= definition, y  = RMSE, color = sexo, shape = `error type`)) +
  geom_point() +
  scale_y_log10(limits = c(.001,.05)) +
  coord_flip() +
  labs(title = "trends across subgroups and not sex")

Dip_saturated %>% 
  dplyr::filter(year < 2020) %>% 
  select(sexo, definition,def,baseline,exposure) %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure) %>% 
  group_by(sexo, definition) %>% 
  summarize(M_RMSE = sqrt(mean((M - Mb) ^ 2)),
            dw_M_RMSE = sqrt(mean(def/sum(def) * ((M - Mb) ^ 2))),
            .groups = "drop") %>% 
  pivot_longer(M_RMSE:dw_M_RMSE, names_to = "error type", values_to = "RMSE") %>% 
  ggplot(aes(x= definition, y  = RMSE, color = sexo, shape = `error type`)) +
  geom_point() +
  scale_y_log10(limits = c(.001,.05)) +
  coord_flip() +
  labs(title = "trends independent by sex and subgroup")

# what about total annual excess under different versions / definitions?

Dip_saturated %>% 
  dplyr::filter(year>= 2020) %>% 
  group_by(year,sexo,definition, subgroup) %>% 
  summarize(exceso = sum(exceso)) %>% 
  mutate(model = "saturated")

defs_keep <- c("COSTE_PLAZA_GRUP", "FACTOR_RURAL5b_GRUP","FACTOR_TIT",
               "PRESTA_MEDICO","RATIO_TOTAL_GRUP","TAMAÑO_50",
               "TITULAR_EM","USUG2Y3_TERCILES")

maybe_elim <- c("TASA_POSIT_GRUP","ZONA_OZONO")
Dip_saturated$definition %>% unique()
Dip_saturated %>% 
  dplyr::filter(definition == "ZONA_OZONO") %>% 
  ggplot(aes(x = edad, y = def / exposure, color = sexo)) +
  geom_line() +
  geom_line(mapping = aes(x = edad, y = baseline / exposure, color = sexo),
            linetype = 2)+
  facet_grid(vars(year),vars(subgroup))

Dip_saturated %>% 
  dplyr::filter(definition != "ZONA_OZONO") %>% 
  head()





# Dip_out$definition %>% unique()
TMR_sexo_subgroup <- list()
TMR_sexo <- list()
TMR_subgroup <- list()
for (defi in Dip_out$definition %>% unique()){
  chunk_sexo_subgroup <-
    Dip_sexo_subgroup %>% 
    dplyr::filter(definition == defi,
                  between(edad,70,90)) %>% 
    group_by(year,subgroup,sexo) %>% 
    summarize(def = mean(def),
              exposure = mean(exposure),
              baseline = mean(baseline),
              .groups = "drop")
  chunk_subgroup <-
    Dip_subgroup %>% 
    dplyr::filter(definition == defi,
                  between(edad,70,90)) %>% 
    group_by(year,subgroup,sexo) %>% 
    summarize(def = mean(def),
              exposure = mean(exposure),
              baseline = mean(baseline),
              .groups = "drop")  
  chunk_sexo <-
    Dip_sexo %>% 
    dplyr::filter(definition == defi,
                  between(edad,70,90)) %>% 
    group_by(year,subgroup,sexo) %>% 
    summarize(def = mean(def),
              exposure = mean(exposure),
              baseline = mean(baseline),
              .groups = "drop")  
  
  TMR_sexo_subgroup[[defi]] <-
    chunk_sexo_subgroup %>% 
    ggplot(aes(x = year, 
               y = baseline/exposure, 
               color = subgroup,
               linetype = sexo)) + 
    geom_line() +
    geom_point(data = chunk, mapping = aes(x = year, y = def/exposure, shape = sexo)) +
    labs(title = defi,
         subtitle = "trends shared across subgroup and sex")
  
  TMR_sexo[[defi]] <-
    chunk_sexo %>% 
    ggplot(aes(x = year, 
               y = baseline/exposure, 
               color = subgroup,
               linetype = sexo)) + 
    geom_line() +
    geom_point(data = chunk, mapping = aes(x = year, y = def/exposure, shape = sexo)) +
    labs(title = defi, subtitle = "trends shared across sex")
  
  TMR_subgroup[[defi]] <-
    chunk_subgroup %>% 
    ggplot(aes(x = year, 
               y = baseline/exposure, 
               color = subgroup,
               linetype = sexo)) + 
    geom_line() +
    geom_point(data = chunk, mapping = aes(x = year, y = def/exposure, shape = sexo)) +
    labs(title = defi, subtitle = "trends shared across subgroups")
}

library(cowplot)

plot_grid(TMR_sexo_subgroup[[1]],
TMR_sexo[[1]],
TMR_subgroup[[1]])


plot_grid(TMR_sexo_subgroup[[2]],
          TMR_sexo[[2]],
          TMR_subgroup[[2]])

length(M_80)
Dip_out %>% 
  dplyr::filter(definition == defi,
                sexo == "Hombres",
                year == 2019) %>% 
  ggplot(aes(x=edad,y = def / exposure, color = subgroup,group = interaction(subgroup))) +
  geom_point() +
  geom_line()

write_csv(Dip_out,"Data/outputs/exceso_residencias_diputaciones_ponderados.csv")
# write_csv(Dip_out_fixed,"Data/outputs/exceso_residencias_diputaciones_fixed.csv")
Dip_out %>% 
  ggplot(aes(x = year, y = baseline / exposure, color = sexo, groups = interaction(sexo, subgroup))) + 
  geom_line() +
  facet_grid(vars(definition),vars(edad))

Dip_out %>% 
  ggplot(aes(x = year, y = exposure, color = sexo, groups = interaction(sexo, subgroup))) + 
  geom_line() +
  facet_grid(vars(definition),vars(edad))
# Dip_out %>% 
#   group_by(geo,sexo,año) %>% 
#   dplyr::summarize(exceso = sum(exceso))

 # "COSTE_PLAZA_GRUP"    "FACTOR_RURAL5b_GRUP" "FACTOR_TIT"          "PRESTA_MEDICO"       "RATIO_TOTAL_GRUP"   
 # "TAMAÑO_50"           "TASA_POSIT_GRUP"     "TITULAR_EM"          "USUG2Y3_TERCILES"    "ZONA_OZONO" 
Dip_out$definition %>% unique()

Dip_out %>% 
  mutate(M = def / exposure,
         Mb = baseline / exposure) %>% 
  dplyr::filter(definition ==  "USUG2Y3_TERCILES") %>% 
  ggplot(aes(x = edad, y = Mb, color = as.factor(year), group = interaction(year, subgroup)))+
  geom_point(aes(x=edad,y=M))+
  geom_line() +
  facet_wrap(sexo~subgroup)

Dip_out %>% 
  dplyr::filter(definition == "COSTE_PLAZA_GRUP") %>% 
  group_by(year,sexo,edad) %>% 
  summarize(def = sum(def), exposure = sum(exposure)) %>% 
  mutate(M = def / exposure) %>% 
  ggplot(aes(x=year,y=M, color = sexo)) +
  geom_line() +
  facet_wrap(~edad) +
  scale_color_manual(values = c(Hombres = "blue", Mujeres = "red"))

Dip_out %>% 
  filter(year == 2020, definition == "COSTE_PLAZA_GRUP") %>%
  ggplot(aes(x = edad, 
             y = ((def / baseline) - 1) * 100, 
             # y = def / usuarios,
             lty = subgroup,
             group = subgroup)) +
  geom_line() +
  geom_line(aes(x = edad, 
            y = ((def / baseline) - 1) * 100, 
            # y = baseline / usuarios,
            lty = subgroup,
            group = subgroup),
            color = "orange") +
  geom_ribbon(aes(x = edad, 
                  y = ((def / baseline) - 1) * 100, 
                  ymin = baseline_lower / baseline,
                  ymax = baseline_upper / baseline,
                  lty = subgroup,
                  group = subgroup),
              fill = "orange",
              alpha = .3) +
  # scale_y_log10() +
  xlim(65,90) +
  labs(y = "porcentaje encima baseline") +
  # labs(y = "tasa mortalidad") +
  facet_grid(~sexo) 
       
Dip_out %>% 
  filter(año == 2020,
         definicion != "Coste/plaza_terciles") %>% 
  group_by(geo, definicion, subgrupo) %>% 
  dplyr::summarize(exceso = sum(exceso),
            exceso_lower = sum(exceso_lower),
            exceso_upper = sum(exceso_upper), .groups = "drop") %>% 
  ggplot(aes(x = subgrupo, y = exceso)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=subgrupo, 
                            ymax = exceso_upper, 
                            ymin = exceso_lower)) +
  facet_grid(vars(geo),vars(definicion), scales = "free") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5)) 

library(cowplot)
d_pond <-
  Dip_out %>% 
  filter(año == 2020,
         definicion != "CostePlaza") %>% 
  group_by(geo, definicion, subgrupo) %>% 
  dplyr::summarize(excesop = 100 * sum(exceso) / sum(baseline),
                   excesop_lower = 100 * sum(exceso_lower) / sum(baseline),
                   excesop_upper = 100 * sum(exceso_upper) / sum(baseline), .groups = "drop") %>% 
  ggplot(aes(x = subgrupo, y = excesop)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=subgrupo, 
                            ymax = excesop_upper, 
                            ymin = excesop_lower)) +
  facet_grid(vars(geo),vars(definicion), scales = "free_x") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5)) +
  labs(y = "exceso / baseline (porcentaje)",
       x = "")+
  ylim(-25,75)+
  geom_hline(yintercept = 0, color = "red", alpha = .5)

d_fixed <-
Dip_out_fixed %>% 
  filter(año == 2020,
         definicion != "CostePlaza") %>% 
  mutate(subgrupo = case_when(grepl(subgrupo,pattern="Bajo*") ~ "1",
                              grepl(subgrupo,pattern="Medio*") ~ "2",
                              grepl(subgrupo,pattern="Alto*") ~ "3",
                              grepl(subgrupo,pattern="100*") ~ "3",
                              grepl(subgrupo,pattern="*99*") ~ "2",
                              grepl(subgrupo,pattern="Menos*") ~ "1",
                              TRUE ~ subgrupo)) %>% 
  group_by(geo, definicion, subgrupo) %>% 
  dplyr::summarize(excesop = 100 * sum(exceso) / sum(baseline),
                   excesop_lower = 100 * sum(exceso_lower) / sum(baseline),
                   excesop_upper = 100 * sum(exceso_upper) / sum(baseline), .groups = "drop") %>% 
  ggplot(aes(x = subgrupo, y = excesop)) +
  geom_point() +
  geom_errorbar(mapping=aes(x=subgrupo, 
                            ymax = excesop_upper, 
                            ymin = excesop_lower)) +
  facet_grid(vars(geo),vars(definicion), scales = "free_x") +
  theme(axis.text.x = element_text(angle = -45, vjust = .5, hjust = 0)) +
  labs(y = "exceso / baseline (porcentaje)",
       x = "") +
  ylim(-25,75) +
  geom_hline(yintercept = 0, color = "red", alpha = .5)

plot_grid(d_pond ,d_fixed,nrow=2,labels=c("usuarios","centros"))

# library(reldist)
# set.seed(1)
# 
# X <- 
# tibble(center = 1:20, 
#        usuarios = (rexp(n = 20, 25) * 800)  %>% 
#          round %>% '+'(5), 
#        a = runif(20) %>% round(3)) %>% 
#   mutate(terc_tamaño = cut(usuarios,
#                        reldist::wtd.quantile(x = usuarios, 
#                                     weight = usuarios, 
#                                     q = c(0,1/3,2/3,1)),
#                        labels = 1:3) %>% as.integer(),
#          terc_tamaño = ifelse(is.na(terc_tamaño),1,terc_tamaño),
#          terc_a = cut(a,
#                       reldist::wtd.quantile(x = a, 
#                                             weight = usuarios, 
#                                             q = c(0,1/3,2/3,1)),
#                       labels = 1:3) %>% as.integer(),
#          terc_a = ifelse(is.na(terc_a),1,terc_a))
# View(X)
# X %>% write_excel_csv("Data/outputs/terciles.csv")
# X
# X %>% 
#   group_by(tercil = terc_tamaño) %>% 
#   dplyr::summarise(usuarios = sum(usuarios))
# 
# X %>% 
#   group_by(tercil = terc_a) %>% 
#   dplyr::summarize(usuarios = sum(usuarios))
}
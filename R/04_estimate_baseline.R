
source("R/00_install_load.R")
# source("R/excess_quebec_211106/excess_quebec/code/00_functions.R")
M <- read_csv("Data/intermediate/tasas_provincias_eus.csv",show_col_types = FALSE) %>% 
  filter(age >= 50) %>% 
  # creating a time variable (along the weeks) 
  arrange(prov, sex, age, date) %>% 
  group_by(prov, sex, age) %>% 
  #mutate(t = 1:n()) %>% # definition of variable t for the secular trend
  ungroup() %>% 
  mutate(week = isoweek,
         year = isoyear, 
         ws = 
              between(date, ymd("2002-01-03"), ymd("2020-02-27")) & 
              !between(week,1,8) &
              week < 50,
              !between(week, 24, 28), # remove summer peak
         ws = as.integer(ws),
         # defining time frame to include in the prediction (prediction data)
         to_pred = between(date, 
                           ymd("2002-01-03"), 
                           ymd("2021-12-31")) %>% as.integer()) %>% 
  select(prov, sex, age, frac, tfrac, date, week, deaths, exposure = pop, 
         ws, to_pred, frac)
source("R/00_install_load.R")
Baseline <- M %>% 
  group_by(prov, sex) %>% 
  do(fit_baseline(chunk = .data, k = 5)) %>% 
  ungroup() %>% 
  mutate(exceso = deaths - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = deaths - baseline_upper,
         exceso_upper = deaths - baseline_lower) %>% 
  select(prov, sexo = sex, edad = age, fecha = date, 
         t_frac = tfrac, semana = week, def = deaths, 
         expos = exposure, incluir = ws, baseline, exceso,
         baseline_lower,baseline_upper, exceso_lower, exceso_upper) %>% 
  labelled::remove_attributes(c("dimnames","dim")) %>% 
  lapply(unname) %>% 
  as_tibble() %>% 
  as.data.frame() 

Baseline %>% 
  write_csv("Data/outputs/eus_baseline_2002_2019.csv")

 #  to_plot <-
 # Baseline %>% 
 #   filter(fecha < dmy("01.03.2020")) %>% 
 #   group_by(prov, semana, incluir) %>% 
 #   summarize(def = sum(def),
 #             baseline = sum(baseline),
 #             .groups = "drop")  %>%
 #   mutate(semana = as.integer(semana)) %>% 
 #    pivot_longer(c(def, baseline), names_to = "measure", values_to="value")
 #  
 #  to_plot %>% 
 #    filter(measure == "def") %>% 
 #   ggplot(aes(x = semana, y = value)) + 
 #   geom_line() +
 #   geom_point(aes(color = incluir)) + facet_wrap(~prov) +
 #    geom_line(data = filter(to_plot, measure == "baseline"),
 #              mapping = aes(x = semana, y = value), color = "orange")


Baseline_compare <- 
  M %>% 
  dplyr::filter(date >= dmy("01.01.2015")) %>% 
  group_by(prov, sex) %>% 
  do(fit_baseline(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = deaths - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = deaths - baseline_upper,
         exceso_upper = deaths - baseline_lower) %>% 
  select(prov, sexo = sex, edad = age, fecha = date, 
         t_frac = tfrac, semana = week, def = deaths, 
         expos = exposure, incluir = ws, baseline, exceso,
         baseline_lower,baseline_upper, exceso_lower, exceso_upper) %>% 
  labelled::remove_attributes(c("dimnames","dim")) %>% 
  lapply(unname) %>% 
  as_tibble() %>% 
  as.data.frame() 

Baseline_compare %>% 
  write_csv("Data/outputs/eus_baseline_2015-2019.csv")


# redo with ages independent
Baseline2 <- 
  M %>% 
  group_by(prov, sex, age) %>% 
  do(fit_baseline_age_indep(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = deaths - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = deaths - baseline_upper,
         exceso_upper = deaths - baseline_lower) %>% 
  select(prov, sexo = sex, edad = age, fecha = date, 
         t_frac = tfrac, semana = week, def = deaths, 
         expos = exposure, incluir = ws, baseline, exceso,
         baseline_lower,baseline_upper, exceso_lower, exceso_upper) %>% 
  labelled::remove_attributes(c("dimnames","dim")) %>% 
  lapply(unname) %>% 
  as_tibble() %>% 
  as.data.frame() 

Baseline2 %>% 
  write_csv("Data/outputs/eus_baseline_edad_indep.csv")

# independent ages, and with restricted fitting period
# to mimic the IMERSO data constraints: result: as expected
# the fitting period is restricted to years of mortality stagnation
# so excess drops much lower. We do not know whether the stagnant
# baseline trend in IMSERSO is generally to be expected, or whether
# the restricted year range just happens to be a stagnant snapshot.
# One cannot blindly assume that general population and the highly
# selected resident pop have a 1:1 mapping of such trends.
Baseline2_compare <- 
  M %>% 
  dplyr::filter(date >= dmy("01.12.2016")) %>% 
  group_by(prov, sex, age) %>% 
  do(fit_baseline_age_indep(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exceso = deaths - baseline,
         baseline_lower = baseline - 2 * se.fit,
         baseline_upper = baseline + 2 * se.fit,
         exceso_lower = deaths - baseline_upper,
         exceso_upper = deaths - baseline_lower) %>% 
  select(prov, sexo = sex, edad = age, fecha = date, 
         t_frac = tfrac, semana = week, def = deaths, 
         expos = exposure, incluir = ws, baseline, exceso,
         baseline_lower,baseline_upper, exceso_lower, exceso_upper) %>% 
  labelled::remove_attributes(c("dimnames","dim")) %>% 
  lapply(unname) %>% 
  as_tibble() %>% 
  as.data.frame() 
Baseline2_compare$exceso %>% sum()
Baseline2_compare$exceso_lower %>% sum()
Baseline2_compare %>% 
  write_csv("Data/outputs/eus_baseline_edad_indep_2017+.csv")

# cumulative totals of baseline, observed, and excess
# BaselineCum <- 
#   Baseline %>% 
#   filter(date >= ymd("2020-01-01")) %>% 
#   arrange(prov,sex,age,date) %>% 
#   group_by(prov, sex, age) %>% 
#   mutate(deaths_cum = cumsum(deaths),
#          baseline_cum = cumsum(baseline)) %>% 
#   ungroup() %>% 
#   mutate(exceso = deaths - baseline,
#          exceso_cum = deaths_cum - baseline_cum) %>% 
#   select(prov, sexo = sex, edad = age, fecha = date, t_frac = tfrac, 
#          semana = week, def = deaths, expos = exposure, incluir = ws, baseline, baseline_cum, exceso, exceso_cum)
# BaselineCum %>% 
#   select(prov, sexo = sex, edad = age, fecha = date, t_frac = tfrac, 
#          semana = week, def = deaths, expos = exposure, incluir = ws, 
#          baseline, baseline_cum, exceso, exceso_cum) %>% 
#   write_csv("Data/cumulative2020+.csv")


# chunk <-
#   Mfitting %>% 
#   filter(sex == "Hombres",
#          prov == "48 Bizkaia")
# model <- 
#     gam(deaths ~ s(t, bs = 'ps', m = c(2,2)) +
#                  poly(age,4) +
#                  s(tfrac, bs = 'cp') + 
#                  offset(log(exposure)),
#         data = chunk,
#         weights = ws,
#         family = quasipoisson(link = "log"),
#         na.action = na.omit)
# model2 <- 
#   gam(deaths ~ s(t, bs = 'ps', m = c(2,2)) +
#         agef +
#         s(tfrac, bs = 'cp') + 
#         offset(log(exposure)),
#       data = chunk,
#       weights = ws,
#       family = quasipoisson(link = "log"),
#       na.action = na.omit)
# 
# 
# 
# model3 <-
# chunk %>% 
#   gam(deaths ~ s(t, bs = 'ps', m = c(2,2)) +
#         ti(tfrac, bs = 'cp',np = FALSE) +
#         ti(age, bs = 'ps', np = FALSE) +
#         offset(log(exposure)),
#       data = .,
#       weights = ws,
#       family = quasipoisson(link = "log"),
#       na.action = na.omit)
# 
# summary(model2)
# predicting mortality baseline estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pred <- predict(model, type = "response", newdata = chunk)
# pred <-
#  predict(model, type = "response", newdata = chunk) %>% 
#   as_tibble() %>% 
#   bind_cols(chunk) 
# pred2 <-
#   predict(model2, type = "response", newdata = chunk) %>% 
#   as_tibble() %>% 
#   bind_cols(chunk) 
# chunk <- chunk %>% 
#   mutate(agef = as.factor(age))
# pred3 <-
#   predict(model3, type = "response", newdata = chunk) %>% 
#   as_tibble() %>% 
#   bind_cols(chunk) 

# pred2 %>% 
#   filter(age == 85) %>% 
#   ggplot(aes(x = date, y = value)) +
#   geom_line(mapping=aes(x=date,y=value), color = "blue") +
#   geom_line(mapping=aes(x=date,y=fit), data = pred%>% 
#               filter(age == 85), color = "red") +
#   geom_line(mapping=aes(x=date,y=value), data = pred3%>% 
#               filter(age == 85), color = "orange") 

# pred3 %>% 
#   filter(year(date) == 2010) %>% 
#   group_by(age) %>% 
#   mutate(value = value / mean(value)) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = week,y=value,color=age,group=age))+
#   geom_line()
# 
# pred %>% 
#   ggplot(aes(x = date, y = age, fill = log(fit/exposure))) +
#   geom_tile()
# 
# pred3 %>% 
#   filter(week == 40) %>% 
#   ggplot(aes(x=age, y = value / exposure, group = date)) +
#   geom_line() + 
#   scale_y_log10()
# 
# pred %>% 
#   filter(age == 85) %>% 
#   ggplot(aes(x = date, y = deaths)) +
#   geom_point(size=.5) +
#   geom_line(aes(x = date, y = fit), color = "blue",size=2) 
# 
# pois_ci <- function(fit, n = 200, level = .95){
#   alpha <- 1 - level
#   lower <- alpha / 2
#   upper <- 1 - lower
#   replicate(500,rpois(n = n, lambda = fit) %>% 
#               quantile(probs = c(lower,upper))) %>% 
#     rowMeans()
# }

# CI <- pred$fit %>% 
#   sapply(pois_ci) %>% 
#   t() %>% 
#   as_tibble()
# dim(CI)
# dim(pred)
# 
# pred <- bind_cols(pred, CI)
# head(pred) %>% View()
# 
# pred %>% 
#   ggplot(aes(x = date, ymax = `97.5%`, ymin = `2.5%`)) +
#   geom_point(aes(x=date,y=deaths),size=.5) +
#   geom_ribbon(fill = "blue",alpha = .1) +
#   geom_line(aes(x = date, y = fit), size = 2, color = "blue")+
#   facet_wrap(~age, scale = "free")
# 
# install.packages("equatiomatic")
# library(equatiomatic)
# extract_eq(model)

# test <- simul_intvals(model,
#               model_type = "gam",
#               nsim = 1000,
#               db = chunk,
#               p = .05)

# M %>% 
#   group_by(prov, isoweek, isoyear, date) %>% 
#   summarize(deaths = sum(deaths),.groups = "drop") %>% 
#   ggplot(aes(x = date, y = deaths)) +
#   geom_line() +
#   facet_wrap(~prov, scales = "free")

source("R/00_install_load.R")
D <- read_csv("Data/intermediate/Def_Eusk_analisis.csv",show_col_types = FALSE)
E <- read_csv("Data/intermediate/Expos_Eusk_analisis.csv",show_col_types = FALSE) %>% 
  mutate(age = ifelse(age > 90, 90, age)) %>% 
  group_by(prov, sex, isoyear, isoweek, age) %>% 
  summarize(personweeks = sum(personweeks),
            .groups = "drop")

M <-
  inner_join(D,E,by = c("prov", "isoyear", "isoweek", "age", "sex")) %>% 
  mutate(isoweek = sprintf("%02d",isoweek)) %>% 
  arrange(prov, sex, isoyear, isoweek, age) %>% 
  mutate(m = deaths / personweeks,
         `ISOweek` = paste0(isoyear, "-W",isoweek,"-4"),
         date = ISOweek::ISOweek2date(`ISOweek`),
         tfrac = decimal_date(date),
         year = year(date),
         frac = tfrac - year) %>% 
  rename(pop = personweeks) %>% 
  select(-ISOweek)

M %>% write_csv(file = "Data/intermediate/tasas_provincias_eus.csv")


# M %>% 
#   filter(prov == "Araba", isoyear == 2020, isoweek %in% c("01","02","03","04"),
#          age >= 80) %>% pull(deaths) %>% sum()
# M %>% 
#   filter(age >= 65,
#          year >= 2016) %>% 
#   group_by(year) %>% 
#   summarize(def = sum(deaths))

# library(ISOweek)
# lower_year <- 2002
# 
# newdata <-  expand.grid(year = lower_year:2021, isoweek = 1:52)
# M %>% 
#   mutate(age = age - age %% 20) %>% 
#   group_by(prov, sex, isoyear, isoweek, age) %>% 
#   summarize(deaths = sum(deaths),
#             personweeks = sum(personweeks),
#             .groups = "drop") %>%
#   filter(prov == "48 Bizkaia", 
#          sex == "Hombres", 
#          age == 80,
#          between(isoyear, lower_year, 2019)) %>% 
#   mutate(m = deaths / personweeks,
#          date = ISOweek::ISOweek2date(paste0(isoyear, "-W",sprintf("%02d",isoweek),"-4")),
#          time = decimal_date(date),
#          year = year(date),
#          tfrac = time - year,
#          beta = year) %>% 
#   group_by(isoweek) %>% 
#   mutate(alpha = mean(m)) %>% 
#   lm(m ~ 1 + factor(isoweek) + year, data = .) %>% 
#   predict(newdata = newdata) %>% 
#   bind_cols(newdata) %>% 
#   arrange(year, isoweek) %>% 
#   rename(mhat = `...1`) %>% 
#   ggplot(aes(x=isoweek, y = mhat, group = year, color = year)) +
#   geom_line()
# 
# 
# lower_year <- 2005
# newdata <-  expand.grid(year = lower_year:2021, tfrac = (1:52 - .5) / 52)
# library(splines)
# 
# 
# M80i <-
#   M %>% 
#   mutate(age = age - age %% 20) %>% 
#   group_by(prov, sex, isoyear, isoweek, age) %>% 
#   summarize(deaths = sum(deaths),
#             personweeks = sum(personweeks),
#             .groups = "drop") %>%
#   filter(prov == "48 Bizkaia", 
#          sex == "Hombres", 
#          age == 80,
#          between(isoyear, lower_year, 2019)) %>% 
#   mutate(m = deaths / personweeks,
#          date = ISOweek::ISOweek2date(paste0(isoyear, "-W",sprintf("%02d",isoweek),"-4")),
#          time = decimal_date(date),
#          year = year(date),
#          tfrac = time - year) 
# 
# to_join <- M %>% 
#   mutate(age = age - age %% 20) %>% 
#   group_by(prov, sex, isoyear, isoweek, age) %>% 
#   summarize(deaths = sum(deaths),
#             personweeks = sum(personweeks),
#             .groups = "drop") %>%
#   filter(prov == "48 Bizkaia", 
#          sex == "Hombres", 
#          age == 80) %>% 
#   mutate(m = deaths / personweeks)
#   
# 
# M80i %>% 
#   lm(log(m) ~ 1 + ns(tfrac, knots = seq(.05,.95,length = 9)) + year, weights = log(personweeks),data = .) %>% 
#   predict(newdata = newdata) %>% 
#   bind_cols(newdata) %>% 
#   arrange(year, tfrac) %>% 
#   rename(mhat = `...1`, isoyear = year) %>% 
#   mutate(isoweek = (tfrac * 52 + .5) %>% as.integer(),
#          mhat = exp(mhat)) %>% 
#   select(-tfrac) %>% 
#   left_join(to_join, by = c("isoyear","isoweek")) %>% 
#   mutate(resid = m - mhat,
#          expected = mhat * personweeks) %>% 
#   ggplot(aes(x = isoweek, y = mhat, color = isoyear, group = isoyear)) +
#   geom_line() +
#   labs(title = paste0("periodo ajuste de"))
# 
# 
# 
#   ggplot(aes(x=tfrac, y = mhat, group = year, color = year)) +
#   geom_line()
# 
# M50p <- 
#   M %>% 
#   filter(age >= 50,
#          prov == "48 Bizkaia", 
#          sex == "Hombres") %>% 
#   mutate(date = ISOweek::ISOweek2date(paste0(isoyear, "-W",sprintf("%02d",isoweek),"-4")),
#          time = decimal_date(date),
#          .year = year(date),
#          tfrac = time - .year)
# 
# newdata <- M50p %>% 
#   select(tfrac, .year, age)
# lower_year <- 2012
# 
# preds <- list()
# for (lower_year in 2002:2016){
#   yrc <-as.character(lower_year)
#   preds[[yrc]] <-
#     M50p %>% 
#     filter(between(.year,lower_year,2019)) %>% 
#     glm(deaths ~ 1 + 
#           ns(tfrac, knots = seq(.05,.95,length = 8)) +
#           ns(age, knots = c(55,85)) +
#           .year, 
#           #ns(age, knots = seq(55,85,by=15)), 
#           weights = personweeks,
#         family = quasipoisson(link = "log"),
#         data = .) %>% 
#     predict(object=.,newdata = newdata, se.fit = TRUE, type = "response") %>% 
#     bind_cols(newdata) %>% 
#     left_join(M50p, by = c("tfrac",".year","age")) %>% 
#     mutate(fit_lower_year = lower_year,
#            mhat = fit / personweeks,
#            m_resid = m - mhat,
#            excess = deaths - fit,
#            excess_lower = deaths - (fit + 2 * se.fit),
#            excess_upper = deaths - (fit - 2 * se.fit))
# }
# preds <- bind_rows(preds)
# preds %>% write_csv("Data/quasipoisson_2d_predictions.csv")
# 
# my_pred <- 
#   M50p %>% 
#   filter(between(.year,lower_year,2019)) %>% 
#   glm(deaths ~ 1 + 
#        ns(tfrac, knots = seq(.05,.95,length = 8)) +
#         ns(age, knots = c(55,85)) +
#        .year + 
#        #ns(age, knots = seq(55,85,by=15)), 
#       weights = personweeks,
#       family = quasipoisson(link = "log"),
#       data = .) %>% 
#   predict(newdata = newdata, se.fit = TRUE, type = "response") %>% 
#   bind_cols(newdata) %>% 
#   left_join(M50p, by = c("tfrac",".year","age"))
# 
# my_pred %>% 
#   group_by(.year,age) %>% 
#   summarize(deaths = sum(fit), 
#             exposure = sum(personweeks),
#             .groups = "drop") %>% 
#   mutate(mhat = deaths / exposure) %>% 
#   ggplot(aes(x = age, y= mhat, color = .year, group = .year)) +
#   geom_line() +
#   scale_y_log10()
# 
# my_pred %>% 
#  filter(isoweek == 5) %>% 
#   mutate(mhat = fit / personweeks) %>% 
#   ggplot(aes(x = age, y= mhat, color = .year, group = .year)) +
#   geom_line() +
#   scale_y_log10()
# 
# 
# preds %>%
#   filter(fit_lower_year == 2010) %>% 
#   mutate(age = ifelse(age == 90, 85, age)) %>% 
#   group_by(.year, isoweek, age) %>% 
#   summarize(fit = sum(fit),
#             personweeks = sum(personweeks),
#             .groups = "drop") %>% 
#   mutate(mhat = fit / personweeks) %>% 
#   filter(age < 90) %>% 
#   ggplot(aes(x = isoweek, y = mhat, color = .year, group = .year)) +
#   geom_line() +
#   facet_wrap(~age, scales = "free") +
#   scale_color_continuous_sequential(palette = "ag_Sunset")
# hcl_palettes(plot = TRUE)
# 
# 
# library(colorspace)
# my_pred %>% 
#   mutate(time = tfrac + .year,
#          mhat = fit / personweeks) %>% 
#   ggplot(aes(x = time, y = age, fill = mhat)) +
#   geom_tile() +
#   scale_fill_continuous_sequential(palette = "viridis",trans = "log",rev=FALSE)
# # ----------------------------------------------------
# # M10 <- 
# # M %>% 
# #   mutate(age = age -age %% 20) %>% 
# #   group_by(prov, isoyear, isoweek, sex, age) %>% 
# #   summarize(deaths = sum(deaths), personweeks = sum(personweeks),
# #             .groups = "drop") %>% 
# #   filter(age >= 40, 
# #          sex == "Hombres") %>% 
# #   mutate(m = deaths / personweeks) 
# # M10 %>%   
# #   filter(isoyear >= 2019) %>% 
# # ggplot(aes(x = isoweek, y = m, color = as.factor(isoyear), group = interaction(sex, isoyear))) +
# #   geom_line(alpha = .5) +
# #   facet_grid(age~prov, scales= "free")  
# # 
# # ME <- 
# # M %>% 
# #   mutate(age = age -age %% 20) %>% 
# #   group_by(prov, isoyear, isoweek, sex, age) %>% 
# #   summarize(deaths = sum(deaths), personweeks = sum(personweeks),
# #             .groups = "drop") %>% 
# #   filter(age >= 40, 
# #          sex == "Hombres") %>% 
# #   mutate(m = deaths / personweeks) %>% 
# #   group_by(isoyear, isoweek, sex, age) %>% 
# #   summarize(deaths = sum(deaths),
# #             personweeks = sum(personweeks),
# #             .groups = "drop") %>% 
# #   mutate(m = deaths / personweeks)
# # ME %>% 
# #   ggplot(aes(x = isoweek, y = m, color = isoyear, group = interaction(sex, isoyear))) +
# #   geom_line(alpha = .5) +
# #   facet_wrap(~age, scales= "free")  

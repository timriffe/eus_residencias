
if (!"pacman" %in% rownames(installed.packages())){
  install.packages("pacman")
}
library(pacman)
CRAN_packages <- c("tidyverse","readr","readxl","mgcv", "INLA","lubridate","cowplot")
if(!sum(!p_isinstalled(CRAN_packages))==0) {
  p_install(
    package = CRAN_packages[!p_isinstalled(CRAN_packages)], 
    character.only = TRUE
  )
}
p_load(CRAN_packages, character.only = TRUE)

linear_interp <- function(chunk){
  all_the_dates <- seq(ymd("2002-01-01"),ymd("2021-12-31"),by="day")
  # Thursdays are isoweek midpoints, I guess.
  Thursdays     <- all_the_dates[weekdays(all_the_dates) == "Thursday"] %>% decimal_date()
  
  
  iso_week_centroids <-
    
    approx(x = chunk$time,
           y = chunk$pob,
           xout = Thursdays,
           method = "linear",
           rule = 2) %>% 
    as_tibble() %>% 
    rename(time = x, pob = y) %>% 
    mutate(isoweek = date_decimal(time) %>% isoweek(),
           isoyear = date_decimal(time) %>% isoyear())
}

adjust_month_exposure <- function(pop, month, year){
  days_in_year <- 365 + lubridate::leap_year(dmy(paste0("01.01.", year)))
  month_days  <- lubridate::days_in_month(dmy(paste("01", month, year, sep = ".")))
  pop * month_days / days_in_year
}

calc_month_midpoint_tfrac <- function(month, year){
  month_days   <- lubridate::days_in_month(dmy(paste("01", month, year, sep = ".")))
  mid_days     <- round(month_days / 2)
  mid_date     <- dmy(paste(mid_days, month, year, sep="."))
  decimal_date(mid_date)
}



fit_baseline <- function(chunk,k=3){
  gam(deaths ~ tfrac + #ti(t, bs = 'ps') +
        # ti(frac, bs = 'cp',np = FALSE) +
        ti(frac, k=k, bs = 'cp', np = FALSE) +
        ti(age, bs = 'ps', np = FALSE) +
        offset(log(exposure)),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>% 
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit)
}

# This keeps a linear secular trend,
# and simplifies the cyclical component
# meant to be fit independently for each
# age group, sex, residency, and geography
fit_baseline_imserso <- function(chunk){
  gam(deaths ~ tfrac + ti(frac, k=3, bs = 'cp', np = FALSE) +
        #ti(tfrac, bs = 'ps') + # optional
        #tfrac +
        offset(log(exposure)),
      #xt = list(max.knots=2,seed=2),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>%   # try using method = "REML"
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit) %>% 
    mutate(baseline = baseline %>% unname %>% c,
           se.fit = se.fit %>% unname %>% c)
}

# linear time trend, age indep, cyclical is fit
fit_baseline_age_indep <- function(chunk){
  gam(deaths ~ tfrac + 
        #ti(t, bs = 'ps') +
        ti(frac, bs = 'cp',np = FALSE) +
        offset(log(exposure)),
      data = chunk,
      weights = ws,
      family = quasipoisson(link = "log"),
      na.action = na.omit) %>% 
    predict(., type = "response", newdata = chunk, se.fit = TRUE) %>% 
    as_tibble() %>% 
    bind_cols(chunk) %>% 
    rename(baseline = fit)
}

fit_baseline_dip_saturated <- function(chunk, wt){
  gam(def ~ tfrac +
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


# gam(deaths ~ tfrac +
#       ti(frac, k = 3, bs = 'cp', np = FALSE) +
#       ti(age, bs = 'ps', np = FALSE) +
#       ti(age, frac, np = FALSE),
#       offset(log(exposure)),
#     data = chunk,
#     weights = ws, # 1s. 0s exclude weeks
#     family = quasipoisson(link = "log"),
#     na.action = na.omit)
# 



library(nasapower)
library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)

# Marco zero de Santos #####
#lat <- -23.934167
#long <- -46.328611

# Marco zero de SP #####
#lat <- 	-23.5475
#long <- 	-46.63611

# Coordenadas do RJ #####
lat <- -22.90642
long <- -43.18223


# Getting climatic information from NASA #####

# sem epi 6 de 2012 começando o dia 5 de fev 2012
# sem epi 51 de 2021 finalizando o dia 25 de dez de 2021

daily_single_ag <- get_power(
  community = "ag",
  lonlat = c(long, lat),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2001-01-01", "2022-12-31"),
  temporal_api = "hourly"
)

daily_single_ag <- daily_single_ag %>% group_by(YEAR, MO, DY) %>% 
  summarise(t_max = max(T2M),
            t_min = min(T2M),
            t_avg = mean(T2M),
            rh_max = max(RH2M),
            rh_min = min(RH2M),
            rh_avg = mean(RH2M),
            p_total = sum(PRECTOTCORR))

daily_single_ag <- daily_single_ag %>% ungroup()

daily_single_ag <- daily_single_ag %>% 
  mutate(YYYYMMDD = paste0(YEAR,'-',MO,'-',DY)) %>% 
  mutate(YYYYMMDD = as.Date(YYYYMMDD))

# Cleaning this and adding the epidemiological week #####
daily_single_ag <- daily_single_ag %>% mutate(EPI_WEEK = epiweek(YYYYMMDD),
                                              EPI_YEAR = epiyear(YYYYMMDD)) 
daily_single_ag <- daily_single_ag %>% select(c(EPI_WEEK,EPI_YEAR, rh_max, rh_min, rh_avg,
                                                t_max, t_min, t_avg, p_total))


# Calculating weekly avg, max, min, etc of climatic variables ######

daily_single_ag <- daily_single_ag %>% group_by(EPI_WEEK, EPI_YEAR) %>% summarise(
  t_max = mean(t_max),
  t_min = mean(t_min),
  t_avg = mean(t_avg),
  rh_max = mean(rh_max),
  rh_min = mean(rh_min),
  rh_avg = mean(rh_avg),
  p_total = sum(p_total)
)

daily_single_ag <- daily_single_ag %>% mutate(tplot = EPI_YEAR + (EPI_WEEK/52))
daily_single_ag <- daily_single_ag %>% arrange(tplot)

save(daily_single_ag, file = 'RJ/climate_RJ.RData')

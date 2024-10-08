library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)

# Loading data from all sources ###########
load('RJ/cases_RJ.RData') # df_santos
load('RJ/climate_RJ.RData') # daily_single_ag

# Putting the whole dataset together ############
df_total <- daily_single_ag
df_total <- df_total %>% left_join(df_santos, by = join_by(EPI_WEEK, EPI_YEAR))
df_total <- df_total %>% select(!c(tplot.x,tplot.y))
df_total[is.na(df_total)] <- 0
rm(daily_single_ag, df_santos)
df_total <- df_total %>% mutate(tplot = EPI_YEAR + (EPI_WEEK/52))

# Reordering 
df_total <- df_total %>% select(EPI_WEEK, EPI_YEAR, tplot, t_max, t_min, t_avg, rh_max, rh_min, rh_avg, p_total, 
                                CASES, LAMBDA, LAMBDA_2)

save(df_total, file = 'RJ/dataset_RJ.RData')


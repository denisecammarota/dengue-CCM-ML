library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(mem)

# Loading data ##############################################
load('RJ/dataset_RJ.RData')

# Using CCM results ##############################################

df_total_aux <- df_total
df_total_aux <- df_total_aux %>% ungroup() %>% mutate(CASES = lead(CASES, 15),
                                                      EPI_WEEK = lead(EPI_WEEK, 15),
                                                      EPI_YEAR = lead(EPI_YEAR, 15),
                                                      tplot = lead(tplot, 15),
                                                      rh_max = lead(rh_max, 1),
                                                      rh_min = lead(rh_min, 3),
                                                      t_min = lead(t_min, 6),
                                                      t_max = lead(t_max, 5),
                                                      LAMBDA = lead(LAMBDA, 15))

df_total_aux <- df_total_aux %>%
  mutate(rh_max_lag = rh_max,
         rh_min_lag = rh_min,
         t_min_lag = t_min,
         t_max_lag = t_max, 
         LAMBDA_lag = LAMBDA,
         p_total_lag = p_total) %>% 
  select(EPI_YEAR, EPI_WEEK, tplot, CASES, rh_max_lag, rh_min_lag,
         t_min_lag, t_max_lag, LAMBDA_lag, p_total_lag)

# Calculating with size of the population ################################
pop <- read.csv2('RJ/pop_RJ.csv')
df_total_aux <- df_total_aux %>% left_join(pop, by = join_by(EPI_YEAR == Ano))
df_total_aux <- df_total_aux %>% mutate(CASES_100K = (CASES/Pop_residente)*(10**5))
df_total_aux <- df_total_aux %>% select(!Pop_residente)
df_total_aux <- df_total_aux %>% select(!CASES)
df_total_aux <- df_total_aux %>% drop_na()
rm(pop, df_total)

# Calculating with the epidemic thresholds #################################
thr_pre <- 19.14
thr_high <- 52.54

df_total_aux <- df_total_aux %>% mutate(label =
  case_when(CASES_100K <= thr_pre ~ 'Pre',
            CASES_100K > thr_pre & CASES_100K <= thr_high ~ 'Epi',
            CASES_100K > thr_high ~ 'High')
)

# Saving dataset for ML ##################################################
save(df_total_aux, file = 'RJ/dataset_ML_RJ.RData')
write_csv(df_total_aux, 'RJ/dataset_ML_RJ.csv')





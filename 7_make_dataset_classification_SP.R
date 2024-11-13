library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(mem)

# Loading data ##############################################
load('SP/dataset_SP.RData')

# Using CCM results ##############################################

df_total_aux <- df_total
df_total_aux <- df_total_aux %>% ungroup() %>% mutate(CASES = lead(CASES, 19),
                                                      EPI_WEEK = lead(EPI_WEEK, 19),
                                                      EPI_YEAR = lead(EPI_YEAR, 19),
                                                      tplot = lead(tplot, 19),
                                                      t_max = lead(t_max, 7),
                                                      t_min = lead(t_min, 2),
                                                      rh_max = lead(rh_max, 10),
                                                      rh_min = lead(rh_min, 0),
                                                      p_total = lead(p_total, 19),
                                                      LAMBDA = lead(LAMBDA, 19))

df_total_aux <- df_total_aux %>%
  mutate(rh_max_lag = rh_max,
         rh_min_lag = rh_min,
         t_min_lag = t_min,
         t_max_lag = t_max, 
         p_total_lag = p_total,
         LAMBDA_lag = LAMBDA) %>% 
  select(EPI_YEAR, EPI_WEEK, tplot, CASES, rh_max_lag, rh_min_lag,
         t_min_lag, t_max_lag, p_total_lag, LAMBDA_lag)

# Calculating with size of the population ################################
pop <- read.csv2('SP/pop_SP.csv')
df_total_aux <- df_total_aux %>% left_join(pop, by = join_by(EPI_YEAR == Ano))
df_total_aux <- df_total_aux %>% mutate(CASES_100K = (CASES/Pop_residente)*(10**5))
df_total_aux <- df_total_aux %>% select(!Pop_residente)
df_total_aux <- df_total_aux %>% select(!CASES)
df_total_aux <- df_total_aux %>% drop_na()
rm(pop, df_total)

# Calculating with the epidemic thresholds #################################
thr_pre <- 5.11
thr_high <- 16.60

df_total_aux <- df_total_aux %>% mutate(label =
  case_when(CASES_100K <= thr_pre ~ 'Pre',
            CASES_100K > thr_pre & CASES_100K <= thr_high ~ 'Epi',
            CASES_100K > thr_high ~ 'High')
)

# Saving dataset for ML ##################################################
save(df_total_aux, file = 'SP/dataset_ML_SP.RData')
write_csv(df_total_aux, 'SP/dataset_ML_SP.csv')





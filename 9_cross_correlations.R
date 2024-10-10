library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(rEDM)
library(reshape2)
library(viridis)

# Comparison with simple cross-correlations - SP ##################
load('SP/dataset_SP.RData')

a <- ccf(df_total$t_max, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$t_min, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$rh_max, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$rh_min, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$p_total, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$LAMBDA, df_total$CASES)
a$lag[which.max(a$acf)]

# Comparison with simple cross-correlations - RJ ##################
load('RJ/dataset_RJ.RData')

a <- ccf(df_total$t_max, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$t_min, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$rh_max, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$rh_min, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$p_total, df_total$CASES)
a$lag[which.max(a$acf)]

a <- ccf(df_total$LAMBDA, df_total$CASES)
a$lag[which.max(a$acf)]

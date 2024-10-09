library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(rEDM)
library(reshape2)
library(viridis)

# Loading full dataset ########
load('RJ/dataset_RJ.RData')

# Mosquito and climate and cases #################################
# Adding here the lambda factor, susceptibles index

## Calculating theta for interaction strengths (MOSQUITO-CLIMATE) ##########
t_tmin <- 9 # ok 
t_tmax <- 10 # ok
t_rhmin <- 12 # ok 
t_rhmax <- 14 # ok
t_lambda <- 0 # ok 
t_ptotal <- 15 # ok 

df_total_aux <- df_total
df_total_aux <- df_total_aux %>% ungroup() %>% mutate(CASES = lead(CASES,15),
                                                      t_min = lead(t_min,6),
                                                      t_max = lead(t_max,5),
                                                      rh_max = lead(rh_max,1),
                                                      rh_min = lead(rh_min,3),
                                                      #p_total = lead(p_total, 15-15),
                                                      LAMBDA = lead(LAMBDA, 15))

df_total_aux <- df_total_aux %>% drop_na()
df_total_aux_2 <- df_total_aux

df_total_aux$t_max <- (df_total_aux$t_max - mean(df_total_aux$t_max))/(sd(df_total_aux$t_max))
df_total_aux$t_min <- (df_total_aux$t_min - mean(df_total_aux$t_min))/(sd(df_total_aux$t_min))
df_total_aux$rh_max <- (df_total_aux$rh_max - mean(df_total_aux$rh_max))/(sd(df_total_aux$rh_max))
df_total_aux$rh_min <- (df_total_aux$rh_min - mean(df_total_aux$rh_min))/(sd(df_total_aux$rh_min))
df_total_aux$CASES <- (df_total_aux$CASES - mean(df_total_aux$CASES))/(sd(df_total_aux$CASES))
df_total_aux$LAMBDA <- (df_total_aux$LAMBDA - mean(df_total_aux$LAMBDA))/(sd(df_total_aux$LAMBDA))
df_total_aux$p_total <- (df_total_aux$p_total - mean(df_total_aux$p_total))/(sd(df_total_aux$p_total))


rho_theta <- PredictNonlinear(dataFrame = df_total_aux, lib = "1 250", pred = "251 500",
                              target = 'CASES', columns = 'CASES', E = 2)

theta_ideal <- 0.75

smap <- SMap(dataFrame = df_total_aux, 
             lib = "1 501", pred = "1 501", E = 2,
             theta = theta_ideal, 
             columns = "t_max t_min rh_max rh_min p_total LAMBDA", 
             target = "CASES",
             Tp = 0,
             embedded = TRUE)
smap


coeffs_tmax <- smap$coefficients$`∂CASES/∂t_max`
coeffs_tmin <- smap$coefficients$`∂CASES/∂t_min`
coeffs_rhmax <-  smap$coefficients$`∂CASES/∂rh_max`
coeffs_rhmin <-  smap$coefficients$`∂CASES/∂rh_min`
coeffs_ptotal <-  smap$coefficients$`∂CASES/∂p_total`
coeffs_lambda <-  smap$coefficients$`∂CASES/∂LAMBDA`

# maximum temperature
a <- coeffs_tmax
b <- df_total_aux_2$t_max
c <- df_total_aux_2$EPI_WEEK
df_fin <- data.frame(int_coeff = a, t_max = b, week = df_total_aux_2$EPI_WEEK)
ggplot(df_fin, aes(x = t_max, y = int_coeff, color = week)) + geom_point() + theme_bw()  + 
  scale_color_viridis()

# minimum temperature
a <- coeffs_tmin
b <- df_total_aux_2$t_min
c <- df_total_aux_2$EPI_WEEK
df_fin <- data.frame(int_coeff = a, t_min = b, week = df_total_aux_2$EPI_WEEK)
ggplot(df_fin, aes(x = t_min, y = int_coeff, color = week)) + geom_point() + theme_bw()  + 
  scale_color_viridis()

# max relative humidity
a <- coeffs_rhmax
b <- df_total_aux_2$rh_max
c <- df_total_aux_2$EPI_WEEK
df_fin <- data.frame(int_coeff = a, rh_max = b, week = df_total_aux$EPI_WEEK)
ggplot(df_fin, aes(x = rh_max, y = int_coeff, color = week)) + geom_point() + theme_bw()  + 
  scale_color_viridis()


# min relative humidity
a <- coeffs_rhmin
b <- df_total_aux_2$rh_min
c <- df_total_aux_2$EPI_WEEK
df_fin <- data.frame(int_coeff = a, rh_min = b, week = df_total_aux$EPI_WEEK)
ggplot(df_fin, aes(x = rh_min, y = int_coeff, color = week)) + geom_point() + theme_bw()  + 
  scale_color_viridis()

# lambda 
a <- coeffs_lambda
b <- df_total_aux_2$LAMBDA
c <- df_total_aux_2$EPI_WEEK
df_fin <- data.frame(int_coeff = a, lambda = b, week = df_total_aux$EPI_WEEK)
ggplot(df_fin, aes(x = lambda, y = int_coeff, color = week)) + geom_point() + theme_bw()  + 
  scale_color_viridis()







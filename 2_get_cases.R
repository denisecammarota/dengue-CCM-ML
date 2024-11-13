library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(zoo)

# Defining lambda function ########
lambda <- function(x){
  y <- lead(x, 1)
  x <- x[1:(length(x)-1)]
  y <- y[!(is.na(y))]
  coeflm <- as.numeric(lm(y ~ x)$coefficients[2])
  return(coeflm)
}

# Loading data from Santos ######
df_santos <- read_csv2("RJ/330455_total_new.csv")
df_santos <- df_santos %>% mutate(EPI_WEEK = as.numeric(WEEK_PRI),
                                  EPI_YEAR = as.numeric(YEAR_PRI)) %>% 
  select(EPI_WEEK, EPI_YEAR, CASES)
df_santos <- df_santos %>% mutate(tplot = EPI_YEAR + (EPI_WEEK/52))
df_santos <- df_santos %>% select(EPI_YEAR, EPI_WEEK, tplot, CASES)
df_santos <- df_santos %>% arrange(tplot)
df_santos 

# Creating lagged data of cummulated cases #############
ws1 <- 12
ws2 <- 16
  
df_santos['LAMBDA'] <- rollapply(df_santos['CASES'], width = ws1, FUN = function(w) lambda(w), align = "right", by.column = FALSE, fill = NA)
df_santos['LAMBDA_2'] <- rollapply(df_santos['CASES'], width = ws2, FUN = function(w) lambda(w), align = "right", by.column = FALSE, fill = NA)


# Filtrating the weeks of interest #########
df_santos <- df_santos %>% filter(EPI_YEAR <= (2022))

save(df_santos, file = 'RJ/cases_RJ.RData')


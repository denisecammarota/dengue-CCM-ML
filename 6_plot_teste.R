library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(mem)

# Loading data from Santos ################################
df_santos <- read_csv2("RJ/330455_total_new.csv")
df_santos <- df_santos %>% mutate(EPI_WEEK = as.numeric(WEEK_PRI),
                                  EPI_YEAR = as.numeric(YEAR_PRI)) %>% 
  select(EPI_WEEK, EPI_YEAR, CASES)
df_santos <- df_santos %>% mutate(tplot = EPI_YEAR + (EPI_WEEK/52))
df_santos <- df_santos %>% select(EPI_YEAR, EPI_WEEK, tplot, CASES)
df_santos <- df_santos %>% arrange(tplot)
df_santos

# Plotting the whole series ##############################
ggplot(df_santos, aes(x = tplot, y = CASES)) + geom_line() + geom_point()


# Plotting all seasons ######################################
p <- ggplot()
for(year in unique(df_santos$EPI_YEAR)){
  df_aux <- df_santos %>% filter(EPI_YEAR == year)
  p <- p + geom_point(data = df_aux, aes(x = EPI_WEEK, y = CASES)) + 
    geom_line(data = df_aux, aes(x = EPI_WEEK, y = CASES))

}

# Plotting while normalizing ###############################
p <- ggplot()
for(year in unique(df_santos$EPI_YEAR)){
  df_aux <- df_santos %>% filter(EPI_YEAR == year)
  norm_fact <- max(df_aux)
  p <- p + geom_point(data = df_aux, aes(x = EPI_WEEK, y = CASES/norm_fact)) + 
    geom_line(data = df_aux, aes(x = EPI_WEEK, y = CASES/norm_fact))
  
}
p

# Plotting year by year ####################################
for(year in unique(df_santos$EPI_YEAR)){
  p <- ggplot()
  df_aux <- df_santos %>% filter(EPI_YEAR == year)
  norm_fact <- max(df_aux)
  p <- p + geom_point(data = df_aux, aes(x = EPI_WEEK, y = CASES)) + 
    geom_line(data = df_aux, aes(x = EPI_WEEK, y = CASES)) + ggtitle(year)
  print(p)  
}


# Filtering some years only ###################################
df_santos <- df_santos %>% filter(EPI_YEAR >= 2001) %>% filter(EPI_YEAR <= 2021)
df_santos <- df_santos %>% select(EPI_YEAR, EPI_WEEK, CASES)

# Calculating weekly rate of cases per 100k inhabitants ############################
pop <- read.csv2('RJ/pop_RJ.csv')
df_santos <- df_santos %>% left_join(pop, by = join_by(EPI_YEAR == Ano))
df_santos['CASES_100K'] <- (df_santos['CASES']/df_santos['Pop_residente'])*(10**5)
df_santos_serie <- df_santos 

# Putting this into form for memmodel #########################################
df_santos <- df_santos %>% mutate(season = ifelse(EPI_WEEK <= 40, EPI_YEAR, EPI_YEAR - 1))
#df_santos <- df_santos %>% filter(EPI_WEEK <= 40)
df_santos <- df_santos %>% select(season, EPI_WEEK, CASES_100K)
df_santos <- df_santos %>% pivot_wider(names_from = season, values_from = CASES_100K)
df_santos[is.na(df_santos)] <- 0  
rm(df_aux,p,pop)
df_santos <- df_santos %>% select(!c(EPI_WEEK))

# Passing this through the memmodel ##########################################
dengue.memmodel <- memmodel(df_santos, i.season = 20)
summary(dengue.memmodel)
plot(dengue.memmodel)

# Plotting the whole thing ####################################################
#thr_preepi <- 5.11
#thr_medium <- 1.00
#thr_high <- 16.60
#thr_vhigh <- 57.54

thr_preepi <- 17.42
thr_medium <- 3.30
thr_high <- 53.52
thr_vhigh <- 183.44

df_santos_serie <- df_santos_serie %>% mutate(tplot = EPI_YEAR + EPI_WEEK/52)
ggplot(df_santos_serie, aes(x = tplot, y = CASES_100K)) + geom_line() + 
  geom_point() + geom_hline(yintercept = thr_preepi, color = 'red') +
  geom_hline(yintercept = thr_high, color = 'blue') +
  theme_bw() +
  xlab('Year') +
  ylab('Cases per 100.000 inhabitants') +
  annotate("text", x = 2019-0.2, y = thr_preepi + 10, label="Pre-Epidemic", size = 3, color = 'red') +
  annotate("text", x = 2019-0.2, y = thr_high + 10, label="High", size = 3, color = 'blue')





library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(rEDM)
library(reshape2)

# Loading full dataset ########
load('RJ/dataset_RJ.RData')


# Calculating the optimal embedding dimension #######
rho_E_CASES <- EmbedDimension(dataFrame = df_total, lib = "1 361", pred = "362 516", columns = "CASES",
                        target = "CASES", maxE = 15)

# Doing CCM #########################################

## CASES AND CLIMATE ####

E_ideal <- 2
vars_clim <- c('t_max', 't_min','t_avg','rh_max','rh_min','rh_avg','p_total', 'LAMBDA', 'LAMBDA_2')
#vars_clim <- c('t_max')
#vars_clim <- c('LAMBDA', 'LAMBDA_2')
tps <- seq(-20,20,1)

for(var in vars_clim){
  print(var)
  a_1_mean <- c()
  a_2_mean <- c()
  a_1_std <- c()
  a_2_std <- c()
  for(tp in tps){
    a <- CCM( dataFrame = df_total, E = E_ideal, Tp = tp,
              columns = var, target = 'CASES',
              libSizes = '20 480 10', sample = 10, showPlot = F) # 20/480/10
    a_1_mean <- c(a_1_mean, mean(a[(dim(a)[1]-5):dim(a)[1],2])) # climate on target (mean)
    a_2_mean <- c(a_2_mean, mean(a[(dim(a)[1]-5):dim(a)[1],3])) # target on climate (mean)
  }
  df <- data.frame(tp = tps, target_on_clim = a_1_mean, clim_on_target = a_2_mean)
  df <- melt(df, id = 'tp')
  name_plot <- paste0('CCM on cases and ',var)
  name_plot_save <- paste0('cases_',paste0(var,'_SP.png'))
  p <- ggplot(df, aes(x = tp, y = value, color = variable)) + geom_line() + geom_point() + ggtitle(name_plot)
  ggsave(filename = name_plot_save)
}

# cases and tmax
CCM( dataFrame = df_total, E = E_ideal, Tp = -1,
     columns = 't_max', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and tmin
CCM( dataFrame = df_total, E = E_ideal, Tp = -1,
     columns = 't_min', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and tavg
CCM( dataFrame = df_total, E = E_ideal, Tp = -1,
     columns = 't_avg', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and rhmax
CCM( dataFrame = df_total, E = E_ideal, Tp = -17,
     columns = 'rh_max', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and rhmin
CCM( dataFrame = df_total, E = E_ideal, Tp = -18,
     columns = 'rh_min', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and rhavg
CCM( dataFrame = df_total, E = E_ideal, Tp = -18,
     columns = 'rh_avg', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and ptotal
CCM( dataFrame = df_total, E = E_ideal, Tp = -10,
     columns = 'p_total', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and mosquito
CCM( dataFrame = df_total, E = E_ideal, Tp = -8,
     columns = 'N_MOSQ_ARM', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)


# cases and susceptibles index
CCM( dataFrame = df_total, E = E_ideal, Tp = 0,
     columns = 'LAMBDA', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

# cases and susceptibles index (lag 2 weeks)
CCM( dataFrame = df_total, E = E_ideal, Tp = 0,
     columns = 'LAMBDA_2', target = 'CASES',
     libSizes = '20 480 10', sample = 10, showPlot = T)

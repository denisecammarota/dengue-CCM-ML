library(sf)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)
library(rEDM)
library(reshape2)

# Loading full dataset ########
load('SP/dataset_SP.RData')

# Cases, climate and mosquito ################################

E_ideal <- 7
#vars_clim <- c('t_max','t_min','t_avg','rh_max','rh_min','rh_avg','p_total')
#vars_clim <- c('N_MOSQ_ARM') # e trocar aqui tb TROCAR!!!
vars_clim <- c('t_max')
tps <- seq(-20,0,1)
n_surrogates <- 100

for(var in vars_clim){
  print(var)
  # we generate the surrogates for this variable
  surr_list = SurrogateData(df_total$LAMBDA_2, method = "seasonal", T_period = 52, num_surr = n_surrogates, 
                            alpha = 3) # trocar aqui a variavel TROCAR!!!!
  df_aux <- cbind(df_total,surr_list)
  df_resul <- data.frame(lag = 100, rho = 0, sig_rho = 0)
  for(tp in tps){
    print(tp)
    ccm_original <- 0
    rho_surr <- c()
    a <- CCM( dataFrame = df_aux, E = E_ideal, Tp = tp,
              columns = var, target = 'CASES',
              libSizes = '10 1100 50', sample = 10, showPlot = F)
    ccm_original <- mean(a[(dim(a)[1]-5):dim(a)[1],3]) # the original value for this lag
    surr_col_names <- colnames(df_aux)[13:dim(df_aux)[2]]
    for(surr_col_name in surr_col_names){ # para cada surrogate, fazemos o ccm 
      b <- CCM(dataFrame = df_aux, E = E_ideal, Tp = tp, columns = surr_col_name,
               target = 'CASES', libSizes = "10 1100 50", sample = 1)
      rho_surr <- append(rho_surr, mean(b[(dim(b)[1]-5):dim(b)[1],3]))
      
    }
    pval <- 1 - ecdf(rho_surr)(ccm_original)
    df_resul <- df_resul %>% add_row(lag = tp, rho = ccm_original, sig_rho = pval)
  }
  df_resul <- df_resul %>% mutate(SIG = ifelse(sig_rho < 0.05,T,F))
  df_resul <- df_resul %>% filter(lag != 100)
  p <- ggplot(df_resul, aes(x = lag, y = rho, color = SIG)) + geom_point() + ggtitle(paste0(var, ' on cases'))
  name_plot <- paste0('surr_cases_',paste0(var,'_SP.png'))
  ggsave(filename = name_plot)
}


#rm(list=ls())



################################################################################################
#Here we fit ARMA(p,q)-GARCH(1,1) ~ skewed t for each asset and optimization step.             #
#ARMA (p,q) parameters are automatically chosen via BIC information criteria, with auto.arima. #
#We store fitted sigma, residualds and arma-Garch parameters on lists.                         #
################################################################################################

import("modules")
armaGarch_Est <- module({
#setwd("C:/Users/gusta/Desktop/tccGustavo/tccGustavo")
import("xts") ##dealing with xts (time series) objects
#library(quantmod) 
import("rugarch") ##skewed-t garch estimation
import("forecast") ##auto-arima estimation
  
export("arma_garch_fit_store") ##available to main 

###########functions


arima_est <- function(data){
  ##ARMA estimation
  xx <- forecast::auto.arima(y=data, max.p=2, max.d = 0,    #auto.arima fit, where we select (p,q) with better BIC criteria
                             max.q=2, seasonal=F, stationary = T,  #max (p,q) = 2, assumes series are already stationary (wont dif)
                             ic = c('bic'), allowmean =F, allowdrift = F)  #wont allow mean or drift since we expect mean(returns) = 0
  ordem <- forecast::arimaorder(xx) #getting arma orders 
  ordem <- c(ordem[1], ordem[3])  #getting [p,q]
  return (ordem)
}

garch_est <- function(data, ordem){
  ##GARCH esitmation 
  armagarchspec <- rugarch::ugarchspec(list(model="sGARCH", garchOrder = c(1,1), variance.targeting = T),
                                            mean.model=list(armaOrder=ordem,include.mean = F), 
                                            distribution.model = 'sstd') #creating garch(1,1) with skewed t specification
  garch_fit <- rugarch::ugarchfit(armagarchspec, data = data, solver = "hybrid") #fitting garch for the j asset
  #we use variance.targeting for faster estimation
  return (garch_fit)
}

coefs_vector <- function(ordem, garch_fit){ ###creating an easy-to-deal matrix of coefs
  aux_list<-vector('list', 9)
  if(ordem[1] == 0 && ordem[2] == 0){           
    aux_list[1:4] = 0                           ##from the form (AR1, AR2, MA1, MA2,  GARCH coefs)
    aux_list[5:9] = garch_fit@fit$coef[1:5]  
  }else
    if(ordem[1] == 1 && ordem[2] == 0){    #if p=1 and q=0, (ar1 coef, 0, 0, 0, garch coefs)
      aux_list[2:4]=0
      aux_list[1]=garch_fit@fit$coef[1]
      aux_list[5:9]= garch_fit@fit$coef[2:6]
    } else
      if(ordem[1] == 2 && ordem[2] == 0){  #if p=2, q=0, (ar1 coef, ar2 coef, 0, 0 garch coefs)
        aux_list[3:4]=0
        aux_list[1:2]=garch_fit@fit$coef[1:2]
        aux_list[5:9]= garch_fit@fit$coef[3:7]
      }
  else
    if(ordem[1]==2 && ordem[2]==1){  #if p=2, q=1, (ar1 coef, ar2 coef, ma1 coef, 0 garch coefs)
      aux_list[4]=0
      aux_list[1:3]=garch_fit@fit$coef[1:3]
      aux_list[5:9]= garch_fit@fit$coef[4:8]
    }
  else
    if(ordem[1]==2 && ordem[2] ==2){ #if p=2, q=2, (ar1 coef, ar2 coef, ma1 coef, ma2 coef,  garch coefs)
      aux_list[1:4]=garch_fit@fit$coef[1:4]
      aux_list[5:9]= garch_fit@fit$coef[5:9]
    }
  else
    if(ordem[1]==0 && ordem[2]==1){  #...
      aux_list[1:2]=0
      aux_list[4]=0
      aux_list[3]=garch_fit@fit$coef[1]
      aux_list[5:9]= garch_fit@fit$coef[2:6]
    }
  else
    if(ordem[1]==0 && ordem[2]==2){
      aux_list[1:2]=0
      aux_list[3:4]=garch_fit@fit$coef[1:2]
      aux_list[5:9]= garch_fit@fit$coef[3:7]
    }
  else
    if(ordem[1]==1 && ordem[2] ==2){
      aux_list[2]=0
      aux_list[1]=garch_fit@fit$coef[1]
      aux_list[3:4]=garch_fit@fit$coef[2:3]
      aux_list[5:9]= garch_fit@fit$coef[4:8]
    } else
      if(ordem[1]==1 && ordem[2]==1){
        aux_list[2]=aux_list[4]=0
        aux_list[1]=garch_fit@fit$coef[1]
        aux_list[3]=garch_fit@fit$coef[2]
        aux_list[5:9]=garch_fit@fit$coef[3:7]
      } 
    return (aux_list)
    }

###############

#######ARMA GARCH ESTIMATION


arma_garch_fit_store <- function(returns, per_est){
  residuos<- vector('list', nrow(returns))     #a list of list to store residuals, 
  coeficientes <- vector('list', nrow(returns)) #arma-garch coeficients and sigma from each asset estimation
  sigma <- vector('list', nrow(returns))
  for (i in 1:nrow(returns)){  ##for period 1:4500, we estimate ARMA-GARCH for each asset
    ret_data <- returns[i:per_est+(i-1)] #goes from window_sup-per_est to window_sup  (per_est=1260)
     
    for (j in 2:ncol(returns))   #for period i, fit the model for every j>1 asset, where j=1 is Date column. 
    {
      ordem <- arima_est(ret_data[,j]) ##ARMA estimation
      garch_fit <- garch_est(ret_data[,j], ordem) ##Garch estimation
      ###storing data from the estimations
      aux_list <- coefs_vector(ordem, garch_fit)
      coeficientes[[i]][[j-1]] <- aux_list  ##storing coeficients vector resulting for the j asset
      residuos[[i]][[j-1]] <- garch_fit@fit$residuals ##storing residuals vector for the j asset
      sigma[[i]][[j-1]] <- garch_fit@fit$sigma ##storing sigma vector for the j asset
      cat("\nFit Col/Per: ",j,i)
    }
  }

############storing resulting data in R data files
  saveRDS(coeficientes, file = "coef.Rds")
  saveRDS(residuos, file = "residuos.Rds")
  saveRDS(sigma, file = "sigma.Rds")
}
}) #closing module
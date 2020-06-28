
###################################################################################################################################
###################################################################################################################################
#For each optimization step (1:4550), this block of code generates K=10000 random variates for Clayton, t and Gumbel copula,      #
#given estimated copula parameters and weights. Then, combines the variates in a linear combination of copulas and uses           #
#the quantile of the resulting dependence structure to simulate K=10000 return scenarios from given Arma-Garch and the dependence #
#strucutre above. Finally, given expected returns, it solves the linear program CVaR minimization and stores the vector of optimal# 
#weights.                                                                                                                         #
###################################################################################################################################
###################################################################################################################################

cop_cvar_pf_opt_mod <- module({
  import("copula") ###for generating copula observations
  import("fGarch") ###for using skew t quantile function
  import("fPortfolio") ###portfolio optimization library
  export("cop_portf_opt") 
  
mix_cop_sim <- function(cop_pars, i, nsim){
  Cc <- Cg <- Ct <- matrix(0,nrow = nsim, ncol = 8)   #clayton, t, gumbel variates matrix
  ctg <- matrix(0, nrow = nsim, ncol = 8)              #copula mixture variates matrix
  ##generating copula variates
  Cc[,]<- cop_pars[[i]][[5]]*copula::rCopula(n = nsim, copula = claytonCopula(param = cop_pars[[i]][[1]], dim = 8))   
  Cg[,]<- cop_pars[[i]][[6]]*copula::rCopula(n = nsim, copula = gumbelCopula(param = cop_pars[[i]][[2]], dim = 8))
  Ct[,]<- cop_pars[[i]][[7]]*copula::rCopula(n = nsim, copula = tCopula(param = cop_pars[[i]][[3]], 
                                                                         df = cop_pars[[i]][[4]], dim = 8))
  ctg <- Cc + Ct + Cg #linear combination of them 
  return(ctg)
}

gauss_cop_sim <- function(cop_pars, i, nsim){
  Gcop <- matrix(0, nrow = nsim, ncol = 8)
  Gcop[,] <- copula::rCopula(n = nsim, copula = normalCopula(param = cop_pars[[i]], dim = 8))
  return(Gcop)
}

z_cop_sim <- function(garch_coefs, ctg, i, nsim){
  zsim <- matrix(0, nrow = nsim, ncol = 8) #'z' copula matrix, resulting from quantile of mixture 
  for(j in 1:8){  #for each asset, generate copula 'z' dependence strucutre, applying the Quantile function 
    #in the mixture of copulas
    # if(j==8 && anyNA(fGarch::qsstd(ctg[,j], nu = garch_coefs[[i]][[j]][[8]], xi =garch_coefs[[i]][[j]][[7]]))){
    #   garch_coefs[[i]][[j]][[7]] <- 1
    # }
    zsim[,j] <- fGarch::qsstd(ctg[,j], nu = garch_coefs[[i]][[j]][[8]], xi = garch_coefs[[i]][[j]][[7]]) / # garch_coefs[[i]][[j]][[7]]
      sd(fGarch::qsstd(ctg[,j], nu = garch_coefs[[i]][[j]][[8]], xi = garch_coefs[[i]][[j]][[7]])) #nu = t's DF, xi = t's skew
  }
  return(zsim)
}

ret_cop_sim <- function(garch_coefs, zsim, sigma_per, resid_per, returns, i, nsim){
  ret_sim <- matrix(0, nrow = nsim, ncol = 8)    #simulated returns matrix
  RZ<-returns[i:(1259+i),2:9] #getting real returns we'll use in one-step forward armaGarch's AR forecasting 
  for(j in 1:8){   #K scenarios generation for each asset
    sigma_f_t1 <- tail(sigma_per[[j]],1) ##(t-1) sigma for GARCH forecasting
    e_f_t2_t1 <- tail(resid_per[[j]],2) ##(t-2, t-1) residuals for MA forecasting
    for(z in 1:nsim){
      ret_sim[z,j] = ((garch_coefs[[i]][[j]][[1]] * RZ[1260,j]) + (garch_coefs[[i]][[j]][[2]] * RZ[1259,j]) +  #AR1 * R_t-1, AR2 * R_t-2
                     (garch_coefs[[i]][[j]][[3]] * e_f_t2_t1[1]) + (garch_coefs[[i]][[j]][[4]] * e_f_t2_t1[2]) +  #MA1*e_t-1, MA2*e_t-2
                     (zsim[z,j] * (sqrt(garch_coefs[[i]][[j]][[9]]) +  ##alfa0
                                     sqrt(garch_coefs[[i]][[j]][[5]]) * e_f_t2_t1[2] + ##alfa1 * e_t-1
                                     sqrt(garch_coefs[[i]][[j]][[6]]) * sigma_f_t1))) ##beta1  * s_t-1
    }
  }
  return(ret_sim)
}

cop_portf_opt <- function(targetReturn, filename, nsim, type){
  cop_pars <- readRDS("copulaParams.Rds") #reading copula parameters
  garch_coefs <- readRDS("coef.Rds") #reading ArmaGarch parameters
  #arma_order <- readRDS("armaOrder.Rds")
  sigma_fit <- readRDS("sigma.Rds") #reading armaGarch sigma. We need this to estimate J one-steap ahead returns 
  residual_fit <- readRDS("residuos.Rds")  #reading armaGach fitted residuals 
  
  #####setting up a fGarch min CVaR optimization
  frontierSpec <- portfolioSpec() 
  setType(frontierSpec) <- "CVaR"  
  setSolver(frontierSpec) <- "solveRglpk.CVAR"  #solving as a linear program as Rockafellar & Uryasev (2000)
  setAlpha(frontierSpec) <- 0.05   #CVaR_0.95  
  setTargetReturn(frontierSpec) <- targetReturn  #daily target return constrain, we do this for 0, 0.00012, 0.00024 and 0.00036
  
  ####initializing returns and weights matrixes
  ret_sim <- matrix(0, nrow = nsim, ncol = 8) #initializaing sim.ret matrix
  cvar_opt <- matrix(0, nrow = 4550, ncol = 8) #initializing optimized portfolio weights matrix
  
  for(i in 1:length(sigma_fit)){  #we do everything for each optimization
    ##generating copula variates 
    if(type == 'mixture'){         #if we want mixture of copula, type should be 'mixture'
      cop_sim <- mix_cop_sim(cop_pars, i, nsim)
    } else if(type == 'gaussian'){ #else, 'gaussian'
      cop_sim <- gauss_cop_sim(cop_pars, i, nsim)
    }
    ##generating zsim 
    zsim <- z_cop_sim(garch_coefs, cop_sim, i, nsim)
    #sigma_per <- sigma_fit[[i]]  #getting fitted sigma and residuals we'll use in MA and Garch's forecasting
    #resid_per <- residual_fit[[i]] 
    
    ##K return scenarios generation, using z and armaGarch
    ret_sim <- ret_cop_sim(garch_coefs, zsim, sigma_fit[[i]], residual_fit[[i]], returns, i, nsim)
    
    ##optimizing portfolio using K simulated return for each assets, for optimization period i 
    retornofPort <- as.timeSeries(ret_sim[, 1:8])
    frontier1g <- fPortfolio::efficientPortfolio(data = retornofPort, spec = frontierSpec, constraints = "LongOnly")
    cvar_opt[i,1:8] <- fPortfolio::getWeights(frontier1g)   #storing resulting weights   
  }
  saveRDS(cvar_opt, file = filename) ##saving weights data, we repeat this for 0,3,6 and 9%
}
})




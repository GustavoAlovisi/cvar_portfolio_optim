
##############################################################################################
#This code is for optimization of linear combination of copulas for each optimization step.  #
#With provided pseudo-uniform [0,1] data, a non-linear minimization of negative loglikelihood#
#is employed, using augmented Lagrange multiplayer solver (Seq. Quad. Prog. based)           #
##############################################################################################

cop_mix_fit_module <- module({

import("copula") #for estimating copulas
import("Rsolnp")  #for non linear optimization of linear combination of copulas
export("cop_mix_fit")

LLCG <- function(params,U, copC, copG, copt){ #negative log likelihood function for estimating copulae weights and parameters
  slot(copC, "parameters") <- params[1]    #Initial Clayton parameter provided
  slot(copG, "parameters") <- params[2]    #Initial Gumbel parameter provided 
  slot(copt, "parameters") <- params[3:4]  #Initial t parameters provided (correlation and Degrees of Freedom)
  pi1 <- params[5] #weight of Clayton copula
  pi2 <- params[6] #weight of Gumbel copula
  pi3 <- params[7] #weight of t copula
  
  opt <- log(pi1 * dCopula(U, copC) + pi2 * dCopula(U, copG)
             + pi3 * dCopula(U, copt))  ##loglikelihood function to be optimized
  if(any(is.infinite(opt))){            ##esse IF está no Pfaff
    opt[which(is.infinite(opt))]<-0
  }
  -sum(opt)
}

eqfun <- function(params,U,copC,copG,copt){ #constrain function so that sum(weights)=1
  z <- params[5]+params[6]+params[7]
  return(z)
}


cop_mix_fit <- function(){
#setwd("C:/Users/gusta/Desktop/tccGustavo/tccGustavo")
#X <- readRDS("unif_EmpDist.RDS")    #reading our empirical cumulative unif. distr.
  par_cdf <- readRDS("unif_ParDist.RDS") #reading our parametric [0,1] cdf
  weight_t <- vector('list', length(par_cdf))  #creating a list to store each period's parameters

  #initializing 8d t copula
  copt <- copula::tCopula(param = 0.5, dim = 8) # 

  ## Initializing archimedian copula objects
  copC <- copula::claytonCopula(2, dim = 8) # delta= 2
  copG <- copula::gumbelCopula(2, dim = 8)  # theta= 2

  lower <- c(0.1, 1, -0.9,(2+.Machine$double.eps), 0,0,0)     #lower and upper bounds of the parameters and weights for bounded non linear opt.
  upper <- c(copC@param.upbnd, copG@param.upbnd, 1,100, 1,1,1) #2+eps so that variance of t copula is defined


  for(i in i:length(par_cdf)){  ####for each (1:4550) optimization, we estimate copula parameters for data
    v<-as.matrix(do.call(cbind, par_cdf[[i]])) ##transforming in Matrix 
    U<-v[,1:8]  ##pseudo-uniform [0,1] observations for each asset
  
    ##Creating elliptical copula objects and estimating "initial guesses" for each copula parameter. 
    #Then, we maximize loglikelihood of the linear combination of the three copulas
    par1 <- copula::fitCopula(copC, U, "itau", estimate.variance = T)@estimate #inversion of Kendall's tau for Clayton 
    par2 <- copula::fitCopula(copG, U,"itau", estimate.variance = T)@estimate #inversion of Kendall's tau for Gumbel 
    par3 <- copula::fitCopula(copt, U,"mpl", estimate.variance = F)@estimate ###mpl para poder estimar tambem DF. Na documentacao diz que nao pode usar 'itau' pois ele n estima DF.
    par4 <- 1/3 #initial guesses for weights = 1/3 each
    par5 <- 1/3
    par6 <- 1/3
  
    ##non linear constrained optimization 
    opt <- Rsolnp::solnp(pars = c(par1,par2,par3,par4,par5,par6), fun = LLCG, LB = lower, 
                        UB = upper, copt=copt,copC = copC, copG = copG, U=U,eqfun = eqfun, 
                        eqB=c(1)) ####RSOLNP
  
    weight_t[[i]]<-opt$pars  ##saving optimization parameters in a list
  }
  saveRDS(weight_t, file = "copulaParams.Rds")  #saving resulting the parameters in a file
}
})#module ending
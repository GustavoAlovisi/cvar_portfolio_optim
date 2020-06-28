#library(stats)
#library(copula) ##used for pobs()
 ##used for the cumulative distribution function of a skewed t (parametric)


#########################################################################################################
#This piece of code transforms our data obtained in arma-garch estimation in a pseudo-unif [0,1] data   #
#to be used in copula estimation. We do this parametrically for every 1:4550 optimization step.         #
#Literature also consider using semi-parametric empirical cdf with copula:pobs()                        #
#########################################################################################################

#setwd("C:/Users/gusta/Desktop/tccGustavo/tccGustavo")
emp_dist_module <- module({
  
import("fGarch")
export("gen_emp_dist")

gen_emp_dist <- function(){
  unif <- vector('list', 4550 ) ##initializing an empty list to save data
  residuos <- readRDS("residuos.Rds") ##reading our data
  sigma <- readRDS("sigma.Rds")
  garch_coef <- readRDS("coef.Rds") 
  
  for(i in 1:length(residuos)){          ##generating Cdf with a parametric skewd t, given degrees of freedom and skewness 
    for(j in 1:8){                       ##for every asset in every 1:4500 optimization
      unif[[i]][[j]] = fGarch::psstd(q=residuos[[i]][[j]]/sigma[[i]][[j]],                           #xi = skew da t
                                   nu = garch_coef[[i]][[j]][[8]], xi = garch_coef[[i]][[j]][[7]]) #nu = Gl da t
    }
  }
  saveRDS(unif, file = "Unif_ParDist.Rds") ##saving cdf data in R data file
}
})#closing module

######alternatively, we can compte cdf non parametrically using copula::pobs() that is widely used in literature (packages)

###Obtaining empirical cdf (non parametric) for each index and period, using copula::pobs()
#for(i in 1:length(residuos)){
#  for(j in 1:8){
#    unif[[i]][[j]] = copula::pobs(residuos[[i]][[j]]/sigma[[i]][[j]])
#  }
#}

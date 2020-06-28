#########main
library(modules)

AG <- modules::use("1_armaGarchEst.R")
ED <- modules::use("2_empDistFit.R")

returns <- read.csv("returnsIndBr.csv")
returns <- returns[1:5809,]


AG$armaGarch_Est$arma_garch_fit_store(returns, per_est)

ED$emp_dist_module$gen_emp_dist()


a<- matrix(data = 1, nrow = 100, ncol = 150)
nrow(a)

armaGarch_Est

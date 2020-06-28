#########main
library(modules)

M1 <- modules::use("1_armaGarchEst.R")
M2 <- modules::use("2_empDistFit.R")
M3 <- modules::use("3_copMixFit.R")

returns <- read.csv("returnsIndBr.csv")
returns <- returns[1:5809,]


M1$armaGarch_est_module$arma_garch_fit_store(returns, per_est)

M2$emp_dist_module$gen_emp_dist()

M3$cop_mix_fit_module$cop_mix_fit()


a<- matrix(data = 1, nrow = 100, ncol = 150)
nrow(a)

armaGarch_Est

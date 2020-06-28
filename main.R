#########main
library(modules)

M1 <- modules::use("1_armaGarchEst.R")
M2 <- modules::use("2_empDistFit.R")
M3 <- modules::use("3_copMixFit.R")
M4 <- modules::use("4_mix_cop_gen_opt.R")

returns <- read.csv("returnsIndBr.csv")
returns <- returns[1:5809,]

##fitting arma-garch models
M1$armaGarch_est_module$arma_garch_fit_store(returns, per_est)

##generating empirical cdf/parametric cdf
M2$emp_dist_module$gen_emp_dist()

##estimating copula parameters
M3$cop_mix_fit_module$cop_mix_fit()

##generating simulated returns using copula-arma-garch and optimizing portfolios weights 
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00012, "mix_cop_wgt_3pct.Rds", 10000, type = 'mixture') #target return, filename, nsim, type
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00024, "mix_cop_wgt_6pct.Rds", 10000, type = 'mixture') #target return, filename, nsim
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00036, "mix_cop_wgt_9pct.Rds", 10000, type = 'mixture') #target return, filename, nsim

M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00012, "gauss_cop_wgt_3pct.Rds", 10000, type = 'gaussian')
M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00024, "gauss_cop_wgt_6pct.Rds", 10000, type = 'gaussian')
M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00036, "gauss_cop_wgt_9pct.Rds", 10000, type = 'gaussian')




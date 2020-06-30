#########main
library(modules)

M1 <- modules::use("1_armaGarchEst.R")
M2 <- modules::use("2_empDistFit.R")
M3 <- modules::use("3_copMixFit.R")
M4 <- modules::use("4_mix_cop_gen_opt.R")
M5 <- modules::use("5_portfolio_benchmark.R")

per_est <- 1260 #using 1260 days for model estimation

##reading log-returns data 
returns <- read.csv("returnsIndBr.csv")

##fitting arma-garch models
M1$armaGarch_est_module$arma_garch_fit_store(returns[1:5809], per_est)

##generating empirical cdf/parametric cdf
M2$emp_dist_module$gen_emp_dist()

##estimating copula parameters
M3$cop_mix_fit_module$cop_mix_fit()

##generating simulated returns using copula-arma-garch and optimizing portfolios weights 
#mixture 8d copula weight optimization
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00012, "mix_cop_wgt_3pct.Rds", 10000, type = 'mixture') #target return, filename, nsim, type
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00024, "mix_cop_wgt_6pct.Rds", 10000, type = 'mixture') #target return, filename, nsim
M4$cop_cvar_pf_opt_mod$cop_portf_opt(0.00036, "mix_cop_wgt_9pct.Rds", 10000, type = 'mixture') #target return, filename, nsim

#gaussian 8d copula weight optimization
M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00012, "gauss_cop_wgt_3pct.Rds", 10000, type = 'gaussian')
M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00024, "gauss_cop_wgt_6pct.Rds", 10000, type = 'gaussian')
M4$cop_cvar_pf_opt_mod$mix_cop_portf_opt(0.00036, "gauss_cop_wgt_9pct.Rds", 10000, type = 'gaussian')

##calculating out of sample returns with given weights
#out-sample using mixture copula optimized weights
ctg_return <- M5$portf_bench_mod$portf_out_sam_ret_cop(returns[1261:5810,,drop=F], "mix_cop_wgt_3pct.Rds")

#out-sample using gaussian copula optimized weights
gauss_return <- M5$portf_bench_mod$portf_out_sam_ret_cop(returns[1261:5810,,drop=F], "gauss_cop_wgt_3pct.Rds")

#equal weights out of sample returs
n1_return <- M5$portf_bench_mod$portf_out_sam_ret_1n(returns[1261:5810])

#ibov returns
ibov_return <- M5$portf_bench_mod$portf_out_sam_ret_1n(returns[1261:5818,4])

#binding in a returns matrix
returns_matrix <- M5$portf_bench_mod$create_returns_matrix(ctg_return, gauss_return, n1_return, 
                                                           ibov_return, c('ctg', 'gaussian', '1_n', 'ibov'))
#performance measures plots and numerical 
M5$portf_bench_mod$drawdown_plots(returns_matrix, "3% portfolio drawdowns")
M5$portf_bench_mod$draw_wealth(returns_matrix)
M5$portf_bench_mod$performance_measures(returns_matrix, "3pct_performance_measures.xlsx") #this saves in a file 







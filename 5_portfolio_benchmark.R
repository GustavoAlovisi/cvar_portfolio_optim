

##########################################################################################################
#This piece of code is used for assessing portfolio out-of-sample performance. Given a vector of optimal##
#weights and real out-of-sample returns, it calculates portfolio out-of-sample returns and performance  ##
#measures and plots. We do this for mix and gaussian portfolio.                                         ##
##########################################################################################################


portf_bench_mod <- module({
  import("PerformanceAnalytics") #used for pretty much all performance measures 
  import("openxlsx") #saving in xlsx 
  export("portf_out_sam_ret_cop") #function for calculating out-sample-portfolio-returns given optimal weights matrix
  export("portf_out_sam_ret_1n") #function for calculating out-sample-portfolioi-returns with no weights provided 
  export("create_returns_matrix") #function to bind and name portfolio (gaussian/ibov/1n...) calculated returns
  export("drawdown_plots") #function for ploting drawdowns of portfolio returns matrix  
  export("draw_wealth")  #function for wealth plotting 
  export("performance_measures") #function for saving numerical performance measures of out-sample-returns 
  
swap_weights <- function(weights, returns){ #this is an aux. function to swap weights needed for PerformanceAnalytics funs.
  time <- as.Date(returns[,1]) ###converting data-set's date into Date
  time2 <- head(time, -1)
  aux_date <- as.Date(c("2001-02-05","2001-02-06")) 
  weights_aux <- as.xts(weights[1:2,1:8],order.by = aux_date)
  weights_aux <- weights_aux[1,]
  weights <- as.xts(weights[-1,1:8], order.by = time2)
  weights <- rbind(weights_aux, weights)
  return(weights)
}

colname_assign <- function(data){ #aux. function that assigns letters to  data columns 
  colnames(data) <- letters[1:ncol(data)]
  return(data)
}

portf_out_sam_ret_cop <- function(returns, weights_rds_name){ #calculating out-sample-portfolio-returns given optimal weights matrix
  weights <- readRDS(weights_rds_name) #reading weights from file 
  weights <- swap_weighs(weights, returns) #swaping weights needed
  weights <- colname_assign(weights) #assigning letters to columns
  simple_ret <- exp(returns) - 1 #transforming log-returns to simple returns 
  simple_ret <- colname_assign(simple_ret) #assigning letters to match weights/returns
  out_sample_returns <- PerformanceAnalytics::Return.portfolio(simple_ret, weights, geometric = T, wealth.index = F)
  return(out_sample_returns) #calculating portfolio returns 
}

portf_out_sam_ret_1n <- function(returns){ #calculating 1n (equal-weights) portfolio returns 
  simple_ret <- exp(returns) - 1
  simple_ret <- colname_assign(simple_ret)
  out_sample_returns <- PerformanceAnalytics::Return.portfolio(simple_ret, geometric = T, wealth.index = F)
  return(out_sample_returns) 
}

create_returns_matrix <- function(outSamRet_3_ctg,outSamRet_3_gaus, outSamRet_3_1n, outSamRet_3_IBOV, matrix_colnames){ 
  returnsmatrix <- cbind(outSamRet_3_ctg, outSamRet_3_gaus, outSamRet_3_1n,outSamRet_3_IBOV) #creating a matrix of portfolio out-of-sample returns
  colnames(returnsmatrix9) <- matrix_colnames                                 #given optimal estimated weights
  return(returnsmatrix)
}


drawdown_plots <- function(returnsmatrix, title){ #drawdown plot of each portfolio out-sample-returns
  chart.Drawdown(returnsmatrix, legend.loc = "bottomright", 
                 colorset = rainbow6equal, main = title)
}

draw_wealth <- function(returnsmatrix){ #wealthplot of each portfolio out-sample-returns
  charts.PerformanceSummary(returnsmatrix) ##wealth + dd plot
}

performance_measures <- function(returnsmatrix, filename){ #numerical perfomance measures 
  bench<-NULL
  bench <- rbind(Return.annualized(returnsmatrix), sd.annualized(returnsmatrix),
                 VaR(returnsmatrix, method = "historical"), CVaR(returnsmatrix, method = "historical"), 
                 SemiDeviation(returnsmatrix), CDD(returnsmatrix, method = "historical"), 
                 maxDrawdown(returnsmatrix, method = "historical"),
                 AverageDrawdown(returnsmatrix, method = "historical"), AverageLength(returnsmatrix, method = "historical"),
                 SharpeRatio.annualized(returnsmatrix, Rf = 0), 
                 BurkeRatio(returnsmatrix, Rf = 0),
                 SortinoRatio(returnsmatrix, MAR = 0), UpsidePotentialRatio(returnsmatrix, MAR = 0), 
                 DownsideFrequency(returnsmatrix, MAR = 0),
                 CalmarRatio(returnsmatrix),
                 DrawdownDeviation(returnsmatrix),
                 Kappa(returnsmatrix,MAR=0, 1), 
                 OmegaSharpeRatio(returnsmatrix, MAR=0)) ###performance measures considered. 
                                                          #This is done using PerformanceAnalytics
  write.xlsx(bench, file = filename, colNames = T, rowNames = T) ##saving performance measures
}
})










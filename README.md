## Worst Case Mixture-Copula Mean-CVaR Portfolio Optimization: An Implementation for Brazilian Indexes

This repository implements the code used for my B.Sc. in Economics completion paper. 
The purpose of the work is to implement an ARMA(p,q)-GARCH(1,1) Mixture of Copula CVaR portfolio optimization. 
- Module 1 is used for ARMA-GARCH fitting on provided log-returns. 
- Module 2 transforms our ARMA-GARCH data into (0,1) CDF data, using ECDF or parametric CDF. 
- Module 3 uses (0,1) transformed data to estimate a linear combination of Clayton, t and Gumbel copula parameters, as well as a gaussian one. 
- Module 4 simulate log-returns using ARMA-GARCH fit and copula parameters, and optimizes a mean-CVaR portfolio. 
- Module 5 calculates out-of-sample performance of optimized weights and several plots and risk/returns benchmarks. 
- Main simply loads log-returns data and calls for module's functions. 

More information can be found at the complete paper in [Sabi UFRGS](https://lume.ufrgs.br/handle/10183/205651) or at a concise [PPT presentation](https://www.overleaf.com/read/hfsdtcnpqrmq). The code in this repo is an updated version of the code presented in my paper. Because resulting files are big, each module loads and saves information in local files. 




setwd("/home/asr/Desktop/ProjStat/Code/RandomForestExp")
oob <- readRDS("oob_experiment.rds")

#All use correctly specifed propensity
#Two first are not cross-fit but the first use the oob-predictions
#Last is cross-fit
gridExtra::grid.arrange(oob[[1]]$plotdist(),oob[[2]]$plotdist(),oob[[3]]$plotdist(), ncol = 3)
gridExtra::grid.arrange(oob[[1]]$plotci(),oob[[2]]$plotci(),oob[[3]]$plotci(), ncol = 1)





# 1. corr_spec_prp and mean_rf_ib, K = 2
# 2. prp_rf_ib and mean_rf_ib, K = 2
# 3. Increasing K of 1 10 .
# 4. Increasing K of 2 to 10.
rate_exp <- readRDS("rate_experiment.rds")
gridExtra::grid.arrange(rate_exp[[1]]$plotdist(10),rate_exp[[2]]$plotdist(10),rate_exp[[3]]$plotdist(10),
                        rate_exp[[4]]$plotdist(10), ncol = 2)

#The doubly RF seems to be overconservative, too high coverage
gridExtra::grid.arrange(rate_exp[[1]]$plotci(),rate_exp[[2]]$plotci(),rate_exp[[3]]$plotci(),
                        rate_exp[[4]]$plotci(), ncol = 2)
mse <- c(
mean((rate_exp[[1]]$TrueATE-rate_exp[[1]]$ATE)^2),mean((rate_exp[[2]]$TrueATE-rate_exp[[2]]$ATE)^2), 
mean((rate_exp[[3]]$TrueATE-rate_exp[[3]]$ATE)^2), mean((rate_exp[[4]]$TrueATE-rate_exp[[4]]$ATE)^2)
)*sqrt(rate_exp[[1]]$sim$n)
bias <- c(
  mean((rate_exp[[1]]$TrueATE-rate_exp[[1]]$ATE)),mean((rate_exp[[2]]$TrueATE-rate_exp[[2]]$ATE)), 
  mean((rate_exp[[3]]$TrueATE-rate_exp[[3]]$ATE)), mean((rate_exp[[4]]$TrueATE-rate_exp[[4]]$ATE))
)*sqrt(rate_exp[[1]]$sim$n)
variance <- c(var(rate_exp[[1]]$ATE),var(rate_exp[[2]]$ATE),var(rate_exp[[3]]$ATE),var(rate_exp[[4]]$ATE))

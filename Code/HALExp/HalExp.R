setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)

#SIMULATION SETTINGS
nsim <- 150
n <- 1000
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

hyperparams <- list(smoothness_orders = 1, num_knots = c(50,25,15))
prp_hal <- HAL$new(A~., name = "HAL prp")
mean_hal <- HAL$new(Y~., name = "HAL mean")
#Create list of Experiment objects to illustrate the importance of the rate criterion for convergence. 
#All optimize the parameters of the random forest automatically
#All use cross-fitting to eliminate bias from overfitting
# All using TMLE as the estimator
# 1. corr_spec_prp and mean_rf_ib
# 2. prp_rf_ib and mean_rf_ib
# 3. Increasing K of 1.
# 4. Increasing K of 2.

exps <- list(
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_hal, mean_lrn = mean_hal,cross_fit = 2), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_hal, mean_lrn = mean_hal,cross_fit = 10), n_sim = nsim)
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
  #Save results in folder 
  saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/HALExp/hal_experiment.rds")
}


readRDS("/home/asr/Desktop/ProjStat/Code/HALExp/hal_experiment.rds")



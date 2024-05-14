setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)

#SIMULATION SETTINGS
nsim <- 200
n <- 6000
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)


prp_rf_ib <- RF$new(A~., name = "RF prp",oob = FALSE, autotune =FALSE,list("num.trees" = 2000))
mean_rf_ib <- RF$new(Y~., name = "RF mean",oob = FALSE, autotune =FALSE, hyperparams = list("num.trees" = 2000))
prp_corr_spec <- GLM$new(A~w1+w2+w3, name = "GLM prp")
#Create list of Experiment objects to illustrate the importance of the rate criterion for convergence. 
#All optimize the parameters of the random forest automatically
#All use cross-fitting to eliminate bias from overfitting
# All using TMLE as the estimator
# 1. corr_spec_prp and mean_rf_ib
# 2. prp_rf_ib and mean_rf_ib
# 3. consider increasing K 

exps <- list(
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_corr_spec, mean_lrn = mean_rf_ib,cross_fit = 10), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = prp_rf_ib, mean_lrn = mean_rf_ib,cross_fit = 10), n_sim = nsim)
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
}

#Save results in folder 
saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/RandomForestExp/rate_experiment_K10.rds")

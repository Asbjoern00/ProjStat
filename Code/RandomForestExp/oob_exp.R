setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
#options(error = recover, warn = 2)

#SIMULATION SETTINGS
nsim <- 250
n <- 1000
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

rf_hyperparams <- list(
  num.tree = 1000
)
corr_spec_prp <- GLM$new(A ~ w1+w2+w3-1, name = "GLM corr. spec.")
mean_rf_oob <- RF$new(Y~., name = "RF mean OOB",oob = TRUE, hyperparams =rf_hyperparams)
mean_rf_ib <- RF$new(Y~., name = "RF mean",oob = FALSE, hyperparams =rf_hyperparams)

#Create list of Experiment objects to illustrate difference between using inbag and out of bag samples for random forests
# All using TMLE as the estimator
# 1. corr_spec_prp and mean_rf_oob
# 2. corr_spec_prp and mean_rf_ib
# 3. corr_spec_prp and mean_rf_ib but with cross-fit set to 2

exps <- list(
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = corr_spec_prp, mean_lrn = mean_rf_oob,cross_fit = FALSE) , n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = corr_spec_prp, mean_lrn = mean_rf_ib,cross_fit = FALSE), n_sim = nsim),
  Experiment$new(sim = sim, est = TMLE$new(prp_lrn = corr_spec_prp, mean_lrn = mean_rf_ib,cross_fit = 2), n_sim = nsim)
)

# For loop to run all experiments
for(i in 1:length(exps)){
  exps[[i]]$run()
}

#Save results in folder 
saveRDS(exps, file = "/home/asr/Desktop/ProjStat/Code/RandomForestExp/oob_experiment.rds")

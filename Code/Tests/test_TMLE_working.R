
setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
#options(error = recover)

#SIMULATION SETTINGS
nsim <- 250
n <- 500
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

miss_spec_prp <- GLM$new(A ~ w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10-1, name = "GLM misspec.")
corr_spec_prp <- GLM$new(A ~ w1+w2+w3, name = "GLM corr. spec.")
prp_reg <- GLMNet$new(A~., name = "GLMNet")


mean_lrn_glm <- GLM$new(Y ~ ., name = "glm")
#mean_lrn_rf <- RF$new(Y ~ ., name = "RF", hyperparams = hyperparams)
#mean_lrn_rf_ob <- RF$new(Y ~ ., name = "RF", oob = FALSE, hyperparams = hyperparams)
mean_lrn_rf_nob <- RF$new(Y ~ ., name = "RF", oob = FALSE, hyperparams = list(num.tree = 3000))
prp_lrn_rf_nob <- RF$new(A ~ ., name = "RF", oob = FALSE, hyperparams = list(num.tree = 3000))
#prp_lrn_rf_ob <- RF$new(A ~ ., name = "RF", oob = FALSE, hyperparams = hyperparams)




mean_lrn_misspec <- GLM$new(Y ~ A + w1 + w2 + w4 + w5 + w6 -1, name = "GLM misspec.")

# Misspecified Propensity Score and Misspecified Mean Learner
exps1 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = TMLE$new(prp_lrn = prp_reg, mean_lrn = mean_lrn_glm, cross_fit = FALSE))#,
)
exps1[[1]]$run(verbose = TRUE)
exps1[[1]]$plotci()


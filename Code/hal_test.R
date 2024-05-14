
setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)

#SIMULATION SETTINGS
nsim <- 100
n <- 800
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

hyperparams <- list(
  smoothness_orders = 1
)
miss_spec_prp <- GLM$new(A ~ w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10-1, name = "GLM misspec.")
corr_spec_prp <- GLM$new(A ~ w1+w2+w3, name = "GLM corr. spec.")
prp_reg <- GLMNet$new(A~., name = "GLMNet")
hal_prp <- HAL$new(A~., name = "HAL", hyperparams = hyperparams)
hal_mean <- HAL$new(Y~., name = "HAL", hyperparams = hyperparams)

mean_lrn_rf_ob <- RF$new(Y ~ ., name = "RF", oob = TRUE)


mean_lrn_misspec <- GLM$new(Y ~ A + w1 + w2 + w4 + w5 + w6 -1, name = "GLM misspec.")

# Misspecified Propensity Score and Misspecified Mean Learner
exps1 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = TMLE$new(prp_lrn = prp_reg, mean_lrn = hal_mean, cross_fit = FALSE)),
  exp_2 = Experiment$new(n_sims = nsim, sim = sim, est = TMLE$new(prp_lrn = prp_reg, mean_lrn = hal_mean, cross_fit = 2))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps1[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps1[[exp]]$cvrg
  K[exp] <- exps1[[exp]]$est$cross_fit
}
p1 <- cowplot::plot_grid(exps1[[1]]$plot(14), exps1[[2]]$plot(10))#, exps1[[3]]$plot(10))
p1

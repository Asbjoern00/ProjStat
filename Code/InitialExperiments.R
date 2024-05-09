setwd("/home/asr/Desktop/ProjStat/Code")
source("Experiment.R")
source("Estimator.R")
source("Simulator.R")
source("LearnerTypes.R")
options(error = recover)

#SIMULATION SETTINGS
nsim <- 400
n <- 6000
sim <- Simulator$new(n = n, sim_cov = sim_cov, sim_A = sim_A, sim_Y = sim_Y)

miss_spec_prp <- GLM$new(A ~ w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10-1, name = "GLM misspec.")
corr_spec_prp <- GLM$new(A ~ w1+w2+w3, name = "GLM corr. spec.")
prp_reg <- GLMNet$new(A~., name = "GLMNet")

mean_lrn_rf <- RF$new(Y ~ ., name = "RF")
mean_lrn_misspec <- GLM$new(Y ~ A + w1 + w2 + w4 + w5 + w6 -1, name = "GLM misspec.")

# Misspecified Propensity Score and Misspecified Mean Learner
exps1 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = FALSE)),
  exp_cf_2 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = 2)),
  exp_cf_3 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = 3))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps1[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps1[[exp]]$cvrg
  K[exp] <- exps1[[exp]]$est$cross_fit
}
p1 <- cowplot::plot_grid(exps1[[1]]$plot(10), exps1[[2]]$plot(10), exps1[[3]]$plot(10))
p1
cowplot::save_plot("p1.png", p1)

t1 <- tibble(cvg = cvg, K = K)
t1
saveRDS(t1, "t1.rds")

# Correctly specfied propensity Score and wrongly specified mean learner

exps2 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = FALSE)),
  exp_cf_2 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = 2)),
  exp_cf_3 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_misspec, cross_fit = 3))
)
cvg <- numeric(length(exps2))
K <- numeric(length(exps2))
for(exp in seq_along(exps2)){
  exps2[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps2[[exp]]$cvrg
  K[exp] <- exps2[[exp]]$est$cross_fit
}

p2 <- cowplot::plot_grid(exps2[[1]]$plot(10), exps2[[2]]$plot(10), exps2[[3]]$plot(10))
p2
cowplot::save_plot("p2.png", p2)

t2 <- tibble(cvg = cvg, K = K)
t2
saveRDS(t2, "t2.rds")


# misspecfied propensity Score and RF mean learner

exps3 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = FALSE)),
  exp_cf_2 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = 2)),
  exp_cf_3 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = miss_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = 3))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps3[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps3[[exp]]$cvrg
  K[exp] <- exps3[[exp]]$est$cross_fit
}

p3 <- cowplot::plot_grid(exps3[[1]]$plot(10), exps3[[2]]$plot(10), exps3[[3]]$plot(10))
p3
cowplot::save_plot("p3.png", p3)

t3 <- tibble(cvg = cvg, K = K)
t3
saveRDS(t3, "t3.rds")

# correctly specified propensity Score and RF mean learner

#options(error=browser)
exps4 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = FALSE)),
  exp_cf_2 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = 2)),
  exp_cf_3 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = corr_spec_prp, mean_lrn = mean_lrn_rf, cross_fit = 3))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps4[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps4[[exp]]$cvrg
  K[exp] <- exps4[[exp]]$est$cross_fit
}

p4 <- cowplot::plot_grid(exps4[[1]]$plot(10), exps4[[2]]$plot(10), exps4[[3]]$plot(10))
cowplot::save_plot("p4.png", p4)
p4

t4 <-tibble(cvg = cvg, K = K)
t4
saveRDS(t4, "t4.rds")

# glmnet propensity Score and RF mean learner

exps5 <- list(
  exp_no_cf = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = prp_reg, mean_lrn = mean_lrn_rf, cross_fit = FALSE)),
  exp_cf_2 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = prp_reg, mean_lrn = mean_lrn_rf, cross_fit = 2)),
  exp_cf_3 = Experiment$new(n_sims = nsim, sim = sim, est = Estimator$new(prp_lrn = prp_reg, mean_lrn = mean_lrn_rf, cross_fit = 3))
)
cvg <- numeric(length(exps1))
K <- numeric(length(exps1))
for(exp in seq_along(exps1)){
  exps5[[exp]]$run(verbose = TRUE)
  cvg[exp] <- exps5[[exp]]$cvrg
  K[exp] <- exps5[[exp]]$est$cross_fit
}

p5 <- cowplot::plot_grid(exps5[[1]]$plot(10), exps5[[2]]$plot(10), exps5[[3]]$plot(10))
p5
cowplot::save_plot("p5.png", p5)

t5 <- tibble(cvg = cvg, K = K)
saveRDS(t5, "t5.rds")
t5
